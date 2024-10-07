module Vec3.Interpreter.Parser

open Token
open Grammar
open Scanner
open System

type Precedence =
    | None = 0
    | Assignment = 1
    | Or = 2
    | And = 3
    | Equality = 4
    | Comparison = 5
    | Term = 6
    | Factor = 7
    | Unary = 8
    | Call = 9
    | Primary = 10

type ParserLabel = string
type ParserError = string

type ParserPosition =
    { CurrentLine: string
      Line: int
      Column: int }

type ParserState =
    { Tokens: Token list
      Position: int
      Label: ParserLabel }

exception ParserException of ParserError * ParserState

type ParseResult<'a> = Result<'a * ParserState, ParserError * ParserState>
// | Ok of 'a * ParserState
// | Error of ParserError * ParserState


let bind (result: ParseResult<'a>) (fn: ParserState -> 'a -> ParseResult<'b>) =
    match result with
    | Ok(value, state) -> fn state value
    | Error(s1, parserState) -> Error(s1, parserState)

let map (result: ParseResult<'a>) (fn: 'a -> 'b) =
    match result with
    | Ok(value, state) -> Ok(fn value, state)
    | Error(s1, parserState) -> Error(s1, parserState)

let flattenAndExtractDims (nestedList: 'a list list) =
    let flatList = List.collect id nestedList
    
    let outerDim = List.length nestedList
    let innerDim = 
        match nestedList with
        | [] -> 0
        | firstInnerList::_ -> List.length firstInnerList

    (flatList, outerDim, innerDim)

let createParserState tokens =
    { Tokens = tokens
      Position = 0
      Label = "Initial" }

let setLabel (state: ParserState) (label: string) : ParserState = { state with Label = label }

let getCurrentToken (state: ParserState) =
    List.tryItem state.Position state.Tokens

let advance (state: ParserState) : ParserState =
    { state with
        Position = state.Position + 1 }

let peek = getCurrentToken

let previous (state: ParserState) : Token option =
    List.tryItem (state.Position - 1) state.Tokens

let nextToken (state: ParserState) : (Token * ParserState) option =
    peek state |> Option.map (fun token -> (token, advance state))

type ParseRule =
    { Prefix: (ParserState -> ParseResult<Expr>) option
      Infix: (ParserState -> Expr -> ParseResult<Expr>) option
      Precedence: Precedence }

// sort of like combinators, maybe move to monadic approach to avoid nesting
// lots of nested maps -> make the result a functor ? or at least extract out common patterns

let nil (state: ParserState) : ParseResult<Expr> = Ok(ELiteral(LUnit, TUnit), state)

let boolean (state: ParserState) =
    let state = setLabel state "Boolean"

    match previous state with
    | Some { lexeme = Lexeme.Keyword True } -> Ok((ELiteral(LBool true, TBool), state))
    | Some { lexeme = Lexeme.Keyword False } -> Ok((ELiteral(LBool false, TBool), state))
    | _ -> Error("Expect boolean.", state)

let string (state: ParserState) =
    let state = setLabel state "String"

    match previous state with
    | Some { lexeme = Lexeme.String s } -> Ok(ELiteral(LString s, TString), state)
    | _ -> Error("Expect string.", state)

let number (state: ParserState) =
    let state = setLabel state "number"

    match previous state with
    | Some { lexeme = Lexeme.Number n } ->
        match n with
        | Number.Integer i -> Ok(ELiteral(LNumber(LInteger(i)), TInteger), state)
        | Number.Float f -> Ok(ELiteral(LNumber(LFloat(f)), TFloat), state)
        | Number.Rational(n, d) -> Ok(ELiteral(LNumber(LRational(n, d)), TRational), state)
        | Number.Complex(r, i) -> Ok(ELiteral(LNumber(LComplex(r, i)), TFloat), state)
    | _ -> Error("Expect number.", state)
// let ident state =
//     let state = setLabel state "Ident"
//     match previous state with
//     | Some { lexeme = Lexeme.Identifier n } -> Ok(Expr.Identifier n, state)
//     | _ -> Error("Expect identifier.", state)


let rec getRule (lexeme: Lexeme) : ParseRule =
    match lexeme with
    | Operator op -> operatorRule op
    | Lexeme.Number _ ->
        { Prefix = Some number
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.String _ ->
        { Prefix = Some string
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.Keyword kw -> keywordRule kw
    | Lexeme.Identifier _ ->
        { Prefix = Some ident
          Infix = None
          Precedence = Precedence.None }
    | _ ->
        { Prefix = None
          Infix = None
          Precedence = Precedence.None }

and operatorRule (op: Operator) : ParseRule =
    match op with
    | Operator.LeftParen ->
        { Prefix = Some leftParenPrefix
          Infix = Some call
          Precedence = Precedence.Call }
    | Operator.LeftBrace ->
        { Prefix = Some parseBlock
          Infix = None
          Precedence = Precedence.None }
    | Operator.LeftBracket ->
        { Prefix = Some parseList
          Infix = Some index 
          Precedence = Precedence.None }
    | Operator.Minus ->
        { Prefix = Some unary
          Infix = Some binary
          Precedence = Precedence.Term }
    | Operator.Plus ->
        { Prefix = Some unary
          Infix = Some binary
          Precedence = Precedence.Term }
    | Operator.Slash
    | Operator.Percent
    | Operator.StarStar
    | Operator.Star ->
        { Prefix = None
          Infix = Some binary
          Precedence = Precedence.Factor }
    | Operator.BangEqual
    | Operator.EqualEqual
    | Operator.Equal ->
        { Prefix = None
          Infix = Some binary
          Precedence = Precedence.Equality }
    | Operator.Greater
    | Operator.GreaterEqual
    | Operator.Less
    | Operator.LessEqual ->
        { Prefix = None
          Infix = Some binary
          Precedence = Precedence.Comparison }
    | Operator.Bang ->
        { Prefix = Some unary
          Infix = None
          Precedence = Precedence.None }

    | _ ->
        { Prefix = None
          Infix = None
          Precedence = Precedence.None }

and keywordRule (kw: Keyword) : ParseRule =
    match kw with
    | True
    | False ->
        { Prefix = Some boolean
          Infix = None
          Precedence = Precedence.None }
    | Nil ->
        { Prefix = Some nil
          Infix = None
          Precedence = Precedence.None }
    | If ->
        { Prefix = Some ifExpr
          Infix = Some ternary
          Precedence = Precedence.None }
    | In ->
        { Prefix = None
          Infix = None // todo, infix for list checking
          Precedence = Precedence.None }
    | _ ->
        { Prefix = None
          Infix = None
          Precedence = Precedence.None }


and expression (state: ParserState) (precedence: Precedence) : ParseResult<Expr> =
    let state = setLabel state "Expression"

    let result = parsePrefix state

    match result with
    | Ok(expr, state) -> parsePrecedence precedence state expr
    | Error _ as f -> f

and ident (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Ident"

    let name = previous state |> Option.get

    match peek state with
    | Some { lexeme = Lexeme.Operator Operator.Equal } ->
        let state = advance state

        match expression state Precedence.Assignment with
        | Ok(value, state) -> Ok(EAssignment(name, value, TInfer), state)
        | Error _ as f -> f
    | _ -> Ok(EIdentifier(name, TInfer), state)

and parsePrefix (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Prefix"

    match peek state with
    | Some token ->
        match (getRule token.lexeme).Prefix with
        | Some prefixFn -> prefixFn <| advance state
        | None -> Error("Prefix function not parsed", state)
    | None -> Error("Unexpected end of input", state)


and parsePrecedence (precedence: Precedence) (state: ParserState) (leftExpr: Expr) : ParseResult<Expr> =
    let rec loop state expr =
        match peek state with
        | Some token ->
            let rule = getRule token.lexeme

            if int precedence <= int rule.Precedence then
                match rule.Infix with
                | Some infixFn ->
                    match infixFn (advance state) expr with
                    | Ok(expr, state) -> loop state expr
                    | Error _ as f -> f
                | None -> Ok(expr, state)
            else
                Ok(expr, state)
        | None -> Ok(expr, state)

    loop state leftExpr

and binary (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let op = previous state |> Option.get
    let rule = getRule op.lexeme
    let nextPrecedence: Precedence = enum (int rule.Precedence + 1)

    match expression state nextPrecedence with
    | Ok(right, state) -> Ok(EBinary(left, op, right, TInfer), state)
    | Error _ as f -> f

and unary (state: ParserState) : ParseResult<Expr> =
    let op = previous state |> Option.get

    match expression state Precedence.Unary with
    | Ok(right, state) -> Ok(EUnary(op, right, TInfer), state)
    | Error _ as f -> f

and parseList (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "List"
    
    let rec loop state exprs =
        match peek state with
        | Some { lexeme = Lexeme.Operator Operator.RightBracket } ->
            let state = advance state
            Ok(EList(List.rev exprs, TInfer), state)
        | _ ->
            match expression state Precedence.None with
            | Ok(expr, state) ->
                match peek state with
                | Some { lexeme = Lexeme.Comma } ->
                    let state = advance state
                    loop state (expr :: exprs)
                | Some { lexeme = Lexeme.Operator Operator.RightBracket } ->
                    let state = advance state
                    Ok(EList(List.rev (expr :: exprs), TInfer), state)
                | _ -> Error("Expected ',' or ']'.", state)
            | Error _ as f -> f

    loop state []

and grouping (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Grouping"

    match expression state Precedence.None with
    | Ok(expr, state) ->
        match nextToken state with
        | Some({ lexeme = Lexeme.Operator Operator.RightParen }, state) -> Ok(EGrouping (expr, TInfer), state)
        | _ -> Error("Expect ')' after expression.", state)
    | Error _ as f -> f

and index (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Index"

    match expression state Precedence.None with
    | Ok(index, state) ->
        match nextToken state with
        | Some({ lexeme = Lexeme.Operator Operator.RightBracket }, state) -> Ok(EIndex(left, index, TInfer), state)
        | _ -> Error("Expect ']' after index.", state)
    | Error _ as f -> f

and ifExpr (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "If"

    match expression state Precedence.None with
    | Ok(condition, state) ->
        match peek state with
        | Some { lexeme = Lexeme.Keyword Keyword.Then } ->
            let state = advance state

            match expression state Precedence.None with
            | Ok(thenBranch, state) ->
                match peek state with
                | Some { lexeme = Lexeme.Keyword Keyword.Else } ->
                    let state = advance state

                    match expression state Precedence.None with
                    | Ok(elseBranch, state) -> Ok(EIf(condition, thenBranch, elseBranch, TInfer), state)
                    | Error _ as f -> f
                    // single if, need to fix type checker if this is allowed
                | _ -> Ok(EIf(condition, thenBranch, ELiteral(LUnit, TUnit), TUnit), state)
            | Error _ as f -> f
        | _ -> Error("Expect 'then' after condition.", state)
    | Error _ as f -> f

// let x = a if 1 < 2 else b
and ternary (state: ParserState) (trueBranch: Expr) : ParseResult<Expr> =
    match expression state Precedence.None with
    | Ok(condition, state) ->
        match peek state with
        | Some { lexeme = Lexeme.Keyword Keyword.Else } ->
            let state = advance state

            match expression state Precedence.None with
            | Ok(falseBranch, state) ->
                Ok(EIf(condition, trueBranch, falseBranch, TInfer), state)
            | Error _ as f -> f
        | _ -> Error("Expect 'then' after condition.", state)
    | Error _ as f -> f

and call (state: ParserState) (callee: Expr) : ParseResult<Expr> =
    let state = setLabel state "Call"

    let rec loop state args =
        match peek state with
        | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
            let state = advance state
            Ok(ECall(callee, args, TInfer), state)
        // match callee with
        // | EIdentifier name -> Ok(ECall(name, args), state)
        // | _ -> Error("Can only call functions and variables.", state)
        | _ ->
            match expression state Precedence.None with
            | Ok(arg, state) ->
                match peek state with
                | Some { lexeme = Lexeme.Comma } ->
                    let state = advance state
                    loop state (arg :: args)
                | _ ->
                    match peek state with
                    | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
                        let state = advance state
                        Ok(ECall(callee, arg :: args, TInfer), state)

                    // match callee with
                    // | EIdentifier name -> Ok(ECall(name, arg :: args), state)
                    | _ -> Error("Can only call functions and variables.", state) // should epxressions be callable ? yes
            | Error _ as f -> f

    loop state []

and leftParenPrefix (state: ParserState) : ParseResult<Expr> =
    match peek state with
    | Some { lexeme = Lexeme.Identifier _ } ->
        match peek (advance state) with
        | Some { lexeme = Lexeme.Comma } -> functionExpr state
        | Some { lexeme = Lexeme.Colon } -> functionExpr state
        | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
            match peek (advance (advance state)) with
            | Some { lexeme = Lexeme.Operator Operator.Arrow } -> functionExpr state
            | Some { lexeme = Lexeme.Colon } -> functionExpr state
            | Some { lexeme = Lexeme.Operator Operator.LeftBrace } -> functionExpr state
            | _ -> grouping state
        | _ -> grouping state
    | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
        match peek (advance state) with
        | Some { lexeme = Lexeme.Operator Operator.Arrow } -> functionExpr state
        | Some { lexeme = Lexeme.Colon } -> functionExpr state
        | Some { lexeme = Lexeme.Operator Operator.LeftBrace } -> functionExpr state
        | _ -> Ok(ELiteral(LUnit, TUnit), state)
    | _ -> grouping state

and functionExpr (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Function"

    let rec parseParameters (state: ParserState) (params': (Token * Grammar.Type) list) =
        match peek state with
        | Some { lexeme = Lexeme.Operator Operator.RightParen } -> Ok(List.rev params', advance state)
        | Some({ lexeme = Lexeme.Identifier _ } as token) ->
            let state = advance state

            let state, paramType =
                match peek state with
                | Some { lexeme = Lexeme.Colon } ->
                    let state = advance state

                    match parseType state with
                    | Ok(paramType, state) -> state, paramType
                    | Error _ -> state, TInfer
                | _ -> state, TInfer

            match peek state with
            | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
                Ok(List.rev ((token, paramType) :: params'), advance state)
            | Some { lexeme = Lexeme.Comma } -> parseParameters (advance state) ((token, paramType) :: params')
            | _ -> Error("Expected ',' or ')'.", state)
        | _ -> Error("Expected parameter name or ')'.", state)

    match parseParameters state [] with
    | Ok(params', state) ->
        match peek state with
        | Some { lexeme = Lexeme.Colon } ->
            let state = advance state

            match parseType state with
            | Ok(returnType, state) ->
                match peek state with
                | Some { lexeme = Lexeme.Operator Operator.Arrow } ->
                    let state = advance state

                    match expression state Precedence.Assignment with
                    | Ok(body, state) ->
                        let paramTypes = List.rev <| List.map snd params'
                        let paramNames = List.rev <| List.map fst params'
                        Ok(ELambda(paramNames, body, TFunction(paramTypes, returnType)), state)
                    | Error _ as f -> f
                | Some { lexeme = Lexeme.Operator Operator.LeftBrace } ->
                    match parseBlock (advance state) with
                    | Ok(body, state) ->
                        let paramTypes = List.rev <| List.map snd params'
                        let paramNames = List.rev <| List.map fst params'
                        Ok(ELambda(paramNames, body, TFunction(paramTypes, returnType)), state)
                    | Error _ as f -> f
                | _ -> Error("Expected '->' after return type.", state)
            | Error(s1, parserState) -> Error(s1, parserState)

        | Some { lexeme = Lexeme.Operator Operator.Arrow } ->
            let state = advance state

            match expression state Precedence.Assignment with
            | Ok(body, state) ->
                let paramTypes = List.rev <| List.map snd params'
                let paramNames = List.rev <| List.map fst params'
                Ok(ELambda(paramNames, body, TFunction(paramTypes, TInfer)), state)
            | Error _ as f -> f
        | Some { lexeme = Lexeme.Operator Operator.LeftBrace } ->
            let state = advance state
            match parseBlock state with
            | Ok(body, state) ->
                let paramTypes = List.rev <| List.map snd params'
                let paramNames = List.rev <| List.map fst params'
                Ok(ELambda(paramNames, body, TFunction(paramTypes, TInfer)), state)
            | Error _ as f -> f
            
        | _ -> Error("Expected '->' after parameter list.", state)
    | Error(s, parserState) -> Error(s, parserState)

and parseFunctionType (state: ParserState) : ParseResult<Grammar.Type> =
    let state = setLabel state "FunctionType"

    match peek state with
    | Some { lexeme = Lexeme.Operator Operator.LeftParen } ->
        let state = advance state

        let rec parseParams (state: ParserState) (paramList: Grammar.Type list) =
            match peek state with
            | Some { lexeme = Lexeme.Operator Operator.RightParen } -> Ok(List.rev paramList, advance state)
            | _ ->
                match parseType state with
                | Ok(param, state) ->
                    match peek state with
                    | Some { lexeme = Lexeme.Comma } -> parseParams (advance state) (param :: paramList)
                    | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
                        Ok(List.rev (param :: paramList), advance state)
                    | _ -> Error("Expected ',' or ')'.", state)
                | Error(s1, parserState) -> Error(s1, parserState)

        match parseParams state [] with
        | Ok(paramList, state) ->
            match peek state with
            | Some { lexeme = Lexeme.Colon } ->
                let state = advance state

                match parseType state with
                | Ok(returnType, state) -> Ok(TFunction(paramList, returnType), state)
                | Error(s1, parserState) -> Error(s1, parserState)
            | _ -> Error("Expected ':' after parameter list.", state)
        | Error(s1, parserState) -> Error(s1, parserState)
    | _ -> Error("Expected '(' before function type.", state)

// sumish types ? int | float etc with constrain type
and parseType (state: ParserState) : ParseResult<Grammar.Type> =
    match peek state with
    | Some { lexeme = Lexeme.Identifier "int" } -> Ok(TInteger, advance state)
    | Some { lexeme = Lexeme.Identifier "float" } -> Ok(TFloat, advance state)
    | Some { lexeme = Lexeme.Identifier "rational" } -> Ok(TRational, advance state)
    | Some { lexeme = Lexeme.Identifier "complex" } -> Ok(TComplex, advance state)
    | Some { lexeme = Lexeme.Identifier "bool" } -> Ok(TBool, advance state)
    | Some { lexeme = Lexeme.Identifier "string" } -> Ok(TString, advance state)
    | Some { lexeme = Lexeme.Identifier "unit" } -> Ok(TUnit, advance state)
    | Some { lexeme = Lexeme.Identifier "never" } -> Ok(TNever, advance state)
    | Some { lexeme = Lexeme.Identifier "any" } -> Ok(TAny, advance state)
    | Some { lexeme = Lexeme.Operator Operator.LeftParen } -> parseFunctionType state
    | Some { lexeme = Lexeme.Operator Operator.LeftBracket } ->
        let state = advance state

        match parseType state with
        | Ok(innerType, state) ->
            match peek state with
            | Some { lexeme = Lexeme.Operator Operator.RightBracket } -> Ok(TTensor(innerType, DAny), advance state)
            | _ -> Error("Expected ']' after type.", state)
        | Error(s1, parserState) -> Error(s1, parserState)
    | _ -> Error("Expected a type after colon.", state)

and parseBlock (state : ParserState) : ParseResult<Expr> =
    let state = setLabel state "Block"

    let rec loop state stmts =
        match peek state with
        | Some { lexeme = Lexeme.Operator Operator.RightBrace } ->
            let state = advance state
            Ok(EBlock(List.rev stmts, TInfer), state)
        | _ ->
            match parseStatement state with
            | Ok(stmt, state) -> loop state (stmt :: stmts)
            | Error(s1, parserState) -> Error(s1, parserState)

    loop state []

and variableDeclaration (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Variable"

    match nextToken state with
    | Some({ lexeme = Lexeme.Identifier _ } as name, state) ->
        let typeResult =
            match peek state with
            | Some { lexeme = Lexeme.Colon } ->
                let state = advance state
                parseType state
            | _ -> Ok(TInfer, state)

        match typeResult with
        | Ok(varType, state) ->
            match nextToken state with
            | Some({ lexeme = Lexeme.Operator Operator.Equal }, state) ->
                match expression state Precedence.Assignment with
                | Ok(expr, state) -> Ok((SVariableDeclaration(name, expr, varType), state))
                | Error(s1, parserState) -> Error(s1, parserState)
            | _ -> Error("Expect '=' after variable name.", state)
        | Error(s1, parserState) -> Error(s1, parserState)
    | _ -> Error("Expect variable name.", state)

and printStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Print"

    match expression state Precedence.Assignment with
    | Ok(expr, state) -> Ok((SPrintStatement(expr, TUnit), state))
    | Error(s1, parserState) -> Error(s1, parserState)

and parseStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Statement"

    match peek state with
    | Some token ->
        match token.lexeme with
        | Lexeme.Keyword kw ->
            match kw with
            | Keyword.Let -> variableDeclaration (advance state)
            | Keyword.Print -> printStatement (advance state)
            | _ ->
                match expression state Precedence.None with
                | Ok(expr, state) -> Ok(SExpression (expr, TInfer), state)
                | Error(s1, parserState) -> Error(s1, parserState)
        | _ ->
            match expression state Precedence.None with
            | Ok(expr, state) -> Ok(SExpression (expr, TInfer), state)
            | Error(s1, parserState) -> Error(s1, parserState)
    | None -> Ok((SExpression(ELiteral(LUnit, TUnit), TInfer), state))

let parseStmtUnsafe (input: string) =
    let tokens = tokenize input

    let initialState = createParserState tokens
    let stmt = parseStatement initialState

    match stmt with
    | Ok(stmt, _) -> stmt
    | Error f -> raise (ParserException(f))

let parseStmt (input: string) =
    let tokens = tokenize input

    let initialState = createParserState tokens
    parseStatement initialState


let parseProgram (state: ParserState) : ParseResult<Program> =
    let rec loop state stmts =
        match peek state with
        | Some _ ->
            match parseStatement state with
            | Ok(stmt, state) -> loop state (stmt :: stmts)
            | Error(s1, parserState) -> Error(s1, parserState)
        | None -> Ok(List.rev stmts, state)

    loop state []

let parseTokens (tokens: Token list) =
    let initialState = createParserState tokens
    let program = parseProgram initialState
    program

let parse (input: string) =
    let tokens = tokenize input

    // match parseTokens tokens with
    // | Ok(program, _) -> program
    // | Error _ as f -> failwith $"{f}"
    parseTokens tokens

let parseFile (file: string) =
    let input = System.IO.File.ReadAllText(file)
    parse input

let formatParserError (error: ParserError) (state: ParserState) =
    let token = getCurrentToken state

    match token with
    | Some { lexeme = name; line = l } -> $"Error: {error} at line {l}, token {name}"
    | _ -> $"Error: {error}"
