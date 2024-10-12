module Vec3.Interpreter.Parser

open Token
open Grammar
open Scanner
open Vec3.Interpreter.Token

type Precedence =
    | None = 0
    | Assignment = 1
    | Or = 2
    | And = 3
    | Equality = 4
    | Comparison = 5
    | Term = 6
    | Factor = 7
    | Exponent = 8
    | Unary = 9
    | Index = 10
    | Call = 11
    | Primary = 12

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

type ParseResult<'a> = Result<ParserState * 'a, ParserError * ParserState>

let createParserState (tokens: Token list) : ParserState =
    { Tokens = tokens
      Position = 0
      Label = "Initial" }

let setLabel (state: ParserState) (label: string) : ParserState = { state with Label = label }

let getCurrentToken (state: ParserState) : Token option =
    List.tryItem state.Position state.Tokens

let peek = getCurrentToken

let advance (state: ParserState) : ParserState =
    { state with
        Position = state.Position + 1 }

let rewind (state: ParserState) : ParserState =
    { state with
        Position = state.Position - 1 }

let previous (state: ParserState) : Token option =
    List.tryItem (state.Position - 1) state.Tokens

let nextToken (state: ParserState) : (ParserState * Token) option =
    peek state |> Option.map (fun token -> (advance state, token))

type ParseRule =
    { Prefix: (ParserState -> ParseResult<Expr>) option
      Infix: (ParserState -> Expr -> ParseResult<Expr>) option
      Precedence: Precedence }

let expect (state: ParserState) (expected: Lexeme) : ParseResult<Lexeme> =
    match peek state with
    | Some { Lexeme = lexeme } when lexeme = expected -> Ok(state, lexeme)
    | Some { Lexeme = lexeme } -> Error($"Expected {expected}, got {lexeme}.", state)
    | None -> Error($"Expected {expected}, got end of input.", state)

// sort of like combinators, maybe move to monadic approach to avoid nesting
// lots of nested maps -> make the result a functor ? or at least extract out common patterns

let nil (state: ParserState) : ParseResult<Expr> = Ok(state, ELiteral(LUnit, TUnit))

let boolean (state: ParserState): ParseResult<Expr> =
    let state = setLabel state "Boolean"

    match previous state with
    | Some { Lexeme = Lexeme.Keyword True } -> Ok(state, ELiteral(LBool true, TBool))
    | Some { Lexeme = Lexeme.Keyword False } -> Ok(state, ELiteral(LBool false, TBool))
    | _ -> Error("Expect boolean.", state)

let string (state: ParserState): ParseResult<Expr> =
    let state = setLabel state "String"

    match previous state with
    | Some { Lexeme = Lexeme.String s } -> Ok(state, ELiteral(LString s, TString))
    | _ -> Error("Expect string.", state)

let number (state: ParserState): ParseResult<Expr> =
    let state = setLabel state "Number"

    match previous state with
    | Some { Lexeme = Lexeme.Number n } ->
        match n with
        | Number.Integer i -> Ok(state, ELiteral(LNumber(LInteger(i)), TInteger))
        | Number.Float f -> Ok(state, ELiteral(LNumber(LFloat(f)), TFloat))
        | Number.Rational(n, d) -> Ok(state, ELiteral(LNumber(LRational(n, d)), TRational))
        | Number.Complex(r, i) -> Ok(state, ELiteral(LNumber(LComplex(r, i)), TComplex))
    | _ -> Error("Expect number.", state)

let ident (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Ident"

    match previous state with
    | Some id -> Ok(state, EIdentifier(id, TInfer))
    | _ -> Error("Expect identifier.", state)

let rec getRule (lexeme: Lexeme) : ParseRule =
    let getOperatorRule (op: Operator) =
        match op with
        | Operator.LeftParen ->
            { Prefix = Some leftParen
              Infix = Some call
              Precedence = Precedence.Call }
        | Operator.LeftBrace ->
            { Prefix = Some block
              Infix = None
              Precedence = Precedence.None }
        | Operator.LeftBracket ->
            { Prefix = Some list
              Infix = Some index
              Precedence = Precedence.Index }
        | Operator.Plus
        | Operator.Minus ->
            { Prefix = Some unary
              Infix = Some binary
              Precedence = Precedence.Term }
        | Operator.Slash
        | Operator.Percent
        | Operator.Star ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Factor }
        | Operator.Caret
        | Operator.StarStar ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Exponent }
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
        | Operator.AmpersandAmpersand ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.And }
        | Operator.PipePipe ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Or }

        | _ ->
            { Prefix = None
              Infix = None
              Precedence = Precedence.None }

    let getKeywordRule (kw: Keyword) =
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
            { Prefix = Some ifElse
              Infix = Some ternary
              Precedence = Precedence.None }
        | In ->
            { Prefix = None
              Infix = None // todo, infix for list checking
              Precedence = Precedence.None }
        | And ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.And }
        | Or ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Or }
        | _ ->
            { Prefix = None
              Infix = None
              Precedence = Precedence.None }

    match lexeme with
    | Operator op -> getOperatorRule op
    | Lexeme.Keyword kw -> getKeywordRule kw

    | Lexeme.Number _ ->
        { Prefix = Some number
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.String _ ->
        { Prefix = Some string
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.Identifier _ ->
        { Prefix = Some ident
          Infix = None
          Precedence = Precedence.None }

    | _ ->
        { Prefix = None
          Infix = None
          Precedence = Precedence.None }

and expression (state: ParserState) (precedence: Precedence) : ParseResult<Expr> =
    let state = setLabel state "Expression"

    match prefix state with
    | Ok(state, expr) -> infix precedence state expr
    | Error _ as f -> f


and prefix (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Prefix"

    match peek state with
    | Some token ->
        match (getRule token.Lexeme).Prefix with
        | Some fn -> fn (advance state)
        | None -> Error("Prefix function not parsed", state)
    | None -> Error("Unexpected end of input", state)


and infix (precedence: Precedence) (state: ParserState) (leftExpr: Expr) : ParseResult<Expr> =
    let state = setLabel state "Infix"

    let rec loop (state: ParserState) (expr: Expr) : ParseResult<Expr> =
        match peek state with
        | Some token ->
            let rule = getRule token.Lexeme

            if int precedence <= int rule.Precedence then
                match rule.Infix with
                | Some fn -> fn (advance state) expr |> Result.bind (fun (state, expr) -> loop state expr)
                | None -> Ok(state, expr)
            else
                Ok(state, expr)
        | None -> Ok(state, expr)

    loop state leftExpr

and binary (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Binary"

    match previous state with
    | Some op ->
        let rule = getRule op.Lexeme
        let nextPrecedence: Precedence = enum (int rule.Precedence + 1)

        match expression state nextPrecedence with
        | Ok(state, right) -> Ok(state, EBinary(left, op, right, TInfer))
        | Error _ as f -> f
    | _ -> Error("Expected operator.", state)

and unary (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Unary"

    match previous state with
    | Some op ->
        match expression state Precedence.Unary with
        | Ok(state, expr) -> Ok(state, EUnary(op, expr, TInfer))
        | Error _ as f -> f
    | _ -> Error("Expected operator.", state)

and commaSeparatedList (state: ParserState) : ParseResult<Expr list> =
    let state = setLabel state "CommaSeparatedList"

    let rec loop (state: ParserState) (exprs: Expr list) : ParseResult<Expr list> =
        match expression state Precedence.None with
        | Ok(state, expr) ->
            match peek state with
            | Some { Lexeme = Lexeme.Comma } ->
                let state = advance state
                loop state (expr :: exprs)
            | _ -> Ok(state, List.rev (expr :: exprs))
        | Error err -> Error(err)

    loop state []

and list (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "List"

    let rec loop (state: ParserState) (exprs: Expr list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightBracket } ->
            let state = advance state
            Ok(state, EList(List.rev exprs, TInfer))
        | _ ->
            match commaSeparatedList state with
            | Ok(state, exprs) ->
                match peek state with
                | Some { Lexeme = Lexeme.Operator Operator.RightBracket } ->
                    let state = advance state
                    Ok(state,EList(List.rev exprs, TInfer))
                | _ -> Error("Expected ']'.", state)
            | Error err -> Error(err)

    loop state []

and grouping (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Grouping"

    match expression state Precedence.None with
    | Ok(state, expr) ->
        match nextToken state with
        | Some(state, { Lexeme = Lexeme.Operator Operator.RightParen }) -> Ok(state, EGrouping(expr, TInfer))
        | _ -> Error("Expect ')' after expression.", state)
    | Error _ as f -> f

and index (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Index"

    match expression state Precedence.None with
    | Ok(state, expr) ->
        match nextToken state with
        | Some(state, { Lexeme = Lexeme.Operator Operator.RightBracket }) -> Ok(state, EIndex(left, expr, TInfer))
        | _ -> Error("Expect ']' after index.", state)
    | Error _ as f -> f

and ifElse (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "If"

    match expression state Precedence.None with
    | Ok(state, condition) ->
        match peek state with
        | Some { Lexeme = Lexeme.Keyword Keyword.Then } ->
            let state = advance state

            match expression state Precedence.None with
            | Ok(state, thenBranch) ->
                match peek state with
                | Some { Lexeme = Lexeme.Keyword Keyword.Else } ->
                    let state = advance state

                    match expression state Precedence.None with
                    | Ok(state, elseBranch) -> Ok(state, EIf(condition, thenBranch, elseBranch, TInfer))
                    | Error _ as f -> f
                | _ -> Ok(state, EIf(condition, thenBranch, ELiteral(LUnit, TUnit), TUnit))
            | Error _ as f -> f
        | _ -> Error("Expect 'then' after condition.", state)
    | Error _ as f -> f

and ternary (state: ParserState) (trueBranch: Expr) : ParseResult<Expr> =
    match expression state Precedence.None with
    | Ok(state, condition) ->
        match peek state with
        | Some { Lexeme = Lexeme.Keyword Keyword.Else } ->
            let state = advance state

            match expression state Precedence.None with
            | Ok(state, falseBranch) -> Ok(state, EIf(condition, trueBranch, falseBranch, TInfer))
            | Error _ as f -> f
        | _ -> Error("Expect 'then' after condition.", state)
    | Error _ as f -> f

and call (state: ParserState) (callee: Expr) : ParseResult<Expr> =
    let state = setLabel state "Call"

    let rec loop (state: ParserState) (args: Expr list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
            let state = advance state
            Ok(state, ECall(callee, args, TInfer))
        | _ ->
            // replace with comma separated list
            match expression state Precedence.None with
            | Ok(state, arg) ->
                match peek state with
                | Some { Lexeme = Lexeme.Comma } ->
                    let state = advance state
                    loop state (arg :: args)
                | _ ->
                    match peek state with
                    | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
                        let state = advance state
                        Ok(state, ECall(callee, arg :: args, TInfer))
                    | _ -> Error("Can only call functions and variables.", state) // should epxressions be callable ? yes
            | Error _ as f -> f

    loop state []

// should clean this
and leftParen (state: ParserState) : ParseResult<Expr> =
    match peek state with
    | Some { Lexeme = Lexeme.Identifier _ } ->
        match peek (advance state) with
        | Some { Lexeme = Lexeme.Comma } -> lambda state
        | Some { Lexeme = Lexeme.Colon } -> lambda state
        | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
            match peek (advance (advance state)) with
            | Some { Lexeme = Lexeme.Operator Operator.Arrow } -> lambda state
            | Some { Lexeme = Lexeme.Colon } -> lambda state
            | Some { Lexeme = Lexeme.Operator Operator.LeftBrace } -> lambda state
            | _ -> grouping state
        | _ -> grouping state
    | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
        match peek (advance state) with
        | Some { Lexeme = Lexeme.Operator Operator.Arrow } -> lambda state
        | Some { Lexeme = Lexeme.Colon } -> lambda state
        | Some { Lexeme = Lexeme.Operator Operator.LeftBrace } -> lambda state
        | _ -> Ok(state, ELiteral(LUnit, TUnit))
    | _ -> grouping state

// and this
and lambda (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Function"

    let rec parseParameters (state: ParserState) (params': (Token * TType) list) : ParseResult<(Token * TType) list> =
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightParen } -> Ok(advance state, List.rev params')
        | Some({ Lexeme = Lexeme.Identifier _ } as token) ->
            let state = advance state

            let paramType, state =
                match peek state with
                | Some { Lexeme = Lexeme.Colon } ->
                    let state = advance state

                    match typeHint state with
                    | Ok(paramType, state) -> state, paramType
                    | Error _ -> TInfer, state
                | _ -> TInfer, state

            match peek state with
            | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
                Ok(advance state, List.rev ((token, paramType) :: params'))
            | Some { Lexeme = Lexeme.Comma } -> parseParameters (advance state) ((token, paramType) :: params')
            | _ -> Error("Expected ',' or ')'.", state)
        | _ -> Error("Expected parameter name or ')'.", state)

    match parseParameters state [] with
    | Ok(state, params') ->
        match peek state with
        | Some { Lexeme = Lexeme.Colon } ->
            let state = advance state

            match typeHint state with
            | Ok(state, returnType) ->
                match peek state with
                | Some { Lexeme = Lexeme.Operator Operator.Arrow } ->
                    let state = advance state

                    match expression state Precedence.Assignment with
                    | Ok(state, body) ->
                        let paramTypes = List.rev <| List.map snd params'
                        let paramNames = List.rev <| List.map fst params'
                        Ok(state, ELambda(paramNames, body, TFunction(paramTypes, returnType)))
                    | Error _ as f -> f
                | Some { Lexeme = Lexeme.Operator Operator.LeftBrace } ->
                    match block (advance state) with
                    | Ok(state, body) ->
                        let paramTypes = List.rev <| List.map snd params'
                        let paramNames = List.rev <| List.map fst params'
                        Ok(state, ELambda(paramNames, body, TFunction(paramTypes, returnType)))
                    | Error _ as f -> f
                | _ -> Error("Expected '->' after return type.", state)
            | Error(s1, parserState) -> Error(s1, parserState)

        | Some { Lexeme = Lexeme.Operator Operator.Arrow } ->
            let state = advance state

            match expression state Precedence.Assignment with
            | Ok(state, body) ->
                let paramTypes = List.rev <| List.map snd params'
                let paramNames = List.rev <| List.map fst params'
                Ok(state, ELambda(paramNames, body, TFunction(paramTypes, TInfer)))
            | Error _ as f -> f
        | Some { Lexeme = Lexeme.Operator Operator.LeftBrace } ->
            let state = advance state

            match block state with
            | Ok(state, body) ->
                let paramTypes = List.rev <| List.map snd params'
                let paramNames = List.rev <| List.map fst params'
                Ok(state, ELambda(paramNames, body, TFunction(paramTypes, TInfer)))
            | Error _ as f -> f

        | _ -> Error("Expected '->' after parameter list.", state)
    | Error(s, parserState) -> Error(s, parserState)

and funcType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "FunctionType"

    let rec parseParams (state: ParserState) (paramList: TType list) : ParseResult<TType list> =
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightParen } -> Ok(advance state, List.rev paramList)
        | _ ->
            match typeHint state with
            | Ok(state, param) ->
                match peek state with
                | Some { Lexeme = Lexeme.Comma } -> parseParams (advance state) (param :: paramList)
                | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
                    Ok(advance state, List.rev (param :: paramList))
                | _ -> Error("Expected ',' or ')'.", state)
            | Error(s1, parserState) -> Error(s1, parserState)

    match parseParams state [] with
    | Ok(state, paramList) ->
        match peek state with
        | Some { Lexeme = Lexeme.Colon } ->
            let state = advance state

            match typeHint state with
            | Ok(state, returnType) -> Ok(state, TFunction(paramList, returnType))
            | Error(s1, parserState) -> Error(s1, parserState)
        | _ -> Error("Expected ':' after parameter list.", state)
    | Error(s1, parserState) -> Error(s1, parserState)

and tensorType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "TensorType"

    match typeHint state with
    | Ok(state, innerType) ->
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightBracket } -> Ok(advance state, TTensor(innerType, DAny))
        | _ -> Error("Expected ']' after type.", state)
    | Error(s1, parserState) -> Error(s1, parserState)

// sumish types ? int | float etc with constrain type
and typeHint (state: ParserState) : ParseResult<TType> =
    match nextToken state with
    | Some (state, { Lexeme = Lexeme.Identifier "int" }) -> Ok(state, TInteger)
    | Some (state, { Lexeme = Lexeme.Identifier "float" }) -> Ok(state, TFloat)
    | Some (state, { Lexeme = Lexeme.Identifier "rational" }) -> Ok(state, TRational)
    | Some (state, { Lexeme = Lexeme.Identifier "complex" }) -> Ok(state, TComplex)
    | Some (state, { Lexeme = Lexeme.Identifier "bool" }) -> Ok(state, TBool)
    | Some (state, { Lexeme = Lexeme.Identifier "string" }) -> Ok(state, TString)
    | Some (state, { Lexeme = Lexeme.Identifier "unit" }) -> Ok(state, TUnit)
    | Some (state, { Lexeme = Lexeme.Identifier "never" }) -> Ok(state, TNever)
    | Some (state, { Lexeme = Lexeme.Identifier "any" }) -> Ok(state, TAny)
    | Some (state, { Lexeme = Lexeme.Operator Operator.LeftParen }) -> funcType state
    | Some (state, { Lexeme = Lexeme.Operator Operator.LeftBracket }) -> tensorType state
    | _ -> Error("Expected a type.", state)

and block (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Block"

    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightBrace } ->
            let state = advance state
            Ok(state, EBlock(List.rev stmts, TInfer))
        | _ ->
            match statement state with
            | Ok(state, stmt) -> loop state (stmt :: stmts)
            | Error(s1, parserState) -> Error(s1, parserState)

    loop state []

and varDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Variable"

    match nextToken state with
    | Some(state, ({ Lexeme = Lexeme.Identifier _ } as name)) ->
        let typeResult =
            match peek state with
            | Some { Lexeme = Lexeme.Colon } ->
                let state = advance state
                typeHint state
            | _ -> Ok(state, TInfer)

        match typeResult with
        | Ok(state, varType) ->
            match nextToken state with
            | Some(state, { Lexeme = Lexeme.Operator Operator.Equal }) ->
                match expression state Precedence.Assignment with
                | Ok(state, expr) -> Ok(state, SVariableDeclaration(name, expr, varType))
                | Error(s1, parserState) -> Error(s1, parserState)
            | _ -> Error("Expect '=' after variable name.", state)
        | Error(s1, parserState) -> Error(s1, parserState)
    | _ -> Error("Expect variable name.", state)

and printStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Print"

    match expression state Precedence.Assignment with
    | Ok(state, expr) -> Ok(state, SPrintStatement(expr, TUnit))
    | Error(s1, parserState) -> Error(s1, parserState)

and statement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Statement"

    match peek state with
    | Some token ->
        match token.Lexeme with
        | Lexeme.Keyword kw ->
            match kw with
            | Keyword.Let -> varDecl (advance state)
            | Keyword.Print -> printStatement (advance state)
            | _ ->
                match expression state Precedence.None with
                | Ok(state, expr) -> Ok(state, SExpression(expr, TInfer))
                | Error(s1, parserState) -> Error(s1, parserState)
        | _ ->
            match expression state Precedence.None with
            | Ok(state, expr) -> Ok(state, SExpression(expr, TInfer))
            | Error(s1, parserState) -> Error(s1, parserState)
    | None -> Ok((state, SExpression(ELiteral(LUnit, TUnit), TInfer)))

let statementUnsafe (input: string) =
    let tokens = tokenize input

    match tokens with
    | Ok tokens ->
        let initialState = createParserState tokens
        let stmt = statement initialState

        match stmt with
        | Ok(stmt, _) -> stmt
        | Error f -> raise (ParserException(f))
    | Error f -> raise (LexerException(f))

let parseStmt (input: string) =
    let tokens = tokenize input

    match tokens with
    | Ok tokens ->
        let initialState = createParserState tokens
        let stmt = statement initialState

        match stmt with
        | Ok(stmt, _) -> Ok(stmt)
        | Error f -> Error f
    | Error f -> raise (LexerException(f))

let parseProgram (state: ParserState) : ParseResult<Program> =
    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Program> =
        match peek state with
        | Some _ ->
            match statement state with
            | Ok(state, stmt) -> loop state (stmt :: stmts)
            | Error(s1, parserState) -> Error(s1, parserState)
        | None -> Ok(state, List.rev stmts)

    loop state []

let parseTokens (tokens: Token list) =
    let initialState = createParserState tokens
    let program = parseProgram initialState
    program

let parse (input: string) =
    let tokens = tokenize input

    match tokens with
    | Ok tokens -> parseTokens tokens
    | Error f -> raise (LexerException(f))

let parseFile (file: string) =
    let input = System.IO.File.ReadAllText(file)
    parse input

let formatParserError (error: ParserError) (state: ParserState) =
    let token = getCurrentToken state

    match token with
    | Some { Lexeme = name
             Position = { Line = l } } -> $"Error: {error} at line {l}, token {name}"
    | _ -> $"Error: {error}"
