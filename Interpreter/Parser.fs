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

type ParseResult<'a> =
    | Success of 'a * ParserState
    | Failure of ParserError * ParserState


let bind (result: ParseResult<'a>) (fn: ParserState -> 'a -> ParseResult<'b>) =
    match result with
    | Success(value, state) -> fn state value
    | Failure(s1, parserState) -> Failure(s1, parserState)

let map (result: ParseResult<'a>) (fn: 'a -> 'b) =
    match result with
    | Success(value, state) -> Success(fn value, state)
    | Failure(s1, parserState) -> Failure(s1, parserState)



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
    // very similar to monadic parsers
    { Prefix: (ParserState -> ParseResult<Expr>) option
      Infix: (ParserState -> Expr -> ParseResult<Expr>) option
      Precedence: Precedence }

// sort of like combinators, maybe move to monadic approach to avoid nesting

// lots of nested maps -> make the result a functor ? or at least extract out common patterns

let nil (state: ParserState) : ParseResult<Expr> = Success(Literal(Unit()), state)

let boolean (state: ParserState) =
    let state = setLabel state "Boolean"

    match previous state with
    | Some { lexeme = Lexeme.Keyword "true" } -> Success((Literal(Bool true), state))
    | Some { lexeme = Lexeme.Keyword "false" } -> Success((Literal(Bool false), state))
    | _ -> Failure("Expect boolean.", state)

let string (state: ParserState) =
    let state = setLabel state "String"

    match previous state with
    | Some { lexeme = Lexeme.String s } -> Success(Literal(Literal.String s), state)
    | _ -> Failure("Expect string.", state)

let number (state: ParserState) =
    let state = setLabel state "number"

    match previous state with
    | Some { lexeme = Lexeme.Number n } -> Success(Literal(Literal.Number n), state)
    | _ -> Failure("Expect number.", state)

// let ident state =
//     let state = setLabel state "Ident"
//     match previous state with
//     | Some { lexeme = Lexeme.Identifier n } -> Success(Expr.Identifier n, state)
//     | _ -> Failure("Expect identifier.", state)


let rec getRule (lexeme: Lexeme): ParseRule =
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

and operatorRule (op: Operator): ParseRule =
    match op with
    | Operator.LeftParen ->
        { Prefix = Some leftParenPrefix
          Infix = Some call
          Precedence = Precedence.Call }
    | Operator.Minus ->
        { Prefix = Some unary
          Infix = Some binary
          Precedence = Precedence.Term }
    | Operator.Plus ->
        { Prefix = None
          Infix = Some binary
          Precedence = Precedence.Term }
    | Operator.Slash
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

// should keywords be encoded in the type system ? stored elsewhere ?
and keywordRule (kw: string): ParseRule =
    match kw with
    | "true"
    | "false" ->
        { Prefix = Some boolean
          Infix = None
          Precedence = Precedence.None }
    | "nil" ->
        { Prefix = Some nil
          Infix = None
          Precedence = Precedence.None }
    | _ ->
        { Prefix = None
          Infix = None
          Precedence = Precedence.None }


and expression (state: ParserState) (precedence: Precedence) : ParseResult<Expr> =
    let state = setLabel state "Expression"

    let result = parsePrefix state

    match result with
    | Success(expr, state) -> parsePrecedence precedence state expr
    | Failure _ as f -> f

and ident (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Ident"

    let name = previous state |> Option.get

    match peek state with
    | Some { lexeme = Lexeme.Operator Operator.Equal } ->
        let state = advance state

        match expression state Precedence.Assignment with
        | Success(value, state) -> Success(Assignment(name, value), state)
        | Failure _ as f -> f
    | _ -> Success(Expr.Identifier(name), state)

and parsePrefix (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Prefix"

    match peek state with
    | Some token ->
        match (getRule token.lexeme).Prefix with
        | Some prefixFn -> prefixFn <| advance state
        | None -> Failure("Prefix function not parsed", state)
    | None -> Failure("Unexpected end of input", state)


and parsePrecedence (precedence: Precedence) (state: ParserState) (leftExpr: Expr) : ParseResult<Expr> =
    let rec loop state expr =
        match peek state with
        | Some token ->
            let rule = getRule token.lexeme

            if int precedence <= int rule.Precedence then
                match rule.Infix with
                | Some infixFn ->
                    match infixFn (advance state) expr with
                    | Success(expr, state) -> loop state expr
                    | Failure _ as f -> f
                | None -> Success(expr, state)
            else
                Success(expr, state)
        | None -> Success(expr, state)

    loop state leftExpr

and binary (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let op = previous state |> Option.get
    let rule = getRule op.lexeme
    let nextPrecedence: Precedence = enum (int rule.Precedence + 1)

    match expression state nextPrecedence with
    | Success(right, state) -> Success(Binary(left, op, right), state)
    | Failure _ as f -> f

and unary (state: ParserState) : ParseResult<Expr> =
    let op = previous state |> Option.get

    match expression state Precedence.Unary with
    | Success(right, state) -> Success(Unary(op, right), state)
    | Failure _ as f -> f

and grouping (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Grouping"

    match expression state Precedence.None with
    | Success(expr, state) ->
        match nextToken state with
        | Some({ lexeme = Lexeme.Operator Operator.RightParen }, state) -> Success(Grouping expr, state)
        | _ -> Failure("Expect ')' after expression.", state)
    | Failure _ as f -> f

and call (state: ParserState) (callee: Expr) : ParseResult<Expr> =
    let state = setLabel state "Call"

    let rec loop state args =
        match peek state with
        | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
            let state = advance state
            match callee with
            | Expr.Identifier name ->
                Success(Call(name, args), state)
            | _ -> Failure("Can only call functions and variables.", state)
        | _ ->
            match expression state Precedence.None with
            | Success(arg, state) ->
                match peek state with
                | Some { lexeme = Lexeme.Comma } ->
                    let state = advance state
                    loop state (arg :: args)
                | _ ->
                    match peek state with
                    | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
                        let state = advance state
                        match callee with
                        | Expr.Identifier name ->
                            Success(Call(name, arg :: args), state)
                        | _ -> Failure("Can only call functions and variables.", state) // should epxressions be callable ? yes
                    | _ -> Failure("Expect ')' after arguments.", state)
            | Failure _ as f -> f

    loop state []

// single paraemter funcs arent parsed correct adance too far
and leftParenPrefix (state: ParserState) : ParseResult<Expr> =
    match peek state with
    | Some { lexeme = Lexeme.Identifier _ } ->
        match peek (advance state) with
        | Some { lexeme = Lexeme.Comma } -> functionExpr state
        | Some { lexeme = Lexeme.Operator Operator.RightParen } ->
            match peek (advance (advance state)) with
            | Some { lexeme = Lexeme.Operator Operator.Arrow } -> functionExpr state
            | _ -> grouping state
        | _ -> grouping state
    | _ -> grouping state

and functionExpr (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Function"

    let rec parseParameters (state: ParserState) (params': Token list) =
        printfn $"parseParameters: %A{peek state}"

        match peek state with
        | Some { lexeme = Lexeme.Operator Operator.RightParen } -> Success(List.rev params', advance state)
        | Some ({ lexeme = Lexeme.Identifier _ } as token) ->
            let state = advance state

            match peek state with
            | Some { lexeme = Lexeme.Operator Operator.RightParen } -> Success(List.rev (token :: params'), advance state)
            | Some { lexeme = Lexeme.Comma } -> parseParameters (advance state) (token :: params')
            | _ -> Failure("Expected ',' or ')'.", state)
        | _ -> Failure("Expected parameter name or ')'.", state)

    match parseParameters state [] with
    | Success(params', state) ->
        match peek state with
        | Some { lexeme = Lexeme.Operator Operator.Arrow } ->
            let state = advance state

            match expression state Precedence.Assignment with
            | Success(body, state) -> Success(Lambda(params', body), state)
            | Failure _ as f -> f
        | _ -> Failure("Expected '->' after parameter list.", state)
    | Failure(s, parserState) -> Failure(s, parserState)



let variableDeclaration (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Variable"

    match nextToken state with
    | Some({ lexeme = Lexeme.Identifier _ } as name, state) ->
        match nextToken state with
        | Some({ lexeme = Lexeme.Operator Operator.Equal }, state) ->
            match expression state Precedence.Assignment with
            | Success(expr, state) -> Success((VariableDeclaration(name, expr), state))
            | Failure(s1, parserState) -> Failure(s1, parserState)
        | _ -> Failure("Expect '=' after variable name.", state)
    | _ -> Failure("Expect variable name.", state)

let parseStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Statement"

    match peek state with
    | Some token ->
        match token.lexeme with
        | Lexeme.Keyword kw ->
            match kw with
            | "let" -> variableDeclaration (advance state)
            | _ ->
                match expression state Precedence.None with
                | Success(expr, state) -> Success(Expression expr, state)
                | Failure(s1, parserState) -> Failure(s1, parserState)
        | _ ->
            match expression state Precedence.None with
            | Success(expr, state) -> Success(Expression expr, state)
            | Failure(s1, parserState) -> Failure(s1, parserState)
    | None -> Failure("Unexpected end of input.", state)

let parseStmt (input: string) =
    let tokens = tokenize input

    let initialState = createParserState tokens
    let stmt = parseStatement initialState
    match stmt with
    | Success(stmt, _) -> stmt
    | Failure _ as f -> failwith $"{f}"

let parseProgram (state: ParserState) : ParseResult<Program> =
    let rec loop state stmts =
        match peek state with
        | Some _ ->
            match parseStatement state with
            | Success(stmt, state) -> loop state (stmt :: stmts)
            | Failure(s1, parserState) -> Failure(s1, parserState)
        | None -> Success(List.rev stmts, state)

    loop state []

let parseTokens (tokens: Token list) =
    let initialState = createParserState tokens
    let program = parseProgram initialState
    program

let parse (input: string) =
    let tokens = tokenize input

    match parseTokens tokens with
    | Success(program, _) -> program
    | Failure _ as f -> failwith $"{f}"
