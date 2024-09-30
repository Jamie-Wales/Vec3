module Vec3.Interpreter.Parser

open Token
open Grammar
open Scanner

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

type ParserState = { Tokens: Token list; Position: int }

type ParseResult<'a> =
    | Success of 'a * ParserState
    | Failure of ParserLabel * ParserError * ParserState

let createParserState tokens = { Tokens = tokens; Position = 0 }

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

let nil (state: ParserState) : ParseResult<Expr> = Success (Literal(Unit), state)

let boolean state =
    match previous state with
    | Some { lexeme = Lexeme.Keyword "true" } -> Success((Literal(Bool true), state))
    | Some { lexeme = Lexeme.Keyword "false" } -> Success((Literal(Bool false), state))
    | _ -> Failure ("Boolean", "Expect boolean.", state)

let string state =
    match previous state with
    | Some { lexeme = Lexeme.String s } -> Success(Literal(Literal.String s), state)
    | _ -> Failure ("String", "Expect string.", state)

let number state =
    match previous state with
    | Some { lexeme = Lexeme.Number n } -> Success(Literal(Literal.Number n), state)
    | _ -> Failure ("Number", "Expect number.", state)

let ident state =
    match previous state with
    | Some { lexeme = Lexeme.Identifier n } -> Success(Expr.Identifier n, state)
    | _ -> Failure ("Ident", "Expect identifier.", state)


let rec getRule (lexeme: Lexeme) =
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
        {
            Prefix = Some ident
            Infix = None
            Precedence = Precedence.None }

and operatorRule op =
    match op with
    | Operator.LeftParen ->
        { Prefix = Some grouping
          Infix = None
          Precedence = Precedence.None }
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

and keywordRule kw =
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


and expression (state: ParserState) (precedence: Precedence): ParseResult<Expr> =
    let result = parsePrefix state
    match result with
    | Success (expr, state) ->  parsePrecedence state precedence expr
    | Failure _ as f -> f

and parsePrefix (state: ParserState): ParseResult<Expr> =
    match peek state with
    | Some token ->
        match (getRule token.lexeme).Prefix with
        | Some prefixFn -> prefixFn <| advance state
        | None -> Failure ("Prefix", "Prefix function not parsed", state)
    | None -> Failure( "parse prefix", "Unexpected end of input", state)


and parsePrecedence (state: ParserState) (precedence: Precedence) (leftExpr: Expr): ParseResult<Expr> =
    let rec loop state expr =
        match peek state with
        | Some token ->
            let rule = getRule token.lexeme

            if int precedence <= int rule.Precedence then
                match rule.Infix with
                | Some infixFn ->
                    match infixFn (advance state) expr with
                    | Success (expr, state) -> loop state expr
                    | Failure _ as f -> f
                | None -> Success (expr, state)
            else
                Success (expr, state)
        | None -> Success (expr, state)

    loop state leftExpr

and binary (state: ParserState) (left: Expr): ParseResult<Expr> =
    let op = previous state |> Option.get
    let rule = getRule op.lexeme
    let nextPrecedence: Precedence = enum (int rule.Precedence + 1)
    match expression state nextPrecedence with
    | Success(right, state) -> Success (Binary(left, op, right), state)
    | Failure _ as f -> f

and unary (state: ParserState): ParseResult<Expr> =
    let op = previous state |> Option.get
    match expression state Precedence.Unary with
    | Success(right, state) -> Success (Unary(op, right), state)
    | Failure _ as f -> f

and grouping (state: ParserState): ParseResult<Expr> =
    match expression state Precedence.None with
    | Success(expr, state) -> 
        match nextToken state with
        | Some({ lexeme = Lexeme.Operator Operator.RightParen }, state) -> Success (Grouping expr, state)
        | _ -> Failure ("Grouping", "Expect ')' after expression.", state)
    | Failure _ as f -> f


let variableDeclaration (state: ParserState): ParseResult<Stmt> =
    match nextToken state with
    | Some({ lexeme = Lexeme.Identifier _ } as name, state) ->
        match nextToken state with
        | Some({ lexeme = Lexeme.Operator Operator.Equal }, state) ->
            match expression state Precedence.Assignment with
            | Success(expr, state) -> Success ((Assignment(name, expr), state))
            | Failure(s, s1, parserState) -> Failure(s,s1,parserState)
        | _ -> Failure ("Variable", "Expect '=' after variable name.", state)
    | _ -> Failure("Variable", "Expect variable name.", state)

let parseStatement (state: ParserState) : ParseResult<Stmt> =
    match peek state with
    | Some token ->
        match token.lexeme with
        | Lexeme.Keyword kw ->
            match kw with
            | "let" -> variableDeclaration (advance state)
            | _ ->
                match expression state Precedence.None with
                | Success(expr, state) -> Success (Expression expr, state)
                | Failure(s, s1, parserState) -> Failure (s,s1,parserState)
        | _ -> 
                match expression state Precedence.None with
                | Success(expr, state) -> Success (Expression expr, state)
                | Failure(s, s1, parserState) -> Failure (s,s1,parserState)
    | None -> Failure("Statement", "Unexpected end of input.", state)

let parseProgram (state: ParserState) =
    let rec loop state stmts =
        match peek state with
        | Some _ ->
            match parseStatement state with
            | Success(stmt, state) -> loop state (stmt :: stmts)
            | Failure(s, s1, parserState) -> failwith $"{s}, {s1}, {parserState}"
        | None -> List.rev stmts

    loop state []

let parseTokens (tokens: Token list) =
    let initialState = createParserState tokens
    let program = parseProgram initialState
    program

let parse (input: string) =
    let tokens = tokenize input
    parseTokens tokens
