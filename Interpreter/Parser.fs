module Vec3.Interpreter.Parser

open Token
open Grammar

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

type ParserState = { Tokens: Token list; Position: int }

let createParserState tokens = { Tokens = tokens; Position = 0 }

let getCurrentToken state =
    if state.Position < List.length state.Tokens then
        Some state.Tokens.[state.Position]
    else
        None

let advance state =
    { state with
        Position = state.Position + 1 }

let peek state = getCurrentToken state

let previous state =
    if state.Position > 0 then
        Some state.Tokens.[state.Position - 1]
    else
        None

let matchToken state =
    match peek state with
    | Some token -> Some(token, advance state)
    | None -> None

type ParseRule =
    { Prefix: (ParserState -> Expr * ParserState) option
      Infix: (ParserState -> Expr -> Expr * ParserState) option
      Precedence: Precedence }

let rec getRule lexeme =
    match lexeme with
    | Lexeme.Operator op ->
        match op with
        | LeftParen ->
            { Prefix = Some grouping
              Infix = None
              Precedence = Precedence.None }
        | Minus ->
            { Prefix = Some unary
              Infix = Some binary
              Precedence = Precedence.Term }
        | Plus ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Term }
        | Slash
        | Star ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Factor }
        | BangEqual
        | Equal ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Equality }
        | Greater
        | GreaterEqual
        | Less
        | LessEqual ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Comparison }
        | _ ->
            { Prefix = None
              Infix = None
              Precedence = Precedence.None }
    | Lexeme.Number _ ->
        { Prefix = Some number
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.String _ ->
        { Prefix = Some string
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.Keyword kw ->
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
    | _ ->
        { Prefix = None
          Infix = None
          Precedence = Precedence.None }

and parse state = expression state Precedence.None
and expression state precedence =
    let (prefixExpr, state) = parsePrefix state
    parsePrecedence state precedence prefixExpr
and parsePrefix state =
    match peek state with
    | Some token ->
        let rule = getRule token.lexeme
        match rule.Prefix with
        | Some prefixFn -> prefixFn (advance state)
        | None -> failwith "Expect expression."
    | None -> failwith "Unexpected end of input."
and parsePrecedence state precedence leftExpr =
    let rec loop state expr =
        match peek state with
        | Some token ->
            let rule = getRule token.lexeme
            if int precedence <= int rule.Precedence then
                match rule.Infix with
                | Some infixFn ->
                    let (newExpr, newState) = infixFn (advance state) expr
                    loop newState newExpr
                | None -> (expr, state)
            else
                (expr, state)
        | None -> (expr, state)
    loop state leftExpr
and binary state left =
    let op = previous state |> Option.get
    let rule = getRule op.lexeme
    let nextPrecedence = enum<Precedence> (int rule.Precedence + 1)
    let right, newState = expression state nextPrecedence
    (Binary(left, op, right), newState)
and unary state =
    let op = previous state |> Option.get
    let right, newState = expression state Precedence.Unary
    (Unary(op, right), newState)
and grouping state =
    let expr, state = expression state Precedence.None
    match matchToken state with
    | Some({ lexeme = Lexeme.Operator RightParen }, newState) -> (Grouping expr, newState)
    | _ -> failwith "Expect ')' after expression."
and number state =
    match previous state with
    | Some { lexeme = Lexeme.Number n } -> (Literal(Number n), state)
    | _ -> failwith "Expect number."
and string state =
    match previous state with
    | Some { lexeme = Lexeme.String s } -> (Literal(String s), state)
    | _ -> failwith "Expect string."
and boolean state =
    match previous state with
    | Some { lexeme = Lexeme.Keyword "true" } -> (Literal(Bool true), state)
    | Some { lexeme = Lexeme.Keyword "false" } -> (Literal(Bool false), state)
    | _ -> failwith "Expect boolean."
and nil state = (Literal(Nil), state)
let parseTokens tokens =
    let initialState = createParserState tokens
    let (expr, _) = parse initialState
    expr

