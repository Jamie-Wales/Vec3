module Vec3.Interpreter.Token

type Number =
    | Float of float
    | Integer of int

type Operator =
    | Plus
    | Minus
    | Star
    | StarStar
    | Slash
    | EqualEqual
    | BangEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | LeftParen
    | RightParen
    | Bang
    | Equal
    | Arrow

type Lexeme =
    | Number of Number
    | String of string
    | Keyword of string
    | Operator of Operator
    | Identifier of string
    | Comma
    | Semicolon
    | Colon

type Token = { lexeme: Lexeme; line: int }

let operatorToString (op: Operator): string =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
    | StarStar -> "**"
    | Slash -> "/"
    | Equal -> "="
    | EqualEqual -> "=="
    | BangEqual -> "!="
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | LeftParen -> "("
    | RightParen -> ")"
    | Bang -> "!"
    | Arrow -> "->"

let numberToString (n: Number) =
    match n with
    | Float f -> $"Float({f})"
    | Integer i -> $"Integer({i})"

let lexemeToString (lex: Lexeme): string =
    match lex with
    | Number n -> $"Number(%s{numberToString n})"
    | String s -> $"String(\"%s{s}\")"
    | Keyword k -> $"Keyword(%s{k})"
    | Operator op -> $"Operator(%s{operatorToString op})"
    | Identifier i -> $"Identifier(%s{i})"
    | Comma -> ","
    | Semicolon -> ";"

let tokenToString (token: Token): string =
    $"{{ lexeme: %s{lexemeToString token.lexeme}; line: %d{token.line} }}"
