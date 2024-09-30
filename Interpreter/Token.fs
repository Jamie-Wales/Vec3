module Vec3.Interpreter.Token

type Number =
    | Float of float
    | Integer of int

type Operator =
    | Plus
    | Minus
    | Star
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

type Lexeme =
    | Number of Number
    | String of string
    | Keyword of string
    | Operator of Operator
    | Identifier of string 

type Token = { lexeme: Lexeme; line: int }

let numberToString (n: Number) =
    match n with
    | Float f -> $"Float({f})"
    | Integer i -> $"Integer({i})"

let operatorToString (op: Operator): string =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
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

let lexemeToString (lex: Lexeme): string =
    match lex with
    | Number n -> $"Number(%s{numberToString n})"
    | String s -> $"String(\"%s{s}\")"
    | Keyword k -> $"Keyword(%s{k})"
    | Operator op -> $"Operator(%s{operatorToString op})"
    | Identifier i -> $"Identifier(%s{i})"

let tokenToString (token: Token): string =
    $"{{ lexeme: %s{lexemeToString token.lexeme}; line: %d{token.line} }}"
