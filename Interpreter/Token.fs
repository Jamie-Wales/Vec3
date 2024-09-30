module Vec3.Interpreter.Token


type Number =
    | Float of float
    | Integer of int

type Operator =
    | Plus
    | Minus
    | Star
    | Slash
    | Equal
    | BangEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | LeftParen
    | RightParen
    | Bang

type Lexeme =
    | Number of Number
    | String of string
    | Keyword of string
    | Operator of Operator
    | Identifier of string 

type Token = { lexeme: Lexeme; line: int }

let numberToString (n: Number) =
    match n with
    | Float f -> sprintf "Float(%f)" f
    | Integer i -> sprintf "Integer(%d)" i

let operatorToString (op: Operator) =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Star -> "*"
    | Slash -> "/"
    | Equal -> "=="
    | BangEqual -> "!="
    | Less -> "<"
    | LessEqual -> "<="
    | Greater -> ">"
    | GreaterEqual -> ">="
    | LeftParen -> "("
    | RightParen -> ")"
    | Bang -> "!"

let lexemeToString (lex: Lexeme) =
    match lex with
    | Number n -> $"Number(%s{numberToString n})"
    | String s -> $"String(\"%s{s}\")"
    | Keyword k -> $"Keyword(%s{k})"
    | Operator op -> $"Operator(%s{operatorToString op})"
    | Identifier i -> $"Identifier(%s{i})"

let tokenToString (token: Token) =
    $"{{ lexeme: %s{lexemeToString token.lexeme}; line: %d{token.line} }}"
