module Vec3.Interpreter.Token

type Number =
    | Float of float
    | Integer of int
    | Rational of int * int
    | Complex of float * float

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
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | Dot
    
type Keyword =
    | Let
    | If
    | Then
    | Else
    | For
    | True
    | False
    | Nil

type BuiltInFunction =
    | Print
    | Input
    | Exit
    | Cos
    | Sin
    | Tan
    

type Lexeme =
    | Number of Number
    | String of string
    | Keyword of Keyword
    | Operator of Operator
    | Identifier of string
    
    | Comma
    | Semicolon
    | Colon

type Token = { lexeme: Lexeme; line: int }


let numberToString (n: Number): string =
    match n with
    | Float f -> $"Float({f})"
    | Integer i -> $"Integer({i})"
    | Rational (n, d) -> $"Rational({n}/{d})"
    | Complex (r, i) -> $"Complex({r}i{i})"


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
    | LeftBrace -> "{"
    | RightBrace -> "}"
    | LeftBracket -> "["
    | RightBracket -> "]"
    | Dot -> "."

let lexemeToString (lex: Lexeme): string =
    match lex with
    | Number n -> $"Number(%s{numberToString n})"
    | String s -> $"String(\"%s{s}\")"
    | Keyword k -> $"Keyword({k})"
    | Operator op -> $"Operator(%s{operatorToString op})"
    | Identifier i -> $"Identifier(%s{i})"
    | Comma -> ","
    | Semicolon -> ";"
    | Colon -> ":"

let tokenToString (token: Token): string =
    $"{{ lexeme: %s{lexemeToString token.lexeme}; line: %d{token.line} }}"
