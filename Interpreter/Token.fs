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
    | Percent
    | Caret
    
    | AmpersandAmpersand
    | PipePipe
    
    | EqualEqual
    | BangEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Equal
    | Bang
    
    | LeftParen
    | RightParen
    | Arrow
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    | DotDot
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
    | Print
    | In
    | And
    | Or
    
    
let keywordMap = 
    [ "let", Keyword.Let
      "if", Keyword.If
      "then", Keyword.Then
      "else", Keyword.Else
      "for", Keyword.For
      "true", Keyword.True
      "false", Keyword.False
      "nil", Keyword.Nil
      "print", Keyword.Print
      "in", Keyword.In
      "and", Keyword.And
      "or", Keyword.Or
       ]
    |> Map.ofList

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

type Position = { Line: int; Column: int; }
type Token = { Lexeme: Lexeme; Position: Position; }

let Empty = { Lexeme = Identifier ""; Position = { Line = 0; Column = 0; } }

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
    | DotDot -> ".."
    | Percent -> "%"
    | AmpersandAmpersand -> "and"
    | PipePipe -> "or"
    | Caret -> "^"

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
    $"{{ lexeme: %s{lexemeToString token.Lexeme}; line: %d{token.Position.Line} }}"
