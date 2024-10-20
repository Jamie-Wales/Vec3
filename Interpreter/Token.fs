module Vec3.Interpreter.Token

type TNumber =
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
    | Hash
    
    | Ampersand
    | Pipe
    
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
    
    | Cross
    | DotStar
    
    | Comma
    | Semicolon
    | Colon
    | ColonColon
    
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
    | Assert
    | With
    
    
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
      "assert", Keyword.Assert
      "with", Keyword.With
       ]
    |> Map.ofList

let isKeyword (s: string): bool =
    Map.containsKey s keywordMap



type Lexeme =
    | Number of TNumber
    | String of string
    | Keyword of Keyword
    | Operator of Operator
    | Identifier of string
    

type Position = { Line: int; Column: int; }
type Token = { Lexeme: Lexeme; Position: Position; }

let Empty = { Lexeme = Identifier ""; Position = { Line = 0; Column = 0; } }

let numberToString (n: TNumber): string =
    match n with
    | Float f -> $"Float({f})"
    | Integer i -> $"Integer({i})"
    | Rational (n, d) -> $"Rational({n}/{d})"
    | Complex (r, i) -> $"Complex({r}i{i})"


let operatorToString (op: Operator): string =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Hash -> "#"
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
    | AmpersandAmpersand -> "&&"
    | PipePipe -> "||"
    | Caret -> "^"
    | Cross -> "X"
    | DotStar -> ".*"
    | Comma -> ","
    | Semicolon -> ";"
    | Colon -> ":"
    | Ampersand -> "&"
    | Pipe -> "|"
    | ColonColon -> "::"

let keywordToString (kw: Keyword): string =
    match kw with
    | Let -> "let"
    | If -> "if"
    | Then -> "then"
    | Else -> "else"
    | For -> "for"
    | True -> "true"
    | False -> "false"
    | Nil -> "nil"
    | Print -> "print"
    | In -> "in"
    | And -> "and"
    | Or -> "or"
    | Assert -> "assert"
    | With -> "with"


let lexemeToString (lex: Lexeme): string =
    match lex with
    | Number n -> $"Number(%s{numberToString n})"
    | String s -> $"String(\"%s{s}\")"
    | Keyword k -> $"Keyword({k})"
    | Operator op -> $"Operator(%s{operatorToString op})"
    | Identifier i -> $"Identifier(%s{i})"

let tokenToString (token: Token): string =
    $"{{ lexeme: %s{lexemeToString token.Lexeme}; line: %d{token.Position.Line} }}"
    
type BuiltInFunction =
    | Print
    | Input
    | Exit
    | Cos
    | Sin
    | Tan
    | Env
    | Sqrt
    | Abs
    | Floor
    | Fold
    | Plot 


let builtInFunctionMap =
    [ Identifier "print", BuiltInFunction.Print
      Identifier "input", BuiltInFunction.Input
      Identifier "exit", BuiltInFunction.Exit
      Identifier "cos", BuiltInFunction.Cos
      Identifier "sin", BuiltInFunction.Sin
      Identifier "tan", BuiltInFunction.Tan 
      Identifier "env", BuiltInFunction.Env
      Identifier "sqrt", BuiltInFunction.Sqrt
      Identifier "abs", BuiltInFunction.Abs
      Identifier "floor", BuiltInFunction.Floor
      Identifier "fold", BuiltInFunction.Fold
      Identifier "plot", BuiltInFunction.Plot
      ]
    |> Map.ofList
    
let isBuiltInFunction (s: Lexeme): bool =
    Map.containsKey s builtInFunctionMap
