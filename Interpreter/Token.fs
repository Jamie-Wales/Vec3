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
    
    | Arrow
    | Dot
    | DotDot
    
    | Cross
    | DotStar
    
    | ColonColon
    | Custom of string
    
type Punctuation =
    | Comma
    | Semicolon
    | Colon
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    
type Keyword =
    | Let
    | If
    | Then
    | Else
    | For
    | True
    | False
    | Nil
    | In
    | And
    | Or
    | Assert
    | With
    | Type
    
    
let keywordMap = 
    [ "let", Keyword.Let
      "if", Keyword.If
      "then", Keyword.Then
      "else", Keyword.Else
      "for", Keyword.For
      "true", Keyword.True
      "false", Keyword.False
      "nil", Keyword.Nil
      "in", Keyword.In
      "and", Keyword.And
      "or", Keyword.Or
      "assert", Keyword.Assert
      "with", Keyword.With
      "type", Keyword.Type
       ]
    |> Map.ofList

let isKeyword (s: string): bool =
    Map.containsKey s keywordMap

let getKeyword (s: string): Keyword =
    Map.find s keywordMap
    
type Lexeme =
    | Number of TNumber
    | String of string
    | Keyword of Keyword
    | Operator of Operator * Placement option
    | Punctuation of Punctuation
    | Identifier of string
    
and Placement = Prefix | Infix | Postfix

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
    | Bang -> "!"
    | Arrow -> "->"
    | Dot -> "."
    | Percent -> "%"
    | AmpersandAmpersand -> "&&"
    | PipePipe -> "||"
    | Caret -> "^"
    | Cross -> "X"
    | DotStar -> ".*"
    | Ampersand -> "&"
    | Pipe -> "|"
    | ColonColon -> "::"
    | DotDot -> ".."
    | Custom s -> s

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
    | In -> "in"
    | And -> "and"
    | Or -> "or"
    | Assert -> "assert"
    | With -> "with"
    | Type -> "type"

let punctuationToString (p: Punctuation): string =
    match p with
    | Comma -> ","
    | Semicolon -> ";"
    | Colon -> ":"
    | LeftParen -> "("
    | RightParen -> ")"
    | LeftBrace -> "{"
    | RightBrace -> "}"
    | LeftBracket -> "["
    | RightBracket -> "]"

let lexemeToString (lex: Lexeme): string =
    match lex with
    | Number n -> $"Number(%s{numberToString n})"
    | String s -> $"String(\"%s{s}\")"
    | Keyword k -> $"Keyword({k})"
    | Operator (op, fix) -> $"""Operator(%s{operatorToString op}){Option.defaultValue "" (Option.map (fun p -> $"({p})") fix)}"""
    | Identifier i -> $"Identifier(%s{i})"
    | Punctuation p -> $"Punctuation({punctuationToString p})"

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
    | Ceil
    
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Pow
    | And
    | Or
    | Not
    | Neg
    | Unneg
    | Eq
    | Neq
    | Lt
    | Lte
    | Gt
    | Gte
    
    | CrossProduct
    | DotProduct
    
    | Cons
    
    | BInt
    | BFloat
    | BComplex
    | BRational
    | BBool
    | BString
    
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
      Identifier "ceil", BuiltInFunction.Ceil
      
      Operator (Plus, Some Infix), BuiltInFunction.Add
      Operator (Minus, Some Infix), BuiltInFunction.Sub
      Operator (Star, Some Infix), BuiltInFunction.Mul
      Operator (Slash, Some Infix), BuiltInFunction.Div
      Operator (Percent, Some Infix), BuiltInFunction.Mod
      Operator (StarStar, Some Infix), BuiltInFunction.Pow
      Operator (AmpersandAmpersand, Some Infix), BuiltInFunction.And
      Operator (PipePipe, Some Infix), BuiltInFunction.Or
      Operator (Bang, Some Prefix), BuiltInFunction.Not
      Operator (Minus, Some Prefix), BuiltInFunction.Neg
      Operator (Plus, Some Prefix), BuiltInFunction.Unneg
      Operator (EqualEqual, Some Infix), BuiltInFunction.Eq
      Operator (BangEqual, Some Infix), BuiltInFunction.Neq
      Operator (Less, Some Infix), BuiltInFunction.Lt
      Operator (LessEqual, Some Infix), BuiltInFunction.Lte
      Operator (Greater, Some Infix), BuiltInFunction.Gt
      Operator (GreaterEqual, Some Infix), BuiltInFunction.Gte
      Operator (Cross, Some Infix), BuiltInFunction.CrossProduct
      Operator (DotStar, Some Infix), BuiltInFunction.DotProduct
      Operator (ColonColon, Some Infix), BuiltInFunction.Cons
      
      Identifier "Int", BuiltInFunction.BInt
      Identifier "Float", BuiltInFunction.BFloat
      Identifier "Complex", BuiltInFunction.BComplex
      Identifier "Rational", BuiltInFunction.BRational
      Identifier "Boolean", BuiltInFunction.BBool
      Identifier "String", BuiltInFunction.BString
      ]
    |> Map.ofList
    
let isBuiltInFunction (s: Lexeme): bool =
    Map.containsKey s builtInFunctionMap
