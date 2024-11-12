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
    | Dollar
    
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
    | Rec
    
    
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
      "rec", Keyword.Rec
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
    | Rec -> "rec"

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
    | Dollar -> "$"

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
    | ACos
    | ASin
    | ATan
    
    | Eval
    
    | Log
    | Exp
    
    | Trunc
    
    | Read
    
    | Env
    | Sqrt
    | Abs
    | Floor
    | Fold
    | Map
    
    | Plot
    | PlotFunction
    | PlotFunctions
    
    | Draw
    
    | Ceil
    | Len
    
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
    
    | Cast
    
    | NewtonRaphson
    | Bisection
    | Differentiate
    | Integrate
    | FindIntegral
    
    | Head
    | Tail
    | Cons
    
let builtInFunctionMap =
    [ Identifier "print", BuiltInFunction.Print
      Identifier "input", BuiltInFunction.Input
      Identifier "exit", BuiltInFunction.Exit
      Identifier "BUILTIN_COS", BuiltInFunction.Cos
      Identifier "BUILTIN_SIN", BuiltInFunction.Sin
      Identifier "BUILTIN_TAN", BuiltInFunction.Tan
      Identifier "BUILTIN_ACOS", BuiltInFunction.ACos
      Identifier "BUILTIN_ATAN", BuiltInFunction.ATan
      Identifier "BUILTIN_ASIN", BuiltInFunction.ASin
      
      Identifier "read", BuiltInFunction.Read
      Identifier "eval", BuiltInFunction.Eval
      
      Identifier "BUILTIN_EXP", BuiltInFunction.Exp
      Identifier "BUILTIN_LOG", BuiltInFunction.Log
      
      Identifier "env", BuiltInFunction.Env
      Identifier "BUILTIN_SQRT", BuiltInFunction.Sqrt
      Identifier "BUILTIN_ABS", BuiltInFunction.Abs
      Identifier "BUILTIN_FLOOR", BuiltInFunction.Floor
      Identifier "fold", BuiltInFunction.Fold
      Identifier "plot", BuiltInFunction.Plot
      Identifier "plotFunc", BuiltInFunction.PlotFunction
      Identifier "plotFuncs", BuiltInFunction.PlotFunctions
      Identifier "BUILTIN_CEIL", BuiltInFunction.Ceil
      Identifier "map", BuiltInFunction.Map
      Identifier "BUILTIN_LEN", BuiltInFunction.Len
      Identifier "BUILTIN_TRUNC", BuiltInFunction.Trunc
      
      Identifier "draw", BuiltInFunction.Draw
      
      Identifier "newtonRaphson", BuiltInFunction.NewtonRaphson
      Identifier "bisection", BuiltInFunction.Bisection
      Identifier "differentiate", BuiltInFunction.Differentiate
      Identifier "integrate", BuiltInFunction.Integrate
      Identifier "findIntegral", BuiltInFunction.FindIntegral
      
      Operator (Plus, Some Infix), BuiltInFunction.Add
      Operator (Minus, Some Infix), BuiltInFunction.Sub
      Operator (Star, Some Infix), BuiltInFunction.Mul
      Operator (Slash, Some Infix), BuiltInFunction.Div
      Operator (Percent, Some Infix), BuiltInFunction.Mod
      Operator (StarStar, Some Infix), BuiltInFunction.Pow
      Operator (Caret, Some Infix), BuiltInFunction.Pow
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
      
      Identifier "cast", BuiltInFunction.Cast
      
      Identifier "head", BuiltInFunction.Head
      Identifier "tail", BuiltInFunction.Tail
      ]
    |> Map.ofList
    
let isBuiltInFunction (s: Lexeme): bool =
    Map.containsKey s builtInFunctionMap
