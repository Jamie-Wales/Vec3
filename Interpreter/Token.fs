/// <summary>
/// Defines the types used in the lexer and parser.
/// </summary>
module Vec3.Interpreter.Token

/// <summary>
/// Type representing a number in the language.
/// </summary>
type Number =
    | LFloat of float
    | LInteger of int
    | LRational of int * int
    | LComplex of float * float

/// <summary>
/// Possible operators in the language.
/// </summary>
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
    
    | Colon
    
    | Comma
    
    | Dollar
    
    /// <summary>
    /// For the future, custom operators can be added.
    /// </summary>
    | Custom of string
    
/// <summary>
/// Represents a punctuation symbol in the language, i.e. a token only used for syntax.
/// </summary>
type Punctuation =
    | Newline
    | Semicolon
    | LeftParen
    | RightParen
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket
    
/// <summary>
/// Represents a keyword in the language.
/// </summary>
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
    | Match
    | Case
    | Async
    
/// <summary>
/// Map of keywords to their respective keyword type.
/// </summary>
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
      "match", Keyword.Match
      "case", Keyword.Case
      "async", Keyword.Async
       ]
    |> Map.ofList

/// <summary>
/// Tests if a string is a keyword.
/// </summary>
/// <param name="s">The string to test.</param>
/// <returns>True if the string is a keyword, false otherwise.</returns>
let isKeyword (s: string): bool =
    Map.containsKey s keywordMap

/// <summary>
/// Get the keyword type of a given string.
/// </summary>
/// <param name="s">The string to get the keyword type of.</param>
/// <returns>The keyword type of the string.</returns>
/// <exception cref="KeyNotFoundException">Thrown if the string is not a keyword.</exception>
let getKeyword (s: string): Keyword =
    Map.find s keywordMap
    
/// <summary>
/// Represents a lexeme in the language (a token).
/// </summary>
type Lexeme =
    | Number of Number
    | String of string
    | Keyword of Keyword
    /// <summary>
    /// A given operator and its placement in the expression.
    /// The placement is used to disambiguate between unary and binary operators.
    /// </summary>
    | Operator of Operator * Placement option
    | Punctuation of Punctuation
    | Identifier of string
    
/// <summary>
/// The placement of an operator in an expression.
/// </summary>
and Placement = Prefix | Infix | Postfix

/// <summary>
/// The position of a token in the source code.
/// </summary>
type Position = { Line: int; Column: int; }

/// <summary>
/// A token in the language.
/// </summary>
type Token = { Lexeme: Lexeme; Position: Position; }

/// <summary>
/// A token representing an empty token (for testing).
/// </summary>
let Empty = { Lexeme = Identifier ""; Position = { Line = 0; Column = 0; } }

/// <summary>
/// Converts a number type to a string.
/// </summary>
/// <param name="n">The number to convert.</param>
/// <returns>The string representation of the number.</returns>
let numberToString (n: Number): string =
    match n with
    | LFloat f -> $"Float({f})"
    | LInteger i -> $"Integer({i})"
    | LRational (n, d) -> $"Rational({n}/{d})"
    | LComplex (r, i) -> $"Complex({r}i{i})"

/// <summary>
/// Converts an operator to a string.
/// </summary>
/// <param name="op">The operator to convert.</param>
/// <returns>The string representation of the operator.</returns>
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
    | Colon -> ":"
    | Comma -> ","
    | Dollar -> "$"
    | Custom s -> s

/// <summary>
/// Converts a keyword to a string.
/// </summary>
/// <param name="kw">The keyword to convert.</param>
/// <returns>The string representation of the keyword.</returns>
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
    | Match -> "match"
    | Case -> "case"
    | Async -> "async"

/// <summary>
/// Converts a punctuation to a string.
/// </summary>
/// <param name="p">The punctuation to convert.</param>
/// <returns>The string representation of the punctuation.</returns>
let punctuationToString (p: Punctuation): string =
    match p with
    | Semicolon -> ";"
    | LeftParen -> "("
    | RightParen -> ")"
    | LeftBrace -> "{"
    | RightBrace -> "}"
    | LeftBracket -> "["
    | RightBracket -> "]"
    | Newline -> "\n"

/// <summary>
/// Converts a lexeme to a string.
/// </summary>
/// <param name="lex">The lexeme to convert.</param>
/// <returns>The string representation of the lexeme.</returns>
let lexemeToString (lex: Lexeme): string =
    match lex with
    | Number n -> $"Number(%s{numberToString n})"
    | String s -> $"String(\"%s{s}\")"
    | Keyword k -> $"Keyword({k})"
    | Operator (op, fix) -> $"""Operator(%s{operatorToString op}){Option.defaultValue "" (Option.map (fun p -> $"({p})") fix)}"""
    | Identifier i -> $"Identifier(%s{i})"
    | Punctuation p -> $"Punctuation({punctuationToString p})"

/// <summary>
/// Converts a token to a string.
/// </summary>
/// <param name="token">The token to convert.</param>
/// <returns>The string representation of the token.</returns>
let tokenToString (token: Token): string =
    $"{{ lexeme: %s{lexemeToString token.Lexeme}; line: %d{token.Position.Line} }}"
    
/// <summary>
/// A built-in function in the language.
/// </summary>
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
    
    | Plot
    | PlotFunction
    | PlotFunctions
    
    | Draw
    
    | Ceil
    
    | Err
    
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
    
    | Cons
    
    | On
    
    | Await
    
    | TaylorSeries
    
/// <summary>
/// Map of built-in functions to their respective function type.
/// </summary>
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
      Identifier "error", BuiltInFunction.Err
      
      Identifier "BUILTIN_EXP", BuiltInFunction.Exp
      Identifier "BUILTIN_LOG", BuiltInFunction.Log
      
      Identifier "env", BuiltInFunction.Env
      Identifier "BUILTIN_SQRT", BuiltInFunction.Sqrt
      Identifier "BUILTIN_ABS", BuiltInFunction.Abs
      Identifier "BUILTIN_FLOOR", BuiltInFunction.Floor
      Identifier "plot", BuiltInFunction.Plot
      Identifier "plotFunc", BuiltInFunction.PlotFunction
      Identifier "plotFuncs", BuiltInFunction.PlotFunctions
      Identifier "BUILTIN_CEIL", BuiltInFunction.Ceil
      Identifier "BUILTIN_TRUNC", BuiltInFunction.Trunc
      
      Identifier "draw", BuiltInFunction.Draw
      
      Identifier "newtonRaphson", BuiltInFunction.NewtonRaphson
      Identifier "bisection", BuiltInFunction.Bisection
      Identifier "differentiate", BuiltInFunction.Differentiate
      Identifier "integrate", BuiltInFunction.Integrate
      Identifier "findIntegral", BuiltInFunction.FindIntegral
      Identifier "taylorSeries", BuiltInFunction.TaylorSeries
      
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
      Identifier "on", BuiltInFunction.On
      
      Identifier "await", BuiltInFunction.Await
      
      ]
    |> Map.ofList
    
/// <summary>
/// Tests if a lexeme is a built-in function.
/// </summary>
/// <param name="s">The lexeme to test.</param>
/// <returns>True if the lexeme is a built-in function, false otherwise.</returns>
let isBuiltInFunction (s: Lexeme): bool =
    Map.containsKey s builtInFunctionMap

let hasSideEffects = function
    | Print -> true
    | Input -> true
    | Exit -> true
    | Cos -> false
    | Sin -> false
    | Tan -> false
    | ACos -> false
    | ASin -> false
    | ATan -> false
    | Eval -> true
    | Log -> false
    | Exp -> false
    | Trunc -> false
    | Read -> true
    | Env -> false
    | Sqrt -> false
    | Abs -> false
    | Floor -> false
    | Plot -> true
    | PlotFunction -> true
    | PlotFunctions -> true
    | Draw -> true
    | Ceil -> false
    | Err -> true
    | Add -> false
    | Sub -> false
    | Mul -> false
    | Div -> false
    | Mod -> false
    | Pow -> false
    | And -> false
    | Or -> false
    | Not -> false
    | Neg -> false
    | Unneg -> false
    | Eq -> false
    | Neq -> false
    | Lt -> false
    | Lte -> false
    | Gt -> false
    | Gte -> false
    | CrossProduct -> false
    | DotProduct -> false
    | Cast -> true
    | NewtonRaphson -> true
    | Bisection -> true
    | Differentiate -> true
    | Integrate -> true
    | FindIntegral -> true
    | Cons -> false
    | On -> true
    | Await -> true
    | TaylorSeries -> true
    