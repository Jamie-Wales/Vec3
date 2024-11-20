/// <summary>
/// Symbolic expression module.
/// DSL for symbolic expressions.
/// </summary>

module Vec3.Interpreter.SymbolicExpression

open System
open Grammar
open Token

// https://bmitc.me/articles/symbolic-expressions-in-fsharp#:~:text=By%20pattern%20matching%20against%20Sum,used%20in%20the%20result%20expression.

/// <summary>
/// The symbolic expression type (a DSL for symbolic expressions).
/// </summary>
type Expression =
    /// <summary>
    /// A variable.
    /// </summary>
    | X // maybe make this Var of string for multiple variables
    
    /// <summary>
    /// A constant value in the expression.
    /// </summary>
    | Const of float
    
    | Negation of Expression
    | Addition of Expression * Expression
    | Subtraction of Expression * Expression
    | Multiplication of Expression * Expression
    | Division of Expression * Expression
    | Power of Expression * Expression
    
    | Sine of Expression
    | Cosine of Expression
    | Tangent of Expression
    
    | ASine of Expression
    | ACosine of Expression
    | ATangent of Expression
    
    | Exponential of Expression
    
    /// <summary>
    /// Log base and value to take the logarithm.
    /// </summary>
    | Logarithm of Expression * Expression // base and value to take the logarithm
    
    | SquareRoot of Expression
    
    | AbsoluteValue of Expression
    | Floor of Expression
    | Ceiling of Expression
    | Truncate of Expression
    
    
/// <summary>
/// Log base function.
/// </summary>
/// <param name="bas">The base of the logarithm.</param>
/// <param name="x">The value to take the logarithm of.</param>
/// <returns>The logarithm of the value with the specified base.</returns>
let logBase (bas: float) (x: float) = Math.Log x / Math.Log bas
    
/// <summary>
/// Evaluate the expression at a given value.
/// </summary>
/// <param name="a">The value to evaluate the expression at.</param>
/// <param name="expression">The expression to evaluate.</param>
/// <returns>The result of evaluating the expression at the given value.</returns>
let rec evaluate (a: float) (expression: Expression) : float =
    match expression with
    | X -> a
    | Const x -> x
    | Negation x -> - evaluate a x
    | Addition (x, y) -> evaluate a x + evaluate a y
    | Subtraction (x, y) -> evaluate a x - evaluate a y
    | Multiplication (x, y) -> evaluate a x * evaluate a y
    | Division (x, y) -> evaluate a x / evaluate a y
    | Power (x, y) -> evaluate a x ** evaluate a y
    
    | Sine x -> Math.Sin (evaluate a x)
    | Cosine x -> Math.Cos (evaluate a x)
    | Tangent x -> Math.Tan (evaluate a x)
    
    | ASine x -> Math.Asin (evaluate a x)
    | ACosine x -> Math.Acos (evaluate a x)
    | ATangent x -> Math.Atan (evaluate a x)
    
    | Exponential x -> Math.Exp (evaluate a x)
    | Logarithm (x, y) -> logBase (evaluate a x) (evaluate a y)
    | SquareRoot x -> Math.Sqrt (evaluate a x)
    
    | AbsoluteValue x -> Math.Abs (evaluate a x)
    | Floor x -> Math.Floor (evaluate a x)
    | Ceiling x -> Math.Ceiling (evaluate a x)
    | Truncate x -> Math.Truncate (evaluate a x)
    
/// <summary>
/// A zero'd position for easier conversion.
/// </summary>
let defaultPosition = { Line = 0; Column = 0 }
 
/// <summary>
/// Convert a symbolic expression to an expression in the grammar.
/// </summary>
/// <param name="expression">The symbolic expression to convert.</param>
/// <returns>The expression in the grammar.</returns>
let rec toExpr (expression: Expression): Expr =
    match expression with
    | X -> EIdentifier ({ Lexeme = Identifier "x"; Position = defaultPosition }, None)
    | Const x -> ELiteral (LNumber (LFloat x), TFloat)
    | Negation x -> ECall (EIdentifier ({ Lexeme = Operator (Minus, Some Prefix); Position = defaultPosition }, None), [toExpr x], None)
    | Addition (x, y) -> ECall (EIdentifier ({ Lexeme = Operator (Plus, Some Infix); Position = defaultPosition }, None), [toExpr x; toExpr y], None)
    | Subtraction (x, y) -> ECall (EIdentifier ({ Lexeme = Operator (Minus, Some Infix); Position = defaultPosition }, None), [toExpr x; toExpr y], None)
    | Multiplication (x, y) -> ECall (EIdentifier ({ Lexeme = Operator (Star, Some Infix); Position = defaultPosition }, None), [toExpr x; toExpr y], None)
    | Division (x, y) -> ECall (EIdentifier ({ Lexeme = Operator (Slash, Some Infix); Position = defaultPosition }, None), [toExpr x; toExpr y], None)
    | Power (x, y) -> ECall (EIdentifier ({ Lexeme = Operator (StarStar, Some Infix); Position = defaultPosition }, None), [toExpr x; toExpr y], None)
    
    | Sine x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_SIN"; Position = defaultPosition }, None), [toExpr x], None)
    | Cosine x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_COS"; Position = defaultPosition }, None), [toExpr x], None)
    | Tangent x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_TAN"; Position = defaultPosition }, None), [toExpr x], None)
    
    | ASine x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ASIN"; Position = defaultPosition }, None), [toExpr x], None)
    | ACosine x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ACOS"; Position = defaultPosition }, None), [toExpr x], None)
    | ATangent x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ATAN"; Position = defaultPosition }, None), [toExpr x], None)
    
    | Exponential x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_EXP"; Position = defaultPosition }, None), [toExpr x], None)
    | Logarithm (x, y) -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_LOG"; Position = defaultPosition }, None), [toExpr x; toExpr y], None)
    | SquareRoot x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_SQRT"; Position = defaultPosition }, None), [toExpr x], None)
    
    | AbsoluteValue x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ABS"; Position = defaultPosition }, None), [toExpr x], None)
    | Floor x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_FLOOR"; Position = defaultPosition }, None), [toExpr x], None)
    | Ceiling x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_CEIL"; Position = defaultPosition }, None), [toExpr x], None)
    | Truncate x -> ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_TRUNC"; Position = defaultPosition }, None), [toExpr x], None)
    
/// <summary>
/// Convert a symbolic expression to a builtin function (a function defined in the f sharp runtime).
/// </summary>
/// <param name="expression">The symbolic expression to convert.</param>
/// <returns>The builtin function that represents the symbolic expression.</returns>
let rec toBuiltin (expression: Expression) : (double -> double) =
    match expression with
    | X -> id
    | Const x -> (fun _ -> x)
    | Negation x -> (fun a -> - toBuiltin x a)
    
    | Sine x -> (fun a -> Math.Sin (toBuiltin x a))
    | Cosine x -> (fun a -> Math.Cos (toBuiltin x a))
    | Tangent x -> (fun a -> Math.Tan (toBuiltin x a))
    
    | ASine x -> (fun a -> Math.Asin (toBuiltin x a))
    | ACosine x -> (fun a -> Math.Acos (toBuiltin x a))
    | ATangent x -> (fun a -> Math.Atan (toBuiltin x a))
    
    | Exponential x -> (fun a -> Math.Exp (toBuiltin x a))
    | Logarithm (x, y) -> (fun a -> logBase (toBuiltin x a) (toBuiltin y a))
    | Addition (x, y) -> (fun a -> toBuiltin x a + toBuiltin y a)
    | Subtraction (x, y) -> (fun a -> toBuiltin x a - toBuiltin y a)
    | Multiplication (x, y) -> (fun a -> toBuiltin x a * toBuiltin y a)
    | Division (x, y) -> (fun a -> toBuiltin x a / toBuiltin y a)
    | Power (x, y) -> (fun a -> toBuiltin x a ** toBuiltin y a)
    | SquareRoot x -> (fun a -> Math.Sqrt (toBuiltin x a))
    
    | AbsoluteValue x -> (fun a -> Math.Abs (toBuiltin x a))
    | Floor x -> (fun a -> Math.Floor (toBuiltin x a))
    | Ceiling x -> (fun a -> Math.Ceiling (toBuiltin x a))
    | Truncate x -> (fun a -> Math.Truncate (toBuiltin x a))
        
/// <summary>
/// Convert an expression in the grammar to a symbolic expression.
/// </summary>
/// <param name="expr">The expression in the grammar to convert (must be pure and have only 1 input / output).</param>
/// <returns>The symbolic expression that represents the expression in the grammar.</returns>
/// <exception cref="InvalidProgramException">If the expression is not a valid symbolic expression.</exception>
let rec fromExpr (expr: Expr) : Expression =
    match expr with
    // need first class support for e etc !!
    | EIdentifier ({ Lexeme = Identifier "E"; Position = _ }, _) -> Exponential(Const 1)
    | EIdentifier ({ Lexeme = Identifier "PI"; Position = _ }, _) -> Const Math.PI
    | EIdentifier ({ Lexeme = Identifier "TAU"; Position = _ }, _) -> Const Math.Tau
    | EIdentifier ({ Lexeme = Identifier _; Position = _ }, _) -> X
    | ELiteral (LNumber (LFloat x), _) -> Const x
    | ELiteral (LNumber (LInteger x), _) -> Const x
    | ECall (EIdentifier ({ Lexeme = Operator (Minus, Some Prefix); Position = _ }, _), [x], _) -> Negation (fromExpr x)
    | ECall (EIdentifier ({ Lexeme = Operator (Plus, Some Infix); Position = _ }, _), [x; y], _) -> Addition (fromExpr x, fromExpr y)
    | ECall (EIdentifier ({ Lexeme = Operator (Minus, Some Infix); Position = _ }, _), [x; y], _) -> Subtraction (fromExpr x, fromExpr y)
    | ECall (EIdentifier ({ Lexeme = Operator (Star, Some Infix); Position = _ }, _), [x; y], _) -> Multiplication (fromExpr x, fromExpr y)
    | ECall (EIdentifier ({ Lexeme = Operator (Slash, Some Infix); Position = _ }, _), [x; y], _) -> Division (fromExpr x, fromExpr y)
    
    | ECall (EIdentifier ({ Lexeme = Operator (Caret, Some Infix); Position = _ }, _), [x; y], _) -> Power (fromExpr x, fromExpr y)
    | ECall (EIdentifier ({ Lexeme = Operator (StarStar, Some Infix); Position = _ }, _), [x; y], _) -> Power (fromExpr x, fromExpr y)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "sin"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_SIN"; Position = _ }, _), [x], _) -> Sine (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "cos"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_COS"; Position = _ }, _), [x], _) -> Cosine (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "tan"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_TAN"; Position = _ }, _), [x], _) -> Tangent (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "asin"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ASIN"; Position = _ }, _), [x], _) -> ASine (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "acos"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ACOS"; Position = _ }, _), [x], _) -> ACosine (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "atan"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ATAN"; Position = _ }, _), [x], _) -> ATangent (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "exp"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_EXP"; Position = _ }, _), [x], _) -> Exponential (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "log"; Position = _ }, _), [x; y], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_LOG"; Position = _ }, _), [x; y], _) -> Logarithm (fromExpr x, fromExpr y)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "sqrt"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_SQRT"; Position = _ }, _), [x], _) -> SquareRoot (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "abs"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_ABS"; Position = _ }, _), [x], _) -> AbsoluteValue (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "floor"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_FLOOR"; Position = _ }, _), [x], _) -> Floor (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "ceil"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_CEIL"; Position = _ }, _), [x], _) -> Ceiling (fromExpr x)
    
    | ECall (EIdentifier ({ Lexeme = Identifier "trunc"; Position = _ }, _), [x], _)
    | ECall (EIdentifier ({ Lexeme = Identifier "BUILTIN_TRUNC"; Position = _ }, _), [x], _) -> Truncate (fromExpr x)
    
    | EGrouping (x, _) -> fromExpr x
    
    | _ -> raise <| InvalidProgramException("Invalid expression")
    
/// <summary>
/// Simplify an expression using algebraic simplification rules.
/// </summary>
/// <param name="expression">The expression to simplify.</param>
/// <returns>The simplified expression.</returns>
let simplify (expression: Expression) : Expression =
    let rec simplifier (expression: Expression) =
        match expression with
        | Addition (Const c1, Const c2) -> Const (c1 + c2)
        | Subtraction (Const c1, Const c2) -> Const (c1 - c2)
        | Multiplication (Const c1, Const c2) -> Const (c1 * c2)
        | Division (Const c1, Const c2) -> Const (c1 / c2)
        | Power (Const c1, Const c2) -> Const (c1 ** c2)
        | Negation (Const c) -> Const (-c)
        
        | Addition (Const 0.0, x) -> simplifier x
        | Addition (x, Const 0.0) -> simplifier x
        
        | Subtraction (x, Const 0.0) -> simplifier x
        | Subtraction (Const c, x) when c = 0.0 ->
            let x = simplifier x
            Negation x
        
        | Multiplication (Const 0.0, _) -> Const 0.0
        | Multiplication (_, Const 0.0) -> Const 0.0
        | Multiplication (Const 1.0, x) -> simplifier x
        | Multiplication (x, Const 1.0) -> simplifier x
        
        | Division (Const 0.0, _) -> Const 0.0
        | Division (x, Const 1.0) -> simplifier x
        
        | Power (x, Const 1.0) -> simplifier x
        | Power (Const 1.0, _) -> Const 1.0
        | Power (Const 0.0, _) -> Const 0.0
        | Power (_, Const 0.0) -> Const 1.0
        
        | Addition (X, X) -> Multiplication (Const 2.0, X)
        | Subtraction (X, X) -> Const 0.0
        | Multiplication (X, X) -> Power (X, Const 2.0)
        | Division (X, X) -> Const 1.0
        | Power (X, Const 2.0) -> Multiplication (X, X)
        
        | Negation x -> Negation (simplifier x)
        | Addition (x, y) -> Addition (simplifier x, simplifier y)
        | Subtraction (x, y) -> Subtraction (simplifier x, simplifier y)
        | Multiplication (x, y) -> Multiplication (simplifier x, simplifier y)
        | Division (x, y) -> Division (simplifier x, simplifier y)
        | Power (x, y) -> Power (simplifier x, simplifier y)
        
        | Sine x -> Sine (simplifier x)
        | Cosine x -> Cosine (simplifier x)
        | Tangent x -> Tangent (simplifier x)
        
        | ASine x -> ASine (simplifier x)
        | ACosine x -> ACosine (simplifier x)
        | ATangent x -> ATangent (simplifier x)
        
        | Exponential x -> Exponential (simplifier x)
        | Logarithm (x, y) -> Logarithm (simplifier x, simplifier y)
        
        | SquareRoot x -> SquareRoot (simplifier x)
        
        | AbsoluteValue (Const x) when x < 0.0 -> Const (-x)
        | AbsoluteValue (Const x) -> Const x
        | AbsoluteValue (Negation x) -> AbsoluteValue x
        | AbsoluteValue x -> AbsoluteValue (simplifier x)
        
        | Floor (Const x) -> Const (Math.Floor x)
        | Floor x -> Floor (simplifier x)
        
        | Ceiling (Const x) -> Const (Math.Ceiling x)
        | Ceiling x -> Ceiling (simplifier x)
        
        | Truncate (Const x) -> Const (Math.Truncate x)
        | Truncate x -> Truncate (simplifier x)
        
        | X -> X
        | Const x -> Const x
        
    // do this recursively until no more simplifications can be made instead
    expression |> simplifier |> simplifier |> simplifier
    
/// <summary>
/// Find the derivative of an expression.
/// </summary>
/// <param name="expression">The expression to differentiate.</param>
/// <returns>The derivative of the expression.</returns>
let differentiate (expression: Expression) : Expression =
    /// <summary>
    /// Inner top down recursive differentiation function.
    /// </summary>
    /// <param name="expression">The expression to differentiate.</param>
    /// <returns>The differentiated expression.</returns>
    let rec diff (expression: Expression) : Expression =
        match simplify expression with
        | X -> Const 1.0
        | Const _ -> Const 0.0
        | Negation x -> Negation (diff x)
        | Addition (x, y) -> Addition (diff x, diff y)
        | Subtraction (x, y) -> Subtraction (diff x, diff y)
        | Multiplication (x, y) -> Addition (Multiplication (diff x, y), Multiplication (x, diff y))
        | Division (x, y) -> Division (Subtraction (Multiplication (diff x, y), Multiplication (x, diff y)), Power (y, Const 2.0))
        | Power (x, y) -> Multiplication (y, Multiplication (Power (x, Subtraction (y, Const 1.0)), diff x))
        
        | Sine x -> Multiplication (Cosine x, diff x)
        | Cosine x -> Negation (Multiplication (Sine x, diff x))
        | Tangent x -> Division (diff (Sine x), Power (Cosine x, Const 2.0))
        
        | ASine x -> Division (diff x, SquareRoot (Subtraction (Const 1.0, Power (x, Const 2.0))))
        | ACosine x -> Negation (Division (diff x, SquareRoot (Subtraction (Const 1.0, Power (x, Const 2.0)))))
        | ATangent x -> Division (diff x, Addition (Const 1.0, Power (x, Const 2.0)))
        
        | Exponential x -> Multiplication (Exponential x, diff x)
        | Logarithm (x, y) -> Division (diff y, Multiplication (y, Logarithm (Const Math.E, x)))
        | SquareRoot x -> Division (diff x, Multiplication (Const 2.0, SquareRoot x))
        
        | AbsoluteValue x -> Division (Multiplication (x, diff x), AbsoluteValue x)
        | Floor _ -> Const 0.0
        | Ceiling _ -> Const 0.0
        | Truncate _ -> Const 0.0
        
    diff expression |> simplify

/// <summary>
/// Find the unbound integral of an expression.
/// </summary>
/// <param name="expression">The expression to integrate.</param>
/// <returns>The integral of the expression.</returns>
let integrate (expression: Expression) : Expression =
    let rec inte (expression: Expression): Expression =
        match simplify expression with
        | X -> Multiplication (Const 0.5, Power (X, Const 2.0))
        | Const x -> Multiplication (Const x, X)
        | Negation x ->
            let x = inte x
            Negation x
        | Addition (x, y) ->
            let x = inte x
            let y = inte y
            Addition (x, y)
        | Subtraction (x, y) ->
            let x = inte x
            let y = inte y
            Subtraction (x, y)
        | Multiplication (x, y) ->
            let x = inte x
            Multiplication (x, y)
        | Division (x, y) ->
            let x = inte x
            Division (x, y)
        | Power (x, y) -> Division (Power (x, Addition (y, Const 1.0)), Addition (y, Const 1.0))
        
        | Sine x -> Negation (Cosine x)
        | Cosine x -> Sine x
        | Tangent x -> Logarithm (Cosine x, Sine x)
        
        | ASine x -> Multiplication (x, ASine x)
        | ACosine x -> Multiplication (x, ACosine x)
        | ATangent x -> Multiplication (x, ATangent x)
        
        | Exponential x -> Exponential x
        | Logarithm (x, y) -> Multiplication (y, Logarithm (Const Math.E, x))
        | SquareRoot x -> Multiplication (Const 0.5, Multiplication (x, SquareRoot x))
        
        | AbsoluteValue x ->
            let x = inte x
            AbsoluteValue x
        | Floor _ -> Const 0.0
        | Ceiling _ -> Const 0.0
        | Truncate _ -> Const 0.0
        
    inte expression |> simplify

/// <summary>
/// Find the definite integral of an expression.
/// </summary>
/// <param name="expression">The expression to integrate.</param>
/// <param name="a">The lower bound of the integral.</param>
/// <param name="b">The upper bound of the integral.</param>
/// <returns>The definite integral of the expression.</returns>
let findIntegral (expression: Expression) (a: float) (b: float) : float =
    let integral = integrate expression
    abs <| evaluate b integral - evaluate a integral

/// <summary>
/// Convert an expression to a string.
/// </summary>
/// <param name="expression">The expression to convert.</param>
/// <returns>The string representation of the expression.</returns>
let rec toString (expression: Expression) : string =
    let expression = simplify expression
    
    match expression with
    | X -> "x"
    | Const x -> x.ToString()
    | Negation x -> $"-({toString x})"
    | Addition (x, y) -> $"({toString x} + {toString y})"
    | Subtraction (x, y) -> $"({toString x} - {toString y})"
    | Multiplication (x, y) -> $"({toString x} * {toString y})"
    | Division (x, y) -> $"({toString x} / {toString y})"
    | Power (x, y) -> $"({toString x} ** {toString y})"
    
    | Sine x -> $"sin({toString x})"
    | Cosine x -> $"cos({toString x})"
    | Tangent x -> $"tan({toString x})"
    
    | ASine x -> $"asin({toString x})"
    | ACosine x -> $"acos({toString x})"
    | ATangent x -> $"atan({toString x})"
    
    | Exponential x -> $"exp({toString x})"
    | Logarithm (x, y) -> $"log({toString x}, {toString y})"
    | SquareRoot x -> $"sqrt({toString x})"
    
    | AbsoluteValue x -> $"abs({toString x})"
    | Floor x -> $"floor({toString x})"
    | Ceiling x -> $"ceil({toString x})"
    | Truncate x -> $"trunc({toString x})"
    
/// <summary>
/// Find the Taylor series of an expression.
/// </summary>
/// <param name="expression">The expression to find the Taylor series of.</param>
/// <param name="n">The number of terms in the Taylor series.</param>
/// <returns>The Taylor series of the expression.</returns>
let rec taylorSeries (expression: Expression) (n: int) : Expression =
    let rec factorial (n: int) =
        if n < 2 then 1
        else n * factorial (n - 1)
        
    let rec taylor (expression: Expression) (n: int) : Expression =
        match expression with
        | X -> if n = 0 then X else if n % 2 = 0 then Const 0.0 else if n % 4 = 1 then X else Negation X
        | Const x -> Const x
        | Negation x -> Negation (taylor x n)
        | Addition (x, y) -> Addition (taylor x n, taylor y n)
        | Subtraction (x, y) -> Subtraction (taylor x n, taylor y n)
        | Multiplication (x, y) -> Multiplication (taylor x n, taylor y n)
        | Division (x, y) -> Division (taylor x n, taylor y n)
        | Power (x, y) -> Power (taylor x n, taylor y n)
        | Sine x -> if n % 4 = 0 then Const 0.0 else if n % 4 = 1 then taylor x n else if n % 4 = 2 then Negation (taylor x n) else Negation (taylor x n)
        | Cosine x -> if n % 4 = 0 then taylor x n else if n % 4 = 1 then Const 0.0 else if n % 4 = 2 then taylor x n else Const 0.0
        | Tangent x -> if n % 2 = 0 then Const 0.0 else if n % 2 = 1 then taylor x n else Const 0.0
        | ASine x -> if n % 2 = 0 then Const 0.0 else if n % 2 = 1 then taylor x n else Const 0.0
        | ACosine x -> if n % 2 = 0 then Const 0.0 else if n % 2 = 1 then taylor x n else Const 0.0
        | ATangent x -> if n % 2 = 0 then Const 0.0 else if n % 2 = 1 then taylor x n else Const 0.0
        | Exponential x -> if n = 0 then Const 1.0 else if n = 1 then taylor x n else Multiplication (taylor x n, taylorSeries x (n - 1))
        | Logarithm (x, y) -> if n = 0 then Const 0.0 else if n = 1 then taylor x n else Division (taylor x n, taylor y n)
        | SquareRoot x -> if n = 0 then Const 1.0 else if n = 1 then taylor x n else Division (taylor x n, Multiplication (Const 2.0, SquareRoot x))
        | AbsoluteValue x -> if n = 0 then taylor x n else AbsoluteValue (taylor x n)
        | Floor x -> if n = 0 then Const 0.0 else Floor (taylor x n)
        | Ceiling x -> if n = 0 then Const 0.0 else Ceiling (taylor x n)
        | Truncate x -> if n = 0 then Const 0.0 else Truncate (taylor x n)
        
    taylor expression n |> simplify
        
        
        
        
        
        