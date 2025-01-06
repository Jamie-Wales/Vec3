/// <summary>
/// Value operations for the interpreter.
/// </summary>

module Vec3.Interpreter.Backend.Value

open Microsoft.FSharp.Core
open Vec3.Interpreter.Backend.Types
open System

/// <summary>
/// Helper function to filter a list by index.
/// </summary>
/// <param name="f">The filter function.</param>
/// <param name="l">The list to filter.</param>
/// <returns>The filtered list.</returns>
let filteri (f: int -> 'T -> bool) (l: 'T list) =
    l |> List.indexed |> List.filter (fun (i, x) -> f i x) |> List.map snd

/// <summary>
/// Prints a value (for debugging).
/// </summary>
/// <param name="value">The value to print.</param>
/// <returns>String representation of the value.</returns>
let printValue value =
    printfn $"Printed value: {valueToString value}"

/// <summary>
/// Determines if a value is truthy.
/// </summary>
let isTruthy =
    function
    | VBoolean false -> false
    | VNil -> false
    | _ -> true

/// <summary>
/// Gets the type of a value.
/// </summary>
/// <param name="v">The value.</param>
/// <returns>The type of the value.</returns>
let typeOf (v: Value) : string =
    match v with
    | VBoolean _ -> "Boolean"
    | VNumber(VInteger _) -> "Integer"
    | VNumber(VFloat _) -> "Float"
    | VNumber(VRational _) -> "Rational"
    | VNumber(VComplex _) -> "Complex"
    | VNumber(VChar _) -> "Char"
    | VString _ -> "String"
    | VList(_, LIST) -> "List"
    | VList(_, RECORD) -> "Record"
    | VList(_, TUPLE) -> "Tuple"
    | VFunction _ -> "Function"
    | VClosure _ -> "Closure"
    | VNil -> "Nil"
    | VBlock _ -> "Block"
    | VEventListener _ -> "EventListener"
    | VPromise _ -> "Promise"
    | VBuiltin _ -> "Builtin"
    | VPlotData _ -> "PlotData"
    | VPlotFunction _ -> "PlotFunction"
    | VPlotFunctions _ -> "PlotFunctions"
    | VShape _ -> "Shape"
    | VShapes _ -> "Shapes"
    | VOutput _ -> "Output"
    | VAsyncFunction _ -> "AsyncFunction"

/// <summary>
/// Casts a value to a specific type.
/// </summary>
/// <param name="org">The original value.</param>
/// <param name="castTyp">The type to cast to.</param>
/// <returns>The cast value.</returns>
let rec cast (org: Value) (castTyp: Value) : Value =
    match castTyp with
    | VBoolean _ -> castToBool org
    | VNumber(VInteger _) -> castToInt org
    | VNumber(VFloat _) -> castToFloat org
    | VNumber(VRational _) -> castToRat org
    | VNumber(VComplex _) -> castToComp org
    | VNumber(VChar _) -> castToChar org
    | VString _ -> castToString org
    | VList(l, LIST) -> castToList org (List.tryHead l)
    | _ -> org

/// <summary>
/// Casts a value to a boolean.
/// </summary>
/// <param name="a">The value to cast.</param>
/// <returns>The cast value.</returns>
and castToBool a =
    match a with
    | VBoolean v -> VBoolean v
    | VNil -> VBoolean false
    | VString s ->
        if String.IsNullOrEmpty s then
            VBoolean false
        else
            VBoolean true
    | VNumber _ -> VBoolean true
    | VList(l, _) -> if (List.isEmpty l) then (VBoolean false) else VBoolean true
    | _ -> VBoolean true

/// <summary>
/// Casts a value to a list.
/// </summary>
/// <param name="a">The value to cast to</param>
/// <param name="typ">The type of the list.</param>
/// <returns>The cast value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be cast to a list.</exception>
and castToList a typ =
    match a with
    | VList(l, _) ->
        if Option.isSome typ then
            VList(List.map (fun el -> cast el (Option.get typ)) l, LIST)
        else
            VList(l, LIST)
    | VString s -> VList(s |> Seq.map (fun c -> VNumber(VChar c)) |> Seq.toList, LIST)
    | _ -> VList([ a ], LIST)

/// <summary>
/// Casts a value to an integer.
/// </summary>
/// <param name="a">The value to cast.</param>
/// <returns>The cast value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be cast to an integer.</exception>
and castToInt a =
    match a with
    | VNumber(VInteger i) -> VNumber(VInteger i)
    | VNumber(VFloat f) -> VNumber(VInteger(int f))
    | VNumber(VRational(num, denom)) -> VNumber(VInteger(num / denom))
    | VNumber(VComplex(r, _)) -> VNumber(VInteger(int r))
    | VNumber(VChar c) -> VNumber(VInteger(int c))
    | VString s ->
        match Int32.TryParse(s) with
        | true, i -> VNumber(VInteger i)
        | _ -> raise <| InvalidOperationException("Invalid string for integer conversion")
    | VBoolean b -> VNumber(VInteger(if b then 1 else 0))
    | _ -> raise <| InvalidOperationException("Cannot cast to integer")

/// <summary>
/// Casts a value to a float.
/// </summary>
/// <param name="a">The value to cast.</param>
/// <returns>The cast value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be cast to a float.</exception>
and castToFloat a =
    match a with
    | VNumber(VFloat f) -> VNumber(VFloat f)
    | VNumber(VInteger i) -> VNumber(VFloat(float i))
    | VNumber(VRational(num, denom)) -> VNumber(VFloat((float num) / (float denom)))
    | VNumber(VComplex(r, _)) -> VNumber(VFloat r)
    | VNumber(VChar c) -> VNumber(VFloat(float c))
    | VString s ->
        match Double.TryParse(s) with
        | true, f -> VNumber(VFloat f)
        | _ -> raise <| InvalidOperationException("Invalid string for float conversion")
    | VBoolean b -> VNumber(VFloat(if b then 1.0 else 0.0))
    | _ -> raise <| InvalidOperationException("Cannot cast to float")

/// <summary>
/// Casts a value to a char.
/// </summary>
/// <param name="a">The value to cast.</param>
/// <returns>The cast value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be cast to a char.</exception>
and castToChar a =
    match a with
    | VNumber(VInteger i) -> VNumber(VChar(char i))
    | VNumber(VFloat i) -> VNumber(VChar(char i))
    | VNumber(VRational(i, _)) -> VNumber(VChar(char i))
    | VNumber(VComplex(i, _)) -> VNumber(VChar(char i))
    | VString s ->
        match String.length s with
        | 1 -> VNumber(VChar s[0])
        | _ ->
            raise
            <| InvalidOperationException $"Invalid string for char conversion, got {s}"
    | _ -> raise <| InvalidOperationException("Cannot cast to char")

/// <summary>
/// Casts a value to a rational.
/// </summary>
/// <param name="a">The value to cast.</param>
/// <returns>The cast value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be cast to a rational.</exception>
and castToRat a =
    match a with
    | VNumber(VRational(num, denom)) -> VNumber(VRational(num, denom))
    | VNumber(VInteger i) -> VNumber(VRational(i, 1))
    | VNumber(VFloat f) ->
        let num = int (f * 10000.0)
        let denom = 10000
        VNumber(VRational(num, denom))
    | VNumber(VChar c) -> VNumber(VRational(int c, 1))
    | VNumber(VComplex(r, _)) -> VNumber(VRational(int (r * 10000.0), 10000))
    | VString s ->
        match Double.TryParse(s) with
        | true, f ->
            let num = int (f * 10000.0)
            let denom = 10000
            VNumber(VRational(num, denom))
        | _ -> raise <| InvalidOperationException("Invalid string for rational conversion")
    | VBoolean b -> VNumber(VRational(if b then (1, 1) else 0, 1))
    | _ -> raise <| InvalidOperationException("Cannot cast to rational")

/// <summary>
/// Casts a value to a complex number.
/// </summary>
/// <param name="a">The value to cast.</param>
/// <returns>The cast value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be cast to a complex number.</exception>
and castToComp a =
    match a with
    | VNumber(VComplex(r, i)) -> VNumber(VComplex(r, i))
    | VNumber(VInteger i) -> VNumber(VComplex(float i, 0.0))
    | VNumber(VFloat f) -> VNumber(VComplex(f, 0.0))
    | VNumber(VRational(num, denom)) -> VNumber(VComplex((float num) / (float denom), 0.0))
    | VNumber(VChar c) -> VNumber(VComplex(float c, 0.0))
    | VString s ->
        match Double.TryParse(s) with
        | true, f -> VNumber(VComplex(f, 0.0))
        | _ -> raise <| InvalidOperationException("Invalid string for complex conversion")
    | VBoolean b -> VNumber(VComplex(if b then 1.0, 0.0 else 0.0, 0.0))
    | _ -> raise <| InvalidOperationException("Cannot cast to complex")

/// <summary>
/// Casts a value to a string.
/// </summary>
/// <param name="a">The value to cast.</param>
/// <returns>The cast value.</returns>
and castToString a =
    match a with
    | VString s -> VString s
    | VBoolean b -> VString(if b then "true" else "false")
    | VNumber(VInteger i) -> VString(string i)
    | VNumber(VFloat f) -> VString(string f)
    | VNumber(VRational(num, denom)) -> VString $"%d{num}/%d{denom}"
    | VNumber(VComplex(r, i)) -> VString $"%f{r} + %f{i}"
    | VNumber(VChar c) -> VString $"'%c{c}'"
    | VList(l, _) ->
        VString(
            "["
            + String.concat
                ", "
                (List.map
                    (fun el ->
                        match castToString el with
                        | VString s -> s
                        | _ -> "")
                    l)
            + "]"
        )
    | _ -> VString ""

/// <summary>
/// Tests if two values are equal.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <returns>True if the values are equal.</returns>
let rec valuesEqual (a: Value) (b: Value) =
    match (a, b) with
    | VNumber x, VNumber y -> numbersEqual x y
    | VBoolean x, VBoolean y -> x = y
    | VString x, VString y -> x = y
    | VFunction(f1, _), VFunction(f2, _) -> f1.Name = f2.Name && f1.Arity = f2.Arity
    | VClosure(c1, _), VClosure(c2, _) -> c1.Function = c2.Function
    | VNil, VNil -> true
    | VList(l1, typ1), VList(l2, typ2) ->
        if List.length l1 <> List.length l2 || typ1 <> typ2 then
            false
        else
            List.forall2 valuesEqual l1 l2
    | _ -> false

/// <summary>
/// Tests if two numbers are equal.
/// </summary>
/// <param name="a">The first number.</param>
/// <param name="b">The second number.</param>
/// <returns>True if the numbers are equal.</returns>
and numbersEqual (a: VNumber) (b: VNumber) =
    match (a, b) with
    | VInteger x, VInteger y -> x = y
    | VFloat x, VFloat y -> x = y
    | VRational(n1, d1), VRational(n2, d2) -> n1 * d2 = n2 * d1
    | VComplex(r1, i1), VComplex(r2, i2) -> r1 = r2 && i1 = i2
    | VChar x, VChar y -> x = y
    | _ ->
        let f1, f2 = (floatValue a, floatValue b)
        f1 = f2

/// <summary>
/// Converts a value to a float.
/// </summary>
/// <returns>The float value.</returns>
and floatValue =
    function
    | VInteger n -> float n
    | VFloat f -> f
    | VRational(n, d) -> float n / float d
    | VComplex(r, _) -> r // Note: This loses imaginary part information
    | VChar c -> float c

/// <summary>
/// Converts a value to a complex number.
/// </summary>
/// <returns>The complex value.</returns>
let toComplex =
    function
    | VInteger n -> VComplex(float n, 0.0)
    | VFloat f -> VComplex(f, 0.0)
    | VRational(n, d) -> VComplex(float n / float d, 0.0)
    | VComplex(r, i) -> VComplex(r, i)
    | VChar c -> VComplex(float c, 0.0)

/// <summary>
/// Adds two values.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <returns>The sum of the values.</returns>
/// <exception cref="InvalidOperationException">If the values cannot be added.</exception>
let rec add a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x + y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) ->
        let n = n1 * d2 + n2 * d1
        let d = d1 * d2
        VNumber(VRational(n, d))
    | VNumber(VComplex(r1, i1)), VNumber(VComplex(r2, i2)) -> VNumber(VComplex(r1 + r2, i1 + i2))
    | VNumber(VChar x), VNumber(VChar y) -> VNumber(VChar(char (int x + int y)))

    | VNumber _ as x, (VNumber(VInteger _) as y) ->
        let y = cast y x
        add x y
    | VNumber(VInteger _) as y, (VNumber _ as x) ->
        let y = cast y x
        add y x

    | VNumber x, VNumber y -> VNumber(VFloat(floatValue x + floatValue y))
    | VList(l1, t), VList(l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> add x y) zipped
        VList(added, t)
    | _ -> raise <| InvalidOperationException("Can only add numbers")

/// <summary>
/// Subtracts two values.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <returns>The difference of the values.</returns>
/// <exception cref="InvalidOperationException">If the values cannot be subtracted.</exception>
let rec subtract a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x - y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) ->
        let n = n1 * d2 - n2 * d1
        let d = d1 * d2
        VNumber(VRational(n, d))
    | VNumber(VComplex(r1, i1)), VNumber(VComplex(r2, i2)) -> VNumber(VComplex(r1 - r2, i1 - i2))
    | VNumber(VChar x), VNumber(VChar y) -> VNumber(VChar(char (int x - int y)))

    | VNumber _ as x, (VNumber(VInteger _) as y) ->
        let y = cast y x
        subtract x y
    | VNumber(VInteger _) as y, (VNumber _ as x) ->
        let y = cast y x
        subtract y x

    | VNumber x, VNumber y -> VNumber(VFloat(floatValue x - floatValue y))
    | VList(l1, t), VList(l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> subtract x y) zipped
        VList(added, t)
    | _ -> raise <| InvalidOperationException("Can only subtract numbers")

/// <summary>
/// Multiplies two values.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <returns>The product of the values.</returns>
/// <exception cref="InvalidOperationException">If the values cannot be multiplied.</exception>
let rec multiply a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x * y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> VNumber(VRational(n1 * n2, d1 * d2))
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) -> VNumber(VComplex(a * c - b * d, a * d + b * c))
    | VNumber(VChar x), VNumber(VChar y) -> VNumber(VChar(char (int x * int y)))
    | VNumber _ as x, (VNumber(VInteger _) as y) ->
        let y = cast y x // Int will always be cast to the other type
        multiply x y
    | VNumber(VInteger _) as y, (VNumber _ as x) ->
        let y = cast y x
        multiply y x
    | VNumber x, VNumber y -> VNumber(VFloat(floatValue x * floatValue y))
    | VList(l1, t), VList(l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> multiply x y) zipped
        VList(added, t)
    | _ -> raise <| InvalidOperationException("Can only multiply numbers")

/// <summary>
/// Divides two values.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <returns>The division of the values.</returns>
/// <exception cref="InvalidOperationException">If the values cannot be divided.</exception>
let rec divide a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) when y <> 0 -> VNumber(VInteger(x / y))
    // else
    //     VNumber(VRational(x, y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) when n2 <> 0 -> VNumber(VRational(n1 * d2, d1 * n2))
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) when c <> 0.0 || d <> 0.0 ->
        let denominator = c * c + d * d
        VNumber(VComplex((a * c + b * d) / denominator, (b * c - a * d) / denominator))
    | VNumber(VChar x), VNumber(VChar y) -> VNumber(VChar(char (int x / int y)))
    | VNumber _ as x, (VNumber(VInteger _) as y) ->
        let y = cast y x
        divide x y
    | VNumber(VInteger _) as y, (VNumber _ as x) ->
        let y = cast y x
        divide y x
    | VNumber x, VNumber y ->
        let f1, f2 = floatValue x, floatValue y

        if f2 = 0.0 then
            raise <| InvalidOperationException("Cannot divide by zero")

        VNumber(VFloat(f1 / f2))
    | VList(l1, t), VList(l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> divide x y) zipped
        VList(added, t)
    | _ -> raise <| InvalidOperationException("Can only divide numbers")

/// <summary>
/// Finds the dot product of two lists.
/// </summary>
/// <param name="a">The first list.</param>
/// <param name="b">The second list.</param>
/// <returns>The dot product of the lists.</returns>
/// <exception cref="InvalidOperationException">If the lists cannot be dotted.</exception>
let dotProduct a b =
    match (a, b) with
    | VList(l1, _), VList(l2, _) ->
        let rec dotVectors lhs rhs =
            match lhs, rhs with
            | [], [] -> 0
            | l :: ls, r :: rs ->
                match l, r with
                | VNumber(VInteger x), VNumber(VInteger y) -> x * y + dotVectors ls rs
                | _ ->
                    raise
                    <| InvalidOperationException("Dot product can only be taken between vectors of integers")
            | _ -> raise <| InvalidOperationException("Vectors must be of the same length")

        VNumber(VInteger(dotVectors l1 l2))
    | _ -> raise <| InvalidOperationException("Can only take dot product of vectors")

/// <summary>
/// Finds the cross product of two lists.
/// </summary>
/// <param name="a">The first list.</param>
/// <param name="b">The second list.</param>
/// <returns>The cross product of the lists.</returns>
/// <exception cref="InvalidOperationException">If the lists cannot be crossed.</exception>
let crossProduct a b =
    match (a, b) with
    | VList(l1, t), VList(l2, _) ->
        let rec crossVectors lhs rhs =
            match lhs, rhs with
            | [ VNumber(VInteger x); VNumber(VInteger y); VNumber(VInteger z) ],
              [ VNumber(VInteger a); VNumber(VInteger b); VNumber(VInteger c) ] ->
                [ VNumber(VInteger(y * c - z * b))
                  VNumber(VInteger(z * a - x * c))
                  VNumber(VInteger(x * b - y * a)) ]
            | _ ->
                raise
                <| InvalidOperationException("Cross product can only be taken between vectors of integers")

        VList(crossVectors l1 l2, t)
    | _ -> raise <| InvalidOperationException("Can only take cross product of vectors")

/// <summary>
/// Negates a value.
/// </summary>
/// <param name="value">The value to negate.</param>
/// <returns>The negated value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be negated.</exception>
let negate value =
    match value with
    | VNumber(VInteger n) -> VNumber(VInteger(-n))
    | VNumber(VFloat n) -> VNumber(VFloat(-n))
    | VNumber(VRational(n, d)) -> VNumber(VRational(-n, d))
    | VNumber(VComplex(r, i)) -> VNumber(VComplex(-r, -i))
    | VNumber(VChar c) -> VNumber(VChar(char (-int c)))
    | _ -> raise <| InvalidOperationException("Can only negate numbers")

/// <summary>
/// Unnegates a value.
/// </summary>
/// <param name="value">The value to unnegate.</param>
/// <returns>The unnegated value.</returns>
/// <exception cref="InvalidOperationException">If the value cannot be unnegated.</exception>
let unnegate value =
    match value with
    | VNumber(VInteger n) -> VNumber(VInteger(if n < 0 then -n else n))
    | VNumber(VFloat n) -> VNumber(VFloat(if n < 0.0 then -n else n))
    | VNumber(VRational(n, d)) -> VNumber(VRational(if n < 0 then -n, d else n, d))
    | VNumber(VComplex(r, i)) ->
        let r = if r < 0.0 then -r else r
        let i = if i < 0.0 then -i else i
        VNumber(VComplex(r, i))
    | _ -> raise <| InvalidOperationException("Can only unnegate numbers")

/// <summary>
/// Raises a value to a power.
/// </summary>
/// <param name="a">The base value.</param>
/// <param name="b">The exponent value.</param>
/// <returns>The value raised to the power.</returns>
/// <exception cref="InvalidOperationException">If the values cannot be raised to a power.</exception>
let rec power a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(int (float x ** float y)))
    | VNumber(VFloat x), VNumber(VFloat y) -> VNumber(VFloat(x ** y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) ->
        VNumber(VRational(int (float n1 ** float n2), int (float d1 ** float d2)))
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) ->
        let r = a ** c * Math.Exp(-b * d)
        let i = a ** d * Math.Exp(b * c)
        VNumber(VComplex(r, i))
    | VNumber _ as x, (VNumber(VInteger _) as y) ->
        let y = cast y x
        power x y
    | VNumber(VInteger _) as y, (VNumber _ as x) ->
        let y = cast y x
        power x y
    | VNumber x, VNumber y -> VNumber(VFloat(floatValue x ** floatValue y))
    | _ -> raise <| InvalidOperationException("Can only raise numbers to a power")

/// <summary>
/// Compares two values.
/// </summary>
/// <param name="a">The first value.</param>
/// <param name="b">The second value.</param>
/// <returns>The comparison of the values.</returns>
/// <exception cref="InvalidOperationException">If the values cannot be compared.</exception>
let compare a b =
    match (a, b) with
    | VNumber x, VNumber y ->
        match (x, y) with
        | VInteger x, VInteger y -> compare x y
        | VRational(n1, d1), VRational(n2, d2) -> compare (n1 * d2) (n2 * d1)
        | VComplex(r1, i1), VComplex(r2, i2) ->
            let res = compare r1 r2

            match res with
            | 0 -> compare i1 i2
            | _ -> res
        | VChar x, VChar y -> compare x y
        | _ -> compare (floatValue x) (floatValue y)
    | _ -> raise <| InvalidOperationException("Can only compare numbers")

/// <summary>
/// Find the root of a function using the Newton-Raphson method.
/// </summary>
/// <param name="f">The function to find the root of.</param>
/// <param name="f'">The derivative of the function.</param>
/// <param name="initialGuess">The initial guess for the root.</param>
/// <param name="tolerance">The tolerance for the root.</param>
/// <param name="maxIterations">The maximum number of iterations.</param>
/// <returns>The root of the function.</returns>
/// <exception cref="InvalidOperationException">If the root cannot be found.</exception>
let newtonRaphson
    (f: float -> float)
    (f': float -> float)
    (initialGuess: float)
    (tolerance: float)
    (maxIterations: int)
    =
    /// <summary>
    /// The recursive Newton-Raphson function.
    /// </summary>
    /// <param name="x">The current guess for the root.</param>
    /// <param name="n">The current iteration.</param>
    /// <returns>The root of the function.</returns>
    /// <exception cref="InvalidOperationException">If the root cannot be found.</exception>
    let rec iterate x n =
        if n >= maxIterations then
            raise <| InvalidOperationException("Exceeded maximum number of iterations.")
        else
            let fx = f x

            if abs fx < tolerance then
                x
            else
                let xNew = x - fx / (f' x)
                iterate xNew (n + 1)

    iterate initialGuess 0

/// <summary>
/// Find the root of a function using the bisection rule.
/// </summary>
/// <param name="f">The function to find the root of.</param>
/// <param name="a">The lower bound of the interval.</param>
/// <param name="b">The upper bound of the interval.</param>
/// <param name="tolerance">The tolerance for the root.</param>
/// <param name="maxIterations">The maximum number of iterations.</param>
/// <returns>The root of the function.</returns>
/// <exception cref="InvalidOperationException">If the root cannot be found.</exception>
let bisection (f: float -> float) (a: float) (b: float) (tolerance: float) (maxIterations: int) =
    /// <summary>
    /// The recursive bisection function.
    /// </summary>
    /// <param name="a">The lower bound of the interval.</param>
    /// <param name="b">The upper bound of the interval.</param>
    /// <param name="n">The current iteration.</param>
    /// <returns>The root of the function.</returns>
    /// <exception cref="InvalidOperationException">If the root cannot be found.</exception>
    let rec iterate (a: float) (b: float) (n: int) =
        let midpoint = (a + b) / 2.0
        let fMid = f midpoint

        if abs fMid < tolerance then
            midpoint // Root found within tolerance
        elif n >= maxIterations then
            raise <| InvalidOperationException("Exceeded maximum number of iterations.")
        elif f a * fMid < 0.0 then
            iterate a midpoint (n + 1) // Root is in the left half
        else
            iterate midpoint b (n + 1) // Root is in the right half

    if f a * f b >= 0.0 then
        raise
        <| InvalidOperationException("Root not bracketed by the initial interval.")
    else
        iterate a b 0


/// <summary>
/// Calculates the determinant of a matrix.
/// </summary>
/// <param name="matrix">The matrix to calculate the determinant of.</param>
/// <returns>The determinant of the matrix.</returns>
/// <exception cref="InvalidOperationException">If the matrix is not square.</exception>
let rec calcDeterminant (matrix: Value) : Value =
    match matrix with
    | VList(rows, _) ->
        let rows =
            List.map
                (function
                | VList(row, _) -> row
                | _ -> [])
                rows

        let size = List.length rows

        if size <> List.length (List.head rows) then
            raise <| InvalidOperationException("Matrix must be square")
        elif size = 1 then
            List.head (List.head rows)
        elif size = 2 then
            match rows with
            | [ [ a; b ]; [ c; d ] ] -> subtract (multiply a d) (multiply b c)
            | _ -> raise <| InvalidOperationException("Invalid matrix")
        else
            // Use first row for cofactor expansion
            let firstRow = List.head rows

            let rec cofactorExpansion acc index =
                if index >= List.length firstRow then
                    acc
                else
                    let element = List.item index firstRow

                    let minorMatrix =
                        let newRows =
                            rows
                            |> List.tail // Skip first row
                            |> List.map (fun r -> r |> filteri (fun i _ -> i <> index)) // Remove column

                        VList(newRows |> List.map (fun r -> VList(r, LIST)), LIST)

                    let sign = if index % 2 = 0 then 1.0 else -1.0

                    let determinantContrib =
                        multiply (multiply (VNumber(VFloat sign)) element) (calcDeterminant minorMatrix)

                    cofactorExpansion (add acc determinantContrib) (index + 1)

            cofactorExpansion (VNumber(VFloat 0.0)) 0
    | _ -> raise <| InvalidOperationException("Can only calculate determinant of a matrix")

/// <summary>
/// Finds the tranpose of a matrix.
/// </summary>
/// <param name="matrix">The matrix to transpose.</param>
/// <returns>The transpose of the matrix.</returns>
/// <exception cref="InvalidOperationException">If the value is not a matrix.</exception>
let rec transposeMatrix (matrix: Value) : Value =
    match matrix with
    | VList(rows, _) ->
        let rows =
            List.map
                (function
                | VList(row, _) -> row
                | _ -> [])
                rows

        let columns =
            List.init (List.length (List.head rows)) (fun i -> List.map (List.item i) rows)

        VList(columns |> List.map (fun c -> VList(c, LIST)), LIST)
    | _ -> raise <| InvalidOperationException("Can only transpose a matrix")

/// <summary>
/// Finds the inverse of a matrix.
/// </summary>
/// <param name="matrix">The matrix to invert.</param>
/// <returns>The inverse of the matrix.</returns>
/// <exception cref="InvalidOperationException">If the value is not a matrix.</exception>
let rec inverseMatrix (matrix: Value) : Value =
    match matrix with
    | VList(rows, _) ->
        let det = calcDeterminant matrix

        let _ =
            match castToFloat det with
            | VNumber(VFloat 0.0) -> raise <| InvalidOperationException("Matrix is singular")
            | _ -> ()

        let rows =
            List.map
                (function
                | VList(row, _) -> row
                | _ -> [])
                rows

        let cofactorMatrix =
            rows
            |> List.mapi (fun i row ->
                row
                |> List.mapi (fun j _ ->
                    let minorMatrix =
                        let newRows =
                            rows
                            |> filteri (fun x _ -> x <> i)
                            |> List.map (fun r -> r |> filteri (fun y _ -> y <> j))

                        VList(newRows |> List.map (fun r -> VList(r, LIST)), LIST)

                    let sign = if (i + j) % 2 = 0 then 1.0 else -1.0
                    multiply (VNumber(VFloat sign)) (calcDeterminant minorMatrix)))

        let adjugateMatrix =
            transposeMatrix (VList(cofactorMatrix |> List.map (fun c -> VList(c, LIST)), LIST))

        let inverseMatrix =
            match adjugateMatrix with
            | VList(rows, _) ->
                let newRows =
                    rows
                    |> List.map (function
                        | VList(row, _) -> VList(List.map (fun el -> divide el det) row, LIST)
                        | _ -> VList([], LIST))

                VList(newRows, LIST)
            | _ -> VList([], LIST)

        inverseMatrix
    | _ -> raise <| InvalidOperationException("Can only invert a matrix")
