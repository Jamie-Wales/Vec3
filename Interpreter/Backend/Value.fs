/// <summary>
/// Value operations for the interpreter.
/// </summary>

module Vec3.Interpreter.Backend.Value

open Microsoft.FSharp.Core
open Vec3.Interpreter.Backend.Types
open System

let printValue value = printfn $"Printed value: {valueToString value}"

let isTruthy =
    function
    | VBoolean false -> false
    | VNil -> false
    | _ -> true

let rec valuesEqual (a: Value) (b: Value) =
    match (a, b) with
    | VNumber x, VNumber y -> numbersEqual x y
    | VBoolean x, VBoolean y -> x = y
    | VString x, VString y -> x = y
    | VFunction (f1, _), VFunction (f2, _) -> f1.Name = f2.Name && f1.Arity = f2.Arity
    | VClosure c1, VClosure c2 -> c1.Function = c2.Function
    | VNil, VNil -> true
    | VList (l1, typ1), VList (l2, typ2) -> 
        if List.length l1 <> List.length l2 || typ1 <> typ2 then false
        else List.forall2 valuesEqual l1 l2
    | _ -> false

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
        
and floatValue =
    function
    | VInteger n -> float n
    | VFloat f -> f
    | VRational(n, d) -> float n / float d
    | VComplex(r, _) -> r  // Note: This loses imaginary part information
    | VChar c -> float c

let toComplex =
    function
    | VInteger n -> VComplex(float n, 0.0)
    | VFloat f -> VComplex(f, 0.0)
    | VRational(n, d) -> VComplex(float n / float d, 0.0)
    | VComplex(r, i) -> VComplex(r, i)
    | VChar c -> VComplex(float c, 0.0)

let rec add a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x + y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> 
        let n = n1 * d2 + n2 * d1
        let d = d1 * d2
        VNumber(VRational(n, d))  // Consider simplifying the fraction
    | VNumber(VComplex(r1, i1)), VNumber(VComplex(r2, i2)) -> 
        VNumber(VComplex(r1 + r2, i1 + i2))
    | VNumber(VChar x), VNumber(VChar y) -> VNumber(VChar(char(int x + int y)))
    | VNumber x, VNumber y ->
        VNumber(VFloat(floatValue x + floatValue y))
    | VList (l1, t), VList (l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> add x y) zipped
        VList (added, t)
    | _ -> raise <| InvalidOperationException("Can only add numbers")

let rec subtract a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x - y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> 
        let n = n1 * d2 - n2 * d1
        let d = d1 * d2
        VNumber(VRational(n, d))  
    | VNumber(VComplex(r1, i1)), VNumber(VComplex(r2, i2)) -> 
        VNumber(VComplex(r1 - r2, i1 - i2))
    | VNumber (VChar x), VNumber (VChar y) -> VNumber (VChar (char (int x - int y)))
    | VNumber x, VNumber y ->
        VNumber(VFloat(floatValue x - floatValue y))
    | VList (l1, t), VList (l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> subtract x y) zipped
        VList (added, t)
    | _ -> raise <| InvalidOperationException("Can only subtract numbers")

let rec multiply a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x * y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> 
        VNumber(VRational(n1 * n2, d1 * d2))  
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) -> 
        VNumber(VComplex(a*c - b*d, a*d + b*c))
    | VNumber(VChar x), VNumber(VChar y) -> VNumber(VChar(char(int x * int y)))
    | VNumber x, VNumber y ->
        VNumber(VFloat(floatValue x * floatValue y))
    | VList (l1, t), VList (l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> multiply x y) zipped
        VList (added, t)
    | _ -> raise <| InvalidOperationException("Can only multiply numbers")

let rec divide a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) when y <> 0 -> 
        if x % y = 0 then VNumber(VInteger(x / y)) else VNumber(VRational(x, y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) when n2 <> 0 -> 
        VNumber(VRational(n1 * d2, d1 * n2))  
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) when c <> 0.0 || d <> 0.0 -> 
        let denominator = c*c + d*d
        VNumber(VComplex((a*c + b*d) / denominator, (b*c - a*d) / denominator))
    | VNumber(VChar x), VNumber(VChar y) -> VNumber(VChar(char(int x / int y)))
    | VNumber x, VNumber y ->
        let f1, f2 = floatValue x, floatValue y
        if f2 = 0.0 then raise <| InvalidOperationException("Cannot divide by zero")
        VNumber(VFloat(f1 / f2))
    | VList (l1, t), VList (l2, _) ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> divide x y) zipped
        VList (added, t)
    | _ -> raise <| InvalidOperationException("Can only divide numbers")

let dotProduct a b =
    match (a, b) with
    | VList (l1, _), VList (l2, _) ->
            let rec dotVectors lhs rhs =
                match lhs, rhs with
                | [], [] -> 0
                | l :: ls, r :: rs ->
                    match l, r with
                    | VNumber(VInteger x), VNumber(VInteger y) -> x * y + dotVectors ls rs
                    | _ -> raise <| InvalidOperationException("Dot product can only be taken between vectors of integers")
                | _ -> raise <| InvalidOperationException("Vectors must be of the same length")
            VNumber(VInteger(dotVectors l1 l2))
    | _ -> raise <| InvalidOperationException("Can only take dot product of vectors")

let crossProduct a b =
    match (a, b) with
    | VList (l1, t), VList (l2, _) ->
            let rec crossVectors lhs rhs =
                match lhs, rhs with
                | [VNumber(VInteger x); VNumber(VInteger y); VNumber(VInteger z)], [VNumber(VInteger a); VNumber(VInteger b); VNumber(VInteger c)] ->
                    [VNumber(VInteger(y * c - z * b)); VNumber(VInteger(z * a - x * c)); VNumber(VInteger(x * b - y * a))]
                | _ -> raise <| InvalidOperationException("Cross product can only be taken between vectors of integers")
            VList(crossVectors l1 l2, t)
    | _ -> raise <| InvalidOperationException("Can only take cross product of vectors")

let negate value =
    match value with
    | VNumber(VInteger n) -> VNumber(VInteger(-n))
    | VNumber(VFloat n) -> VNumber(VFloat(-n))
    | VNumber(VRational(n, d)) -> VNumber(VRational(-n, d))
    | VNumber(VComplex(r, i)) -> VNumber(VComplex(-r, -i))
    | VNumber(VChar c) -> VNumber(VChar(char(-int c)))
    | _ -> raise <| InvalidOperationException("Can only negate numbers")

let unnegate value =
    match value with
    | VNumber(VInteger n) -> VNumber(VInteger(if n < 0 then -n else n))
    | VNumber(VFloat n) -> VNumber(VFloat(if n < 0.0 then -n else n))
    | VNumber(VRational(n, d)) -> VNumber(VRational(if n < 0 then -n, d else n, d))
    // | VNumber(VComplex(r, i)) -> VNumber(VComplex(if r < 0.0 then -r else r, if i < 0.0 then -i else i))
    | _ -> raise <| InvalidOperationException("Can only unnegate numbers")

let power a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(int (float x ** float y)))
    | VNumber(VFloat x), VNumber(VFloat y) -> VNumber(VFloat(x ** y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> 
        VNumber(VRational(int (float n1 ** float n2), int (float d1 ** float d2)))
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) ->
        let r = a ** c * Math.Exp(-b * d)
        let i = a ** d * Math.Exp(b * c)
        VNumber(VComplex(r, i))
    | VNumber x, VNumber y ->
        VNumber(VFloat(floatValue x ** floatValue y))
    | _ -> raise <| InvalidOperationException("Can only raise numbers to a power")

let compare a b =
    match (a, b) with
    | VNumber x, VNumber y ->
        match (x, y) with
        | VInteger x, VInteger y -> compare x y
        | VRational(n1, d1), VRational(n2, d2) -> 
            compare (n1 * d2) (n2 * d1)
        | VComplex (r1, i1), VComplex (r2, i2) ->
            let res = compare r1 r2
            match res with
            | 0 -> compare i1 i2
            | _ -> res
        | VChar x, VChar y -> compare x y
        | _ -> compare (floatValue x) (floatValue y)
    | _ -> raise <| InvalidOperationException("Can only compare numbers")

let rec cast org castTyp =
      match castTyp with
      | VBoolean _ -> castToBool org
      | VNumber(VInteger _) -> castToInt org
      | VNumber(VFloat _) -> castToFloat org
      | VNumber(VRational _) -> castToRat org
      | VNumber(VComplex _) -> castToComp org
      | VNumber(VChar _) -> castToChar org
      | VString _ -> castToString org
      | VList (l, LIST) -> castToList org (List.tryHead l)
      | _ -> org
      
and castToBool a =
    match a with
    | VBoolean v -> VBoolean v
    | VNil -> VBoolean false
    | VString s -> if String.IsNullOrEmpty s then VBoolean false else VBoolean true
    | VNumber _ -> VBoolean true
    | VList (l, _) -> if (List.isEmpty l) then (VBoolean false) else VBoolean true
    | _ -> VBoolean true

and castToList a typ =
    match a with
    | VList (l, _) -> if Option.isSome typ then VList (List.map (fun el -> cast el (Option.get typ)) l, LIST) else VList (l, LIST)
    | VString s -> VList (s |> Seq.map (fun c -> VNumber (VChar c)) |> Seq.toList, LIST)
    | _ -> VList ([a], LIST)
    
and castToInt a =
    match a with
    | VNumber (VInteger i) -> VNumber (VInteger i)
    | VNumber (VFloat f) -> VNumber (VInteger (int f))
    | VNumber (VRational (num, denom)) -> VNumber (VInteger (num / denom))
    | VNumber (VComplex (r, _)) -> VNumber (VInteger (int r))
    | VNumber (VChar c) -> VNumber (VInteger (int c))
    | VString s -> 
        match Int32.TryParse(s) with
        | true, i -> VNumber (VInteger i)
        | _ -> raise <| InvalidOperationException("Invalid string for integer conversion")
    | VBoolean b -> VNumber (VInteger (if b then 1 else 0))
    | _ -> raise <| InvalidOperationException("Cannot cast to integer")
    
and castToFloat a =
    match a with
    | VNumber (VFloat f) -> VNumber (VFloat f)
    | VNumber (VInteger i) -> VNumber (VFloat (float i))
    | VNumber (VRational (num, denom)) -> VNumber (VFloat ((float num) / (float denom)))
    | VNumber (VComplex (r, _)) -> VNumber (VFloat r)
    | VNumber (VChar c) -> VNumber (VFloat (float c))
    | VString s ->
        match Double.TryParse(s) with
        | true, f -> VNumber (VFloat f)
        | _ -> raise <| InvalidOperationException("Invalid string for float conversion")
    | VBoolean b -> VNumber (VFloat (if b then 1.0 else 0.0))
    | _ -> raise <| InvalidOperationException("Cannot cast to float")

and castToChar a =
    match a with
    | VNumber (VInteger i) -> VNumber (VChar (char i))
    | VString s -> 
        match String.length s with
        | 1 -> VNumber (VChar s.[0])
        | _ -> raise <| InvalidOperationException("Invalid string for char conversion")
    | _ -> raise <| InvalidOperationException("Cannot cast to char")
    
    
and castToRat a =
    match a with
    | VNumber (VRational (num, denom)) -> VNumber (VRational (num, denom))
    | VNumber (VInteger i) -> VNumber (VRational (i, 1))
    | VNumber (VFloat f) ->
        let num = int (f * 10000.0)
        let denom = 10000
        VNumber (VRational (num, denom))
    | VNumber (VChar c) -> VNumber (VRational (int c, 1))
    | VNumber (VComplex (r, _)) -> VNumber (VRational (int (r * 10000.0), 10000))
    | VString s ->
        match Double.TryParse(s) with
        | true, f ->
            let num = int (f * 10000.0)
            let denom = 10000
            VNumber (VRational (num, denom))
        | _ -> raise <| InvalidOperationException("Invalid string for rational conversion")
    | VBoolean b -> VNumber (VRational (if b then (1, 1) else 0, 1))
    | _ -> raise <| InvalidOperationException("Cannot cast to rational")
    
and castToComp a =
    match a with
    | VNumber (VComplex (r, i)) -> VNumber (VComplex (r, i))
    | VNumber (VInteger i) -> VNumber (VComplex (float i, 0.0))
    | VNumber (VFloat f) -> VNumber (VComplex (f, 0.0))
    | VNumber (VRational (num, denom)) -> VNumber (VComplex ((float num) / (float denom), 0.0))
    | VNumber (VChar c) -> VNumber (VComplex (float c, 0.0))
    | VString s ->
        match Double.TryParse(s) with
        | true, f -> VNumber (VComplex (f, 0.0))
        | _ -> raise <| InvalidOperationException("Invalid string for complex conversion")
    | VBoolean b -> VNumber (VComplex (if b then 1.0, 0.0 else 0.0, 0.0))
    | _ -> raise <| InvalidOperationException("Cannot cast to complex")
    
and castToString a =
    match a with
    | VString s -> VString s
    | VBoolean b -> VString (if b then "true" else "false")
    | VNumber (VInteger i) -> VString (string i)
    | VNumber (VFloat f) -> VString (string f)
    | VNumber (VRational (num, denom)) -> VString $"%d{num}/%d{denom}"
    | VNumber (VComplex (r, i)) -> VString $"%f{r} + %f{i}"
    | VNumber (VChar c) -> VString $"'%c{c}'"
    | VList (l, _) -> VString ("[" + String.concat ", " (List.map (fun el -> match castToString el with VString s -> s | _ -> "") l) + "]")
    | _ -> VString ""

let newtonRaphson (f: float -> float) (f' : float -> float) (initialGuess: float) (tolerance: float) (maxIterations: int) =
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

let bisection (f: float -> float) (a: float) (b: float) (tolerance: float) (maxIterations: int) =
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
        raise <| InvalidOperationException("Root not bracketed by the initial interval.")
    else
        iterate a b 0
 