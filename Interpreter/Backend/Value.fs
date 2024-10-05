module Vec3.Interpreter.Backend.Value

type Value =
    | VNumber of VNumber
    | String of string
    | Boolean of bool
    | Nil

and VNumber =
    | VInteger of int  
    | VFloat of float
    | VRational of int * int  
    | VComplex of float * float

let valueToString =
    function
    | VNumber(VInteger n) -> string n
    | VNumber(VFloat f) -> string f
    | VNumber(VRational(n, d)) -> sprintf "%d/%d" n d
    | VNumber(VComplex(r, i)) -> sprintf "%f + %fi" r i
    | Boolean b -> string b
    | String s -> s
    | Nil -> "nil"

let printValue value = printfn "%s" (valueToString value)

let isTruthy =
    function
    | Boolean false -> false
    | Nil -> false
    | _ -> true

let rec valuesEqual (a: Value) (b: Value) =
    match (a, b) with
    | VNumber x, VNumber y -> numbersEqual x y
    | Boolean x, Boolean y -> x = y
    | String x, String y -> x = y
    | Nil, Nil -> true
    | _ -> false

and numbersEqual (a: VNumber) (b: VNumber) =
    match (a, b) with
    | VInteger x, VInteger y -> x = y
    | VFloat x, VFloat y -> x = y
    | VRational(n1, d1), VRational(n2, d2) -> n1 * d2 = n2 * d1
    | VComplex(r1, i1), VComplex(r2, i2) -> r1 = r2 && i1 = i2
    | _ -> 
        let (f1, f2) = (floatValue a, floatValue b)
        f1 = f2

and floatValue =
    function
    | VInteger n -> float n
    | VFloat f -> f
    | VRational(n, d) -> float n / float d
    | VComplex(r, _) -> r  // Note: This loses imaginary part information

let toComplex =
    function
    | VInteger n -> VComplex(float n, 0.0)
    | VFloat f -> VComplex(f, 0.0)
    | VRational(n, d) -> VComplex(float n / float d, 0.0)
    | VComplex(r, i) -> VComplex(r, i)

let rec add a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x + y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> 
        let n = n1 * d2 + n2 * d1
        let d = d1 * d2
        VNumber(VRational(n, d))  // Consider simplifying the fraction
    | VNumber(VComplex(r1, i1)), VNumber(VComplex(r2, i2)) -> 
        VNumber(VComplex(r1 + r2, i1 + i2))
    | VNumber x, VNumber y ->
        VNumber(VFloat(floatValue x + floatValue y))
    | _ -> failwith "Can only add numbers"

let rec subtract a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x - y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> 
        let n = n1 * d2 - n2 * d1
        let d = d1 * d2
        VNumber(VRational(n, d))  
    | VNumber(VComplex(r1, i1)), VNumber(VComplex(r2, i2)) -> 
        VNumber(VComplex(r1 - r2, i1 - i2))
    | VNumber x, VNumber y ->
        VNumber(VFloat(floatValue x - floatValue y))
    | _ -> failwith "Can only subtract numbers"

let rec multiply a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) -> VNumber(VInteger(x * y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) -> 
        VNumber(VRational(n1 * n2, d1 * d2))  
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) -> 
        VNumber(VComplex(a*c - b*d, a*d + b*c))
    | VNumber x, VNumber y ->
        VNumber(VFloat(floatValue x * floatValue y))
    | _ -> failwith "Can only multiply numbers"

let rec divide a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) when y <> 0 -> 
        if x % y = 0 then VNumber(VInteger(x / y)) else VNumber(VRational(x, y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) when n2 <> 0 -> 
        VNumber(VRational(n1 * d2, d1 * n2))  // Consider simplifying the fraction
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) when c <> 0.0 || d <> 0.0 -> 
        let denominator = c*c + d*d
        VNumber(VComplex((a*c + b*d) / denominator, (b*c - a*d) / denominator))
    | VNumber x, VNumber y ->
        let f1, f2 = floatValue x, floatValue y
        if f2 = 0.0 then failwith "Division by zero"
        VNumber(VFloat(f1 / f2))
    | _ -> failwith "Can only divide numbers"

let negate value =
    match value with
    | VNumber(VInteger n) -> VNumber(VInteger(-n))
    | VNumber(VFloat n) -> VNumber(VFloat(-n))
    | VNumber(VRational(n, d)) -> VNumber(VRational(-n, d))
    | VNumber(VComplex(r, i)) -> VNumber(VComplex(-r, -i))
    | _ -> failwith "Can only negate numbers"

let compare a b =
    match (a, b) with
    | VNumber x, VNumber y ->
        match (x, y) with
        | VInteger x, VInteger y -> compare x y
        | VRational(n1, d1), VRational(n2, d2) -> 
            compare (n1 * d2) (n2 * d1)
        | VComplex _, VComplex _ -> 
            failwith "Cannot compare complex numbers"
        | _ -> compare (floatValue x) (floatValue y)
    | _ -> failwith "Can only compare numbers"