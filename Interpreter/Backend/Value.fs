module Vec3.Interpreter.Backend.Value

open Vec3.Interpreter.Backend.Types
let printValue value = printfn $"Printed value: {valueToString value}"

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
    | Function f1, Function f2 -> f1.Name = f2.Name && f1.Arity = f2.Arity
    | Closure c1, Closure c2 -> c1.Function = c2.Function
    | Nil, Nil -> true
    | List l1, List l2 -> 
        if List.length l1 <> List.length l2 then false
        else List.forall2 valuesEqual l1 l2
    | _ -> false

and numbersEqual (a: VNumber) (b: VNumber) =
    match (a, b) with
    | VInteger x, VInteger y -> x = y
    | VFloat x, VFloat y -> x = y
    | VRational(n1, d1), VRational(n2, d2) -> n1 * d2 = n2 * d1
    | VComplex(r1, i1), VComplex(r2, i2) -> r1 = r2 && i1 = i2
    | _ -> 
        let f1, f2 = (floatValue a, floatValue b)
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
    | List l1, List l2 ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> add x y) zipped
        List added
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
    | List l1, List l2 ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> subtract x y) zipped
        List added
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
    | List l1, List l2 ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> multiply x y) zipped
        List added
    | _ -> failwith "Can only multiply numbers"

let rec divide a b =
    match (a, b) with
    | VNumber(VInteger x), VNumber(VInteger y) when y <> 0 -> 
        if x % y = 0 then VNumber(VInteger(x / y)) else VNumber(VRational(x, y))
    | VNumber(VRational(n1, d1)), VNumber(VRational(n2, d2)) when n2 <> 0 -> 
        VNumber(VRational(n1 * d2, d1 * n2))  
    | VNumber(VComplex(a, b)), VNumber(VComplex(c, d)) when c <> 0.0 || d <> 0.0 -> 
        let denominator = c*c + d*d
        VNumber(VComplex((a*c + b*d) / denominator, (b*c - a*d) / denominator))
    | VNumber x, VNumber y ->
        let f1, f2 = floatValue x, floatValue y
        if f2 = 0.0 then failwith "Division by zero"
        VNumber(VFloat(f1 / f2))
    | List l1, List l2 ->
        let zipped = List.zip l1 l2
        let added = List.map (fun (x, y) -> divide x y) zipped
        List added
    | _ -> failwith "Can only divide numbers"

let dotProduct a b =
    match (a, b) with
    | List l1, List l2 ->
            let rec dotVectors lhs rhs =
                match lhs, rhs with
                | [], [] -> 0
                | l :: ls, r :: rs ->
                    match l, r with
                    | VNumber(VInteger x), VNumber(VInteger y) -> x * y + dotVectors ls rs
                    | _ -> failwith "invalid"
                | _ -> failwith "invalid"
            VNumber(VInteger(dotVectors l1 l2))
    | _ -> failwith "Can only take dot product of vectors"

let crossProduct a b =
    match (a, b) with
    | List l1, List l2 ->
            let rec crossVectors lhs rhs =
                match lhs, rhs with
                | [VNumber(VInteger x); VNumber(VInteger y); VNumber(VInteger z)], [VNumber(VInteger a); VNumber(VInteger b); VNumber(VInteger c)] ->
                    [VNumber(VInteger(y * c - z * b)); VNumber(VInteger(z * a - x * c)); VNumber(VInteger(x * b - y * a))]
                | _ -> failwith "invalid"
            List(crossVectors l1 l2)
    | _ -> failwith "Can only take cross product of vectors"

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