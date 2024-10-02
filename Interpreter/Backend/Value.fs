module Vec3.Interpreter.Backend.Value

type VNumber =
   | VInteger of int
   | VFloat of float
     
type Value =
    | VNumber of VNumber
    | Boolean of bool
    | String of string
    | Nil

let valueToString = function
    | VNumber (VInteger n) -> string n
    | VNumber (VFloat f) -> string f
    | Boolean b -> string b
    | String s -> s
    | Nil -> "nil"

let printValue value = printfn "%s" (valueToString value)

let isTruthy = function
    | Boolean false -> false
    | Nil -> false
    | _ -> true

let valuesEqual (a: Value) (b: Value) =
    match (a, b) with
    | VNumber (VInteger x), VNumber (VInteger y) -> x = y
    | VNumber (VFloat x), VNumber (VFloat y) -> x = y
    | Boolean x, Boolean y -> x = y
    | String x, String y -> x = y
    | Nil, Nil -> true
    | _ -> false

let add a b =
    match (a, b) with
    | VNumber (VInteger x), VNumber (VInteger y) -> VNumber (VInteger (x + y))
    | VNumber (VFloat x), VNumber (VFloat y) -> VNumber (VFloat (x + y))
    | _ -> failwith "Can only add numbers"

let subtract a b =
    match (a, b) with
    | VNumber (VInteger x), VNumber (VInteger y) -> VNumber (VInteger (x - y))
    | VNumber (VFloat x), VNumber (VFloat y) -> VNumber (VFloat (x - y))
    | _ -> failwith "Can only subtract numbers"

let multiply a b =
    match (a, b) with
    | VNumber (VInteger x), VNumber (VInteger y) -> VNumber (VInteger (x * y))
    | VNumber (VFloat x), VNumber (VFloat y) -> VNumber (VFloat (x * y))
    | _ -> failwith "Can only multiply numbers"

let divide a b =
    match (a, b) with
    | VNumber (VInteger x), VNumber (VInteger y) when y <> 0 -> VNumber (VInteger (x / y))
    | VNumber (VFloat x), VNumber (VFloat y) when y <> 0.0 -> VNumber (VFloat (x / y))
    | VNumber _, VNumber (VInteger 0) | VNumber _, VNumber (VFloat 0.0) -> failwith "Division by zero"
    | _ -> failwith "Can only divide numbers"

let negate = function
    | VNumber (VInteger n) -> VNumber (VInteger (-n))
    | VNumber (VFloat n) -> VNumber (VFloat (-n))
    | _ -> failwith "Can only negate numbers"

let compare a b =
    match (a, b) with
    | VNumber (VInteger x), VNumber (VInteger y) -> compare x y
    | VNumber (VFloat x), VNumber (VFloat y) -> compare x y
    | _ -> failwith "Can only compare numbers"