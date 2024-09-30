module Vec3.Interpreter.Backend.Value

type Value =
    | Number of int
    | Boolean of bool
    | Nil

let valueToString =
    function
    | Number n -> string n
    | Boolean b -> string b
    | Nil -> "nil"

let printValue value = printfn $"%s{valueToString value}"

let isTruthy =
    function
    | Boolean false -> false
    | Nil -> false
    | _ -> true

let valuesEqual (a: Value) (b: Value) =
    match (a, b) with
    | Number x, Number y -> x = y
    | Boolean x, Boolean y -> x = y
    | Nil, Nil -> true
    | _ -> false

let add a b =
    match (a, b) with
    | Number x, Number y -> Number(x + y)
    | _ -> failwith "Can only add numbers"

let subtract a b =
    match (a, b) with
    | Number x, Number y -> Number(x - y)
    | _ -> failwith "Can only subtract numbers"

let multiply a b =
    match (a, b) with
    | Number x, Number y -> Number(x * y)
    | _ -> failwith "Can only multiply numbers"

let divide a b =
    match (a, b) with
    | Number x, Number y when y <> 0 -> Number(x / y)
    | Number _, Number 0 -> failwith "Division by zero"
    | _ -> failwith "Can only divide numbers"

let negate =
    function
    | Number n -> Number(-n)
    | _ -> failwith "Can only negate numbers"

let compare a b =
    match (a, b) with
    | Number x, Number y -> compare x y
    | _ -> failwith "Can only compare numbers"
