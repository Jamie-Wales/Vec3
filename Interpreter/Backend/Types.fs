module Vec3.Interpreter.Backend.Types

type LineInfo = { Offset: int; LineNumber: int }

type Chunk = {
    Code: ResizeArray<byte>
    Lines: ResizeArray<LineInfo>
    ConstantPool: ResizeArray<Value>
}

and Value =
    | VNumber of VNumber
    | String of string
    | Boolean of bool
    | Function of Function
    | Closure of Closure
    | Nil

and VNumber =
    | VInteger of int  
    | VFloat of float
    | VRational of int * int  
    | VComplex of float * float

and Function = {
    Arity: int
    Chunk: Chunk
    Name: string
}

and Closure = {
    Function: Function
    UpValues: Value list
}

let valueToString =
    function
    | VNumber(VInteger n) -> string n
    | VNumber(VFloat f) -> string f
    | VNumber(VRational(n, d)) -> $"%d{n}/%d{d}"
    | VNumber(VComplex(r, i)) -> $"%f{r} + %f{i}i"
    | Boolean b -> string b
    | String s -> s
    | Function f -> $"<fn {f.Name}>"
    | Closure c -> $"<closure {c.Function.Name}>"
    | Nil -> "nil"
