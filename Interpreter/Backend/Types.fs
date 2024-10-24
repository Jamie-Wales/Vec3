module Vec3.Interpreter.Backend.Types

open System
open ScottPlot

type LineInfo = { Offset: int; LineNumber: int }

type StreamType =
    | ConstantPool
    | Disassembly
    | Execution
    | StandardOutput
    | Globals

type OutputStreams = {
    ConstantPool: seq<string>
    Disassembly: seq<string>
    Execution: seq<string>
    StandardOutput: seq<string>
    Globals: seq<string>
}

type Chunk =
    { Code: ResizeArray<byte>
      Lines: ResizeArray<LineInfo>
      ConstantPool: ResizeArray<Value> }
    
and Value =
    | VNumber of VNumber
    | String of string
    | Boolean of bool
    | Function of Function
    | Closure of Closure
    | Nil
    | List of Value list
    | Builtin of (Value list -> VM -> VM)
    | PlotData of string * Value list * Value list  


and VNumber =
    | VInteger of int
    | VFloat of float
    | VRational of int * int
    | VComplex of float * float

and Local =
    { Name: string; Depth: int; Index: int }

and Function =
    { Arity: int
      Chunk: Chunk
      Name: string
      Locals: Local list }

and Closure =
    { Function: Function
      UpValues: Value list }
    
and CallFrame = {
    Function: Function
    IP: int
    StackBase: int
    Locals: Value array
}

and VM = {
    Frames: ResizeArray<CallFrame>
    Stack: ResizeArray<Value>
    ScopeDepth: int
    Globals: Map<String, Value>
    Streams: OutputStreams
    ExecutionHistory: ResizeArray<VM>  
}

let rec valueToString =
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
    | List l -> $"""[{String.concat ", " (List.map valueToString l)}]"""
    | Builtin _ -> "<builtin>"
    | PlotData _ -> "<plot data>"