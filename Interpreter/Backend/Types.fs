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
    | VString of string
    | VBoolean of bool
    | VFunction of Function * (double -> double) option
    | VClosure of Closure
    | VNil
    | VList of Value list
    | VBuiltin of (Value list -> VM -> VM)
    | VPlotData of string * Value list * Value list
    | VPlotFunction of string * (double -> double)


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
    | VBoolean b -> string b
    | VString s -> s
    | VFunction (f, _) -> $"<fn {f.Name}>"
    | VClosure c -> $"<closure {c.Function.Name}>"
    | VNil -> "nil"
    | VList l -> $"""[{String.concat ", " (List.map valueToString l)}]"""
    | VBuiltin _ -> "<builtin>"
    | VPlotData _ -> "<plot data>"
    | VPlotFunction _ -> "<plot function>"