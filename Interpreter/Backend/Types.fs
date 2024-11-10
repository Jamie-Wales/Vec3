module Vec3.Interpreter.Backend.Types

open Vec3.Interpreter
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.PrettyPrinter

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
    
    
and PlotType = 
    | Scatter
    | Line
    | Bar
    | Histogram
and Value =
    | VNumber of VNumber
    | VString of string
    | VBoolean of bool
    | VFunction of Function
    | VClosure of Closure
    | VNil
    | VList of Value list * CompoundType
    | VBuiltin of (Value list -> VM -> VM)
    | VPlotData of string * Value list * Value list * PlotType
    | VPlotFunction of string * (double -> double)
    | VBlock of Expr
 
and CompoundType = LIST | RECORD | TUPLE

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
      Locals: Local list
      UpValues: Local list
      BuiltIn: (double -> double) option
      }

and Closure =
    { Function: Function
      UpValues: Value array }

and CallFrame = {
    Closure: Closure
    IP: int
    StackBase: int
    Locals: Value array
}

and VM = 
    { Frames: ResizeArray<CallFrame>
      Stack: ResizeArray<Value>
      ScopeDepth: int
      Globals: Map<string, Value>
      Streams: OutputStreams
      ExecutionHistory: ResizeArray<VM>
      Plots: ResizeArray<Value> }  

let rec valueToString =
    function
    | VNumber(VInteger n) -> string n
    | VNumber(VFloat f) -> $"%f{f}"
    | VNumber(VRational(n, d)) -> $"%d{n}/%d{d}"
    | VNumber(VComplex(r, i)) -> $"%f{r} + %f{i}i"
    | VBoolean b -> string b
    | VString s -> s
    | VFunction f -> $"<fn {f.Name}>"
    | VClosure c -> $"<closure {c.Function.Name}>"
    | VNil -> "nil"
    | VList (l, typ) -> $"""[{String.concat ", " (List.map valueToString l)}]"""
    | VBuiltin _ -> "<builtin>"
    | VPlotData _ -> "<plot data>"
    | VPlotFunction _ -> "<plot function>"
    | VBlock v-> printExpr v