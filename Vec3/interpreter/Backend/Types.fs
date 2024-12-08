// <summary>
/// Types for the backend.
/// </summary>
module Vec3.Interpreter.Backend.Types

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.SymbolicExpression

type LineInfo = { Offset: int; LineNumber: int }

type StreamType =
    | ConstantPool
    | Disassembly
    | Execution
    | StandardOutput
    | Globals

type OutputStreams =
    { ConstantPool: seq<string>
      Disassembly: seq<string>
      Execution: seq<string>
      StandardOutput: Ref<seq<string>>
      Globals: seq<string> }

type PlotType =
    | Scatter
    | Line
    | Bar
    | Histogram
    | Signal

type CompoundType =
    | LIST
    | RECORD
    | TUPLE

and Local = {
    Name: string
    Depth: int
    Index: int
}

and VNumber =
    | VInteger of int
    | VFloat of float
    | VRational of int * int
    | VComplex of float * float
    | VChar of char

and Chunk =
    { Code: ResizeArray<byte>
      Lines: ResizeArray<LineInfo>
      ConstantPool: ResizeArray<Value> }

and Function =
    { Arity: int
      Chunk: Chunk
      Name: string
      Locals: Local list }

and Closure =
    { Function: Function
      UpValues: Local list 
      UpValuesValues: Value array }

and Value =
    | VNumber of VNumber
    | VString of string
    | VBoolean of bool
    | VFunction of Function * Expression option
    | VAsyncFunction of Function
    | VClosure of Closure * Expression option
    | VNil
    | VList of Value list * CompoundType
    | VBuiltin of (Value list -> Value) * string
    | VPlotData of string * Value list * Value list * PlotType
    | VPlotFunction of string * (double -> double) * float option * float option * float option
    | VPlotFunctions of string * (double -> double) list
    | VShape of (float * float * float * float * string * string * int * bool)
    | VShapes of (float * float * float * float * string * string) list * int
    | VOutput of string
    | VBlock of Expr
    | VEventListener of int * int * Function
    | VPromise of Async<Value>

and CallFrame =
    { Closure: Closure
      IP: int
      StackBase: int }

and VM =
    { Frames: ResizeArray<CallFrame>
      Stack: ResizeArray<Value>
      ScopeDepth: int
      Globals: Map<string, Value>
      Streams: OutputStreams
      ExecutionHistory: ResizeArray<VM>
      Plots: ResizeArray<Value>
      Canvas: ResizeArray<Value>
      EventListeners: ResizeArray<int * int * Function> }
let rec valueToString =
    function
    | VNumber(VInteger n) -> string n
    | VNumber(VFloat f) -> $"%f{f}"
    | VNumber(VRational(n, d)) -> $"%d{n}/%d{d}"
    | VNumber(VComplex(r, i)) -> $"%f{r} + %f{i}i"
    | VNumber(VChar c) -> $"'{c}'"
    | VBoolean b -> string b
    | VString s -> s
    | VFunction(func, Some f) -> $"<fn {func.Name} : f(x) = {toString f}>"
    | VFunction(f, _) -> $"<fn {f.Name}>"
    | VClosure(c, Some f) -> $"<closure {c.Function.Name} : f(x) = {toString f}>"
    | VClosure(c, _) -> $"<closure {c.Function.Name}>"
    | VNil -> "()"
    | VList(l, typ) ->
        match typ with
        | LIST -> $"""[{String.concat ", " (List.map valueToString l)}]"""
        | RECORD ->
            let fields =
                l
                |> List.map (fun v ->
                    match v with
                    | VList([ VString s; v ], _) -> s, v
                    | _ -> "Unknown", v)
                |> List.map (fun (s, v) -> $"""{s}: {valueToString v}""")

            $"""{{{String.concat ", " fields}}}"""
        | TUPLE -> $"""({String.concat ", " (List.map valueToString l)})"""
    | VBuiltin _ -> "<builtin>"
    | VPlotData _ -> "<plot data>"
    | VPlotFunction _ -> "<plot function>"
    | VPlotFunctions _ -> "<plot functions>"
    | VBlock _ -> $"<block>"
    | VShape _ -> "<shape>"
    | VShapes _ -> "<shapes>"
    | VOutput _ -> "<output>"
    | VEventListener _ -> "<event listener>"
    | VAsyncFunction _ -> "<async function>"
    | VPromise _ -> "<promise>"
