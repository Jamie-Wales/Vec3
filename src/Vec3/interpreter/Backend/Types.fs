// <summary>
/// Types for the backend.
/// </summary>
module Vec3.Interpreter.Backend.Types

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.SymbolicExpression

/// <summary>
/// Debug information for a line.
/// </summary>
type LineInfo = { Offset: int; LineNumber: int }

/// <summary>
/// Different types of streams.
/// </summary>
type StreamType =
    | ConstantPool
    | Disassembly
    | Execution
    | StandardOutput
    | Globals

/// <summary>
/// Output streams of the virtual machine.
/// </summary>
type OutputStreams =
    { ConstantPool: seq<string>
      Disassembly: seq<string>
      Execution: seq<string>
      StandardOutput: Ref<seq<string>>
      Globals: seq<string> }

/// <summary>
/// Different types of plots.
/// </summary>
type PlotType =
    | Scatter
    | Line
    | Bar
    | Histogram
    | Signal

/// <summary>
/// Different types of compound values.
/// </summary>
type CompoundType =
    | LIST
    | RECORD
    | TUPLE

/// <summary>
/// Local variable.
/// </summary>
and Local =
    { Name: string // Name of the variable
      Depth: int // Scope depth
      Index: int } // Where it is stored in the stack

/// <summary>
/// Possible number types.
/// </summary>
and VNumber =
    | VInteger of int
    | VFloat of float
    | VRational of int * int
    | VComplex of float * float
    | VChar of char

/// <summary>
/// A chunk of code.
/// </summary>
and Chunk =
    { Code: ResizeArray<byte>
      Lines: ResizeArray<LineInfo>
      ConstantPool: ResizeArray<Value> }

/// <summary>
/// A function.
/// </summary>
and Function =
    { Arity: int // Number of arguments
      Chunk: Chunk
      Name: string
      Locals: Local list }

/// <summary>
/// A closure.
/// </summary>
and Closure =
    { Function: Function
      UpValues: Local list
      UpValuesValues: Value array }

/// <summary>
/// A value.
/// </summary>
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

/// <summary>
/// Represents a call frame.
/// </summary>
and CallFrame =
    { Closure: Closure // Current closure
      IP: int // Instruction Pointer
      StackBase: int }

/// <summary>
/// Represents the virtual machine.
/// </summary>
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

/// <summary>
/// Simplifies a rational number.
/// </summary>
let simplifyRat n d =
    let rec gcd a b = if b = 0 then a else gcd b (a % b)

    let g = gcd n d
    n / g, d / g

/// <summary>
/// Converts a value to a string.
/// </summary>
let rec valueToString =
    function
    | VNumber(VInteger n) -> string n
    | VNumber(VFloat f) -> $"%f{f}"
    | VNumber(VRational(n, d)) ->
        let n, d = simplifyRat n d
        $"%d{n}/%d{d}"
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
    | VBuiltin(_, name) -> $"<builtin : {name}>"
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
