module Vec3.Interpreter.Backend.Types

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.PrettyPrinter
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
      StandardOutput: seq<string>
      Globals: seq<string> }

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
    | VFunction of Function * Expression option
    | VClosure of Closure
    | VNil
    | VList of Value list * CompoundType
    | VBuiltin of (Value list -> Value) * string
    | VPlotData of string * Value list * Value list * PlotType
    | VPlotFunction of string * (double -> double)
    | VPlotFunctions of string * (double -> double) list
    | VShape of (float * float * float * float * string * string)
    | VShapes of (float * float * float * float * string * string) list
    | VOutput of string
    | VBlock of Expr

and CompoundType =
    | LIST
    | RECORD
    | TUPLE

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

and CallFrame =
    { Function: Function
      IP: int
      StackBase: int
      Locals: Value array }

and VM =
    { Frames: ResizeArray<CallFrame>
      Stack: ResizeArray<Value>
      ScopeDepth: int
      Globals: Map<string, Value>
      Streams: OutputStreams
      ExecutionHistory: ResizeArray<VM>
      Plots: ResizeArray<Value>
      Canvas: ResizeArray<Value> }

let rec valueToString =
    function
    | VNumber(VInteger n) -> string n
    | VNumber(VFloat f) -> $"%f{f}"
    | VNumber(VRational(n, d)) -> $"%d{n}/%d{d}"
    | VNumber(VComplex(r, i)) -> $"%f{r} + %f{i}i"
    | VBoolean b -> string b
    | VString s -> s
    | VFunction(func, Some f) ->
        $"<fn {func.Name} : f(x) = {toString f}>"
    | VFunction(f, _) -> $"<fn {f.Name}>"
    | VClosure c -> $"<closure {c.Function.Name}>"
    | VNil -> "nil"
    | VList(l, typ) ->
        match typ with
        | LIST ->
            $"""[{String.concat ", " (List.map valueToString l)}]"""
        | RECORD ->
            let fields =
                l
                |> List.map (fun v -> match v with VList([VString s; v], _) -> s, v | _ -> failwith "bad")
                |> List.map (fun (s, v) -> $"""{s}: {valueToString v}""")
            $"""{{{String.concat ", " fields}}}"""
        | TUPLE ->
            $"""({String.concat ", " (List.map valueToString l)})"""
    | VBuiltin _ -> "<builtin>"
    | VPlotData _ -> "<plot data>"
    | VPlotFunction _ -> "<plot function>"
    | VPlotFunctions _ -> "<plot functions>"
    | VBlock v -> printExpr v
    | VShape _ -> "<shape>"
    | VShapes _ -> "<shapes>"
    | VOutput _ -> "<output>"
