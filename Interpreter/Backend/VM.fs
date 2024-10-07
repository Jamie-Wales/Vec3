module Vec3.Interpreter.Backend.VM

open System
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Value

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
type VM = {
    Chunk: Chunk
    IP: int
    Stack: ResizeArray<Value>
    ScopeDepth: int
    Globals: Map<String, Value>
    Streams: OutputStreams
    ExecutionHistory: ResizeArray<VM>  
}

let createOutputStreams() = {
    ConstantPool = Seq.empty
    Disassembly = Seq.empty
    Execution = Seq.empty
    StandardOutput = Seq.empty
    Globals = Seq.empty
}
let createVM (chunk: Chunk) =
    let constantPool = 
        chunk.ConstantPool 
        |> Seq.indexed 
        |> Seq.map (fun (i, value) -> $"[{i}] {valueToString value}")
    
    let disassembly = 
        disassembleChunkToString chunk "program"
        |> fun s -> s.Split(Environment.NewLine) |> Seq.ofArray
    
    { Chunk = chunk
      IP = 0
      Stack = ResizeArray<Value>(256)
      ScopeDepth = 0
      Globals = Map.empty
      Streams = { ConstantPool = constantPool
                  Disassembly = disassembly
                  Execution = Seq.empty
                  StandardOutput = Seq.empty
                  Globals = Seq.empty }
      ExecutionHistory = ResizeArray<VM>() }

// New function to save VM state
let saveVMState (vm: VM) =
    vm.ExecutionHistory.Add(vm)

let appendToStream (stream: seq<string>) (str: string) =
    Seq.append stream [str]

let appendOutput (vm: VM) (streamType: StreamType) (str: string) =
    let updatedStreams =
        match streamType with
        | ConstantPool -> { vm.Streams with ConstantPool = appendToStream vm.Streams.ConstantPool str }
        | Disassembly -> { vm.Streams with Disassembly = appendToStream vm.Streams.Disassembly str }
        | Execution -> { vm.Streams with Execution = appendToStream vm.Streams.Execution str }
        | StandardOutput -> { vm.Streams with StandardOutput = appendToStream vm.Streams.StandardOutput str }
        | Globals -> { vm.Streams with Globals = appendToStream vm.Streams.Globals str }
    { vm with Streams = updatedStreams }
    
let push (vm: VM) (value: Value) =
    vm.Stack.Add(value)
    vm

let pop (vm: VM) =
    let value = vm.Stack[vm.Stack.Count - 1]
    vm.Stack.RemoveAt(vm.Stack.Count - 1)
    (value, vm)

let peek (vm: VM) offset =
    vm.Stack[vm.Stack.Count - 1 - offset]

let readByte (vm: VM) =
    let byte = vm.Chunk.Code[vm.IP]
    ({ vm with IP = vm.IP + 1 }, byte)

let readConstant (vm: VM) =
    let vm, byte = readByte vm
    if int byte >= vm.Chunk.ConstantPool.Count then
        failwithf "Constant index out of range: %d (pool size: %d)" (int byte) vm.Chunk.ConstantPool.Count
    let constant = vm.Chunk.ConstantPool[int byte]
    (constant, vm)

let readConstantLong (vm: VM) =
    let vm, byte1 = readByte vm
    let vm, byte2 = readByte vm
    let vm, byte3 = readByte vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)
    if index >= vm.Chunk.ConstantPool.Count then
        failwithf "Long constant index out of range: %d (pool size: %d)" index vm.Chunk.ConstantPool.Count
    let constant = vm.Chunk.ConstantPool[index]
    (constant, vm)

let defineGlobal (vm: VM) (name: string) (value: Value) =
    let updatedGlobals = Map.add name value vm.Globals
    let updatedVM = { vm with Globals = updatedGlobals }
    let globalOutput = $"{name} = {valueToString value}"
    appendOutput updatedVM Globals globalOutput

let getGlobal (vm: VM) (name: string) =
    Map.tryFind name vm.Globals

let binaryOp (vm: VM) (op: Value -> Value -> Value) =
    let b, vm = pop vm
    let a, vm = pop vm
    let result = op a b
    push vm result
let stepVM (vm: VM) =
    if vm.IP >= vm.Chunk.Code.Count then
        vm  
    else
        saveVMState vm
        let vm, instruction = readByte vm
        let updatedVM = 
            match byteToOpCode instruction with
            | opcode ->
                let vm = appendOutput vm Execution $"Executing: {opCodeToString opcode}"
                match opcode with
                | CONSTANT ->
                    let constant, vm = readConstant vm
                    push vm constant
                | CONSTANT_LONG ->
                    let constant, vm = readConstantLong vm
                    push vm constant
                | ADD -> binaryOp vm add
                | SUBTRACT -> binaryOp vm subtract
                | MULTIPLY -> binaryOp vm multiply
                | DIVIDE -> binaryOp vm divide
                | NEGATE ->
                    let value, vm = pop vm
                    push vm (negate value)
                | EQUAL ->
                    let b, vm = pop vm
                    let a, vm = pop vm
                    push vm (Boolean (valuesEqual a b))
                | GREATER ->
                    let b, vm = pop vm
                    let a, vm = pop vm
                    match (a, b) with
                    | VNumber x, VNumber y -> push vm (Boolean (x > y))
                    | _ -> failwith "Operands must be numbers"
                | LESS ->
                    let b, vm = pop vm
                    let a, vm = pop vm
                    match (a, b) with
                    | VNumber x, VNumber y -> push vm (Boolean (x < y))
                    | _ -> failwith "Operands must be numbers"
                | TRUE -> push vm (Boolean true)
                | FALSE -> push vm (Boolean false)
                | NOT ->
                    let value, vm = pop vm
                    push vm (Boolean (not (isTruthy value)))
                | PRINT ->
                    let value, vm = pop vm
                    appendOutput vm StandardOutput $"{valueToString value}"
                | POP ->
                    let _, vm = pop vm
                    vm
                | DEFINE_GLOBAL ->
                    let constant, vm = readConstant vm
                    match constant with
                    | Value.String name ->
                        let value, vm = pop vm
                        let vm = appendOutput vm Execution $"Defining global variable: {name} = {valueToString value}"
                        defineGlobal vm name value
                    | _ -> failwith "Expected string constant for variable name"
                | GET_GLOBAL ->
                    let constant, vm = readConstant vm
                    match constant with
                    | Value.String name ->
                        match getGlobal vm name with
                        | Some value -> 
                            push vm value
                        | None -> 
                            failwith $"Undefined variable '{name}'"
                    | _ -> failwith "Expected string constant for variable name"
                | RETURN ->
                    if vm.Stack.Count > 0 then
                        let result, vm = pop vm
                        appendOutput vm Execution $"Return value: {valueToString result}"
                    else
                        vm
                | _ -> failwith $"Unimplemented opcode: {opCodeToString
                opcode}"
        updatedVM
let stepBackVM (vm: VM) =
    if vm.ExecutionHistory.Count > 0 then
        let previousState = vm.ExecutionHistory[vm.ExecutionHistory.Count - 1]
        vm.ExecutionHistory.RemoveAt(vm.ExecutionHistory.Count - 1)
        previousState
    else
        vm 
let rec run (vm: VM) =
    if vm.IP >= vm.Chunk.Code.Count then
        vm  
    else
        let vm, instruction = readByte vm
        match byteToOpCode instruction with
        | CONSTANT ->
            let constant, vm = readConstant vm
            run (push vm constant)
        | CONSTANT_LONG ->
            let constant, vm = readConstantLong vm
            run (push vm constant)
        | ADD -> run (binaryOp vm add)
        | SUBTRACT -> run (binaryOp vm subtract)
        | MULTIPLY -> run (binaryOp vm multiply)
        | DIVIDE -> run (binaryOp vm divide)
        | NEGATE ->
            let value, vm = pop vm
            run (push vm (negate value))
        | EQUAL ->
            let b, vm = pop vm
            let a, vm = pop vm
            run (push vm (Boolean (valuesEqual a b)))
        | GREATER ->
            let b, vm = pop vm
            let a, vm = pop vm
            match (a, b) with
            | VNumber x, VNumber y -> run (push vm (Boolean (x > y)))
            | _ -> failwith "Operands must be numbers"
        | LESS ->
            let b, vm = pop vm
            let a, vm = pop vm
            match (a, b) with
            | VNumber x, VNumber y -> run (push vm (Boolean (x < y)))
            | _ -> failwith "Operands must be numbers"
        | TRUE -> run (push vm (Boolean true))
        | FALSE -> run (push vm (Boolean false))
        | NOT ->
            let value, vm = pop vm
            run (push vm (Boolean (not (isTruthy value))))
        | PRINT ->
            let value, vm = pop vm
            let vm = appendOutput vm StandardOutput $"{valueToString value}"
            run vm
        | POP ->
            let _, vm = pop vm
            run vm
        | DEFINE_GLOBAL ->
            let constant, vm = readConstant vm
            match constant with
            | Value.String name ->
                let value, vm = pop vm
                let vm = appendOutput vm Execution $"Defining global variable: {name} = {valueToString value}"
                run (defineGlobal vm name value)
            | _ -> failwith "Expected string constant for variable name"
        | GET_GLOBAL ->
            let constant, vm = readConstant vm
            match constant with
            | Value.String name ->
                match getGlobal vm name with
                | Some value -> 
                    run (push vm value)
                | None -> 
                    failwith $"Undefined variable '{name}'"
            | _ -> failwith "Expected string constant for variable name"
        | RETURN ->
            if vm.Stack.Count > 0 then
                let result, vm = pop vm
                appendOutput vm Execution $"Return value: {valueToString result}"
            else
                vm
        | _ -> failwith $"Unimplemented opcode: {instruction}"
let interpretWithMode (chunk: Chunk) (vm: VM option) (isRepl: bool) =
    let vm = 
        match vm with
        | Some existingVM when not isRepl -> 
            { existingVM with Chunk = chunk; IP = 0 }
        | _ -> 
            let newVM = createVM chunk
            appendOutput newVM ConstantPool "=== Constant Pool ==="
    let vm = appendOutput vm Execution "\n=== Program Execution ==="
    let finalVm = run vm
    
    (finalVm, finalVm.Streams)

let interpret (chunk: Chunk) =
    let (_, streams) = interpretWithMode chunk None false
    streams

let replExecute (chunk: Chunk) (vm: VM option) =
    interpretWithMode chunk vm true

let getStreamContent (stream: seq<string>) =
    String.concat Environment.NewLine (Seq.toArray stream)

let resetStreams (vm: VM) =
    { vm with Streams = createOutputStreams() }
