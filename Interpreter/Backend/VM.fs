module Vec3.Interpreter.Backend.VM

open System
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Types
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

type CallFrame = {
    Function: Function
    IP: int
    StackBase: int
    Locals: Value array
}

type VM = {
    Frames: ResizeArray<CallFrame>
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

let getCurrentFrame (vm: VM) =
    vm.Frames[vm.Frames.Count - 1]

let readByte (vm: VM) =
    let frame = getCurrentFrame vm
    let byte = frame.Function.Chunk.Code[frame.IP]
    let updatedFrame = { frame with IP = frame.IP + 1 }
    vm.Frames[vm.Frames.Count - 1] <- updatedFrame
    (vm, byte)

let readConstant (vm: VM) =
    let vm, byte = readByte vm
    let frame = getCurrentFrame vm
    if int byte >= frame.Function.Chunk.ConstantPool.Count then
        failwithf $"Constant index out of range: %d{int byte} (pool size: %d{frame.Function.Chunk.ConstantPool.Count})"
    let constant = frame.Function.Chunk.ConstantPool[int byte]
    (constant, vm)

let readConstantLong (vm: VM) =
    let vm, byte1 = readByte vm
    let vm, byte2 = readByte vm
    let vm, byte3 = readByte vm
    let frame = getCurrentFrame vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)
    if index >= frame.Function.Chunk.ConstantPool.Count then
        failwithf $"Long constant index out of range: %d{index} (pool size: %d{frame.Function.Chunk.ConstantPool.Count})"
    let constant = frame.Function.Chunk.ConstantPool[index]
    (constant, vm)

let createVM (mainFunc: Function) : VM =
    let constantPool = 
        mainFunc.Chunk.ConstantPool 
        |> Seq.indexed 
        |> Seq.map (fun (i, value) -> $"[{i}] {valueToString value}")
    let disassembly = 
        disassembleChunkToString mainFunc.Chunk mainFunc.Name
        |> fun s -> s.Split(Environment.NewLine) |> Seq.ofArray
    let vm = {
        Frames = ResizeArray<CallFrame>()
        Stack = ResizeArray<Value>(256)
        ScopeDepth = 0
        Globals = Map.empty
        Streams = { 
            ConstantPool = constantPool
            Disassembly = disassembly
            Execution = Seq.empty
            StandardOutput = Seq.empty
            Globals = Seq.empty 
        }
        ExecutionHistory = ResizeArray<VM>()
    }
    let mainFrame = {
        Function = mainFunc
        IP = 0
        StackBase = 0
        Locals = [||]
    }
    vm.Frames.Add(mainFrame)
    vm

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

let callValue (vm: VM) (argCount: int) : VM =
    let callee = peek vm argCount
    match callee with
    | Value.Function func ->
        if argCount <> func.Arity then
            failwith $"Expected {func.Arity} arguments but got {argCount}"
        let frame = {
            Function = func
            IP = 0
            StackBase = vm.Stack.Count - argCount
            Locals = Array.zeroCreate func.Locals.Length
        }
        vm.Frames.Add(frame)
        vm
    | _ -> failwith $"Can only call functions, got: {valueToString callee}"
    
let rec run (vm: VM) =
    let rec runLoop vm =
        if vm.Frames.Count = 0 then
            vm
        else
            let frame = getCurrentFrame vm
            if frame.IP >= frame.Function.Chunk.Code.Count then
                if vm.Frames.Count > 1 then
                    let result, vm =
                        if vm.Stack.Count > 0 then
                            pop vm
                        else
                            Value.Nil, vm  // Default to Nil if the stack is empty
                    vm.Frames.RemoveAt(vm.Frames.Count - 1)
                    let callerFrame = getCurrentFrame vm
                    vm.Stack.RemoveRange(callerFrame.StackBase, vm.Stack.Count - callerFrame.StackBase)
                    let vm = push vm result
                    runLoop vm
                else
                    // Last frame has finished execution
                    vm.Frames.RemoveAt(vm.Frames.Count - 1)
                    let vm =
                        if vm.Stack.Count = 0 then
                            push vm Value.Nil
                        else
                            vm
                    vm  // Do not call runLoop again since there are no frames left
            else
                saveVMState vm
                let vm, instruction = readByte vm
                let vm =
                    match byteToOpCode instruction with
                    | opcode ->
                        let vm = appendOutput vm Execution $"Executing: {opCodeToString opcode}"
                        match opcode with
                        | CONSTANT ->
                            let constant, vm = readConstant vm
                            let vm = push vm constant
                            appendOutput vm Execution $"Pushed constant onto stack: {valueToString constant}"
                            vm
                        | CONSTANT_LONG ->
                            let constant, vm = readConstantLong vm
                            let vm = push vm constant
                            appendOutput vm Execution $"Pushed long constant onto stack: {valueToString constant}"
                            vm
                        | GET_LOCAL ->
                            let vm, slot = readByte vm
                            let frame = getCurrentFrame vm
                            let index = frame.StackBase + int slot
                            if index >= vm.Stack.Count then
                                failwith $"GET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"
                            let value = vm.Stack.[index]
                            let vm = push vm value
                            vm
                        | SET_LOCAL ->
                            let vm, slot = readByte vm
                            let value, vm = pop vm
                            let frame = getCurrentFrame vm
                            let index = frame.StackBase + int slot
                            if index >= vm.Stack.Count then
                                failwith $"SET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"
                            vm.Stack.[index] <- value
                            vm
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
                        | NIL -> push vm Value.Nil
                        | NOT ->
                            let value, vm = pop vm
                            push vm (Boolean (not (isTruthy value)))
                        | PRINT ->
                            let value, vm = pop vm
                            let vm = appendOutput vm StandardOutput $"{valueToString value}"
                            vm
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
                            | _ -> failwith "Expected string constant for variable name in DEFINE_GLOBAL"
                        | GET_GLOBAL ->
                            let constant, vm = readConstant vm
                            match constant with
                            | Value.String name ->
                                match getGlobal vm name with
                                | Some value ->
                                    push vm value
                                | None ->
                                    failwith $"Undefined variable '{name}'"
                            | _ -> failwith "Expected string constant for variable name in GET_GLOBAL"
                        | SET_GLOBAL ->
                            let constant, vm = readConstant vm
                            match constant with
                            | Value.String name ->
                                let value, vm = pop vm
                                if vm.Globals.ContainsKey(name) then
                                    let vm = defineGlobal vm name value
                                    vm
                                else
                                    failwith $"Undefined variable '{name}'"
                            | _ -> failwith "Expected string constant for variable name in SET_GLOBAL"
                        | CALL ->
                            let vm, argCount = readByte vm
                            let vm = callValue vm (int argCount)
                            vm
                        | RETURN ->
                            let result, vm =
                                if vm.Stack.Count > 0 then
                                    pop vm
                                else
                                    Value.Nil, vm  
                            vm.Frames.RemoveAt(vm.Frames.Count - 1)
                            if vm.Frames.Count = 0 then
                                let vm = push vm result
                                vm  
                            else
                                let callerFrame = getCurrentFrame vm
                                vm.Stack.RemoveRange(callerFrame.StackBase, vm.Stack.Count - callerFrame.StackBase)
                                let vm = push vm result
                                runLoop vm
                        | _ -> failwith $"Unimplemented opcode: {opCodeToString opcode}"
                runLoop vm  
    runLoop vm

    
let interpretWithMode (func: Function) (vm: VM option) (isRepl: bool) =
    let vm = 
        match vm with
        | Some existingVM when isRepl ->
            let newFrame = {
                Function = func
                IP = 0
                StackBase = existingVM.Stack.Count
                Locals = [||]
            }
            existingVM.Frames.Add(newFrame)
            existingVM
        | _ -> 
            let newVM = createVM func
            appendOutput newVM ConstantPool "=== Constant Pool ==="
    let vm = appendOutput vm Execution "\n=== Program Execution ==="
    let finalVm = run vm
    (finalVm, finalVm.Streams)
let interpret (func: Function) =
    let _, streams = interpretWithMode func None false
    streams

let replExecute (func: Function) (vm: VM option) =
    interpretWithMode func vm true

let getStreamContent (stream: seq<string>) =
    String.concat Environment.NewLine (Seq.toArray stream)

let resetStreams (vm: VM) =
    { vm with Streams = createOutputStreams() }
    
let stepVM (vm: VM) =
    let currentFrame = getCurrentFrame vm
    if currentFrame.IP >= currentFrame.Function.Chunk.Code.Count then
        if vm.Frames.Count > 1 then
            vm.Frames.RemoveAt(vm.Frames.Count - 1)
            vm
        else
            vm
    else
        saveVMState vm
        let vm, instruction = readByte vm
        let updatedVM = 
            match byteToOpCode instruction with
            | opcode ->
                let vm = appendOutput vm Execution $"Executing: {opCodeToString opcode}"
                match opcode with
                | GET_LOCAL ->
                    let vm, slot = readByte vm
                    let frame = getCurrentFrame vm
                    let value = vm.Stack.[frame.StackBase + int slot]
                    let _ = push vm value
                    vm
                | SET_LOCAL ->
                    let vm, slot = readByte vm
                    let value, vm = pop vm
                    let frame = getCurrentFrame vm
                    vm.Stack.[frame.StackBase + int slot] <- value
                    vm
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
                | CALL ->
                        let vm, argCount = readByte vm
                        callValue vm (int argCount)
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
                    if vm.Frames.Count > 1 then
                        let returnValue, vm = 
                            if vm.Stack.Count > 0 then
                                pop vm
                            else
                                Value.Nil, vm
                        vm.Frames.RemoveAt(vm.Frames.Count - 1)
                        push vm returnValue
                    else
                        vm
 
                | _ -> failwith $"Unimplemented opcode: {opCodeToString opcode}"
        updatedVM

let stepBackVM (vm: VM) =
    if vm.ExecutionHistory.Count > 0 then
        let previousState = vm.ExecutionHistory[vm.ExecutionHistory.Count - 1]
        vm.ExecutionHistory.RemoveAt(vm.ExecutionHistory.Count - 1)
        previousState
    else
        vm 