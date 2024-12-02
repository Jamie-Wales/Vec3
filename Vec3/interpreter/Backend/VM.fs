module Vec3.Interpreter.Backend.VM

open System
open Compiler
open Microsoft.FSharp.Collections
open Vec3.Interpreter
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Backend.Builtins
open Vec3.Interpreter.Token
open Grammar
open Vec3.Interpreter.Backend

type OpCodeResult =
    | Return of Value
    | Call of byte * byte
    | Continue

let output = ref Seq.empty

let loadFunction (vm: VM) (func: Function) : VM =
    let closure = {
        Function = func
        UpValues = []
        UpValuesValues = [||]
    }
    let frame = {
        Closure = closure
        IP = 0
        StackBase = vm.Stack.Count
    }
    vm.Frames.Add(frame)
    vm

let createOutputStreams () = {
    ConstantPool = Seq.empty
    Disassembly = Seq.empty
    Execution = Seq.empty
    StandardOutput = output
    Globals = Seq.empty
}

let getCurrentFrame (vm: VM) = vm.Frames[vm.Frames.Count - 1]

let readByte (vm: VM) =
    let frame = getCurrentFrame vm
    let byte = frame.Closure.Function.Chunk.Code[frame.IP]
    let updatedFrame = { frame with IP = frame.IP + 1 }
    vm.Frames[vm.Frames.Count - 1] <- updatedFrame
    (vm, byte)

let readConstant (vm: VM) =
    let vm, byte = readByte vm
    let frame = getCurrentFrame vm
    if int byte >= frame.Closure.Function.Chunk.ConstantPool.Count then
        raise <| InvalidProgramException($"Constant index out of range: {byte} (pool size: {frame.Closure.Function.Chunk.ConstantPool.Count})")
    let constant = frame.Closure.Function.Chunk.ConstantPool[int byte]
    (constant, vm)

let readConstantLong (vm: VM) =
    let vm, byte1 = readByte vm
    let vm, byte2 = readByte vm
    let vm, byte3 = readByte vm
    let frame = getCurrentFrame vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)
    
    if index >= frame.Closure.Function.Chunk.ConstantPool.Count then
        raise <| InvalidProgramException($"Long constant index out of range: {index} (pool size: {frame.Closure.Function.Chunk.ConstantPool.Count})")
    
    let constant = frame.Closure.Function.Chunk.ConstantPool[index]
    (constant, vm)

let saveVMState (vm: VM) = vm.ExecutionHistory.Add(vm)

let appendToStream (stream: seq<string>) (str: string) = Seq.append stream [str]

let appendOutput (vm: VM) (streamType: StreamType) (str: string) =
    let updatedStreams =
        match streamType with
        | ConstantPool -> { vm.Streams with ConstantPool = appendToStream vm.Streams.ConstantPool str }
        | Disassembly -> { vm.Streams with Disassembly = appendToStream vm.Streams.Disassembly str }
        | Execution -> { vm.Streams with Execution = appendToStream vm.Streams.Execution str }
        | StandardOutput ->
            let appended = appendToStream vm.Streams.StandardOutput.Value str
            output.Value <- appended
            { vm.Streams with StandardOutput = output }
        | Globals -> { vm.Streams with Globals = appendToStream vm.Streams.Globals str }
    { vm with Streams = updatedStreams }

let push (vm: VM) (value: Value) =
    vm.Stack.Add(value)
    vm

let pop (vm: VM) =
    let value = vm.Stack[vm.Stack.Count - 1]
    vm.Stack.RemoveAt(vm.Stack.Count - 1)
    (value, vm)

let peek (vm: VM) offset = vm.Stack[vm.Stack.Count - 1 - offset]

let defineGlobal (vm: VM) (name: string) (value: Value) =
    let updatedGlobals = Map.add name value vm.Globals
    let updatedVM = { vm with Globals = updatedGlobals }
    let globalOutput = $"{name} = {valueToString value}"
    appendOutput updatedVM Globals globalOutput

let getGlobal (vm: VM) (name: string) = Map.tryFind name vm.Globals

let rec executeOpcode (vm: VM) (opcode: OP_CODE) : VM * OpCodeResult =
    let vm = appendOutput vm Execution $"Executing: {opCodeToString opcode}"
    match opcode with
    | RETURN -> 
        let currentFrame = getCurrentFrame vm
        let argCount = List.length currentFrame.Closure.Function.Locals
        let vm, shouldPop = readByte vm
        let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
        
        let vm = 
            List.fold 
                (fun vm _ -> 
                    let _, vm = pop vm 
                    vm
                ) vm [0..argCount - 1]
        
        let vm = 
            if int shouldPop = 1 then 
                let _, vm = pop vm
                vm 
            else 
                vm
                 
        vm.Frames.RemoveAt(vm.Frames.Count - 1)
        (vm, Return result)
        
    | CALL ->
        let vm, argCount = readByte vm
        let vm, recursive = readByte vm
        (vm, Call(argCount, recursive))
        
    | _ -> 
        let vm = executeOpcodeImpl vm opcode
        (vm, Continue)

and executeOpcodeImpl (vm: VM) (opcode: OP_CODE) : VM =
    match opcode with
    | CONSTANT ->
        let constant, vm = readConstant vm
        let vm = push vm constant
        appendOutput vm Execution $"Pushed constant onto stack: {valueToString constant}"
        
    | CONSTANT_LONG ->
        let constant, vm = readConstantLong vm
        let vm = push vm constant
        appendOutput vm Execution $"Pushed long constant onto stack: {valueToString constant}"
        
    | GET_LOCAL ->
        let vm, slot = readByte vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot
        if index >= vm.Stack.Count then
            raise <| InvalidProgramException($"GET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}")
        let value = vm.Stack[index]
        push vm value
        
    | SET_LOCAL ->
        let vm, slot = readByte vm
        let value, vm = pop vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot
        if index >= vm.Stack.Count then
            raise <| InvalidProgramException($"SET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}")
        vm.Stack[index] <- value
        vm
        
    | TRUE -> push vm (VBoolean true)
    | FALSE -> push vm (VBoolean false)
    | NIL -> push vm VNil
    
    | POP ->
        let _, vm = pop vm
        vm
        
    | DEFINE_GLOBAL ->
        let constant, vm = readConstant vm
        match constant with
        | VString name ->
            let value, vm = pop vm
            let vm = appendOutput vm Execution $"Defining global variable: {name} = {valueToString value}"
            defineGlobal vm name value
        | _ -> raise <| InvalidProgramException("Expected string constant for variable name in DEFINE_GLOBAL")
        
    | GET_GLOBAL ->
        let constant, vm = readConstant vm
        match constant with
        | VString name ->
            match getGlobal vm name with
            | Some value -> push vm value
            | None -> raise <| InvalidProgramException($"Undefined variable '{name}'")
        | _ -> raise <| InvalidProgramException("Expected string constant for variable name in GET_GLOBAL")
        
    | SET_GLOBAL ->
        let constant, vm = readConstant vm
        match constant with
        | VString name ->
            let value, vm = pop vm
            if vm.Globals.ContainsKey(name) then
                defineGlobal vm name value
            else
                raise <| InvalidProgramException($"Undefined variable '{name}'")
        | _ -> raise <| InvalidProgramException("Expected string constant for variable name in SET_GLOBAL")
        
    | GET_UPVALUE ->
        let vm, slot = readByte vm
        let frame = getCurrentFrame vm
        let upValue = frame.Closure.UpValues[int slot]
        
        match upValue.Location with
        | Local index ->
            let value = vm.Stack[frame.StackBase + index]
            push vm value
        | Enclosing index ->
            let enclosingFrame = vm.Frames[vm.Frames.Count - 2]
            let value = enclosingFrame.Closure.UpValuesValues[index]
            push vm value
        | Global name ->
            match Map.tryFind name vm.Globals with
            | Some value -> push vm value
            | None -> raise <| InvalidProgramException($"Undefined variable '{name}'")
        
    | CLOSURE ->
        let constant, vm = readConstant vm
        match constant with
        | VFunction(func, f) ->
            let vm, upValueCount = readByte vm
            let rec popUpValues (vm: VM) (count: int) : VM * UpValue list =
                match count with
                | n when n > 0 ->
                    let vm, index = readByte vm
                    let vm, isLocal = readByte vm
                    let name, vm = readConstant vm
                    let name =
                      match name with
                      | VString s -> s
                      | _ -> raise <| InvalidProgramException("Expected string constant for upvalue name")
                    let location = 
                        if isLocal = 1uy then Local(int index)
                        else Enclosing(int index)
                              
                    let vm, upValues = popUpValues vm (n - 1)
                    let upValue = {
                        Index = int index
                        Name = name
                        Location = location
                    }
                    vm, upValue :: upValues
                | _ -> vm, []
                
            let vm, upValues = popUpValues vm (int upValueCount)
            
            let upValuesValues = 
                upValues 
                |> List.map (fun upValue ->
                    match upValue.Location with
                    | Local idx -> 
                        let frame = getCurrentFrame vm
                        vm.Stack[frame.StackBase + idx]
                    | Enclosing idx ->
                        let enclosingFrame = vm.Frames[vm.Frames.Count - 2]
                        enclosingFrame.Closure.UpValuesValues[idx]
                    | Global name ->
                        match Map.tryFind name vm.Globals with
                        | Some v -> v
                        | None -> VNil)
                |> Array.ofList
                
            let closure = VClosure(
                { Function = func
                  UpValues = upValues
                  UpValuesValues = upValuesValues },
                f
            )
            push vm closure
        | _ -> raise <| InvalidProgramException("Expected function constant for closure")
        
    | JUMP ->
        let vm, byte1 = readByte vm
        let vm, byte2 = readByte vm
        let jump = int ((byte1 <<< 8) ||| byte2)
        let frame = getCurrentFrame vm
        let frame = { frame with IP = frame.IP + jump }
        vm.Frames[vm.Frames.Count - 1] <- frame
        vm
        
    | JUMP_IF_FALSE ->
        let vm, byte1 = readByte vm
        let vm, byte2 = readByte vm
        let jump = int ((byte1 <<< 8) ||| byte2)
        let condition, vm = pop vm
        if not (isTruthy condition) then
            let frame = getCurrentFrame vm
            let frame = { frame with IP = frame.IP + jump }
            vm.Frames[vm.Frames.Count - 1] <- frame
            vm
        else
            vm
            
    | COMPOUND_CREATE ->
        let structure, vm = pop vm
        let count, vm = pop vm
        match (structure, count) with
        | VList(values, typ), VNumber(VInteger n) when n >= 0 ->
            let values' = [0 .. n - 1] 
                         |> List.map (fun _ -> let value, _ = pop vm in value) 
                         |> List.rev
            let list = VList(List.append values values', typ)
            push vm list
        | _ -> raise <| InvalidProgramException("Expected list and integer for list create")
        
    | _ -> raise <| InvalidProgramException($"Unimplemented opcode: {opCodeToString opcode}")

let rec runFrameRecursive (vm: VM) : VM * Value =
    let rec loop (vm: VM) (acc: Value) =
        let frame = getCurrentFrame vm
        if frame.IP >= frame.Closure.Function.Chunk.Code.Count then
            vm, acc
        else
            let vm, instruction = readByte vm
            let opcode = byteToOpCode instruction
            let vm, result = executeOpcode vm opcode
            match result with
            | Return value -> vm, value
            | Call(argCount, recursive) ->
                let vm = callValue vm (int argCount) (int recursive)
                loop vm acc
            | Continue -> loop vm acc
    loop vm VNil

and runLoop (vm: VM) : VM =
    let rec loop (vm: VM) =
        if vm.Frames.Count = 0 then
            vm
        else
            let frame = getCurrentFrame vm
            if frame.IP >= frame.Closure.Function.Chunk.Code.Count then
                if vm.Frames.Count > 1 then
                    let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
                    vm.Frames.RemoveAt(vm.Frames.Count - 1)
                    let callerFrame = getCurrentFrame vm
                    vm.Stack.RemoveRange(callerFrame.StackBase, vm.Stack.Count - callerFrame.StackBase)
                    let vm = push vm result
                    loop vm
                else
                    vm.Frames.RemoveAt(vm.Frames.Count - 1)
                    if vm.Stack.Count = 0 then push vm VNil else vm
            else
                saveVMState vm
                let vm, instruction = readByte vm
                let opcode = byteToOpCode instruction
                let vm, result = executeOpcode vm opcode
                match result with
                | Return value -> 
                    let vm = push vm value
                    loop vm
                | Call(argCount, recursive) ->
                    let vm = callValue vm (int argCount) (int recursive)
                    loop vm
                | Continue -> 
                    loop vm
    loop vm
and callValue (vm: VM) (argCount: int) (recursive: int) : VM =
    let callee = peek vm argCount
    match (callee, recursive) with
    | (VFunction(func, _), 0) ->
        if argCount <> func.Arity then
            raise <| InvalidProgramException($"Expected {func.Arity} arguments but got {argCount}")
        let closure = {
            Function = func
            UpValues = []
            UpValuesValues = [||]
        }
        let frame = {
            Closure = closure
            IP = 0
            StackBase = vm.Stack.Count - argCount
        }
        vm.Frames.Add(frame)
        vm
        
    | (VClosure(closure, _), 0) ->
        if argCount <> closure.Function.Arity then
            raise <| InvalidProgramException($"Expected {closure.Function.Arity} arguments but got {argCount}")
        let frame = {
            Closure = closure
            IP = 0
            StackBase = vm.Stack.Count - argCount
        }
        vm.Frames.Add(frame)
        vm
        
    | (VBuiltin(func, _), _) ->
        let args = [0 .. argCount - 1] 
                  |> List.map (fun _ -> let value, _ = pop vm in value) 
                  |> List.rev
        
        let value = func args
        let vm, value = 
            match value with
            | VShape(_, _, _, _, _, _, id, _) ->
                vm.Canvas.Add(value)
                vm.Plots.Add(value)
                let record = VList([VList([VString "id"; VNumber(VInteger id)], LIST)], RECORD)
                vm, record
            | VPlotData _ ->
                vm.Plots.Add(value)
                vm, VNil
            | VPlotFunction _ ->
                vm.Plots.Add(value)
                vm, VNil
            | VPlotFunctions _ ->
                vm.Plots.Add(value)
                vm, VNil
            | VShapes(_, id) ->
                vm.Canvas.Add(value)
                vm.Plots.Add(value)
                let record = VList([VList([VString "id"; VNumber(VInteger id)], LIST)], RECORD)
                vm, record
            | VOutput s -> 
                appendOutput vm StandardOutput s, VNil
            | VEventListener(id, event, func) ->
                vm.EventListeners.Add(id, event, func)
                vm, VNil
            | _ -> vm, value
        
        let _, vm = pop vm
        push vm value
        
    | (VClosure(closure, _), 1) ->
        if argCount <> closure.Function.Arity then
            raise <| InvalidProgramException($"Expected {closure.Function.Arity} arguments but got {argCount}")
        let frame = {
            Closure = closure
            IP = 0
            StackBase = vm.Stack.Count - argCount
        }
        vm.Frames.Add(frame)
        
        match runFrameRecursive vm with
        | vm, VFunction _ ->
            vm.Frames.RemoveAt(vm.Frames.Count - 1)
            callValue vm argCount 1
        | vm, v -> 
            let vm = push vm v
            vm
            
    | (VFunction(func, _), 1) ->
        if argCount <> func.Arity then
            raise <| InvalidProgramException($"Expected {func.Arity} arguments but got {argCount}")
        let closure = {
            Function = func
            UpValues = []
            UpValuesValues = [||]
        }
        let frame = {
            Closure = closure
            IP = 0
            StackBase = vm.Stack.Count - argCount
        }
        vm.Frames.Add(frame)
        
        match runFrameRecursive vm with
        | vm, VFunction _ ->
            vm.Frames.RemoveAt(vm.Frames.Count - 1)
            callValue vm argCount 1
        | vm, v -> 
            let vm = push vm v
            vm
            
    | (VAsyncFunction func, _) ->
        let runAsyncFunction (func: Function) (args: Value list) : Async<Value> =
            async {
                let res = runFunction vm func args
                return res
            }
            
        let args = [0 .. argCount - 1] 
                  |> List.map (fun _ -> let value, _ = pop vm in value) 
                  |> List.rev
                  
        let res = runAsyncFunction func args
        push vm (VPromise res)
        
    | _ -> raise <| InvalidProgramException($"Invalid call combination: {callee}, {recursive}")

and run (vm: VM) = runLoop vm

and runFunction (vm: VM) (func: Function) (args: Value list) : Value =
    let vm = push vm (VFunction(func, None))
    args |> List.iter (fun arg -> push vm arg |> ignore)
    let vm = callValue vm (List.length args) 0
    let vm = runLoop vm
    let result, _ = pop vm
    result
    


let rec createNewVM (mainFunc: Function) : VM =
    let constantPool = 
        mainFunc.Chunk.ConstantPool 
        |> Seq.indexed 
        |> Seq.map (fun (i, value) -> $"[{i}] {valueToString value}")
    
    let vm = {
        Frames = ResizeArray<CallFrame>()
        Stack = ResizeArray<Value>(256)
        ScopeDepth = 0
        Globals = Map.fold (fun acc key value -> Map.add key value acc) (specialCasedBuiltins ()) builtins
        Streams = {
            ConstantPool = constantPool
            Disassembly = [""]
            Execution = Seq.empty
            StandardOutput = output
            Globals = Seq.empty
        }
        ExecutionHistory = ResizeArray<VM>()
        Plots = ResizeArray<Value>()
        Canvas = ResizeArray<Value>()
        EventListeners = ResizeArray<int * int * Function>()
    }
    
    let closure = {
        Function = mainFunc
        UpValues = []
        UpValuesValues = [||]
    }
    
    let mainFrame = {
        Closure = closure
        IP = 0
        StackBase = 0
    }
    
    vm.Frames.Add(mainFrame)
    vm

and specialCasedBuiltins () : Map<string, Value> =
    [
        Identifier "eval", 
        VBuiltin((fun args ->
            match args with
            | [VBlock e] ->
                match e with
                | EBlock(stmts, _) ->
                    let compiled = compileProgram stmts
                    match compiled with
                    | Ok(func, _) ->
                        let block = createNewVM(func)
                        let vm' = run block
                        vm'.Stack[vm'.Stack.Count - 1]
                    | Error err -> raise <| InvalidProgramException($"{err}")
                | _ -> raise <| InvalidProgramException("eval expects a block")
            | _ -> raise <| InvalidProgramException("eval expects a block")), 
        "Eval")
        
        Identifier "differentiate",
        VBuiltin((fun args ->
            match args with
            | [VClosure(_, Some f)] ->
                let diff = SymbolicExpression.differentiate f
                let expr = SymbolicExpression.toExpr diff
                let param = {
                    Lexeme = Identifier "x"
                    Position = { Line = 0; Column = 0 }
                }
                let expr = SExpression(
                    ELambda([param, None], expr, None, true, None, false), 
                    None
                )
                let compiled = compileProgram [expr]
                match compiled with
                | Ok(func, _) ->
                    let block = createNewVM (func)
                    let vm' = run block
                    vm'.Stack[vm'.Stack.Count - 1]
                | Error err -> raise <| InvalidProgramException($"{err}")
            | _ -> raise <| InvalidProgramException("differentiate expects a function")),
        "Differentiate")
        
        Identifier "integrate",
        VBuiltin((fun args ->
            match args with
            | [VClosure(_, Some f)] ->
                let integral = SymbolicExpression.integrate f
                let expr = SymbolicExpression.toExpr integral
                let param = {
                    Lexeme = Identifier "x"
                    Position = { Line = 0; Column = 0 }
                }
                let expr = SExpression(
                    ELambda([param, None], expr, None, true, None, false),
                    None
                )
                let compiled = compileProgram [expr]
                match compiled with
                | Ok(func, _) ->
                    let block = createNewVM (func)
                    let vm' = run block
                    vm'.Stack[vm'.Stack.Count - 1]
                | Error err -> raise <| InvalidProgramException($"{err}")
            | _ -> raise <| InvalidProgramException("integrate expects a function")),
        "Integrate")
        
        Identifier "taylorSeries",
        VBuiltin((fun args ->
            match args with
            | [VClosure(_, Some f); VNumber(VInteger n)] ->
                let series = SymbolicExpression.taylorSeries f n
                let expr = SymbolicExpression.toExpr series
                let param = {
                    Lexeme = Identifier "x"
                    Position = { Line = 0; Column = 0 }
                }
                let expr = SExpression(
                    ELambda([param, None], expr, None, true, None, false),
                    None
                )
                let compiled = compileProgram [expr]
                match compiled with
                | Ok(func, _) ->
                    let block = createNewVM (func)
                    let vm' = run block
                    vm'.Stack[vm'.Stack.Count - 1]
                | Error err -> raise <| InvalidProgramException($"{err}")
            | _ -> raise <| InvalidProgramException("taylorSeries expects a function and an integer")),
        "TaylorSeries")
    ]
    |> List.map (fun (key, value) -> lexemeToString key, value)
    |> Map.ofList
