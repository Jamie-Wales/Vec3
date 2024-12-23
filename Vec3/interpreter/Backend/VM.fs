/// <summary>
/// The virtual machine for the interpreter.
/// </summary>

module Vec3.Interpreter.Backend.VM

open System
open Compiler
open Microsoft.FSharp.Collections
open Vec3.Interpreter
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Backend.Builtins

open Vec3.Interpreter.SymbolicExpression
open Vec3.Interpreter.Token
open Grammar

/// <summary>
/// The possible results of executing an opcode.
/// </summary>
type OpCodeResult =
    | Return of Value
    | Call of byte * byte
    | Continue

/// <summary>
/// Represents the sdout output steam
let output = ref Seq.empty

/// <summary>
/// Load a function into the VM.
/// </summary>
/// <param name="vm">The VM to load the function into.</param>
/// <param name="func">The function to load.</param>
/// <returns>The VM with the function loaded.</returns>
let loadFunction (vm: VM) (func: Function) : VM =
    let closure =
        { Function = func
          UpValues = []
          UpValuesValues = [||] }

    let frame =
        { Closure = closure
          IP = 0
          StackBase = vm.Stack.Count }

    vm.Frames.Add(frame)
    vm

/// <summary>
/// Initialize the output streams for the VM.
/// </summary>
let createOutputStreams () =
    { ConstantPool = Seq.empty
      Disassembly = Seq.empty
      Execution = Seq.empty
      StandardOutput = output
      Globals = Seq.empty }

/// <summary>
/// Retrieve the current frame from the VM.
/// </summary>
/// <param name="vm">The VM to retrieve the frame from.</param>
/// <returns>The current frame.</returns>
let getCurrentFrame (vm: VM) = vm.Frames[vm.Frames.Count - 1]

/// <summary>
/// Read a byte from the current frame.
/// </summary>
/// <param name="vm">The VM to read the byte from.</param>
/// <returns>The VM and the byte read.</returns>
let readByte (vm: VM) =
    let frame = getCurrentFrame vm
    let byte = frame.Closure.Function.Chunk.Code[frame.IP]
    let updatedFrame = { frame with IP = frame.IP + 1 }
    vm.Frames[vm.Frames.Count - 1] <- updatedFrame
    (vm, byte)

/// <summary>
/// Read a constant from the current frame.
/// </summary>
/// <param name="vm">The VM to read the constant from.</param>
/// <returns>The constant and the updated VM.</returns>
let readConstant (vm: VM) =
    let vm, byte = readByte vm
    let frame = getCurrentFrame vm

    if int byte >= frame.Closure.Function.Chunk.ConstantPool.Count then
        raise
        <| InvalidProgramException
            $"Constant index out of range: {byte} (pool size: {frame.Closure.Function.Chunk.ConstantPool.Count})"

    let constant = frame.Closure.Function.Chunk.ConstantPool[int byte]
    (constant, vm)

/// <summary>
/// Read a long constant from the current frame.
/// </summary>
/// <param name="vm">The VM to read the constant from.</param>
/// <returns>The constant and the updated VM.</returns>
let readConstantLong (vm: VM) =
    let vm, byte1 = readByte vm
    let vm, byte2 = readByte vm
    let vm, byte3 = readByte vm
    let frame = getCurrentFrame vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)

    if index >= frame.Closure.Function.Chunk.ConstantPool.Count then
        raise
        <| InvalidProgramException
            $"Long constant index out of range: %d{index} (pool size: %d{frame.Closure.Function.Chunk.ConstantPool.Count})"

    let constant = frame.Closure.Function.Chunk.ConstantPool[index]
    (constant, vm)

/// <summary>
/// Save the current VM state.
/// </summary>
/// <param name="vm">The VM to save the state of.</param>
/// <returns>The updated VM.</returns>
let saveVMState (vm: VM) =
    vm.ExecutionHistory.Add(vm)
    vm

/// <summary>
/// Append a string to a stream.
/// </summary>
/// <param name="stream">The stream to append to.</param>
/// <param name="str">The string to append.</param>
/// <returns>The updated stream.</returns>
let appendToStream (stream: seq<string>) (str: string) = Seq.append stream [ str ]

/// <summary>
/// Append output to a stream.
/// </summary>
/// <param name="vm">The VM to append the output to.</param>
/// <param name="streamType">The type of stream to append to.</param>
/// <param name="str">The string to append.</param>
/// <returns>The updated VM.</returns>
let appendOutput (vm: VM) (streamType: StreamType) (str: string) =
    let updatedStreams =
        match streamType with
        | ConstantPool ->
            { vm.Streams with
                ConstantPool = appendToStream vm.Streams.ConstantPool str }
        | Disassembly ->
            { vm.Streams with
                Disassembly = appendToStream vm.Streams.Disassembly str }
        | Execution ->
            { vm.Streams with
                Execution = appendToStream vm.Streams.Execution str }
        | StandardOutput ->
            let appended = appendToStream vm.Streams.StandardOutput.Value str
            output.Value <- appended

            { vm.Streams with
                StandardOutput = output }
        | Globals ->
            { vm.Streams with
                Globals = appendToStream vm.Streams.Globals str }

    { vm with Streams = updatedStreams }

/// <summary>
/// Find an upvalue in the VM.
/// </summary>
/// <param name="vm">The VM to find the upvalue in.</param>
/// <param name="depth">The depth to search for the upvalue.</param>
/// <param name="index">The index of the upvalue.</param>
/// <returns>The upvalue.</returns>
let findUpValue (vm: VM) (depth: int) (index: int) =
    let frame = getCurrentFrame vm

    let rec findUpValue' frame depth index =
        if depth = 0 then
            vm.Stack[frame.StackBase + index]
        else
            let frame = vm.Frames[vm.Frames.Count - depth]
            findUpValue' frame (depth - 1) index

    findUpValue' frame depth index

/// <summary>
/// Push a value onto the stack.
/// </summary>
/// <param name="vm">The VM to push the value onto.</param>
/// <param name="value">The value to push.</param>
/// <returns>The updated VM.</returns>
let push (vm: VM) (value: Value) =
    vm.Stack.Add(value)
    vm

/// <summary>
/// Pop a value from the stack.
/// </summary>
/// <param name="vm">The VM to pop the value from.</param>
/// <returns>The value and the updated VM.</returns>
let pop (vm: VM) =
    let value = vm.Stack[vm.Stack.Count - 1]
    vm.Stack.RemoveAt(vm.Stack.Count - 1)
    (value, vm)

/// <summary>
/// Peek at a value on the stack.
/// </summary>
/// <param name="vm">The VM to peek at the value on.</param>
/// <param name="offset">The offset to peek at.</param>
/// <returns>The value.</returns>
let peek (vm: VM) offset = vm.Stack[vm.Stack.Count - 1 - offset]

/// <summary>
/// Add a global variable to the VM.
/// </summary>
/// <param name="vm">The VM to add the global variable to.</param>
/// <param name="name">The name of the global variable.</param>
/// <param name="value">The value of the global variable.</param>
/// <returns>The updated VM.</returns>
let defineGlobal (vm: VM) (name: string) (value: Value) =
    let updatedGlobals = Map.add name value vm.Globals
    let updatedVM = { vm with Globals = updatedGlobals }
    let globalOutput = $"{name} = {valueToString value}"
    appendOutput updatedVM Globals globalOutput

/// <summary>
/// Get a global variable from the VM.
/// </summary>
/// <param name="vm">The VM to get the global variable from.</param>
/// <param name="name">The name of the global variable.</param>
/// <returns>The global variable option.</returns>
let getGlobal (vm: VM) (name: string) = Map.tryFind name vm.Globals

/// <summary>
/// Get the string content of a stream.
/// </summary>
/// <param name="stream">The stream to get the content of.</param>
/// <returns>The content of the stream.</returns>
let getStreamContent (stream: seq<string>) =
    String.concat Environment.NewLine (Seq.toArray stream)

/// <summary>
/// Reset the output streams of the VM.
/// </summary>
/// <param name="vm">The VM to reset the streams of.</param>
/// <returns>The updated VM.</returns>
let resetStreams (vm: VM) =
    { vm with
        Streams = createOutputStreams () }

/// <summary>
/// Execute an opcode.
/// </summary>
/// <param name="vm">The VM to execute the opcode on.</param>
/// <param name="opcode">The opcode to execute.</param>
/// <returns>The updated VM and the result of the opcode execution.</returns>
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
                    vm)
                vm
                [ 0 .. argCount - 1 ]

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

/// <summary>
/// The implementation of the opcode execution.
/// </summary>
/// <param name="vm">The VM to execute the opcode on.</param>
/// <param name="opcode">The opcode to execute.</param>
/// <returns>The updated VM.</returns>
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

        // Add validation
        if frame.StackBase < 0 || frame.StackBase >= vm.Stack.Count then
            raise
            <| InvalidProgramException($"Invalid stack base: {frame.StackBase} (stack size: {vm.Stack.Count})")

        if index < 0 || index >= vm.Stack.Count then
            raise
            <| InvalidProgramException(
                $"GET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"
            )

        let value = vm.Stack[index]
        push vm value

    | SET_LOCAL ->
        let vm, slot = readByte vm
        let value, vm = pop vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot

        if index >= vm.Stack.Count then
            raise
            <| InvalidProgramException(
                $"SET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"
            )

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

            let vm =
                appendOutput vm Execution $"Defining global variable: {name} = {valueToString value}"

            defineGlobal vm name value
        | _ ->
            raise
            <| InvalidProgramException
                $"Expected string constant for variable name in DEFINE_GLOBAL, got {valueToString constant}"

    | GET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            match getGlobal vm name with
            | Some value -> push vm value
            | None -> raise <| InvalidProgramException($"Undefined variable '{name}'")
        | _ ->
            raise
            <| InvalidProgramException
                $"Expected string constant for variable name in GET_GLOBAL, got {valueToString constant}"

    | SET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            let value, vm = pop vm

            if vm.Globals.ContainsKey(name) then
                defineGlobal vm name value
            else
                raise <| InvalidProgramException($"Undefined variable '{name}'")
        | o -> raise <| InvalidProgramException($"Undefined variable '{o}'")

    | CLOSURE ->
        let constant, vm = readConstant vm

        match constant with
        | VFunction(func, f) ->
            let vm, upValueCount = readByte vm

            let rec popUpValues (vm: VM) (count: int) : VM * Local list =
                match count with
                | n when n > 0 ->
                    let vm, index = readByte vm
                    let vm, depth = readByte vm
                    let name, vm = readConstant vm

                    let name =
                        match name with
                        | VString s -> s
                        | _ -> raise <| InvalidProgramException "Expected string constant for upvalue name"


                    let vm, upValues = popUpValues vm (n - 1)

                    let upValue =
                        { Index = int index
                          Depth = int depth
                          Name = name }

                    vm, upValue :: upValues

                | _ -> vm, []

            let rec getUpvaluesVals (vm: VM) (count: int) : Value list =
                match count with
                | n when n > 0 && n < vm.Stack.Count ->
                    let value = peek vm (n - 1)
                    let values = getUpvaluesVals vm (n - 1)
                    value :: values
                | _ -> []

            let prevUpvals = getCurrentFrame vm |> _.Closure.UpValuesValues
            // let prevUv = getCurrentFrame vm |> _.Closure.UpValues

            let vm, vals = popUpValues vm (int upValueCount)
            // let vals = List.append prevUv vals
            let upvalues =
                Seq.toArray <| getUpvaluesVals vm (int upValueCount - Array.length prevUpvals)

            let upvalues = Array.append prevUpvals upvalues


            // how do i set the upvalues ?
            // where do i get the upvalues from ?
            // answer: from the stack

            let closure =
                VClosure(
                    { Function = func
                      UpValues = vals
                      UpValuesValues = upvalues },
                    f
                )

            let vm = push vm closure
            vm
        | VAsyncFunction func ->
            let vm, upValueCount = readByte vm

            let rec popUpValues (vm: VM) (count: int) : VM * Local list =
                match count with
                | n when n > 0 ->
                    let vm, index = readByte vm
                    let vm, depth = readByte vm
                    let name, vm = readConstant vm

                    let name =
                        match name with
                        | VString s -> s
                        | _ -> raise <| InvalidProgramException "Expected string constant for upvalue name"


                    let vm, upValues = popUpValues vm (n - 1)

                    let upValue =
                        { Index = int index
                          Depth = int depth
                          Name = name }

                    vm, upValue :: upValues

                | _ -> vm, []

            let rec getUpvaluesVals (vm: VM) (count: int) : Value list =
                match count with
                | n when n > 0 && n < vm.Stack.Count ->
                    let value = peek vm (n - 1)
                    let values = getUpvaluesVals vm (n - 1)
                    value :: values
                | _ -> []

            let vm, _ = popUpValues vm (int upValueCount)

            // how do i set the upvalues ?
            // where do i get the upvalues from ?
            // answer: from the stack

            let async = VAsyncFunction func

            let vm = push vm async
            vm

        | _ ->
            raise
            <| InvalidProgramException $"Expected function constant for closure, got {valueToString constant}"

    | GET_UPVALUE ->
        let vm, slot = readByte vm

        let rec findClosure (curDepth: int) =
            let closure = peek vm curDepth

            match closure with
            | VClosure _ -> closure
            | _ -> findClosure (curDepth + 1)

        let closure = findClosure 0

        match closure with
        | VClosure({ UpValues = upValues
                     UpValuesValues = vals },
                   _) ->
            let upValues = List.rev upValues
            // let upValue = upValues[int slot]
            let vals = Array.rev vals
            let upValue = vals[if int slot = 0 then 0 else int slot - 1]
            let vm = push vm upValue
            vm
        | _ -> raise <| InvalidProgramException "Expected closure for GET_UPVALUE"

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
            let values' =
                [ 0 .. n - 1 ] |> List.map (fun _ -> let value, _ = pop vm in value) |> List.rev

            let list = VList(List.append values values', typ)
            push vm list
        | _ -> raise <| InvalidProgramException("Expected list and integer for list create")

    | _ ->
        raise
        <| InvalidProgramException($"Unimplemented opcode: {opCodeToString opcode}")

/// <summary>
/// Run the current recursive frame without creating a new frame.
/// </summary>
/// <param name="vm">The VM to run the frame on.</param>
/// <returns>The updated VM and the result of the frame execution.</returns>
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

/// <summary>
/// The main run loop of the VM.
/// </summary>
/// <param name="vm">The VM to run the loop on.</param>
/// <returns>The updated VM.</returns>
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
                let vm = saveVMState vm
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
                | Continue -> loop vm

    loop vm

/// <summary>
/// Call a value in the VM.
/// </summary>
/// <param name="vm">The VM to call the value in.</param>
/// <param name="argCount">The number of arguments to call the value with.</param>
/// <param name="recursive">Whether the call is recursive.</param>
/// <returns>The updated VM.</returns>
and callValue (vm: VM) (argCount: int) (recursive: int) : VM =
    let callee = peek vm argCount

    match (callee, recursive) with
    | VBuiltin(func, _), _ ->
        let args =
            [ 0 .. argCount - 1 ]
            |> List.map (fun _ -> let value, _ = pop vm in value)
            |> List.rev

        let value = func args

        let vm, value =
            match value with
            | VShape(_, _, _, _, _, _, id, _) ->
                vm.Canvas.Add(value)
                vm.Plots.Add(value)
                let record = VNumber(VInteger id)
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
                let record = VNumber(VInteger id)
                vm, record
            | VOutput s -> appendOutput vm StandardOutput s, VNil
            | VEventListener(id, event, func) ->
                vm.EventListeners.Add(id, event, func)
                vm, VNil
            | _ -> vm, value

        let _, vm = pop vm
        push vm value

    | VFunction(func, _), 0
    | VClosure({ Function = func }, _), 0 ->
        if argCount <> func.Arity then
            raise
            <| InvalidProgramException($"Expected {func.Arity} arguments but got {argCount}")

        let stackBase = vm.Stack.Count - argCount

        let frame =
            { Closure =
                match callee with
                | VClosure(closure, _) ->
                    { closure with
                        UpValuesValues = Array.copy closure.UpValuesValues }
                | VFunction(f, _) ->
                    { Function = f
                      UpValues = []
                      UpValuesValues = [||] }
                | _ -> failwith "Impossible"
              IP = 0
              StackBase = stackBase }

        vm.Frames.Add(frame)
        vm

    | VAsyncFunction func, 0 ->
        let runAsyncFunction (func: Function) (args: Value list) : Async<Value> =
            async {
                let res = runFunction vm func args
                return res
            }

        let args =
            [ 0 .. argCount - 1 ]
            |> List.map (fun _ -> let value, _ = pop vm in value)
            |> List.rev

        let res = runAsyncFunction func args
        push vm (VPromise res)

    | VFunction(func, _), 1
    | VClosure({ Function = func }, _), 1 ->
        if argCount <> func.Arity then
            raise
            <| InvalidProgramException($"Expected {func.Arity} arguments but got {argCount}")

        let stackBase = vm.Stack.Count - argCount - 1

        let frame =
            { Closure =
                match callee with
                | VClosure(closure, _) ->
                    { closure with
                        UpValuesValues = Array.copy closure.UpValuesValues }
                | VFunction(f, _) ->
                    { Function = f
                      UpValues = []
                      UpValuesValues = [||] }
                | _ -> failwith "Impossible"
              IP = 0
              StackBase = stackBase }

        // Remove old frame and add new one
        // vm.Frames.RemoveAt(vm.Frames.Count - 1)
        vm.Frames.Add(frame)

        // Execute the tail call directly here
        let rec loop (vm: VM) =
            let frame = getCurrentFrame vm

            if frame.IP >= frame.Closure.Function.Chunk.Code.Count then
                vm
            else
                let vm = saveVMState vm
                let vm, instruction = readByte vm
                let opcode = byteToOpCode instruction
                let vm, result = executeOpcode vm opcode

                match result with
                | Return value ->
                    // pop the old frame
                    // let _, vm = pop vm
                    push vm value
                | Call(argCount, recursive) ->
                    let vm = callValue vm (int argCount) (int recursive)
                    loop vm
                | Continue -> loop vm

        loop vm

    | _ ->
        raise
        <| InvalidProgramException $"Can only call functions and closures {callee}"

/// <summary>
/// Run the VM.
/// </summary>
/// <param name="vm">The VM to run.</param>
/// <returns>The updated VM.</returns>
and run (vm: VM) = runLoop vm

/// <summary>
/// Join two standard output streams.
/// </summary>
/// <param name="vm1">The first VM.</param>
/// <param name="vm2">The second VM.</param>
/// <returns>The updated VM.</returns>
and joinOutput (vm1: VM) (vm2: VM) =
    appendOutput vm1 StandardOutput (vm2.Streams.StandardOutput.ToString())

/// <summary>
/// Create a new VM.
/// </summary>
/// <param name="mainFunc">The main function to create the VM with.</param>
/// <returns>The new VM.</returns>
and createNewVM (mainFunc: Function) : VM =
    let constantPool =
        mainFunc.Chunk.ConstantPool
        |> Seq.indexed
        |> Seq.map (fun (i, value) -> $"[{i}] {valueToString value}")

    let vm =
        { Frames = ResizeArray<CallFrame>()
          Stack = ResizeArray<Value>(256)
          ScopeDepth = 0
          Globals = Map.fold (fun acc key value -> Map.add key value acc) (specialCasedBuiltins ()) builtins
          Streams =
            { ConstantPool = constantPool
              Disassembly = [ "" ]
              Execution = Seq.empty
              StandardOutput = output
              Globals = Seq.empty }
          ExecutionHistory = ResizeArray<VM>()
          Plots = ResizeArray<Value>()
          Canvas = ResizeArray<Value>()
          EventListeners = ResizeArray<int * int * Function>() }

    let closure =
        { Function = mainFunc
          UpValues = []
          UpValuesValues = [||] }

    let mainFrame =
        { Closure = closure
          IP = 0
          StackBase = 0 }

    vm.Frames.Add(mainFrame)
    vm

/// <summary>
/// Builtins that need access to the VM.
/// </summary>
/// <returns>The builtins.</returns>
and specialCasedBuiltins () : Map<string, Value> =
    [ Identifier "eval",
      VBuiltin(
          (fun args ->
              match args with
              | [ VBlock e ] ->
                  match e with
                  | EBlock(stmts, _, _) ->
                      let compiled = compileProgram stmts

                      match compiled with
                      | Ok(func, _) ->
                          let block = createNewVM func
                          let vm' = run block
                          vm'.Stack[vm'.Stack.Count - 1]
                      | Error err -> raise <| InvalidProgramException $"{err}"
                  | _ -> raise <| InvalidProgramException "eval expects a block"
              | _ -> raise <| InvalidProgramException "eval expects a block"),
          "Eval"
      )

      Identifier "differentiate",
      VBuiltin(
          (fun args ->
              match args with
              | [ VClosure(_, Some f) ] ->
                  let diff = SymbolicExpression.differentiate f
                  let expr = SymbolicExpression.toExpr diff

                  let param =
                      { Lexeme = Identifier "x"
                        Position = { Line = 0; Column = 0 } }

                  let expr =
                      SExpression(ELambda([ (param, None) ], expr, None, true, None, false), None)

                  let compiled = compileProgram [ expr ]

                  match compiled with
                  | Ok(func, _) ->
                      let block = createNewVM func
                      let vm' = run block
                      vm'.Stack[vm'.Stack.Count - 1]
                  | Error err -> raise <| InvalidProgramException $"{err}"

              | _ -> raise <| InvalidProgramException "differentiate expects a function"),
          "Differentiate"
      )
      Identifier "tangentFunc",
      VBuiltin(
          (fun args ->
              match args with
              | [ VClosure(_, Some f); VNumber(VInteger n) ] ->
                  let t = tangentAt f n
                  let expr = toExpr t

                  let param =
                      { Lexeme = Identifier "x"
                        Position = { Line = 0; Column = 0 } }

                  let expr =
                      SExpression(ELambda([ (param, None) ], expr, None, true, None, false), None)

                  let compiled = compileProgram [ expr ]

                  match compiled with
                  | Ok(func, _) ->
                      let block = createNewVM func
                      let vm' = run block
                      vm'.Stack[vm'.Stack.Count - 1]
                  | Error err -> raise <| InvalidProgramException $"{err}"
              | [ VClosure(_, Some f); VNumber(VFloat n) ] ->
                  let t = tangentAt f n
                  let expr = toExpr t

                  let param =
                      { Lexeme = Identifier "x"
                        Position = { Line = 0; Column = 0 } }

                  let expr =
                      SExpression(ELambda([ (param, None) ], expr, None, true, None, false), None)

                  let compiled = compileProgram [ expr ]

                  match compiled with
                  | Ok(func, _) ->
                      let block = createNewVM func
                      let vm' = run block
                      vm'.Stack[vm'.Stack.Count - 1]
                  | Error err -> raise <| InvalidProgramException $"{err}"
              | _ -> raise <| InvalidProgramException "integrate expects a function"),
          "Integrate"
      )

      Identifier "integrate",
      VBuiltin(
          (fun args ->
              match args with
              | [ VClosure(_, Some f) ] ->
                  let integral = SymbolicExpression.integrate f
                  let expr = toExpr integral

                  let param =
                      { Lexeme = Identifier "x"
                        Position = { Line = 0; Column = 0 } }

                  let expr =
                      SExpression(ELambda([ (param, None) ], expr, None, true, None, false), None)

                  let compiled = compileProgram [ expr ]

                  match compiled with
                  | Ok(func, _) ->
                      let block = createNewVM func
                      let vm' = run block
                      vm'.Stack[vm'.Stack.Count - 1]
                  | Error err -> raise <| InvalidProgramException $"{err}"
              | _ -> raise <| InvalidProgramException "integrate expects a function"),
          "Integrate"
      )
      Identifier "taylorSeries",
      VBuiltin(
          (fun args ->
              match args with
              | [ VClosure(_, Some f); VNumber(VInteger n) ] ->
                  let series = SymbolicExpression.taylorSeries f n
                  let expr = SymbolicExpression.toExpr series

                  let param =
                      { Lexeme = Identifier "x"
                        Position = { Line = 0; Column = 0 } }

                  let expr =
                      SExpression(ELambda([ (param, None) ], expr, None, true, None, false), None)

                  let compiled = compileProgram [ expr ]

                  match compiled with
                  | Ok(func, _) ->
                      let block = createNewVM func
                      let vm' = run block
                      vm'.Stack[vm'.Stack.Count - 1]
                  | Error err -> raise <| InvalidProgramException $"{err}"
              | _ ->
                  raise
                  <| InvalidProgramException "taylorSeries expects a function and an integer"),
          "TaylorSeries"
      )

      ]
    |> List.map (fun (key, value) -> lexemeToString key, value)
    |> Map.ofList

/// <summary>
/// Run a function in the VM.
/// </summary>
/// <param name="vm">The VM to run the function in.</param>
/// <param name="func">The function to run.</param>
/// <param name="args">The arguments to run the function with.</param>
/// <returns>The result of the function.</returns>
and runFunction (vm: VM) (func: Function) (args: Value list) : Value =
    let vm = push vm (VFunction(func, None))
    args |> List.iter (fun arg -> push vm arg |> ignore)
    let vm = callValue vm (List.length args) 0
    let vm = runLoop vm
    let result, _ = pop vm
    result
