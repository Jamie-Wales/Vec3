/// <summary>
/// The virtual machine for the interpreter.
/// </summary>

// might do a hacky thing for upvalues, store them in an env or something
// and then just look them up in the env.
// but how do i set them ??
// think current issue is how thte return clears teh stack of info.
// also maybe affects recursive funcs ??

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

let output = ref Seq.empty

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

let createOutputStreams () =
    { ConstantPool = Seq.empty
      Disassembly = Seq.empty
      Execution = Seq.empty
      StandardOutput = output
      Globals = Seq.empty }

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
        raise
        <| InvalidProgramException
            $"Constant index out of range: {byte} (pool size: {frame.Closure.Function.Chunk.ConstantPool.Count})"

    let constant = frame.Closure.Function.Chunk.ConstantPool[int byte]
    (constant, vm)

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


let saveVMState (vm: VM) = vm.ExecutionHistory.Add(vm)

let appendToStream (stream: seq<string>) (str: string) = Seq.append stream [ str ]

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

let findUpValue (vm: VM) (depth: int) (index: int) =
    let frame = getCurrentFrame vm

    let rec findUpValue' frame depth index =
        if depth = 0 then
            vm.Stack[frame.StackBase + index]
        else
            let frame = vm.Frames[vm.Frames.Count - depth]
            findUpValue' frame (depth - 1) index

    findUpValue' frame depth index

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

let getStreamContent (stream: seq<string>) =
    String.concat Environment.NewLine (Seq.toArray stream)

let resetStreams (vm: VM) =
    { vm with
        Streams = createOutputStreams () }


let rec runFrameRecursive vm =
    let frame = getCurrentFrame vm

    if frame.IP >= frame.Closure.Function.Chunk.Code.Count then
        VNil, vm
    else
        let vm, instruction = readByte vm
        let opcode = byteToOpCode instruction

        match opcode with
        | CALL ->
            let vm, argCount = readByte vm
            let vm, _ = readByte vm
            let callee = peek vm (int argCount)
            callee, vm
        | _ ->
            let vm = executeOpcode vm opcode
            runCurrentFrame vm

and runCurrentFrame vm =
    let frame = getCurrentFrame vm

    if frame.IP >= frame.Closure.Function.Chunk.Code.Count then
        VNil, vm
    else
        let vm, instruction = readByte vm
        let opcode = byteToOpCode instruction

        match opcode with
        | RETURN ->
            let currentFrame = getCurrentFrame vm
            let argCount = List.length currentFrame.Closure.Function.Locals
            let vm, shouldPop = readByte vm

            let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
            // get values
            List.iter (fun _ -> let _, _ = pop vm in ()) [ 0 .. (int argCount) - 1 ]

            if int shouldPop = 1 then
                let _ = pop vm
                ()

            vm.Frames.RemoveAt(vm.Frames.Count - 1)
            result, vm
        | _ ->
            let vm = executeOpcode vm opcode
            runCurrentFrame vm


and executeOpcode (vm: VM) (opcode: OP_CODE) =
    let vm = appendOutput vm Execution $"Executing: {opCodeToString opcode}"

    match opcode with
    | CONSTANT ->
        let constant, vm = readConstant vm
        let vm = push vm constant

        let _ =
            appendOutput vm Execution $"Pushed constant onto stack: {valueToString constant}"

        vm
    | CONSTANT_LONG ->
        let constant, vm = readConstantLong vm
        let vm = push vm constant

        let _ =
            appendOutput vm Execution $"Pushed long constant onto stack: {valueToString constant}"

        vm
    | GET_UPVALUE -> // broken currently, how do i get the upvalues ????????????????????????/
        let vm, slot = readByte vm

        let rec findClosure (curDepth: int) =
            let closure = peek vm curDepth

            match closure with
            | VClosure _ -> closure
            | _ -> findClosure (curDepth + 1)

        let closure = findClosure 1

        match closure with
        | VClosure({ UpValues = upValues
                     UpValuesValues = vals },
                   _) ->
            let upValues = List.rev upValues
            let upValue = upValues[int slot]
            let vals = Array.rev vals
            let upValue = vals[upValue.Index]
            let vm = push vm upValue
            vm
        | _ -> raise <| InvalidProgramException "Expected closure for GET_UPVALUE"

    | GET_LOCAL ->
        let vm, slot = readByte vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot

        if index >= vm.Stack.Count then
            raise
            <| InvalidProgramException
                $"GET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"

        let value = vm.Stack[index]
        let vm = push vm value
        vm
    | SET_LOCAL ->
        let vm, slot = readByte vm
        let value, vm = pop vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot

        if index >= vm.Stack.Count then
            raise
            <| InvalidProgramException
                $"SET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"

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
            <| InvalidProgramException "Expected string constant for variable name in DEFINE_GLOBAL"
    | GET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            match getGlobal vm name with
            | Some value -> push vm value
            | None -> raise <| InvalidProgramException $"Undefined variable '{name}'"
        | _ ->
            raise
            <| InvalidProgramException "Expected string constant for variable name in GET_GLOBAL"
    | SET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            let value, vm = pop vm

            if vm.Globals.ContainsKey(name) then
                let vm = defineGlobal vm name value
                vm
            else
                raise <| InvalidProgramException $"Undefined variable '{name}'"
        | _ ->
            raise
            <| InvalidProgramException "Expected string constant for variable name in SET_GLOBAL"
    | CALL ->
        let vm, argCount = readByte vm
        let vm, recursive = readByte vm
        callValue vm (int argCount) (int recursive)
    | RETURN ->
        let currentFrame = getCurrentFrame vm
        let argCount = List.length currentFrame.Closure.Function.Locals
        //  let localCount = currentFrame.Function.Locals.Length
        let vm, shouldPop = readByte vm
        let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm

        // pop arg count
        List.iter (fun _ -> let _, _ = pop vm in ()) [ 0 .. (int argCount) - 1 ]
        // List.iter (fun _ -> let _, _ = pop vm in ()) [ 0 .. (int localCount) - 1 ]
        if int shouldPop = 1 then
            let _ = pop vm
            ()


        vm.Frames.RemoveAt(vm.Frames.Count - 1)

        if vm.Frames.Count = 0 then
            let vm = push vm result
            vm
        else
            let vm = push vm result
            runLoop vm
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
            let prevUv = getCurrentFrame vm |> _.Closure.UpValues
            let vm, vals = popUpValues vm (int upValueCount)
            let vals = List.append vals prevUv
            let upvalues = Seq.toArray <| getUpvaluesVals vm (int upValueCount)
            let upvalues = Array.append upvalues prevUpvals

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
        | _ -> raise <| InvalidProgramException "Expected function constant for closure"
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
            let vm = push vm list
            vm
        | _ -> raise <| InvalidProgramException("Expected list and integer for list create")

    | _ ->
        raise
        <| InvalidProgramException($"Unimplemented opcode: {opCodeToString opcode}")

and runLoop vm =
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
                runLoop vm
            else
                vm.Frames.RemoveAt(vm.Frames.Count - 1)
                let vm = if vm.Stack.Count = 0 then push vm VNil else vm
                vm
        else
            saveVMState vm
            let vm, instruction = readByte vm

            let vm =
                let opcode = byteToOpCode instruction
                executeOpcode vm opcode

            runLoop vm

and joinOutput (vm1: VM) (vm2: VM) =
    appendOutput vm1 StandardOutput (vm2.Streams.StandardOutput.ToString())

and callValue (vm: VM) (argCount: int) (recursive: int) : VM =
    let callee = peek vm argCount

    match recursive with
    | 0 ->
        match callee with
        | VFunction(func, _) ->
            if argCount <> func.Arity then
                raise
                <| InvalidProgramException $"Expected {func.Arity} arguments but got {argCount} for function {func}"

            // remove caller from stack
            let closure =
                { Function = func
                  UpValues = []
                  UpValuesValues = [||] }

            let frame =
                { Closure = closure
                  IP = 0
                  StackBase = vm.Stack.Count - argCount }

            vm.Frames.Add(frame)
            vm
        | VClosure(closure, _) ->
            if argCount <> closure.Function.Arity then
                raise
                <| InvalidProgramException $"Expected {closure.Function.Arity} arguments but got {argCount} for closure"

            let frame =
                { Closure = closure
                  IP = 0
                  StackBase = vm.Stack.Count - argCount }

            vm.Frames.Add(frame)
            vm
        | VBuiltin(func, _) ->
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
                    let record = VList([ VList([ VString "id"; VNumber(VInteger id) ], LIST) ], RECORD)
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
                    let record = VList([ VList([ VString "id"; VNumber(VInteger id) ], LIST) ], RECORD)
                    vm, record
                | VOutput s -> appendOutput vm StandardOutput s, VNil
                | VEventListener(id, event, func) ->
                    vm.EventListeners.Add(id, event, func)
                    vm, VNil
                | _ -> vm, value

            let _, vm = pop vm
            push vm value
        | VAsyncFunction func ->
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
        | _ ->
            raise
            <| InvalidProgramException $"Can only call functions and closures {callee}"
    | 1 ->
        match callee with
        | VClosure(closure, _) ->
            if argCount <> closure.Function.Arity then
                raise
                <| InvalidProgramException $"Expected {closure.Function.Arity} arguments but got {argCount} for closure"

            let frame =
                { Closure = closure
                  IP = 0
                  StackBase = vm.Stack.Count - argCount }

            vm.Frames.Add(frame)

            match runFrameRecursive vm with
            | VFunction _, vm ->
                vm.Frames.RemoveAt(vm.Frames.Count - 1)
                callValue vm argCount 1
            | v, vm -> let _ = push vm v in vm

        | VFunction(func, _) ->
            if argCount <> func.Arity then
                raise
                <| InvalidProgramException
                    $"Expected {func.Arity} arguments but got {argCount} for function {func.Name}"

            let closure =
                { Function = func
                  UpValues = []
                  UpValuesValues = [||] }

            let frame =
                { Closure = closure
                  IP = 0
                  StackBase = vm.Stack.Count - argCount }

            vm.Frames.Add(frame)

            match runFrameRecursive vm with
            | VFunction _, vm ->
                vm.Frames.RemoveAt(vm.Frames.Count - 1)
                callValue vm argCount 1
            | v, vm -> let _ = push vm v in vm
        | VBuiltin(func, _) ->
            let args =
                [ 0 .. argCount - 1 ]
                |> List.map (fun _ -> let value, _ = pop vm in value)
                |> List.rev

            let value = func args

            let vm, value =
                match value with
                | VShape(_, _, _, _, _, _, id, _) ->
                    vm.Plots.Add(value)
                    vm.Canvas.Add(value)
                    let record = VList([ VList([ VString "id"; VNumber(VInteger id) ], LIST) ], RECORD)
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
                    vm.Plots.Add(value)
                    vm.Canvas.Add(value)
                    let record = VList([ VList([ VString "id"; VNumber(VInteger id) ], LIST) ], RECORD)
                    vm, record
                | VOutput s -> appendOutput vm StandardOutput s, VNil
                | VEventListener(id, event, func) ->
                    vm.EventListeners.Add(id, event, func)
                    vm, VNil
                | _ -> vm, value

            let _, vm = pop vm
            push vm value
        | _ ->
            raise
            <| InvalidProgramException $"Can only call functions and closures {callee}"
    | _ -> raise <| InvalidProgramException $"Invalid recursive value {recursive}"



and run (vm: VM) = runLoop vm


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

and specialCasedBuiltins () : Map<string, Value> =
    [ Identifier "eval",
      VBuiltin(
          (fun args ->
              match args with
              | [ VBlock e ] ->
                  match e with
                  | EBlock(stmts, _) ->
                      let compiled = compileProgram stmts

                      match compiled with
                      | Ok(func, _) ->
                          let block = createNewVM (func)
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
                      let block = createNewVM (func)
                      let vm' = run block
                      vm'.Stack[vm'.Stack.Count - 1]
                  | Error err -> raise <| InvalidProgramException $"{err}"

              | _ -> raise <| InvalidProgramException "differentiate expects a function"),
          "Differentiate"
      )

      Identifier "integrate",
      VBuiltin(
          (fun args ->
              match args with
              | [ VClosure(_, Some f) ] ->
                  let integral = SymbolicExpression.integrate f
                  let expr = SymbolicExpression.toExpr integral

                  let param =
                      { Lexeme = Identifier "x"
                        Position = { Line = 0; Column = 0 } }

                  let expr =
                      SExpression(ELambda([ (param, None) ], expr, None, true, None, false), None)

                  let compiled = compileProgram [ expr ]

                  match compiled with
                  | Ok(func, _) ->
                      let block = createNewVM (func)
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
                      let block = createNewVM (func)
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

and runFunction (vm: VM) (func: Function) (args: Value list) : Value =
    let vm = push vm (VFunction(func, None))
    args |> List.iter (fun arg -> push vm arg |> ignore)
    let vm = callValue vm (List.length args) 0
    let vm = runLoop vm
    let result, _ = pop vm
    result