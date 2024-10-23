module Vec3.Interpreter.Backend.VM

open System
open Microsoft.FSharp.Collections
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Token

let createOutputStreams () =
    { ConstantPool = Seq.empty
      Disassembly = Seq.empty
      Execution = Seq.empty
      StandardOutput = Seq.empty
      Globals = Seq.empty }

let getCurrentFrame (vm: VM) = vm.Frames[vm.Frames.Count - 1]

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
        failwith $"Long constant index out of range: %d{index} (pool size: %d{frame.Function.Chunk.ConstantPool.Count})"

    let constant = frame.Function.Chunk.ConstantPool[index]
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
            { vm.Streams with
                StandardOutput = appendToStream vm.Streams.StandardOutput str }
        | Globals ->
            { vm.Streams with
                Globals = appendToStream vm.Streams.Globals str }

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

let binaryOp (vm: VM) (op: Value -> Value -> Value) =
    let b, vm = pop vm
    let a, vm = pop vm
    let result = op a b
    push vm result

let callValue (vm: VM) (argCount: int) : VM =
    let callee = peek vm argCount
    printfn $"Calling value: {valueToString callee}"

    match callee with
    | VFunction func ->
        if argCount <> func.Arity then
            failwith $"Expected {func.Arity} arguments but got {argCount}"

        let frame =
            { Function = func
              IP = 0
              StackBase = vm.Stack.Count - argCount
              Locals = Array.zeroCreate func.Locals.Length }

        vm.Frames.Add(frame)
        vm
    | VClosure closure ->
        if argCount <> closure.Function.Arity then
            failwith $"Expected {closure.Function.Arity} arguments but got {argCount}"

        let frame =
            { Function = closure.Function
              IP = 0
              StackBase = vm.Stack.Count - argCount
              Locals = Array.zeroCreate closure.Function.Locals.Length }

        vm.Frames.Add(frame)
        let frame = getCurrentFrame vm
        closure.UpValues |> Seq.iteri (fun i upValue -> frame.Locals[i] <- upValue)
        vm
    | VBuiltin func ->
        printfn $"Calling builtin function: {func}"

        let args =
            [ 0 .. argCount - 1 ]
            |> List.map (fun _ -> let value, _ = pop vm in value)
            |> List.rev

        func args vm
    | _ -> failwith $"Can only call functions, got: {valueToString callee}"

let rec builtins () =
    [ Identifier "plot",
      VBuiltin(fun args vm ->
          match args with
          | [ VString title; VList xs; VList ys ] ->
              let result = VPlotData(title, xs, ys)
              push vm result
          | _ ->
              failwith
                  $"""plot expects a title, x values, and y values, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "print",
      VBuiltin(fun args vm ->
          let vm =
              appendOutput vm StandardOutput $"""{String.concat " " (List.map valueToString args)}"""

          push vm VNil)
      Identifier "sqrt",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] ->
              let result = VNumber(VFloat(sqrt f))
              push vm result
          | _ -> failwith $"""sqrt expects a float, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "abs",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(abs f)))
          | _ -> failwith $"""abs expects a float, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "power",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat x); VNumber(VFloat y) ] ->
              let result = VNumber(VFloat(pown x (int y)))
              push vm result
          | [ VNumber(VFloat x); VNumber(VInteger y) ] ->
              let result = VNumber(VFloat(pown x y))
              push vm result
          | [ VNumber(VInteger x); VNumber(VFloat y) ] ->
              let result = VNumber(VFloat(pown x (int y)))
              push vm result
          | [ VNumber(VInteger x); VNumber(VInteger y) ] ->
              let result = VNumber(VInteger(pown x y))
              push vm result
          | _ -> failwith "Power expects two floats")
      Identifier "floor",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(floor f)))
          | _ -> failwith $"""floor expects a float, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "cos",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(cos f)))
          | _ -> failwith $"""cos expects a float, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "sin",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(sin f)))
          | _ -> failwith $"""sin expects a float, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "tan",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(tan f)))
          | _ -> failwith $"""tan expects a float, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "input",
      VBuiltin(fun _ vm ->
          let input = Console.ReadLine()
          push vm (VString input))
      Identifier "cons",
      VBuiltin(fun args vm ->
          match args with
          | [ value; VList l ] ->
              let list = VList(value :: l)
              push vm list
          | _ ->
              failwith $"""cons expects a value and a list, got: {String.concat ", " (List.map valueToString args)}""")
      Identifier "exit",
      VBuiltin(fun _ vm ->
          Environment.Exit(0)
          vm)
      // works if predefined func, fails if lambda, i suspect lambda is not being compiled correctly
      // fails if function is complex, even let fun = (x, acc) -> x + acc, the plus gets called after ?????
      Identifier "fold",
      VBuiltin(fun args vm ->
          match args with
          | [ VList l; acc; VFunction f ] ->
              let rec runCurrentFrame vm =
                  let frame = getCurrentFrame vm

                  if frame.IP >= frame.Function.Chunk.Code.Count then
                      VNil, vm
                  else
                      let vm, instruction = readByte vm
                      let opcode = byteToOpCode instruction
                      printfn $"Executing: {opcode}"
                      printfn " in runCurrentFrame in fold"

                      match opcode with
                      | RETURN ->
                          printfn $"Returning from fold"
                          let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
                          vm.Frames.RemoveAt(vm.Frames.Count - 1)
                          result, vm
                      | _ ->
                          let vm = executeOpcode vm opcode
                          runCurrentFrame vm


              let rec fold acc vm =
                  function
                  | [] -> acc, vm
                  | x :: xs ->
                      printfn $"Folding: {valueToString acc} {valueToString x}"

                      // let vm = push vm f
                      let vm = push vm x
                      let vm = push vm acc
                      // add result back to stack
                      let frame =
                          { Function = f
                            IP = 0
                            StackBase = vm.Stack.Count - 2
                            Locals = Array.zeroCreate f.Locals.Length }

                      vm.Frames.Add(frame)

                      let result, vm = runCurrentFrame vm
                      // let result, vm = pop vm
                      printfn $"Folded: {valueToString result}"

                      fold result vm xs

              let result, vm = fold acc vm l
              push vm result
          | _ ->
              failwith
                  $"""fold expects a list, an initial value, and a function, got: {String.concat ", " (List.map valueToString args)}""")

      Identifier "dotProduct",
      VBuiltin(fun args vm ->
          match args with
          | [ VList l1' as l1; VList l2' as l2 ] when List.length l1' = List.length l2' ->
              let result = dotProduct l1 l2
              push vm result
          | _ -> failwith "dotProduct expects two lists of the same length")
      Identifier "crossProduct",
      VBuiltin(fun args vm ->
          match args with
          | [ VList l1' as l1; VList l2' as l2 ] when List.length l1' = 3 && List.length l2' = 3 ->
              let result = crossProduct l1 l2
              push vm result
          | _ -> failwith "crossProduct expects two lists of length 3")

      Identifier "range",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VInteger start); VNumber(VInteger stop) ] ->
              let range = [ for i in start..stop -> VNumber(VInteger i) ]
              let list = VList range
              push vm list
          | _ -> failwith "Range expects two integers")

      Operator(Plus, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> add a b |> push vm
          | _ -> failwith "Expected two arguments for +")

      Operator(Minus, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> subtract a b |> push vm
          | _ -> failwith "Expected two arguments for -")

      Operator(Star, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> multiply a b |> push vm
          | _ -> failwith "Expected two arguments for *")

      Operator(Slash, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> divide a b |> push vm
          | _ -> failwith "Expected two arguments for /")

      Operator(Percent, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VInteger a); VNumber(VInteger b) ] -> push vm (VNumber(VInteger(a % b)))
          | _ -> failwith "Expected two integers for %")

      Operator(StarStar, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VInteger a); VNumber(VInteger b) ] -> push vm (VNumber(VInteger(pown a b)))
          | _ -> failwith "Expected two integers for **")

      Operator(Equal, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> valuesEqual a b |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for ==")

      Operator(BangEqual, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> valuesEqual a b |> not |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for !=")

      Operator(Less, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b < 0 |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for <")

      Operator(LessEqual, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b <= 0 |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for <=")

      Operator(Greater, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b > 0 |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for >")

      Operator(GreaterEqual, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b >= 0 |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for >=")

      Operator(AmpersandAmpersand, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> (isTruthy a && isTruthy b) |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for &&")

      Operator(PipePipe, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> (isTruthy a || isTruthy b) |> VBoolean |> push vm
          | _ -> failwith "Expected two arguments for ||")

      Operator(Bang, Some Prefix),
      VBuiltin(fun args vm ->
          match args with
          | [ a ] -> not (isTruthy a) |> VBoolean |> push vm
          | _ -> failwith "Expected one argument for !")

      Operator(Minus, Some Prefix),
      VBuiltin(fun args vm ->
          match args with
          | [ a ] -> negate a |> push vm
          | _ -> failwith "Expected one argument for -")

      Operator(Plus, Some Prefix),
      VBuiltin(fun args _ ->
          match args with
          | _ -> failwith "Expected one argument for +")

      Operator(Caret, Some Infix),
      VBuiltin(fun args _ ->
          match args with
          | _ -> failwith "Expected two arguments for ^")

      Operator(DotStar, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> dotProduct a b |> push vm
          | _ -> failwith "Expected two arguments for .*")

      Operator(Cross, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> crossProduct a b |> push vm

          | _ -> failwith "Expected two arguments for X")

      Operator(ColonColon, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; VList l ] -> VList(a :: l) |> push vm
          | _ -> failwith "Expected a value and a list for ::") ]

    |> List.map (fun (key, value) -> lexemeToString key, value)
    |> Map.ofList

and createVM (mainFunc: Function) : VM =
    let constantPool =
        mainFunc.Chunk.ConstantPool
        |> Seq.indexed
        |> Seq.map (fun (i, value) -> $"[{i}] {valueToString value}")

    let disassembly =
        disassembleChunkToString mainFunc.Chunk mainFunc.Name
        |> fun s -> s.Split(Environment.NewLine) |> Seq.ofArray

    let vm =
        { Frames = ResizeArray<CallFrame>()
          Stack = ResizeArray<Value>(256)
          ScopeDepth = 0
          // turn key to string
          Globals = builtins ()
          Streams =
            { ConstantPool = constantPool
              Disassembly = disassembly
              Execution = Seq.empty
              StandardOutput = Seq.empty
              Globals = Seq.empty }
          ExecutionHistory = ResizeArray<VM>() }

    let mainFrame =
        { Function = mainFunc
          IP = 0
          StackBase = 0
          Locals = [||] }

    vm.Frames.Add(mainFrame)
    vm

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
    | GET_LOCAL ->
        let vm, slot = readByte vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot

        if index >= vm.Stack.Count then
            failwith $"GET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"

        let value = vm.Stack[index]
        let vm = push vm value
        vm
    | SET_LOCAL ->
        let vm, slot = readByte vm
        let value, vm = pop vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot

        if index >= vm.Stack.Count then
            failwith $"SET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"

        vm.Stack[index] <- value
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
        push vm (VBoolean(valuesEqual a b))
    | GREATER ->
        let b, vm = pop vm
        let a, vm = pop vm

        match (a, b) with
        | VNumber x, VNumber y -> push vm (VBoolean(x > y))
        | _ -> failwith "Operands must be numbers"
    | LESS ->
        let b, vm = pop vm
        let a, vm = pop vm

        match (a, b) with
        | VNumber x, VNumber y -> push vm (VBoolean(x < y))
        | _ -> failwith "Operands must be numbers"
    | MOD ->
        let b, vm = pop vm
        let a, vm = pop vm

        match (a, b) with
        | VNumber(VInteger x), VNumber(VInteger y) -> push vm (VNumber(VInteger(x % y)))
        | _ -> failwith "Operands must be integers"
    | TRUE -> push vm (VBoolean true)
    | FALSE -> push vm (VBoolean false)
    | NIL -> push vm VNil
    | NOT ->
        let value, vm = pop vm
        push vm (VBoolean(not (isTruthy value)))
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
        | _ -> failwith "Expected string constant for variable name in DEFINE_GLOBAL"
    | GET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            match getGlobal vm name with
            | Some value ->
                printfn $"GET_GLOBAL: {name} = {valueToString value}"
                push vm value
            | None -> failwith $"Undefined variable '{name}'"
        | _ -> failwith "Expected string constant for variable name in GET_GLOBAL"
    | SET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
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
        let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
        vm.Frames.RemoveAt(vm.Frames.Count - 1)

        if vm.Frames.Count = 0 then
            let vm = push vm result
            vm
        else
            let callerFrame = getCurrentFrame vm
            vm.Stack.RemoveRange(callerFrame.StackBase, vm.Stack.Count - callerFrame.StackBase)
            let vm = push vm result
            runLoop vm
    | ASSERT ->
        let msg, vm = pop vm
        let value, vm = pop vm

        if not (isTruthy value) then
            failwithf $"Assertion failed: {valueToString msg}"

        vm
    | CLOSURE ->
        let constant, vm = readConstant vm

        match constant with
        | VFunction func ->
            let upValues =
                func.Locals
                |> Seq.filter (fun local -> local.Depth > 0)
                |> Seq.map (fun local -> vm.Stack[local.Index])
                |> Seq.toList

            let closure = VClosure { Function = func; UpValues = upValues }
            let vm = push vm closure
            vm
        | _ -> failwith "Expected function constant for closure"
    | JUMP ->
        printfn $"Jumping"
        let vm, byte1 = readByte vm
        let vm, byte2 = readByte vm
        let jump = (int byte1 <<< 8) ||| int byte2
        let frame = getCurrentFrame vm
        let frame = { frame with IP = frame.IP + jump }
        vm.Frames[vm.Frames.Count - 1] <- frame
        vm
    | JUMP_IF_FALSE ->
        printfn $"Jumping if false"
        let vm, byte1 = readByte vm
        let vm, byte2 = readByte vm
        let jump = (int byte1 <<< 8) ||| int byte2
        let condition, vm = pop vm

        printfn $"Condition: {valueToString condition}"

        if not (isTruthy condition) then
            printfn $"Jumping"
            let frame = getCurrentFrame vm
            let frame = { frame with IP = frame.IP + jump }
            vm.Frames[vm.Frames.Count - 1] <- frame

        vm

    | COMPOUND_CREATE ->
        let structure, vm = pop vm
        let count, vm = pop vm

        match (structure, count) with
        | VList values, VNumber(VInteger n) when n >= 0 ->
            let values' =
                [ 0 .. n - 1 ] |> List.map (fun _ -> let value, _ = pop vm in value) |> List.rev

            let list = VList <| List.append values values'
            let vm = push vm list
            vm
        | _ -> failwith "Expected non-negative integer for list size"

    | COMPOUND_GET ->
        let key, vm = pop vm
        let structure, vm = pop vm

        match (structure, key) with
        | VList values, VNumber(VInteger i) when i >= 0 && i < List.length values ->
            let value = List.item i values
            let vm = push vm value
            vm
        | VList values, VString key ->
            let value =
                values
                |> Seq.tryFind (fun value ->
                    match value with
                    | VList [ VString k; _ ] when k = key -> true
                    | _ -> false)

            match value with
            | Some(VList [ _; v ]) -> push vm v
            | _ -> push vm VNil
        | _ -> failwith "Invalid index"

    | _ -> failwith $"Unimplemented opcode: {opCodeToString opcode}"

and runLoop vm =
    if vm.Frames.Count = 0 then
        vm
    else
        let frame = getCurrentFrame vm

        if frame.IP >= frame.Function.Chunk.Code.Count then
            if vm.Frames.Count > 1 then
                let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm // Default to Nil if the stack is empty
                vm.Frames.RemoveAt(vm.Frames.Count - 1)
                let callerFrame = getCurrentFrame vm
                vm.Stack.RemoveRange(callerFrame.StackBase, vm.Stack.Count - callerFrame.StackBase)
                let vm = push vm result
                runLoop vm
            else
                // Last frame has finished execution
                vm.Frames.RemoveAt(vm.Frames.Count - 1)
                let vm = if vm.Stack.Count = 0 then push vm VNil else vm
                vm // Do not call runLoop again since there are no frames left
        else
            saveVMState vm
            let vm, instruction = readByte vm

            let vm =
                let opcode = byteToOpCode instruction
                printfn $"Executing: {opcode}"
                executeOpcode vm opcode

            runLoop vm


and run (vm: VM) = runLoop vm


let interpretWithMode (func: Function) (vm: VM option) (isRepl: bool) =
    let vm =
        match vm with
        | Some existingVM ->
            let newFrame =
                { Function = func
                  IP = 0
                  StackBase = existingVM.Stack.Count
                  Locals = [||] }

            existingVM.Frames.Add(newFrame)
            existingVM
        | None ->
            let newVM = createVM func
            appendOutput newVM ConstantPool "=== Constant Pool ==="

    let vm = appendOutput vm Execution "\n=== Program Execution ==="
    let finalVm = run vm
    (finalVm, finalVm.Streams)

let interpret (func: Function) (vm: VM option) = interpretWithMode func vm false

let replExecute (func: Function) (vm: VM option) = interpretWithMode func vm true

let getStreamContent (stream: seq<string>) =
    String.concat Environment.NewLine (Seq.toArray stream)

let resetStreams (vm: VM) =
    { vm with
        Streams = createOutputStreams () }

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
                    let value = vm.Stack[frame.StackBase + int slot]
                    let _ = push vm value
                    vm
                | SET_LOCAL ->
                    let vm, slot = readByte vm
                    let value, vm = pop vm
                    let frame = getCurrentFrame vm
                    vm.Stack[frame.StackBase + int slot] <- value
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
                    push vm (VBoolean(valuesEqual a b))
                | GREATER ->
                    let b, vm = pop vm
                    let a, vm = pop vm

                    match (a, b) with
                    | VNumber x, VNumber y -> push vm (VBoolean(x > y))
                    | _ -> failwith "Operands must be numbers"
                | LESS ->
                    let b, vm = pop vm
                    let a, vm = pop vm

                    match (a, b) with
                    | VNumber x, VNumber y -> push vm (VBoolean(x < y))
                    | _ -> failwith "Operands must be numbers"
                | TRUE -> push vm (VBoolean true)
                | FALSE -> push vm (VBoolean false)
                | NOT ->
                    let value, vm = pop vm
                    push vm (VBoolean(not (isTruthy value)))
                | POP ->
                    let _, vm = pop vm
                    vm
                | CALL ->
                    let vm, argCount = readByte vm
                    callValue vm (int argCount)
                | DEFINE_GLOBAL ->
                    let constant, vm = readConstant vm

                    match constant with
                    | VString name ->
                        let value, vm = pop vm

                        let vm =
                            appendOutput vm Execution $"Defining global variable: {name} = {valueToString value}"

                        defineGlobal vm name value
                    | _ -> failwith "Expected string constant for variable name"
                | GET_GLOBAL ->
                    let constant, vm = readConstant vm

                    match constant with
                    | VString name ->
                        match getGlobal vm name with
                        | Some value -> push vm value
                        | None -> failwith $"Undefined variable '{name}'"
                    | _ -> failwith "Expected string constant for variable name"
                | RETURN ->
                    if vm.Frames.Count > 1 then
                        let returnValue, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
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
