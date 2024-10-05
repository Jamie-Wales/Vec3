module Vec3.Interpreter.Backend.VM

open System
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Value

type VM = {
    Chunk: Chunk
    IP: int
    Stack: ResizeArray<Value>
    ScopeDepth: int
    Globals:Map<String, Value>
}

let createVM chunk =
    { Chunk = chunk
      IP = 0
      Stack = ResizeArray<Value>(256)
      ScopeDepth = 0
      Globals = Map.empty
      }
    
let push (vm: VM) (value: Value) =
    vm.Stack.Add(value)
    vm

let pop (vm: VM) =
    let value = vm.Stack[vm.Stack.Count - 1]
    vm.Stack.RemoveAt(vm.Stack.Count - 1)
    (value, vm)

let peek (vm: VM) offset =
    vm.Stack[vm.Stack.Count - 1 - offset]

let printGlobals (vm: VM) =
    printfn "\n=== Globals ==="
    for KeyValue(name, value) in vm.Globals do
        printfn $"{name} = {valueToString value}"
let readByte (vm: VM) =
    let byte = vm.Chunk.Code[vm.IP]
    ({ vm with IP = vm.IP + 1 }, byte)


let readConstant (vm: VM) =
    let vm, byte = readByte vm
    printfn $"Reading constant at index {int byte}"
    if int byte >= vm.Chunk.ConstantPool.Count then
        failwithf "Constant index out of range: %d (pool size: %d)" (int byte) vm.Chunk.ConstantPool.Count
    let constant = vm.Chunk.ConstantPool[int byte]
    printfn $"Constant value: {constant}"
    (constant, vm)

let readConstantLong (vm: VM) =
    let vm, byte1 = readByte vm
    let vm, byte2 = readByte vm
    let vm, byte3 = readByte vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)
    printfn $"Reading long constant at index {index}"
    if index >= vm.Chunk.ConstantPool.Count then
        failwithf "Long constant index out of range: %d (pool size: %d)" index vm.Chunk.ConstantPool.Count
    let constant = vm.Chunk.ConstantPool[index]
    printfn $"Long constant value: {constant}"
    (constant, vm)
let defineGlobal (vm: VM) (name: string) (value: Value) =
    { vm with Globals = Map.add name value vm.Globals }

let getGlobal (vm: VM) (name: string) =
    Map.tryFind name vm.Globals
let binaryOp (vm: VM) (op: Value -> Value -> Value) =
    printfn $"Performing binary operation. Stack size: {vm.Stack.Count}"
    let b, vm = pop vm
    let a, vm = pop vm
    printfn $"Operands: a = {a}, b = {b}"
    let result = op a b
    printfn $"Result: {result}"
    push vm result

let rec run (vm: VM) =
    printfn $"IP: {vm.IP}, Stack: {vm.Stack |> Seq.toList}"
    let vm, instruction = readByte vm
    printfn $"Executing instruction: {byteToOpCode instruction}"
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
        printValue value
        run vm
    | POP ->
        let _, vm = pop vm
        run vm
    | DEFINE_GLOBAL ->
        let vm, nameIndex = readByte vm
        match vm.Chunk.ConstantPool[int nameIndex] with
        | Value.String name ->
            let value, vm = pop vm
            printfn $"Defining global variable: {name} = {value}"
            run (defineGlobal vm name value)
         | _ -> failwith "Expected string constant for variable name"

    | GET_GLOBAL ->
        let vm, nameIndex = readByte vm
        match vm.Chunk.ConstantPool[int nameIndex] with
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
            printfn "Return value: %A" result
            printGlobals vm
            vm 
           else
            printGlobals vm
            vm
        | _ -> failwith $"Unimplemented opcode: {instruction}"

let interpret (chunk: Chunk) =
    printfn "=== Constant Pool ==="
    for i, value in chunk.ConstantPool |> Seq.indexed do
        printfn $"[{i}] {value}"
    printfn "\n=== Disassembled Chunk ==="
    disassembleChunk chunk "program"
    printfn "\n=== Program Execution ==="
    let vm = createVM chunk
    run vm
