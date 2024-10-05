module Vec3.Interpreter.Backend.VM

open System
open System.Text
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Value
type VM = {
    Chunk: Chunk
    IP: int
    Stack: ResizeArray<Value>
    ScopeDepth: int
    Globals: Map<String, Value>
    Output: StringBuilder
}

let createVM chunk =
    { Chunk = chunk
      IP = 0
      Stack = ResizeArray<Value>(256)
      ScopeDepth = 0
      Globals = Map.empty
      Output = StringBuilder() }

let appendOutput (vm: VM) (str: string) =
    vm.Output.AppendLine(str) |> ignore
    vm
    
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
    let vm = appendOutput vm $"IP: {vm.IP}, Stack: {vm.Stack |> Seq.toList}"
    let vm, instruction = readByte vm
    let vm = appendOutput vm $"Executing instruction: {byteToOpCode instruction}"
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
        printfn "Printing"
        run (appendOutput vm $"Printed value: {valueToString value}")
    | POP ->
        let _, vm = pop vm
        run vm
    | DEFINE_GLOBAL ->
        let vm, nameIndex = readByte vm
        match vm.Chunk.ConstantPool[int nameIndex] with
        | Value.String name ->
            let value, vm = pop vm
            let vm = appendOutput vm $"Defining global variable: {name} = {valueToString value}"
            run { vm with Globals = Map.add name value vm.Globals }
        | _ -> failwith "Expected string constant for variable name"
    | GET_GLOBAL ->
        let vm, nameIndex = readByte vm
        match vm.Chunk.ConstantPool[int nameIndex] with
        | Value.String name ->
            match Map.tryFind name vm.Globals with
            | Some value -> 
                run (push vm value)
            | None -> 
                failwith $"Undefined variable '{name}'"
        | _ -> failwith "Expected string constant for variable name"
    | RETURN ->
        if vm.Stack.Count > 0 then
            let result, vm = pop vm
            let vm = appendOutput vm $"Return value: {valueToString result}"
            let vm = appendOutput vm "\n=== Globals ==="
            let vm = 
                vm.Globals
                |> Map.fold (fun vm name value -> 
                    appendOutput vm $"{name} = {valueToString value}") vm
            vm  // Return the updated VM
        else
            let vm = appendOutput vm "\n=== Globals ==="
            let vm = 
                vm.Globals
                |> Map.fold (fun vm name value -> 
                    appendOutput vm $"{name} = {valueToString value}") vm
            vm  // Return the updated VM
    | _ -> failwith $"Unimplemented opcode: {instruction}"

let interpret (chunk: Chunk) =
    let vm = createVM chunk
    let vm = appendOutput vm "=== Constant Pool ==="
    let vm = 
        chunk.ConstantPool 
        |> Seq.indexed 
        |> Seq.fold (fun vm (i, value) -> 
            appendOutput vm $"[{i}] {valueToString value}") vm
    
    let vm = appendOutput vm "\n=== Disassembled Chunk ==="
    let disassembledChunk = disassembleChunkToString chunk "program"
    let vm = appendOutput vm disassembledChunk
    
    let vm = appendOutput vm "\n=== Program Execution ==="
    let finalVm = run vm
    finalVm.Output.ToString()
