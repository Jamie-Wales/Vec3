module Vec3.Interpreter.Backend.VM

open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Value

type VM = {
    Chunk: Chunk
    IP: int
    Stack: ResizeArray<Value>
}

let createVM chunk =
    { Chunk = chunk
      IP = 0
      (* 
       it may be worth actually making this a static array, so we can
       stack overflow but for now, I like resize array.
       *)
      Stack = ResizeArray<Value>(256) }  

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
    vm.Chunk.ConstantPool[int byte]

let readConstantLong (vm: VM) =
    let vm, byte1 = readByte vm
    let vm, byte2 = readByte vm
    let vm, byte3 = readByte vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)
    vm.Chunk.ConstantPool[index]

let binaryOp (vm: VM) op =
    let b, vm = pop vm
    let a, vm = pop vm
    match (a, b) with
    | VNumber x, VNumber y -> push vm (op (VNumber x) (VNumber y))
    | _ -> failwith "Operands must be numbers"

let rec run (vm: VM) =
    let vm, instruction = readByte vm
    match byteToOpCode instruction with
    | CONSTANT ->
        let constant = readConstant vm
        run (push vm constant)
    | CONSTANT_LONG ->
        let constant = readConstantLong vm
        run (push vm constant)
    | ADD -> run (binaryOp vm add)
    | SUBTRACT -> run (binaryOp vm subtract)
    | MULTIPLY -> run (binaryOp vm multiply)
    | DIVIDE -> run (binaryOp vm divide)
    | NEGATE ->
        let value, vm = pop vm
        match value with
        | VNumber n -> run (push vm (negate (VNumber n)))
        | _ -> failwith "Operand must be a number"
    | RETURN ->
        let result, vm = pop vm
        printfn "Return value: %s" (valueToString result)
        vm  
    | TRUE -> run (push vm (Boolean true))
    | FALSE -> run (push vm (Boolean false))
    | NOT ->
        let value, vm = pop vm
        run (push vm (Boolean (not (isTruthy value))))
    | EQUAL ->
        let b, vm = pop vm
        let a, vm = pop vm
        run (push vm (Boolean (valuesEqual a b)))
    | GREATER ->
        let b, vm = pop vm
        let a, vm = pop vm
        match (a, b) with
        | VNumber x, VNumber y -> run (push vm (Boolean (compare (VNumber x) (VNumber y) > 0)))
        | _ -> failwith "Operands must be numbers"
    | LESS ->
        let b, vm = pop vm
        let a, vm = pop vm
        match (a, b) with
        | VNumber x, VNumber y -> run (push vm (Boolean (compare (VNumber x) (VNumber y) < 0)))
        | _ -> failwith "Operands must be numbers"
    | PRINT ->
        let value, vm = pop vm
        printValue value
        run vm
    | POP ->
        let _, vm = pop vm
        run vm
    | _ -> failwith $"Unimplemented opcode: {instruction}"

let interpret (chunk: Chunk) =
    let vm = createVM chunk
    run vm