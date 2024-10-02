module Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Value

type VM = {
    chunk: Chunk
    ip: int
    stack: Value array
    stackTop: int
}

let createVM chunk =
    { chunk = chunk
      ip = 0
      stack = Array.zeroCreate 256  // Adjust size as needed
      stackTop = 0 }

let push (vm: VM) (value: Value) =
    vm.stack.[vm.stackTop] <- value
    { vm with stackTop = vm.stackTop + 1 }

let pop (vm: VM) =
    let newStackTop = vm.stackTop - 1
    (vm.stack.[newStackTop], { vm with stackTop = newStackTop })

let peek (vm: VM) offset =
    vm.stack.[vm.stackTop - 1 - offset]

let readByte (vm: VM) =
    let byte = vm.chunk.code.[vm.ip]
    ({ vm with ip = vm.ip + 1 }, byte)

let readConstant (vm: VM) =
    let (vm, byte) = readByte vm
    vm.chunk.constant_pool.[int byte]

let readConstantLong (vm: VM) =
    let (vm, byte1) = readByte vm
    let (vm, byte2) = readByte vm
    let (vm, byte3) = readByte vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)
    vm.chunk.constant_pool.[index]

let binaryOp (vm: VM) op =
    let (b, vm) = pop vm
    let (a, vm) = pop vm
    match (a, b) with
    | (Number x, Number y) -> push vm (Number (op x y))
    | _ -> failwith "Operands must be numbers"

let rec run (vm: VM) =
    let (vm, instruction) = readByte vm
    match byteToOpCode instruction with
    | CONSTANT ->
        let constant = readConstant vm
        run (push vm constant)
    | CONSTANT_LONG ->
        let constant = readConstantLong vm
        run (push vm constant)
    | ADD -> run (binaryOp vm (+))
    | SUBTRACT -> run (binaryOp vm (-))
    | MULTIPLY -> run (binaryOp vm (*))
    | DIVIDE -> run (binaryOp vm (/))
    | NEGATE ->
        let (value, vm) = pop vm
        match value with
        | Number n -> run (push vm (Number -n))
        | _ -> failwith "Operand must be a number"
    | RETURN ->
        let (result, vm) = pop vm
        printfn $"Return value: %A{result}"
        vm  
    | TRUE -> run (push vm (Boolean true))
    | FALSE -> run (push vm (Boolean false))
    | NOT ->
        let (value, vm) = pop vm
        run (push vm (Boolean (not (isTruthy value))))
    | EQUAL ->
        let (b, vm) = pop vm
        let (a, vm) = pop vm
        run (push vm (Boolean (a = b)))
    | GREATER ->
        let (b, vm) = pop vm
        let (a, vm) = pop vm
        match (a, b) with
        | (Number x, Number y) -> run (push vm (Boolean (x > y)))
        | _ -> failwith "Operands must be numbers"
    | LESS ->
        let (b, vm) = pop vm
        let (a, vm) = pop vm
        match (a, b) with
        | (Number x, Number y) -> run (push vm (Boolean (x < y)))
        | _ -> failwith "Operands must be numbers"
    | PRINT ->
        let (value, vm) = pop vm
        printfn "%A" value
        run vm
    | POP ->
        let (_, vm) = pop vm
        run vm
    | _ -> failwith (sprintf "Unimplemented opcode: %A" instruction)

and isTruthy = function
    | Boolean false -> false
    | NilValue -> false
    | _ -> true

let interpret (chunk: Chunk) =
    let vm = createVM chunk
    run vm