module Vec3.Interpreter.Backend.Chunk

open System.Text
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Types



let emptyChunk () =
    { Code = ResizeArray<byte>()
      ConstantPool = ResizeArray<Value>()
      Lines = ResizeArray<LineInfo>() }

let writeChunk (chunk: Chunk) (byte: byte) (line: int) =
    let offset = chunk.Code.Count
    chunk.Code.Add(byte)
    chunk.Lines.Add({ Offset = offset; LineNumber = line })

let addConstant (chunk: Chunk) (value: Value) =
    chunk.ConstantPool.Add(value)
    let index = chunk.ConstantPool.Count
    index - 1

let writeConstant (chunk: Chunk) (value: Value) (line: int) =
    let index = addConstant chunk value

    if index < 256 then
        writeChunk chunk (opCodeToByte OP_CODE.CONSTANT) line
        writeChunk chunk (byte index) line
    else
        writeChunk chunk (opCodeToByte OP_CODE.CONSTANT_LONG) line
        writeChunk chunk (byte (index &&& 0xff)) line
        writeChunk chunk (byte ((index >>> 8) &&& 0xff)) line
        writeChunk chunk (byte ((index >>> 16) &&& 0xff)) line

let getLineNumber (chunk: Chunk) (offset: int) =
    chunk.Lines
    |> Seq.tryFind (fun li -> li.Offset = offset)
    |> Option.map _.LineNumber
    |> Option.defaultValue -1

let private simpleInstruction name offset =
    printfn $"{name}"
    offset + 1

let private constantInstruction (chunk: Chunk) name offset =
    let constant = int chunk.Code[offset + 1]
    printfn $"{name, -16} {constant, 4} '{valueToString chunk.ConstantPool[constant]}'"
    offset + 2

let private constantLongInstruction (chunk: Chunk) name offset =
    let constant =
        (int chunk.Code[offset + 1])
        ||| ((int chunk.Code[offset + 2]) <<< 8)
        ||| ((int chunk.Code[offset + 3]) <<< 16)

    printfn $"{name, -16} {constant, 4} '{valueToString chunk.ConstantPool[constant]}'"
    offset + 4

let disassembleInstruction (chunk: Chunk) offset =
    printf $"{offset:D4} "

    if offset > 0 && getLineNumber chunk offset = getLineNumber chunk (offset - 1) then
        printf "   | "
    else
        printf $"{getLineNumber chunk offset, 4} "

    match byteToOpCode chunk.Code[offset] with
    | OP_CODE.RETURN -> simpleInstruction "OP_RETURN" offset
    | OP_CODE.CONSTANT -> constantInstruction chunk "OP_CONSTANT" offset
    | OP_CODE.CONSTANT_LONG -> constantLongInstruction chunk "OP_CONSTANT_LONG" offset
    | OP_CODE.ADD -> simpleInstruction "OP_ADD" offset
    | OP_CODE.SUBTRACT -> simpleInstruction "OP_SUBTRACT" offset
    | OP_CODE.MULTIPLY -> simpleInstruction "OP_MULTIPLY" offset
    | OP_CODE.DIVIDE -> simpleInstruction "OP_DIVIDE" offset
    | OP_CODE.NEGATE -> simpleInstruction "OP_NEGATE" offset
    | OP_CODE.EQUAL -> simpleInstruction "OP_EQUAL" offset
    | OP_CODE.GREATER -> simpleInstruction "OP_GREATER" offset
    | OP_CODE.LESS -> simpleInstruction "OP_LESS" offset
    | OP_CODE.TRUE -> simpleInstruction "OP_TRUE" offset
    | OP_CODE.FALSE -> simpleInstruction "OP_FALSE" offset
    | OP_CODE.NOT -> simpleInstruction "OP_NOT" offset
    | OP_CODE.PRINT -> simpleInstruction "OP_PRINT" offset
    | OP_CODE.POP -> simpleInstruction "OP_POP" offset
    | OP_CODE.DEFINE_GLOBAL -> constantInstruction chunk "OP_DEFINE_GLOBAL" offset
    | OP_CODE.GET_GLOBAL -> constantInstruction chunk "OP_GET_GLOBAL" offset
    | OP_CODE.CALL -> constantInstruction chunk "OP_CALL" offset
    | OP_CODE.CLOSURE -> constantInstruction chunk "OP_CLOSURE" offset
    | OP_CODE.ASSERT -> simpleInstruction "OP_ASSERT" offset
    | OP_CODE.JUMP -> 
        let jump = (int chunk.Code[offset + 1] <<< 8) ||| int chunk.Code[offset + 2]
        printf $"OP_JUMP          {offset, 4} -> {offset + 3 + jump, 4}"
        offset + 3
    | OP_CODE.JUMP_IF_FALSE ->
        let jump = (int chunk.Code[offset + 1] <<< 8) ||| int chunk.Code[offset + 2]
        printf $"OP_JUMP_IF_FALSE {offset, 4} -> {offset + 3 + jump, 4}"
        offset + 3
    | OP_CODE.DOTPRODUCT -> simpleInstruction "OP_DOTPRODUCT" offset
    | OP_CODE.CROSSPRODUCT -> simpleInstruction "OP_CROSSPRODUCT" offset
    | OP_CODE.LIST_APPEND -> simpleInstruction "OP_LIST_APPEND" offset
    | OP_CODE.LIST_CREATE -> simpleInstruction "OP_LIST_CREATE" offset
    | _ ->
        printfn $"Unknown opcode {chunk.Code[offset]}"
        offset + 1

let disassembleChunk (chunk: Chunk) name =
    printfn $"== %s{name} =="

    let rec disassembleRec offset =
        if offset < chunk.Code.Count then
            disassembleInstruction chunk offset |> disassembleRec

    disassembleRec 0

let disassembleChunkToString (chunk: Chunk) name =
    let sb = StringBuilder()
    let appendLine (text: string) = sb.AppendLine(text) |> ignore

    appendLine $"=== Disassembly of {name} ==="
    appendLine $"Constant Pool Size: {chunk.ConstantPool.Count}"
    appendLine $"Code Size: {chunk.Code.Count} bytes"
    appendLine ""

    let simpleInstruction name offset =
        appendLine $"{offset:D4} | {name}"
        offset + 1

    let constantInstruction chunk name offset =
        let constant = int chunk.Code[offset + 1]
        let value = chunk.ConstantPool[constant]
        appendLine $"{offset:D4} | {name, -16} {constant, 4} | {valueToString value}"
        offset + 2

    let constantLongInstruction chunk name offset =
        let constant =
            (int chunk.Code[offset + 1])
            ||| ((int chunk.Code[offset + 2]) <<< 8)
            ||| ((int chunk.Code[offset + 3]) <<< 16)

        let value = chunk.ConstantPool[constant]
        appendLine $"{offset:D4} | {name, -16} {constant, 4} | {valueToString value}"
        offset + 4

    let byteInstruction name offset =
        let slot = int chunk.Code[offset + 1]
        appendLine $"{offset:D4} | {name, -16} {slot, 4}"
        offset + 2

    let jumpInstruction name sign offset =
        let jump = (int chunk.Code[offset + 1] <<< 8) ||| int chunk.Code[offset + 2]
        let target = offset + 3 + sign * jump
        appendLine $"{offset:D4} | {name, -16} {offset, 4} -> {target, 4}"
        offset + 3

    let rec disassembleInstruction offset =
        let lineInfo =
            chunk.Lines
            |> Seq.tryFind (fun li -> li.Offset = offset)
            |> Option.map (fun li -> $"[Line {li.LineNumber, 4}]")
            |> Option.defaultValue "[No Line]"

        sb.Append($"{offset:D4} {lineInfo} | ") |> ignore

        match byteToOpCode chunk.Code[offset] with
        | OP_CODE.RETURN -> simpleInstruction "OP_RETURN" offset
        | OP_CODE.CONSTANT -> constantInstruction chunk "OP_CONSTANT" offset
        | OP_CODE.CONSTANT_LONG -> constantLongInstruction chunk "OP_CONSTANT_LONG" offset
        | OP_CODE.ADD -> simpleInstruction "OP_ADD" offset
        | OP_CODE.SUBTRACT -> simpleInstruction "OP_SUBTRACT" offset
        | OP_CODE.MULTIPLY -> simpleInstruction "OP_MULTIPLY" offset
        | OP_CODE.DIVIDE -> simpleInstruction "OP_DIVIDE" offset
        | OP_CODE.NEGATE -> simpleInstruction "OP_NEGATE" offset
        | OP_CODE.EQUAL -> simpleInstruction "OP_EQUAL" offset
        | OP_CODE.GREATER -> simpleInstruction "OP_GREATER" offset
        | OP_CODE.LESS -> simpleInstruction "OP_LESS" offset
        | OP_CODE.TRUE -> simpleInstruction "OP_TRUE" offset
        | OP_CODE.FALSE -> simpleInstruction "OP_FALSE" offset
        | OP_CODE.NOT -> simpleInstruction "OP_NOT" offset
        | OP_CODE.PRINT -> simpleInstruction "OP_PRINT" offset
        | OP_CODE.POP -> simpleInstruction "OP_POP" offset
        | OP_CODE.DEFINE_GLOBAL -> constantInstruction chunk "OP_DEFINE_GLOBAL" offset
        | OP_CODE.GET_GLOBAL -> constantInstruction chunk "OP_GET_GLOBAL" offset
        | OP_CODE.SET_GLOBAL -> constantInstruction chunk "OP_SET_GLOBAL" offset
        | OP_CODE.GET_LOCAL -> byteInstruction "OP_GET_LOCAL" offset
        | OP_CODE.SET_LOCAL -> byteInstruction "OP_SET_LOCAL" offset
        | OP_CODE.JUMP -> jumpInstruction "OP_JUMP" 1 offset
        | OP_CODE.JUMP_IF_FALSE -> jumpInstruction "OP_JUMP_IF_FALSE" 1 offset
        | OP_CODE.LOOP -> jumpInstruction "OP_LOOP" -1 offset
        | OP_CODE.CALL -> byteInstruction "OP_CALL" offset
        | OP_CODE.ASSERT -> simpleInstruction "OP_ASSERT" offset
        | OP_CODE.CLOSURE ->
            let constant = int chunk.Code[offset + 1]
            let function' = chunk.ConstantPool[constant]
            appendLine $"{offset:D4} | OP_CLOSURE       {constant, 4} | {valueToString function'}"
            offset + 2
        | OP_CODE.DOTPRODUCT -> simpleInstruction "OP_DOTPRODUCT" offset
        | OP_CODE.CROSSPRODUCT -> simpleInstruction "OP_CROSSPRODUCT" offset
        | OP_CODE.LIST_APPEND -> simpleInstruction "OP_LIST_APPEND" offset
        | OP_CODE.LIST_CREATE -> simpleInstruction "OP_LIST_CREATE" offset
        | _ ->
            appendLine $"{offset:D4} | Unknown opcode {chunk.Code[offset]}"
            offset + 1

    let rec disassembleRec offset =
        if offset < chunk.Code.Count then
            disassembleRec (disassembleInstruction offset)

    disassembleRec 0
    appendLine ""
    appendLine "=== End of Disassembly ==="
    sb.ToString()
