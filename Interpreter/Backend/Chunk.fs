module Vec3.Interpreter.Backend.Chunk

open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Value

type LineInfo = { Offset: int; LineNumber: int }

type Chunk =
    { Code: ResizeArray<byte>
      ConstantPool: ResizeArray<Value>
      Lines: ResizeArray<LineInfo> }

let emptyChunk() =
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
    printfn $"{name,-16} {constant,4} '{valueToString chunk.ConstantPool[constant]}'"
    offset + 2

let private constantLongInstruction (chunk: Chunk) name offset =
    let constant = 
        (int chunk.Code[offset + 1]) ||| 
        ((int chunk.Code[offset + 2]) <<< 8) ||| 
        ((int chunk.Code[offset + 3]) <<< 16)
    printfn $"{name,-16} {constant,4} '{valueToString chunk.ConstantPool[constant]}'"
    offset + 4

let disassembleInstruction (chunk: Chunk) offset =
    printf $"{offset:D4} "
    if offset > 0 && getLineNumber chunk offset = getLineNumber chunk (offset - 1) then
        printf "   | "
    else
        printf $"{getLineNumber chunk offset,4} "
    
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
    | _ ->
        printfn $"Unknown opcode {chunk.Code[offset]}"
        offset + 1

let disassembleChunk (chunk: Chunk) name =
    printfn $"== %s{name} =="
    let rec disassembleRec offset =
        if offset < chunk.Code.Count then
            disassembleInstruction chunk offset
            |> disassembleRec
    disassembleRec 0
