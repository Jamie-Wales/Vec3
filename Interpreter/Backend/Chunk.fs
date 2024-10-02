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
    let index = chunk.ConstantPool.Count
    chunk.ConstantPool.Add(value)
    index

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
    printfn $"%s{name}"
    offset + 1

let private constantInstruction (chunk: Chunk) name offset =
    let constant = int chunk.Code[offset + 1]
    printf $"%16s{name} %4d{constant} '"
    printValue chunk.ConstantPool[constant]
    printfn "'"
    offset + 2

let disassembleInstruction (chunk: Chunk) offset =
    printf $"%04d{offset} "
    if offset > 0 && getLineNumber chunk offset = getLineNumber chunk (offset - 1) then
        printf "   | "
    else
        printf $"%4d{getLineNumber chunk offset} "
    
    match byteToOpCode chunk.Code[offset] with
    | OP_CODE.RETURN -> simpleInstruction "OP_RETURN" offset
    | OP_CODE.CONSTANT -> constantInstruction chunk "OP_CONSTANT" offset
    | OP_CODE.ADD -> simpleInstruction "OP_ADD" offset
    | _ ->
        printfn $"Unknown opcode %d{chunk.Code[offset]}"
        offset + 1

let disassembleChunk (chunk: Chunk) name =
    printfn $"== %s{name} =="
    let rec disassembleRec offset =
        if offset < chunk.Code.Count then
            disassembleInstruction chunk offset
            |> disassembleRec
    disassembleRec 0
