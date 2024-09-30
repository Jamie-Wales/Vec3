module Vec3.Interpreter.Backend.Chunk

open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Value

type LineInfo = { offset: int; lineNumber: int }

type Chunk =
    { code: byte list
      constant_pool: Value list
      lines: LineInfo list }

let emptyChunk =
    { code = []
      constant_pool = []
      lines = [] }

let writeChunk (chunk: Chunk) (byte: byte) (line: int) =
    let newOffset = List.length chunk.code

    { chunk with
        code = chunk.code @ [ byte ]
        lines =
            chunk.lines
            @ [ { offset = newOffset
                  lineNumber = line } ] }

let addConstant (chunk: Chunk) (value: Value) =
    let index = List.length chunk.constant_pool

    let newChunk =
        { chunk with
            constant_pool = chunk.constant_pool @ [ value ] }

    (newChunk, index)

let writeConstant (chunk: Chunk) (value: Value) (line: int) =
    let chunkWithConstant, index = addConstant chunk value

    if index < 256 then
        chunkWithConstant
        |> fun c -> writeChunk c (opCodeToByte OP_CODE.CONSTANT) line
        |> fun c -> writeChunk c (byte index) line
    else
        chunkWithConstant
        |> fun c -> writeChunk c (opCodeToByte OP_CODE.CONSTANT_LONG) line
        |> fun c -> writeChunk c (byte (index &&& 0xff)) line
        |> fun c -> writeChunk c (byte ((index >>> 8) &&& 0xff)) line
        |> fun c -> writeChunk c (byte ((index >>> 16) &&& 0xff)) line

let getLineNumber (chunk: Chunk) (offset: int) =
    chunk.lines
    |> List.tryFind (fun li -> li.offset = offset)
    |> Option.map (fun li -> li.lineNumber)
    |> Option.defaultValue -1

let simpleInstruction name offset =
    printfn $"%s{name}"
    offset + 1

let constantInstruction (chunk: Chunk) name offset =
    let constant = int chunk.code[offset + 1]
    printf $"%16s{name} %4d{constant} '"
    printValue chunk.constant_pool[constant]
    printfn "'"
    offset + 2

let disassembleInstruction (chunk: Chunk) offset =
    printf $"%04d{offset} "

    if offset > 0 && getLineNumber chunk offset = getLineNumber chunk (offset - 1) then
        printf "   | "
    else
        printf $"%4d{getLineNumber chunk offset} "

    let instruction = chunk.code[offset]

    match byteToOpCode instruction with
    | OP_CODE.RETURN -> simpleInstruction "OP_RETURN" offset
    | OP_CODE.CONSTANT -> constantInstruction chunk "OP_CONSTANT" offset
    | OP_CODE.ADD -> simpleInstruction "OP_ADD" offset
    | _ ->
        printfn $"Unknown opcode %d{instruction}"
        offset + 1

let disassembleChunk (chunk: Chunk) name =
    printfn $"== %s{name} =="

    let rec disassembleRec offset =
        if offset < List.length chunk.code then
            let newOffset = disassembleInstruction chunk offset
            disassembleRec newOffset

    disassembleRec 0
