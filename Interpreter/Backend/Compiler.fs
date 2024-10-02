module Vec3.Interpreter.Backend.Compiler

open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

type CompilerState =
    { Chunk: Chunk
      Locals: Map<string, int>
      ScopeDepth: int
      CurrentLine: int
    }

type CompilerError = string * CompilerState
type CompilerResult<'a> = Result<'a * CompilerState, CompilerError>
type Compiler<'a> = CompilerState -> CompilerResult<'a>

let emitByte (byte: byte) (state: CompilerState) : CompilerResult<unit> =
   let () = writeChunk state.Chunk byte state.CurrentLine in 
   Ok ((), state)

let emitBytes (bytes: byte seq) (state: CompilerState) : CompilerResult<unit> =
   let () = Seq.iter (fun byte -> writeChunk state.Chunk byte state.CurrentLine) bytes in
   Ok ((), state)
   
let emitConstant (value: Value) (state: CompilerState) : CompilerResult<unit> =
   let () = writeConstant state.Chunk value state.CurrentLine in
   Ok ((), state)

let emitOpCode (opCode: OP_CODE) (state: CompilerState) : CompilerResult<unit> =
   emitByte (opCodeToByte opCode) state

              
let rec compileExpr (expr: Expr) : Compiler<unit> =
    fun state ->
        match expr with
        | Literal lit -> compileLiteral lit state
        | _ -> Error ("Unsupported expression type", state)
              
and compileLiteral (lit: Literal) : Compiler<unit> =
    let compileNumber (n: TNumber) state =
        match n with
        | TNumber.Integer i -> emitConstant (VNumber(VInteger i)) state
        | TNumber.Float f -> emitConstant (VNumber(VFloat f)) state
    
    fun state ->
        match lit with
        | TNumber n -> compileNumber n state
        | Literal.String s -> emitConstant (Value.String s) state
        | Bool b ->
            if b then emitOpCode OP_CODE.TRUE state
            else emitOpCode OP_CODE.FALSE state
        | Unit -> emitConstant Value.Nil state  // Assuming Value.Nil represents Unit
