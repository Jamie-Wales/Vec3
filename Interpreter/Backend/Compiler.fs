module Vec3.Interpreter.Backend.Compiler

open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token




type CompilerState ={
    currentFunction : Chunk.Function
    CurrentLine: int
    ScopeDepth: int
}

type CompilerError = string * CompilerState
type CompilerResult<'a> = Result<'a * CompilerState, CompilerError>
type Compiler<'a> = CompilerState -> CompilerResult<'a>

let emitByte (byte: byte) (state: CompilerState) : CompilerResult<unit> =
   let () = writeChunk state.currentFunction.Chunk byte state.CurrentLine in 
   Ok ((), state)

let emitBytes (bytes: byte seq) (state: CompilerState) : CompilerResult<unit> =
   Seq.iter (fun byte -> writeChunk state.currentFunction.Chunk byte state.CurrentLine) bytes
   Ok ((), state)
   
let emitConstant (value: Value) (state: CompilerState) : CompilerResult<unit> =
   let () = writeConstant state.currentFunction.Chunk value state.CurrentLine in
   Ok ((), state)

let emitOpCode (opCode: OP_CODE) (state: CompilerState) : CompilerResult<unit> =
   emitByte (opCodeToByte opCode) state

let rec compileLiteral (lit: Literal) : Compiler<unit> =
    let compileNumber (n: Vec3.Interpreter.Grammar.Number) state =
        match n with
        | LInteger i -> emitConstant (VNumber(VInteger i)) state
        | LFloat f -> emitConstant (VNumber(VFloat f)) state
        | LRational (n, d) -> emitConstant (VNumber(VRational(n, d))) state
        | LComplex (r, i) -> emitConstant (VNumber(VComplex(r, i))) state
    fun state ->
        match lit with
        | LNumber n -> compileNumber n state
        | LString s -> emitConstant (Value.String s) state
        | LBool b ->
            if b then emitOpCode OP_CODE.TRUE state
            else emitOpCode OP_CODE.FALSE state
        | LUnit -> emitConstant Value.Nil state

let rec compileExpr (expr: Expr) : Compiler<unit> =
    fun state ->
        match expr with
        | ELiteral (lit, _) -> compileLiteral lit state
        | EBinary (left, op, right, _) -> compileBinary left op right state
        | EIdentifier (i, _)-> compileIdentifier i state
        | EGrouping (e, _ ) -> compileGrouping e state
        | EUnary (token, u, _ ) -> compileUnary token u state
        | ELambda (token, l, _) -> compileLambda token l state
        | _ -> Error ("Unsupported expression type", state)
        
and compileLambda (op: Token list) (expr: Expr) : Compiler<unit> =
    fun state ->
        compileExpr expr state 
        
and compileUnary (op: Token) (expr: Expr) : Compiler<unit> =
    fun state ->
        let _ = compileExpr expr state 
        let emitBinaryOp opCode state =
            emitOpCode opCode state
            
        match op.Lexeme with
        | Operator Bang -> emitBinaryOp OP_CODE.NOT state
        | Operator Minus -> emitBinaryOp OP_CODE.NEGATE state
        | _ -> Error ($"Unsupported binary operator: {op.Lexeme}", state)
        
    
and compileBinary (left: Expr) (op: Token) (right: Expr) : Compiler<unit> =
    fun state ->
        let compileOperands state =
            compileExpr left state
            |> Result.bind (fun ((), state) -> compileExpr right state)
        
        let emitBinaryOp opCode state =
            compileOperands state
            |> Result.bind (fun ((), state) -> emitOpCode opCode state)
        
        match op.Lexeme with
        | Operator Plus -> emitBinaryOp OP_CODE.ADD state
        | Operator Minus -> emitBinaryOp OP_CODE.SUBTRACT state
        | Operator Star -> emitBinaryOp OP_CODE.MULTIPLY state
        | Operator Slash -> emitBinaryOp OP_CODE.DIVIDE state
        | Operator EqualEqual -> emitBinaryOp OP_CODE.EQUAL state
        | Operator BangEqual ->
            emitBinaryOp OP_CODE.EQUAL state
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.NOT state)
        | Operator Greater -> emitBinaryOp OP_CODE.GREATER state
        | Operator GreaterEqual ->
            emitBinaryOp OP_CODE.LESS state
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.NOT state)
        | Operator Less -> emitBinaryOp OP_CODE.LESS state
        | Operator LessEqual ->
            emitBinaryOp OP_CODE.GREATER state
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.NOT state)
        | _ -> Error ($"Unsupported binary operator: {op.Lexeme}", state)

and compileGrouping grouping : Compiler<unit> =
    fun state ->
        compileExpr grouping state
        
and compileIdentifier (token: Token) : Compiler<unit> =
    fun state ->
        if state.currentFunction.Locals.ContainsKey token.Lexeme then
            let index = state.currentFunction.Locals[token.Lexeme]
            emitBytes [| byte (opCodeToByte OP_CODE.GET_LOCAL); byte index |] state
        else
            let constIndex = addConstant state.currentFunction.Chunk (Value.String (lexemeToString token.Lexeme))
            emitBytes [| byte (opCodeToByte OP_CODE.GET_GLOBAL); byte constIndex |] state
    
let rec compileStmt (stmt: Stmt) : Compiler<unit> =
    fun state ->
        match stmt with
        | SExpression (expr, _) ->
            compileExpr expr state
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.POP state)
        | SVariableDeclaration (name, initializer, _) ->
            compileVariableDeclaration name initializer state
        | SPrintStatement (expr, _) ->
            compileExpr expr state
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.PRINT state)

and compileVariableDeclaration (name: Token) (initializer: Expr) : Compiler<unit> =
    fun state ->
        compileExpr initializer state
        |> Result.bind (fun ((), state) ->
            if state.ScopeDepth > 0 then
                let locals = Map.add name.Lexeme state.currentFunction.Locals.Count state.currentFunction.Locals
                Ok ((), { state with currentFunction.Locals = locals })
            else
                let constIndex = addConstant state.currentFunction.Chunk (Value.String (lexemeToString name.Lexeme))
                emitBytes [| byte (opCodeToByte OP_CODE.DEFINE_GLOBAL); byte constIndex |] state)

    
let compileProgramState (program: Program) (state:CompilerState): CompilerResult<Chunk> =
    let rec compileStmts stmts state =
        match stmts with
        | [] -> Ok ((), state)
        | stmt::rest ->
            compileStmt stmt state
            |> Result.bind (fun ((), state) -> compileStmts rest state)
    
    compileStmts program state 
    |> Result.bind (fun ((), state) -> 
        emitOpCode OP_CODE.RETURN state
        |> Result.map (fun ((), state) -> (state.currentFunction.Chunk, state)))
   
let initFunction (name:string) =
   {
       Chunk =emptyChunk()
       Locals = Map.empty
       Name = name
    }
   
let compileProgram (program: Program) : CompilerResult<Chunk.Function> =
    let initialState =
        {
          currentFunction = initFunction("REPL_Input")
          ScopeDepth = 0
          CurrentLine = 1 
        }
    
    let rec compileStmts stmts state =
        match stmts with
        | [] -> Ok ((), state)
        | stmt::rest ->
            compileStmt stmt state
            |> Result.bind (fun ((), state) -> compileStmts rest state)
    
    compileStmts program initialState
    |> Result.bind (fun ((), state) -> 
        emitOpCode OP_CODE.RETURN state
        |> Result.map (fun ((), state) -> (state.currentFunction, state)))
