module Vec3.Interpreter.Backend.Compiler

open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

type CompilerState = {
    CurrentFunction: Function
    CurrentLine: int
    ScopeDepth: int
}

type CompilerError = string * CompilerState
type CompilerResult<'a> = Result<'a * CompilerState, CompilerError>
type Compiler<'a> = CompilerState -> CompilerResult<'a>

let emitByte (byte: byte) (state: CompilerState) : CompilerResult<unit> =
    writeChunk state.CurrentFunction.Chunk byte state.CurrentLine
    Ok ((), state)

let emitBytes (bytes: byte seq) (state: CompilerState) : CompilerResult<unit> =
    Seq.iter (fun byte -> writeChunk state.CurrentFunction.Chunk byte state.CurrentLine) bytes
    Ok ((), state)

let emitConstant (value: Value) (state: CompilerState) : CompilerResult<unit> =
    writeConstant state.CurrentFunction.Chunk value state.CurrentLine |> ignore
    Ok ((), state)

let emitOpCode (opCode: OP_CODE) (state: CompilerState) : CompilerResult<unit> =
    emitByte (opCodeToByte opCode) state

let initFunction (name: string) =
    { 
        Arity = 0
        Chunk = emptyChunk()
        Name = name
    }

let addLocal (name: string) (state: CompilerState) : CompilerState =
    let newConstantPool = ResizeArray(state.CurrentFunction.Chunk.ConstantPool)
    newConstantPool.Add(Value.String name)
    let newChunk = { state.CurrentFunction.Chunk with ConstantPool = newConstantPool }
    { state with CurrentFunction = { state.CurrentFunction with Chunk = newChunk } }

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
        | EIdentifier (i, _) -> compileIdentifier i state
        | EGrouping (e, _) -> compileGrouping e state
        | EUnary (token, u, _) -> compileUnary token u state
        | ELambda (parameters, body, _) -> compileLambda parameters body state
        | _ -> Error ("Unsupported expression type", state)

and compileLambda (parameters: Token list) (body: Expr) : Compiler<unit> =
    fun state ->
        let functionName = $"lambda_{state.CurrentFunction.Name}"
        let lambdaFunction = initFunction functionName
        
        let lambdaState = 
            { state with 
                CurrentFunction = lambdaFunction
                ScopeDepth = 0 }
        
        let compiledParamsState = 
            parameters 
            |> List.fold (fun state param -> 
                let newState = addLocal (lexemeToString param.Lexeme) state
                { newState with 
                    CurrentFunction = { newState.CurrentFunction with Arity = newState.CurrentFunction.Arity + 1 } }
            ) lambdaState
        
        let bodyResult = compileExpr body compiledParamsState
        
        match bodyResult with
        | Ok ((), finalLambdaState) ->
            match emitOpCode OP_CODE.RETURN finalLambdaState with
            | Ok ((), finalState) ->
                let constIndex = addConstant state.CurrentFunction.Chunk (Value.Function finalState.CurrentFunction)
                emitBytes [| byte (opCodeToByte OP_CODE.CONSTANT); byte constIndex |] state
            | Error e -> Error e
        | Error e -> Error e

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

and compileUnary (op: Token) (expr: Expr) : Compiler<unit> =
    fun state ->
        let _ = compileExpr expr state 
        let emitUnaryOp opCode state =
            emitOpCode opCode state
            
        match op.Lexeme with
        | Operator Bang -> emitUnaryOp OP_CODE.NOT state
        | Operator Minus -> emitUnaryOp OP_CODE.NEGATE state
        | _ -> Error ($"Unsupported unary operator: {op.Lexeme}", state)

and compileGrouping grouping : Compiler<unit> =
    fun state ->
        compileExpr grouping state
        
and compileIdentifier (token: Token) : Compiler<unit> =
    fun state ->
        let localIndex = 
            state.CurrentFunction.Chunk.ConstantPool
            |> Seq.tryFindIndex (function 
                | Value.String s when s = lexemeToString token.Lexeme -> true 
                | _ -> false)
        
        match localIndex with
        | Some index ->
            emitBytes [| byte (opCodeToByte OP_CODE.GET_LOCAL); byte index |] state
        | None ->
            let constIndex = addConstant state.CurrentFunction.Chunk (Value.String (lexemeToString token.Lexeme))
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
                let newState = addLocal (lexemeToString name.Lexeme) state
                Ok ((), newState)
            else
                let constIndex = addConstant state.CurrentFunction.Chunk (Value.String (lexemeToString name.Lexeme))
                emitBytes [| byte (opCodeToByte OP_CODE.DEFINE_GLOBAL); byte constIndex |] state)

let compileProgramState (program: Program) (state: CompilerState): CompilerResult<Chunk> =
    let rec compileStmts stmts state =
        match stmts with
        | [] -> Ok ((), state)
        | stmt::rest ->
            compileStmt stmt state
            |> Result.bind (fun ((), state) -> compileStmts rest state)
    
    compileStmts program state 
    |> Result.bind (fun ((), state) -> 
        emitOpCode OP_CODE.RETURN state
        |> Result.map (fun ((), state) -> (state.CurrentFunction.Chunk, state)))


let compileProgram (program: Program) : CompilerResult<Function> =
    let initialState =
        {
            CurrentFunction = initFunction "REPL_Input"
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
        |> Result.map (fun ((), state) -> (state.CurrentFunction, state)))
