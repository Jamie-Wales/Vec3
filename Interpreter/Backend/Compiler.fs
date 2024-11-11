module Vec3.Interpreter.Backend.Compiler

open Microsoft.FSharp.Core
open System
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.SymbolicExpression
open Vec3.Interpreter.Token

let identMap: Map<Lexeme, Value> ref = ref Map.empty

type CompilerState =
    { CurrentFunction: Function
      CurrentLine: int
      ScopeDepth: int
      LocalCount: int }

type CompilerError = string * CompilerState
type CompilerResult<'a> = Result<'a * CompilerState, CompilerError>
type Compiler<'a> = CompilerState -> CompilerResult<'a>

let emitByte (byte: byte) (state: CompilerState) : CompilerResult<unit> =
    writeChunk state.CurrentFunction.Chunk byte state.CurrentLine
    Ok((), state)

let emitBytes (bytes: byte seq) (state: CompilerState) : CompilerResult<unit> =
    Seq.iter (fun byte -> writeChunk state.CurrentFunction.Chunk byte state.CurrentLine) bytes
    Ok((), state)

let emitConstant (value: Value) (state: CompilerState) : CompilerResult<unit> =
    writeConstant state.CurrentFunction.Chunk value state.CurrentLine
    Ok((), state)

let emitOpCode (opCode: OP_CODE) (state: CompilerState) : CompilerResult<unit> = emitByte (opCodeToByte opCode) state

let emitJump (opCode: OP_CODE) (state: CompilerState) : int =
    emitOpCode opCode state
    |> Result.map (fun _ -> state.CurrentFunction.Chunk.Code.Count - 1)
    |> Result.defaultValue 0

let emitJumpBack (offset: int) (state: CompilerState) : CompilerResult<unit> =
    let jump = state.CurrentFunction.Chunk.Code.Count - offset - 1
    let bytes = [| byte (jump &&& 0xff); byte ((jump >>> 8) &&& 0xff) |]
    emitBytes bytes state

let initFunction (name: string) =
    { Arity = 0
      Chunk = emptyChunk ()
      Name = name
      Locals = [] }

let addLocal (name: string) (state: CompilerState) : CompilerState =
    let local =
        { Name = name
          Depth = state.ScopeDepth
          Index = state.LocalCount }

    let updatedFunction =
        { state.CurrentFunction with
            Locals = local :: state.CurrentFunction.Locals }

    { state with
        CurrentFunction = updatedFunction
        LocalCount = state.LocalCount + 1 }

let rec compileLiteral (lit: Literal) : Compiler<unit> =
    let compileNumber (n: Vec3.Interpreter.Grammar.Number) state =
        match n with
        | LInteger i -> emitConstant (VNumber(VInteger i)) state
        | LFloat f -> emitConstant (VNumber(VFloat f)) state
        | LRational(n, d) -> emitConstant (VNumber(VRational(n, d))) state
        | LComplex(r, i) -> emitConstant (VNumber(VComplex(r, i))) state

    fun state ->
        match lit with
        | LNumber n -> compileNumber n state
        | LString s -> emitConstant (VString s) state
        | LBool b ->
            if b then
                emitOpCode OP_CODE.TRUE state
            else
                emitOpCode OP_CODE.FALSE state
        | LUnit -> emitConstant VNil state

let compileCodeBlock (expr: Expr) state : CompilerResult<unit> = emitConstant (VBlock(expr)) state

let rec compileExpr (expr: Expr) : Compiler<unit> =
    fun state ->
        match expr with
        | ELiteral(lit, _) -> compileLiteral lit state
        | EIdentifier(i, _) -> compileIdentifier i state
        | EGrouping(e, _) -> compileGrouping e state
        | ELambda(parameters, body, _, pr, _) ->
            let parameters = List.map fst parameters
            compileLambda parameters body pr state
        | ECall(callee, arguments, _) -> compileCall callee arguments false state 
        | EList(elements, _) -> compileList elements state
        | EIndex(list, index, _) -> compileIndex list index state
        | ETuple(elements, _) -> compileTuple elements state
        | ECodeBlock(expr) -> compileCodeBlock expr state
        | ERange(start, stop, _) ->
            let expression =
                ECall(
                    EIdentifier(
                        { Lexeme = Identifier "range"
                          Position = { Line = 0; Column = 0 } },
                        None
                    ),
                    [ start; stop ],
                    None
                )

            compileExpr expression state

        | ERecordEmpty _ ->
            emitConstant (VNumber(VInteger 0)) state
            |> Result.bind (fun ((), state) -> emitConstant (VList([], RECORD)) state)
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_CREATE state)

        | ERecordExtend((name, value, _), record, _) ->
            let name =
                match name with
                | { Lexeme = Identifier n } -> n
                | _ -> raise <| System.Exception("Invalid record field name")

            // pretty horrbile, but it works
            // record is a list of lists, where each list is a pair of a string and a value
            // might be better to have specific value for pair, but then the compound create would need to be changed
            // or extra instruction for create pair, otherwise any two eleemnt list would be a pair

            let constIndex = addConstant state.CurrentFunction.Chunk (VString name)

            emitBytes [| byte (opCodeToByte OP_CODE.CONSTANT); byte constIndex |] state
            |> Result.bind (fun ((), state) -> compileExpr value state)
            |> Result.bind (fun ((), state) -> emitConstant (VNumber(VInteger 2)) state)
            |> Result.bind (fun ((), state) -> emitConstant (VList([], RECORD)) state)
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_CREATE state)
            |> Result.bind (fun ((), state) -> emitConstant (VNumber(VInteger 1)) state)
            |> Result.bind (fun ((), state) -> compileExpr record state)
            |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_CREATE state)


        | ERecordSelect(expr, token, _) ->
            let name =
                match token with
                | { Lexeme = Identifier n } -> n
                | _ -> raise <| System.Exception("Invalid record field name")

            // same as index, but with a string and a record (push string)
            compileExpr expr state
            |> Result.bind (fun ((), state) ->
                let constIndex = addConstant state.CurrentFunction.Chunk (VString name)

                emitBytes [| byte (opCodeToByte OP_CODE.CONSTANT); byte constIndex |] state
                |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_GET state))

        | EBlock(stmts, _) -> compileBlock stmts state // scope is fucked up think its global
        | EIf(condition, thenBranch, elseBranch, _) -> compileIf condition thenBranch elseBranch state
        | ETernary(cond, thenB, elseB, _) -> compileIf cond thenB elseB state
        | ETail(ex, _) ->
            match ex with
            | ECall(name, args, _) -> compileCall name args true state
            | e -> compileExpr e state
                
and compileIndex (list: Expr) (index: Expr) : Compiler<unit> =
    fun state ->
        compileExpr list state
        |> Result.bind (fun ((), state) -> compileExpr index state)
        |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_GET state)

and compileTuple (elements: Expr list) : Compiler<unit> =
    fun state ->
        let rec compileElements elements state =
            match elements with
            | [] -> Ok((), state)
            | element :: rest ->
                compileExpr element state
                |> Result.bind (fun ((), state) -> compileElements rest state)

        // same as list
        compileElements elements state
        |> Result.bind (fun ((), state) -> emitConstant (VNumber(VInteger elements.Length)) state)
        |> Result.bind (fun ((), state) -> emitConstant (VList([], TUPLE)) state)
        |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_CREATE state)

and compileList (elements: Expr list) : Compiler<unit> =
    fun state ->
        let rec compileElements elements state =
            match elements with
            | [] -> Ok((), state)
            | element :: rest ->
                compileExpr element state
                |> Result.bind (fun ((), state) -> compileElements rest state)

        compileElements elements state
        |> Result.bind (fun ((), state) -> emitConstant (VNumber(VInteger elements.Length)) state)
        |> Result.bind (fun ((), state) -> emitConstant (VList([], LIST)) state)
        |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_CREATE state)


and compileIf (condition: Expr) (thenBranch: Expr) (elseBranch: Expr) : Compiler<unit> =
    fun state ->
        compileExpr condition state
        |> Result.bind (fun ((), state) ->
            let elseJump = emitJump OP_CODE.JUMP_IF_FALSE state

            emitOpCode OP_CODE.POP state
            |> Result.bind (fun ((), state) -> compileExpr thenBranch state)
            |> Result.bind (fun ((), state) ->
                let endJump = emitJump OP_CODE.JUMP state

                emitJumpBack elseJump state
                // |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.POP state)
                |> Result.bind (fun ((), state) -> compileExpr elseBranch state)
                |> Result.bind (fun ((), state) -> emitJumpBack endJump state)))

// block is a new scope and an expression, therefore last expression is returned in the block
and compileBlock (stmts: Stmt list) : Compiler<unit> =
    fun state ->
        let state =
            { state with
                ScopeDepth = state.ScopeDepth + 1 }

        let rec compileStmts stmts state =
            match stmts with
            | [] -> Ok((), state)
            | [ stmt ] ->
                match stmt with
                | SExpression(expr, _) -> compileExpr expr state
                | _ ->
                    compileStmt stmt state
                    |> Result.bind (fun ((), state) -> emitConstant VNil state)

            | stmt :: rest ->
                compileStmt stmt state
                |> Result.bind (fun ((), state) -> compileStmts rest state)

        compileStmts stmts state
        |> Result.map (fun ((), newState) ->
            ((),
             { newState with
                 ScopeDepth = newState.ScopeDepth - 1 }))


and compileLambda (parameters: Token list) (body: Expr) (pur: bool) : Compiler<unit> =
    fun state ->
        let functionName = $"lambda_{state.CurrentFunction.Name}"
        let lambdaFunction = initFunction functionName

        let lambdaState =
            { state with
                CurrentFunction = lambdaFunction
                ScopeDepth = state.ScopeDepth + 1
                LocalCount = 0 }

        let compiledParamsState =
            parameters
            |> List.fold
                (fun state param ->
                    let state = addLocal (lexemeToString param.Lexeme) state

                    { state with
                        CurrentFunction =
                            { state.CurrentFunction with
                                Arity = state.CurrentFunction.Arity + 1 } })
                lambdaState

        let builtin =
            if pur then
                Some <| compileAsBuiltin parameters body
            else
                None

        compileExpr body compiledParamsState
        |> Result.bind (fun ((), finalLambdaState) ->
            match emitOpCode OP_CODE.RETURN finalLambdaState with
            | Ok((), finalState) ->
                let constIndex =
                    addConstant state.CurrentFunction.Chunk (VFunction(finalState.CurrentFunction, builtin))

                emitBytes [| byte (opCodeToByte OP_CODE.CONSTANT); byte constIndex |] state
            | Error e -> Error e)

and compileAsBuiltin (parameters: Token list) (body: Expr) : Expression =
    // what we could do is make every unit return its compiled value
    // add to a map of lexeme to builtins on function def
    // then we would be able to use other values and other functions in this (type checker verifies that this is valid)

    if parameters.Length <> 1 then
        raise <| InvalidProgramException("Builtin functions can only have one parameter")

    fromExpr body

and compileCall (callee: Expr) (arguments: Expr list) (recursive: bool) : Compiler<unit> =
    fun state ->
        let rec compileArguments arguments state =
            match arguments with
            | [] -> Ok((), state)
            | arg :: rest ->
                compileExpr arg state
                |> Result.bind (fun ((), state) -> compileArguments rest state)

        compileExpr callee state
        |> Result.bind (fun ((), state) -> compileArguments arguments state)
        |> Result.bind (fun ((), state) ->
            let b =
                match recursive with
                | false -> 0
                | true -> 1
            emitBytes [| byte (opCodeToByte OP_CODE.CALL); byte (List.length arguments); (byte b) |] state)

and compileGrouping grouping : Compiler<unit> = fun state -> compileExpr grouping state

and compileIdentifier (token: Token) : Compiler<unit> =
    fun state ->
        let name = lexemeToString token.Lexeme

        match state.CurrentFunction.Locals |> List.tryFind (fun local -> local.Name = name) with
        | Some local -> emitBytes [| byte (opCodeToByte OP_CODE.GET_LOCAL); byte local.Index |] state
        | None ->
            let constIndex = addConstant state.CurrentFunction.Chunk (VString name)
            emitBytes [| byte (opCodeToByte OP_CODE.GET_GLOBAL); byte constIndex |] state

and compileStmt (stmt: Stmt) : Compiler<unit> =
    fun state ->
        match stmt with
        | SExpression(expr, _) -> compileExpr expr state
        | SVariableDeclaration(name, initializer, _) -> compileVariableDeclaration name initializer state
        | SAssertStatement(expr, msg, _) ->
            let callee =
                EIdentifier(
                    { Lexeme = Identifier "assert"
                      Position = { Column = 0; Line = 0 } },
                    None
                )

            let args =
                if Option.isNone msg then
                    [ expr ]
                else
                    [ Option.get msg; expr ]

            compileCall callee args false state
        | STypeDeclaration _ -> Ok((), state)

and compileVariableDeclaration (name: Token) (initializer: Expr) : Compiler<unit> =
    fun state ->
        compileExpr initializer state
        |> Result.bind (fun ((), state) ->
            if state.ScopeDepth > 0 then
                let newState = addLocal (lexemeToString name.Lexeme) state
                Ok((), newState)
            else
                let constIndex =
                    addConstant state.CurrentFunction.Chunk (VString(lexemeToString name.Lexeme))

                emitBytes [| byte (opCodeToByte OP_CODE.DEFINE_GLOBAL); byte constIndex |] state)

let compileProgramState (program: Program) (state: CompilerState) : CompilerResult<Chunk> =
    let rec compileStmts stmts state =
        match stmts with
        | [] -> Ok((), state)
        | stmt :: rest ->
            compileStmt stmt state
            |> Result.bind (fun ((), state) -> compileStmts rest state)

    compileStmts program state
    |> Result.bind (fun ((), state) ->
        emitOpCode OP_CODE.RETURN state
        |> Result.map (fun ((), state) -> (state.CurrentFunction.Chunk, state)))


let compileProgram (program: Program) : CompilerResult<Function> =
    let initialState =
        { CurrentFunction = initFunction "REPL_Input"
          ScopeDepth = 0
          CurrentLine = 1
          LocalCount = 0 }

    let rec compileStmts stmts state =
        match stmts with
        | [] -> Ok((), state)
        | stmt :: rest ->
            compileStmt stmt state
            |> Result.bind (fun ((), state) -> compileStmts rest state)

    compileStmts program initialState
    |> Result.bind (fun ((), state) ->
        emitOpCode OP_CODE.RETURN state
        |> Result.map (fun ((), state) -> (state.CurrentFunction, state)))
