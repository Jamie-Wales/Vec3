/// <summary>
/// Compiler for the backend.
/// Compiles the AST to bytecode.
/// </summary>

module Vec3.Interpreter.Backend.Compiler

open Microsoft.FSharp.Core
open Vec3.Interpreter
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.SymbolicExpression
open Vec3.Interpreter.Token

/// <summary>
/// Map of identifiers to values.
/// To be used for function composition in the symbolic expression evaluator.
/// </summary>
let identMap: Map<Lexeme, Value> ref = ref Map.empty

/// <summary>
/// The state of the compiler.
/// </summary>
type CompilerState =
    { CurrentFunction: Closure // The current function being compiled
      CurrentLine: int // Debug information
      ScopeDepth: int
      LocalCount: int }

/// <summary>
/// Type of a compiler error.
/// </summary>
type CompilerError = string * CompilerState
/// <summary>
/// Monadic Type of a compiler result.
/// </summary>
type CompilerResult<'a> = Result<'a * CompilerState, CompilerError>

/// <summary>
/// Represents a compiler.
/// </summary>
type Compiler<'a> = CompilerState -> CompilerResult<'a>

/// <summary>
/// Emits a byte to the chunk.
/// </summary>
/// <param name="byte">The byte to emit.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let emitByte (byte: byte) (state: CompilerState) : CompilerResult<unit> =
    writeChunk state.CurrentFunction.Function.Chunk byte state.CurrentLine
    Ok((), state)

/// <summary>
/// Emits a series of bytes to the chunk.
/// </summary>
/// <param name="bytes">The bytes to emit.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let emitBytes (bytes: byte seq) (state: CompilerState) : CompilerResult<unit> =
    Seq.iter (fun byte -> writeChunk state.CurrentFunction.Function.Chunk byte state.CurrentLine) bytes
    Ok((), state)

/// <summary>
/// Emits a constant to the chunk.
/// </summary>
/// <param name="value">The value to emit.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let emitConstant (value: Value) (state: CompilerState) : CompilerResult<unit> =
    writeConstant state.CurrentFunction.Function.Chunk value state.CurrentLine
    Ok((), state)

/// <summary>
/// Emits an opcode to the chunk.
/// </summary>
/// <param name="opCode">The opcode to emit.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let emitOpCode (opCode: OP_CODE) (state: CompilerState) : CompilerResult<unit> = emitByte (opCodeToByte opCode) state

/// <summary>
/// Emits a jump back instruction to the chunk (if statement).
/// </summary>
/// <param name="offset">The offset to jump back to.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let emitJumpBack (offset: int) (state: CompilerState) : CompilerResult<unit> =
    let jump = state.CurrentFunction.Function.Chunk.Code.Count - offset - 1
    let bytes = [| byte (jump &&& 0xff); byte ((jump >>> 8) &&& 0xff) |]
    emitBytes bytes state

/// <summary>
/// Emits a jump instruction to the chunk.
/// </summary>
/// <param name="instruction">The instruction to emit.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let emitJump (instruction: byte) (state: CompilerState) =
    emitByte instruction state
    |> Result.bind (fun ((), state) ->
        emitByte (byte 0xff) state // Placeholder for jump
        |> Result.bind (fun ((), state) ->
            emitByte (byte 0xff) state
            |> Result.map (fun ((), state) -> state.CurrentFunction.Function.Chunk.Code.Count - 2)))

/// <summary>
/// Patches a jump instruction.
/// </summary>
/// <param name="offset">The offset to patch.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let patchJump (offset: int) (state: CompilerState) =
    let jump = state.CurrentFunction.Function.Chunk.Code.Count - offset - 2

    state.CurrentFunction.Function.Chunk.Code[offset] <- (byte jump >>> 8) &&& byte 0xff
    state.CurrentFunction.Function.Chunk.Code[offset + 1] <- byte jump &&& byte 0xff

// Ok((), state)

/// <summary>
/// Initialises a function.
/// </summary>
/// <param name="name">The name of the function.</param>
/// <returns>The initialised function.</returns>
let initFunction (name: string) =
    { Arity = 0
      Chunk = emptyChunk ()
      Name = name
      Locals = [] }

/// <summary>
/// Adds a local to the state
/// </summary>
/// <param name="name">The name of the local.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let addLocal (name: string) (state: CompilerState) : CompilerState =
    let local: Local =
        { Name = name
          Depth = state.ScopeDepth
          Index = state.LocalCount }

    let updatedFunction =
        { state.CurrentFunction.Function with
            Locals = local :: state.CurrentFunction.Function.Locals }

    { state with
        CurrentFunction.Function = updatedFunction
        LocalCount = state.LocalCount + 1 }

/// <summary>
/// Adds an upvalue to the state.
/// </summary>
/// <param name="name">The name of the upvalue.</param>
/// <param name="depth">The depth of the upvalue.</param>
/// <param name="state">The current state.</param>
/// <returns>The new state.</returns>
let addUpValue (name: string) (depth: int) (state: CompilerState) : CompilerState =
    let upValue =
        { Name = name
          Index = state.CurrentFunction.UpValues.Length
          Depth = depth }

    let updatedFunction =
        { state.CurrentFunction with
            UpValues = upValue :: state.CurrentFunction.UpValues }

    { state with
        CurrentFunction = updatedFunction }


/// <summary>
/// Compile a simple literal.
/// </summary>
/// <param name="lit">The literal to compile.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
let rec compileLiteral (lit: Literal) (state: CompilerState) : CompilerResult<unit> =
    /// <summary>
    /// How to compile a number.
    /// </summary>
    /// <param name="n">The number to compile.</param>
    /// <param name="state">The current state.</param>
    /// <returns>The monadic result</returns>
    let compileNumber (n: Token.Number) (state: CompilerState) =
        match n with
        | LInteger i -> emitConstant (VNumber(VInteger i)) state
        | LFloat f -> emitConstant (VNumber(VFloat f)) state
        | LRational(n, d) -> emitConstant (VNumber(VRational(n, d))) state
        | LComplex(r, i) -> emitConstant (VNumber(VComplex(r, i))) state
        | LChar c -> emitConstant (VNumber(VChar c)) state

    match lit with
    | LNumber n -> compileNumber n state
    | LString s -> emitConstant (VString s) state
    | LBool b ->
        if b then
            emitOpCode OP_CODE.TRUE state
        else
            emitOpCode OP_CODE.FALSE state
    | LUnit -> emitConstant VNil state

/// <summary>
/// Compile a quoted value.
/// </summary>
/// <param name="expr">The expression to compile.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
let compileCodeBlock (expr: Expr) state : CompilerResult<unit> = emitConstant (VBlock expr) state

/// <summary>
/// Flatten a recursive record type into a list of key value pairs.
/// </summary>
/// <param name="record">The record to flatten.</param>
/// <returns>The list of key value pairs.</returns>
/// <exception cref="System.Exception">Invalid record field name.</exception>
let flattenRecord (record: Expr) : (string * Expr) list =
    let idToString =
        function
        | Identifier i -> i
        | _ -> raise <| System.Exception("Invalid record field name")

    let rec flattenRecord' (record: Expr) (acc: (string * Expr) list) =
        match record with
        | ERecordExtend((name, value, _), rest, _) -> flattenRecord' rest ((idToString name.Lexeme, value) :: acc)
        | _ -> acc

    flattenRecord' record []

/// <summary>
/// Compile an expression.
/// </summary>
/// <param name="expr">The expression to compile.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
/// <exception cref="System.Exception">Invalid record field name.</exception>
let rec compileExpr (expr: Expr) (state: CompilerState) : unit CompilerResult =
    match expr with
    | ELiteral(lit, _) -> compileLiteral lit state
    | EIdentifier(i, _) -> compileIdentifier i state
    | EGrouping(e, _) -> compileGrouping e state
    | ELambda(parameters, body, _, _, _, isAsync) ->
        let parameters = List.map fst parameters
        compileLambda parameters body isAsync state
    | ECall(callee, arguments, _) -> compileCall callee arguments false state
    | EList(elements, _) -> compileList elements state
    | EIndex(list, start, _) -> compileIndex list start state
    | EIndexRange(list, start, end_, _) -> compileIndexRange list start end_ state
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

    | ERecordRestrict(e, _, _) -> compileExpr e state
    | ERecordExtend((name, value, _), record, _) ->
        let idToString =
            function
            | Identifier i -> i
            | _ -> raise <| System.Exception("Invalid record field name")

        let flattend = (idToString name.Lexeme, value) :: flattenRecord record
        let keys = List.map fst flattend
        let values = List.map snd flattend

        let keysList =
            EList(List.map (fun k -> ELiteral(LString k, TString)) keys, Some TAny)

        let valuesList = EList(values, Some TAny)

        let expression =
            ECall(
                EIdentifier(
                    { Lexeme = Identifier "record"
                      Position = { Line = 0; Column = 0 } },
                    None
                ),
                [ keysList; valuesList ],
                None
            )

        compileExpr expression state

    | ERecordSelect(expr, token, _) ->
        let name =
            match token with
            | { Lexeme = Identifier n } -> n
            | _ -> raise <| System.Exception("Invalid record field name")

        // compile as call
        let callee =
            EIdentifier(
                { Lexeme = Identifier "select"
                  Position = { Line = 0; Column = 0 } },
                None
            )

        compileCall callee [ expr; ELiteral(LString name, TString) ] false state

    | EBlock(stmts, t, _) -> compileBlock stmts t state
    | EIf(condition, thenBranch, elseBranch, _) -> compileIf condition thenBranch elseBranch state
    | ETernary(cond, thenB, elseB, _) -> compileIf cond thenB elseB state
    | ETail(ex, _) ->
        match ex with
        | ECall(name, args, _) -> compileCall name args true state
        | e -> compileExpr e state

/// <summary>
/// Compile index range expression.
/// </summary>
/// <param name="list">The list to index.</param>
/// <param name="start">The start index.</param>
/// <param name="end_">The end index.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileIndexRange (list: Expr) (start: Expr) (end_: Expr) (state: CompilerState) : unit CompilerResult =
    let callee =
        EIdentifier(
            { Lexeme = Identifier "index"
              Position = { Line = 0; Column = 0 } },
            None
        )

    compileCall callee [ list; start; end_ ] false state

/// <summary>
/// Compile index expression.
/// </summary>
/// <param name="list">The list to index.</param>
/// <param name="start">The start index.</param>
/// <param name="state">The current state.</param>
and compileIndex (list: Expr) (start: Expr) (state: CompilerState) : CompilerResult<unit> =
    let callee =
        EIdentifier(
            { Lexeme = Identifier "index"
              Position = { Line = 0; Column = 0 } },
            None
        )

    compileCall callee [ list; start ] false state

/// <summary>
/// Compile a tuple.
/// </summary>
/// <param name="elements">The elements of the tuple.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileTuple (elements: Expr list) (state: CompilerState) : CompilerResult<unit> =
    let rec compileElements elements state =
        match elements with
        | [] -> Ok((), state)
        | element :: rest ->
            compileExpr element state
            |> Result.bind (fun ((), state) -> compileElements rest state)

    compileElements elements state
    |> Result.bind (fun ((), state) -> emitConstant (VNumber(VInteger elements.Length)) state)
    |> Result.bind (fun ((), state) -> emitConstant (VList([], TUPLE)) state)
    |> Result.bind (fun ((), state) -> emitOpCode OP_CODE.COMPOUND_CREATE state)

/// <summary>
/// Compile a list.
/// </summary>
/// <param name="elements">The elements of the list.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileList (elements: Expr list) (state: CompilerState) : CompilerResult<unit> =
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


/// <summary>
/// Compile an if expression.
/// </summary>
/// <param name="condition">The condition of the if statement.</param>
/// <param name="thenBranch">The then branch of the if statement.</param>
/// <param name="elseBranch">The else branch of the if statement.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileIf (condition: Expr) (thenBranch: Expr) (elseBranch: Expr) (state: CompilerState) : CompilerResult<unit> =
    compileExpr condition state
    |> Result.bind (fun ((), state) ->
        emitJump (opCodeToByte OP_CODE.JUMP_IF_FALSE) state
        |> Result.bind (fun thenJump ->
            compileExpr thenBranch state
            |> Result.bind (fun ((), state) ->
                emitJump (opCodeToByte OP_CODE.JUMP) state
                |> Result.bind (fun elseJump ->
                    patchJump thenJump state

                    compileExpr elseBranch state
                    |> Result.bind (fun ((), state) ->
                        patchJump elseJump state
                        Ok((), state))))))

/// <summary>
/// Compile a block expression.
/// Block is a new scope and an expression, therefore last expression is returned in the block
/// Compiled as an immediately invoked function for scoping.
/// </summary>
/// <param name="stmts">The statements in the block.</param>
/// <param name="isFunc">Whether the block is a function.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileBlock (stmts: Stmt list) (isFunc: bool) (state: CompilerState) : CompilerResult<unit> =
    if not isFunc then
        let func = ELambda([], EBlock(stmts, true, None), None, false, None, false)
        let call = ECall(func, [], None)

        compileExpr call state
    else

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

/// <summary>
/// Compile a lambda function.
/// </summary>
/// <param name="parameters">The parameters of the lambda function.</param>
/// <param name="body">The body of the lambda function.</param>
/// <param name="isAsync">Whether the lambda function is async.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileLambda (parameters: Token list) (body: Expr) (isAsync: bool) (state: CompilerState) : CompilerResult<unit> =
    let functionName = $"lambda_{state.CurrentFunction.Function.Name}"
    let lambdaFunction = initFunction functionName

    let currentFunction = state.CurrentFunction

    let upvalues = currentFunction.Function.Locals @ currentFunction.UpValues

    let lambdaState =
        { state with
            CurrentFunction.Function = lambdaFunction
            ScopeDepth = state.ScopeDepth + 1
            LocalCount = 0 }

    let compiledParamsState =
        parameters
        |> List.fold
            (fun state param ->
                let state = addLocal (lexemeToString param.Lexeme) state

                { state with
                    CurrentFunction.Function.Arity = state.CurrentFunction.Function.Arity + 1 })
            lambdaState

    let compiledParamsState =
        upvalues
        |> List.fold (fun state upvalue -> addUpValue upvalue.Name upvalue.Depth state) compiledParamsState

    let builtin = compileAsBuiltin parameters body

    let body =
        match body with
        | EBlock(stmts, _, typeOption) -> EBlock(stmts, true, typeOption)
        | ETail(e, typeOption) ->
            match e with
            | EBlock(stmts, _, typeOption) -> EBlock(stmts, true, typeOption)
            | _ -> ETail(body, typeOption)
        | _ -> body

    compileExpr body compiledParamsState
    |> Result.bind (fun ((), finalLambdaState) ->
        emitOpCode OP_CODE.RETURN finalLambdaState
        |> Result.bind (fun ((), finalState) ->
            emitByte (byte 1) finalLambdaState |> ignore

            let constIndex =
                addConstant
                    state.CurrentFunction.Function.Chunk
                    (if isAsync then
                         VAsyncFunction(finalState.CurrentFunction.Function)
                     else
                         VFunction(finalState.CurrentFunction.Function, builtin))

            emitBytes [| byte (opCodeToByte OP_CODE.CLOSURE); byte constIndex |] state
            |> Result.bind (fun ((), state) ->
                emitByte (byte (List.length upvalues)) state
                |> Result.bind (fun ((), state) ->
                    let upvalues =
                        upvalues
                        |> Seq.map (fun upvalue ->
                            let constIndex =
                                addConstant state.CurrentFunction.Function.Chunk (VString upvalue.Name)

                            [| byte upvalue.Index; byte upvalue.Depth; byte constIndex |])
                        |> Seq.concat

                    emitBytes upvalues state))))

/// <summary>
/// Get the symbolic expression from a lambda.
/// </summary>
/// <param name="parameters">The parameters of the lambda.</param>
/// <param name="body">The body of the lambda.</param>
/// <returns>Maybe the symbolic expression</returns>
and compileAsBuiltin (parameters: Token list) (body: Expr) : Expression option =
    // what we could do is make every unit return its compiled value
    // add to a map of lexeme to builtins on function def
    // then we would be able to use other values and other functions in this (type checker verifies that this is valid)

    if parameters.Length <> 1 then
        None
    else
        try
            let expr = fromExpr body
            Some expr
        with _ ->
            None

/// <summary>
/// Compile a function call.
/// </summary>
/// <param name="callee">The callee of the function call.</param>
/// <param name="arguments">The arguments of the function call.</param>
/// <param name="recursive">Whether the function call is recursive.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileCall (callee: Expr) (arguments: Expr list) (recursive: bool) (state: CompilerState) : CompilerResult<unit> =
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

/// <summary>
/// Compile an expression in a grouping.
/// </summary>
/// <param name="grouping">The grouping expression.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileGrouping grouping state : CompilerResult<unit> = compileExpr grouping state


/// <summary>
/// Compile an identifier.
/// </summary>
/// <param name="token">The identifier token.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileIdentifier (token: Token) (state: CompilerState) : CompilerResult<unit> =
    let name = lexemeToString token.Lexeme

    // Determine if the identifier is a local, upvalue, or global
    match
        state.CurrentFunction.Function.Locals
        |> List.tryFind (fun local -> local.Name = name)
    with
    | Some local -> emitBytes [| byte (opCodeToByte OP_CODE.GET_LOCAL); byte local.Index |] state
    | None ->
        match
            state.CurrentFunction.UpValues
            |> List.tryFind (fun upvalue -> upvalue.Name = name)
        with
        | Some upvalue -> emitBytes [| byte (opCodeToByte OP_CODE.GET_UPVALUE); byte upvalue.Index |] state
        | None ->
            let constIndex = addConstant state.CurrentFunction.Function.Chunk (VString name)
            emitBytes [| byte (opCodeToByte OP_CODE.GET_GLOBAL); byte constIndex |] state

/// <summary>
/// Compile a statement.
/// </summary>
/// <param name="stmt">The statement to compile.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileStmt (stmt: Stmt) (state: CompilerState) : CompilerResult<unit> =
    match stmt with
    | SExpression(expr, _) -> compileExpr expr state
    | SVariableDeclaration(name, initializer, _) -> compileVariableDeclaration name initializer state
    | SAsync(name, tup, expr, _) ->
        let assign =
            SVariableDeclaration(name, ELambda(tup, expr, None, false, None, true), None)

        compileStmt assign state
    | SRecFunc(name, tup, expr, _) ->
        let assign =
            SVariableDeclaration(name, ELambda(tup, expr, None, false, None, false), None)

        compileStmt assign state
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
    | SImport(_, path, isStd, _) ->
        try
            // default dir
            let path = if isStd then $"../../../stdlib/{path}.vec3" else path
            let parsed = Parser.parseFile path false

            match parsed with
            | Ok(_, program) ->
                List.iter (fun stmt -> compileStmt stmt state |> ignore) program
                Ok((), state)
            | Error(msg, _) -> Error($"{msg}", state)


        with e ->
            printfn $"Error compiling: {e.Message}"
            Error(e.Message, state)

/// <summary>
/// Compile a variable declaration.
/// </summary>
/// <param name="name">The name of the variable.</param>
/// <param name="initializer">The initializer of the variable.</param>
/// <param name="state">The current state.</param>
/// <returns>The monadic result</returns>
and compileVariableDeclaration (name: Token) (initializer: Expr) (state: CompilerState) : CompilerResult<unit> =
    compileExpr initializer state
    |> Result.bind (fun ((), state) ->
        if state.ScopeDepth > 0 then
            let newState = addLocal (lexemeToString name.Lexeme) state
            Ok((), newState)
        else
            let constIndex =
                addConstant state.CurrentFunction.Function.Chunk (VString(lexemeToString name.Lexeme))

            emitBytes [| byte (opCodeToByte OP_CODE.DEFINE_GLOBAL); byte constIndex |] state)

/// <summary>
/// Compile a program.
/// </summary>
/// <param name="program">The program to compile.</param>
/// <param name="state">The current state.</param>
/// <returns>The chunk result</returns>
and compileProgramState (program: Program) (state: CompilerState) : CompilerResult<Chunk> =
    let rec compileStmts stmts state =
        match stmts with
        | [] -> Ok((), state)
        | stmt :: rest ->
            compileStmt stmt state
            |> Result.bind (fun ((), state) -> compileStmts rest state)

    compileStmts program state
    |> Result.bind (fun ((), state) ->
        emitOpCode OP_CODE.RETURN state
        |> Result.map (fun ((), state) ->
            (emitByte (byte 0) state |> ignore
             state.CurrentFunction.Function.Chunk, state)))


/// <summary>
/// Compile a program.
/// </summary>
/// <param name="program">The program to compile.</param>
/// <returns>The function result</returns>
and compileProgram (program: Program) : CompilerResult<Function> =
    let func = initFunction "REPL_Input"

    let initialState =
        { CurrentFunction =
            { Function = func
              UpValues = []
              UpValuesValues = [||] }
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
        |> Result.map (fun ((), state) ->
            (emitByte (byte 0) state |> ignore
             state.CurrentFunction.Function, state)))
