/// <summary>
/// Compiler for the backend.
/// Compiles the AST to bytecode.
/// </summary>

module Vec3.Interpreter.Backend.Compiler

open Microsoft.FSharp.Core
open System
open Vec3.Interpreter
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Prelude
open Vec3.Interpreter.SymbolicExpression
open Vec3.Interpreter.Token

let identMap: Map<Lexeme, Value> ref = ref Map.empty

type CompilerState =
    { CurrentFunction: Closure
      CurrentLine: int
      ScopeDepth: int
      LocalCount: int }

type CompilerError = string * CompilerState
type CompilerResult<'a> = Result<'a * CompilerState, CompilerError>
type Compiler<'a> = CompilerState -> CompilerResult<'a>

let emitByte (byte: byte) (state: CompilerState) : CompilerResult<unit> =
    writeChunk state.CurrentFunction.Function.Chunk byte state.CurrentLine
    Ok((), state)

let emitBytes (bytes: byte seq) (state: CompilerState) : CompilerResult<unit> =
    Seq.iter (fun byte -> writeChunk state.CurrentFunction.Function.Chunk byte state.CurrentLine) bytes
    Ok((), state)

let emitConstant (value: Value) (state: CompilerState) : CompilerResult<unit> =
    writeConstant state.CurrentFunction.Function.Chunk value state.CurrentLine
    Ok((), state)

let emitOpCode (opCode: OP_CODE) (state: CompilerState) : CompilerResult<unit> = emitByte (opCodeToByte opCode) state

let emitJumpBack (offset: int) (state: CompilerState) : CompilerResult<unit> =
    let jump = state.CurrentFunction.Function.Chunk.Code.Count - offset - 1
    let bytes = [| byte (jump &&& 0xff); byte ((jump >>> 8) &&& 0xff) |]
    emitBytes bytes state

let emitJump (instruction: byte) (state: CompilerState) =
    emitByte instruction state
    |> Result.bind (fun ((), state) ->
        emitByte (byte 0xff) state
        |> Result.bind (fun ((), state) ->
            emitByte (byte 0xff) state
            |> Result.map (fun ((), state) -> state.CurrentFunction.Function.Chunk.Code.Count - 2)))

let patchJump (offset: int) (state: CompilerState) =
    let jump = state.CurrentFunction.Function.Chunk.Code.Count - offset - 2

    state.CurrentFunction.Function.Chunk.Code[offset] <- (byte jump >>> 8) &&& byte 0xff
    state.CurrentFunction.Function.Chunk.Code[offset + 1] <- byte jump &&& byte 0xff

let initFunction (name: string) =
    { Arity = 0
      Chunk = emptyChunk ()
      Name = name
      Locals = [] }

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


let rec compileLiteral (lit: Literal) : Compiler<unit> =
    let compileNumber (n: Token.Number) state =
        match n with
        | LInteger i -> emitConstant (VNumber(VInteger i)) state
        | LFloat f -> emitConstant (VNumber(VFloat f)) state
        | LRational(n, d) -> emitConstant (VNumber(VRational(n, d))) state
        | LComplex(r, i) -> emitConstant (VNumber(VComplex(r, i))) state
        | LChar c -> emitConstant (VNumber(VChar c)) state

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

let compileCodeBlock (expr: Expr) state : CompilerResult<unit> = emitConstant (VBlock expr) state

let rec compileExpr (expr: Expr) (state: CompilerState) : unit CompilerResult =
    match expr with
    | ELiteral(lit, _) -> compileLiteral lit state
    | EIdentifier(i, _) -> compileIdentifier i state
    | EGrouping(e, _) -> compileGrouping e state
    | ELambda(parameters, body, _, pr, _, isAsync) ->
        let parameters = List.map fst parameters
        compileLambda parameters body pr isAsync state
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
        let name =
            match name with
            | { Lexeme = Identifier n } -> n
            | _ -> raise <| System.Exception("Invalid record field name")

        let constIndex = addConstant state.CurrentFunction.Function.Chunk (VString name)

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

            // compile as call
            let callee =
                EIdentifier(
                    { Lexeme = Identifier "select"
                      Position = { Line = 0; Column = 0 } },
                    None
                )

            compileCall callee [ expr; ELiteral(LString name, TString) ] false state

        | EBlock(stmts, _) -> compileBlock stmts state
        | EIf(condition, thenBranch, elseBranch, _) -> compileIf condition thenBranch elseBranch state
        | ETernary(cond, thenB, elseB, _) -> compileIf cond thenB elseB state
        | ETail(ex, _) ->
            match ex with
            | ECall(name, args, _) ->
                compileCall name args true state
            | e -> compileExpr e state
        | EMatch(expr, cases, _) -> compileMatch expr cases state

and compileIndexRange (list: Expr) (start: Expr) (end_: Expr) (state: CompilerState) : unit CompilerResult =
    let callee =
        EIdentifier(
            { Lexeme = Identifier "index"
              Position = { Line = 0; Column = 0 } },
            None
        )

    compileCall callee [ list; start; end_ ] false state

and compileIndex (list: Expr) (start: Expr) : Compiler<unit> =
    fun state ->
        let callee =
            EIdentifier(
                { Lexeme = Identifier "index"
                  Position = { Line = 0; Column = 0 } },
                None
            )

        compileCall callee [ list; start ] false state
        
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


and compileLambda (parameters: Token list) (body: Expr) (pur: bool) (isAsync: bool) : Compiler<unit> =
    fun state ->
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

        let builtin =
            if pur then
                Some <| compileAsBuiltin parameters body
            else
                None

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

and compileAsBuiltin (parameters: Token list) (body: Expr) : Expression =
    // what we could do is make every unit return its compiled value
    // add to a map of lexeme to builtins on function def
    // then we would be able to use other values and other functions in this (type checker verifies that this is valid)

    if parameters.Length <> 1 then
        raise
        <| InvalidProgramException("Builtin functions can only have one parameter")

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

and compileMatch (expr: Expr) (cases: (Pattern * Expr) list) : Compiler<unit> =
    fun state ->
        // hwo to do this ?
        // compile expr
        // compile each case
        // if match then jump to end
        // if no match then jump to next case
        // if no case then error
        // how to compile cons ?
        // how to compile record ?
        // answer is to compile as call to builtin

        // translate to series of if else
        // EIF(ECall(match, [ pattern compile, expr compile ]), case, else EIF(ECall(match, [ pattern compile, expr compile ]), case, else ...))

        // compile as call to lambda for access to local variables, sorry no impl case as call with pattern
        let rec patternToExpression (pattern: Pattern) : Expr =
            match pattern with
            | PWildcard -> ELiteral(LBool true, TBool)
            | PIdentifier name -> EIdentifier(name, None)
            | PTuple ps -> ETuple(List.map patternToExpression ps, None)
            | PList ps -> EList(List.map patternToExpression ps, None)
            | PCons(head, tail) -> EList([ patternToExpression head; patternToExpression tail ], None)
            | PLiteral lit -> ELiteral(lit, TAny)
            | PRecordEmpty -> ERecordEmpty TRowEmpty
            | PType _ -> ELiteral(LBool true, TBool)

        let rec generateExpression (cases: (Pattern * Expr) list) : Expr =
            match cases with
            | [] ->
                let errIdentifier =
                    EIdentifier(
                        { Lexeme = Identifier "error"
                          Position = { Line = 0; Column = 0 } },
                        None
                    )

                ECall(errIdentifier, [ ELiteral(LString "No match found", TString) ], None)

            | (pattern, case) :: rest ->
                ETernary(
                    ECall(
                        EIdentifier(
                            { Lexeme = Identifier "match"
                              Position = { Line = 0; Column = 0 } },
                            None
                        ),
                        [ (patternToExpression pattern); expr ],
                        None
                    ),
                    case,
                    generateExpression rest,
                    None
                )

        let expression = generateExpression cases
        compileExpr expression state

and compileStmt (stmt: Stmt) : Compiler<unit> =
    fun state ->
        match stmt with
        | SExpression(expr, _) -> compileExpr expr state
        | SVariableDeclaration(name, initializer, _) -> compileVariableDeclaration name initializer state
        | SAsync(name, tup, expr, _) ->
            let assign =
                SVariableDeclaration(name, ELambda(tup, expr, None, false, None, true), None)

            compileStmt assign state
        | SRecFunc(name, tup, expr, _) ->
            printfn $"RecFunc: {name.Lexeme}, {expr}"
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
        | SImport(_, path, _) ->
            // handle imports here
            try
                let parsed = Parser.parseFile path

                match parsed with
                | Ok(_, program) ->
                    let _, _, _, p = preludeChecked
                    let program = p @ program
                    List.iter (fun stmt -> compileStmt stmt state |> ignore) program
                    Ok((), state)
                | Error e -> Error($"{e}", state)

            with e ->
                Error(e.Message, state)

and compileVariableDeclaration (name: Token) (initializer: Expr) : Compiler<unit> =
    fun state ->
        compileExpr initializer state
        |> Result.bind (fun ((), state) ->
            if state.ScopeDepth > 0 then
                let newState = addLocal (lexemeToString name.Lexeme) state
                Ok((), newState)
            else
                let constIndex =
                    addConstant state.CurrentFunction.Function.Chunk (VString(lexemeToString name.Lexeme))

                emitBytes [| byte (opCodeToByte OP_CODE.DEFINE_GLOBAL); byte constIndex |] state)

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
