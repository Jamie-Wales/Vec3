module Vec3.Interpreter.Typing.Checker

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Inference
open Exceptions
open System

// TODO
// - Type inference for params
// - to do that i need union types
// - as plus operator can be used for int, float, rational, complex etc, therefore:
// (x) -> x + x
// x: int | float | rational | complex
// but
// (x) -> x + 5
// 5: int
// therefore, x: int
// union types i need to parse etc and work out how to validate, i suppose i need to type operators or treat them as
// funcs
// but ONLY params should be able to be union types, not return types
// also how to check for any ?

let rec checkExpression (env: TypeEnv) (expr: Expr) : Result<TType, TypeErrors> =
    // type env holds all of the infered types
    // expr is the expression to check
    match expr with
    | ELiteral (lit, _) -> Ok <| checkLiteral lit
    | EIdentifier (token, _) -> checkIdentifier env token


let rec checkExpr (env: TypeEnv) (expr: Expr) : Result<TType, TypeErrors> =
    match expr with
    | ELiteral (lit, _) -> Ok <| checkLiteral lit
    | EIdentifier (token, _) -> checkIdentifier env token
    | EUnary(op, expr, _) ->
        let exprType = checkExpr env expr

        match exprType with
        | Ok TInteger when op.Lexeme = Operator Minus -> Ok TInteger
        | Ok TFloat when op.Lexeme = Operator Minus -> Ok TFloat
        | Ok TRational when op.Lexeme = Operator Minus -> Ok TRational

        | Ok TInteger when op.Lexeme = Operator Plus -> Ok TInteger
        | Ok TFloat when op.Lexeme = Operator Plus -> Ok TFloat
        | Ok TRational when op.Lexeme = Operator Plus -> Ok TRational

        | Ok TBool when op.Lexeme = Operator Bang -> Ok TBool
        | Ok TInteger when op.Lexeme = Operator Bang -> Ok TInteger
        | Ok TFloat when op.Lexeme = Operator Bang -> Ok TFloat
        | Ok TRational when op.Lexeme = Operator Bang -> Ok TRational

        | Ok t -> Error [ TypeError.InvalidOperator(op, t) ]
        | Error errors -> Error errors
    | EBinary(lhs, op, rhs, _) ->
        let lhsType = checkExpr env lhs
        let rhsType = checkExpr env rhs

        match lhsType, rhsType with
        | Ok TInteger, Ok TInteger when op.Lexeme = Operator Plus -> Ok TInteger
        // | Ok TInfer, Ok TInteger when op.Lexeme = Operator Plus -> Ok TInteger
        // | Ok TInteger, Ok TInfer when op.Lexeme = Operator Plus -> Ok TInteger
        
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator Plus -> Ok TFloat
        // | Ok TInfer, Ok TFloat when op.Lexeme = Operator Plus -> Ok TFloat
        // | Ok TFloat, Ok TInfer when op.Lexeme = Operator Plus -> Ok TFloat
        
        | Ok TRational, Ok TRational when op.Lexeme = Operator Plus -> Ok TRational
        // | Ok TInfer, Ok TRational when op.Lexeme = Operator Plus -> Ok TRational
        // | Ok TRational, Ok TInfer when op.Lexeme = Operator Plus -> Ok TRational

        
        | Ok TInteger, Ok TInteger when op.Lexeme = Operator Minus -> Ok TInteger
        // | Ok TInfer, Ok TInteger when op.Lexeme = Operator Minus -> Ok TInteger
        // | Ok TInteger, Ok TInfer when op.Lexeme = Operator Minus -> Ok TInteger
        
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator Minus -> Ok TFloat
        // | Ok TInfer, Ok TFloat when op.Lexeme = Operator Minus -> Ok TFloat
        // | Ok TFloat, Ok TInfer when op.Lexeme = Operator Minus -> Ok TFloat
        
        | Ok TRational, Ok TRational when op.Lexeme = Operator Minus -> Ok TRational
        // | Ok TInfer, Ok TRational when op.Lexeme = Operator Minus -> Ok TRational
        // | Ok TRational, Ok TInfer when op.Lexeme = Operator Minus -> Ok TRational
        

        | Ok TInteger, Ok TInteger when op.Lexeme = Operator Star -> Ok TInteger
        // | Ok TInfer, Ok TInteger when op.Lexeme = Operator Star -> Ok TInteger
        // | Ok TInteger, Ok TInfer when op.Lexeme = Operator Star -> Ok TInteger
        
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator Star -> Ok TFloat
        // | Ok TInfer, Ok TFloat when op.Lexeme = Operator Star -> Ok TFloat
        // | Ok TFloat, Ok TInfer when op.Lexeme = Operator Star -> Ok TFloat
        
        | Ok TRational, Ok TRational when op.Lexeme = Operator Star -> Ok TRational
        // | Ok TInfer, Ok TRational when op.Lexeme = Operator Star -> Ok TRational
        // | Ok TRational, Ok TInfer when op.Lexeme = Operator Star -> Ok TRational
        

        | Ok TInteger, Ok TInteger when op.Lexeme = Operator Slash -> Ok TInteger
        // | Ok TInfer, Ok TInteger when op.Lexeme = Operator Slash -> Ok TInteger
        // | Ok TInteger, Ok TInfer when op.Lexeme = Operator Slash -> Ok TInteger
        
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator Slash -> Ok TFloat
        // | Ok TInfer, Ok TFloat when op.Lexeme = Operator Slash -> Ok TFloat
        // | Ok TFloat, Ok TInfer when op.Lexeme = Operator Slash -> Ok TFloat
        
        | Ok TRational, Ok TRational when op.Lexeme = Operator Slash -> Ok TRational
        // | Ok TInfer, Ok TRational when op.Lexeme = Operator Slash -> Ok TRational
        // | Ok TRational, Ok TInfer when op.Lexeme = Operator Slash -> Ok TRational
        
        
        | Ok TInteger, Ok TInteger when op.Lexeme = Operator StarStar -> Ok TInteger
        // | Ok TInfer, Ok TInteger when op.Lexeme = Operator StarStar -> Ok TInteger
        // | Ok TInteger, Ok TInfer when op.Lexeme = Operator StarStar -> Ok TInteger
        
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator StarStar -> Ok TFloat
        // | Ok TInfer, Ok TFloat when op.Lexeme = Operator StarStar -> Ok TFloat
        // | Ok TFloat, Ok TInfer when op.Lexeme = Operator StarStar -> Ok TFloat
        
        | Ok TRational, Ok TRational when op.Lexeme = Operator StarStar -> Ok TRational
        // | Ok TInfer, Ok TRational when op.Lexeme = Operator StarStar -> Ok TRational
        // | Ok TRational, Ok TInfer when op.Lexeme = Operator StarStar -> Ok TRational
        
        | Ok TInteger, Ok TInteger when op.Lexeme = Operator EqualEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator EqualEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.Lexeme = Operator EqualEqual -> Ok TBool
        | Ok TBool, Ok TBool when op.Lexeme = Operator EqualEqual -> Ok TBool

        | Ok TInteger, Ok TInteger when op.Lexeme = Operator BangEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator BangEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.Lexeme = Operator BangEqual -> Ok TBool
        | Ok TInteger, Ok TInteger when op.Lexeme = Operator Less -> Ok TBool

        | Ok TInteger, Ok TInteger when op.Lexeme = Operator Less -> Ok TBool
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator Less -> Ok TBool
        | Ok TRational, Ok TRational when op.Lexeme = Operator Less -> Ok TBool

        | Ok TInteger, Ok TInteger when op.Lexeme = Operator LessEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator LessEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.Lexeme = Operator LessEqual -> Ok TBool

        | Ok TInteger, Ok TInteger when op.Lexeme = Operator Greater -> Ok TBool
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator Greater -> Ok TBool
        | Ok TRational, Ok TRational when op.Lexeme = Operator Greater -> Ok TBool

        | Ok TInteger, Ok TInteger when op.Lexeme = Operator GreaterEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.Lexeme = Operator GreaterEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.Lexeme = Operator GreaterEqual -> Ok TBool
        
        | Error errors, Error errors' -> Error(errors @ errors')
        | Error errors, Ok _ -> Error errors
        | Ok _, Error errors -> Error errors
        | Ok t, Ok t' -> Error [ TypeError.InvalidOperandType(op, t, t') ]
    | EGrouping (expr, _) -> checkExpr env expr
    | EAssignment(token, expr, _) ->
        let exprType = checkExpr env expr

        match token.Lexeme with
        | Identifier _ as id ->
            match Map.tryFind id env with
            | Some t ->
                match exprType with
                | Ok t' when t = t' -> Ok t
                | Ok t' -> Error [ TypeError.InvalidAssignment(token, t, t') ]
                | Error errors -> Error errors
            | None -> Error [ TypeError.UndefinedVariable token ]
        | _ -> Error [ TypeError.UndefinedVariable token ]
    | ECall(callee, args, _) ->
        let calleeType = checkExpr env callee

        match calleeType with
        | Ok(TFunction(paramList, returnType)) ->
            let argResults = List.map (checkExpr env) args

            let validArgs =
                List.forall
                    (function
                    | Ok _ -> true
                    | _ -> false)
                    argResults

            if List.length paramList <> List.length args then
                Error [ TypeError.InvalidArgumentCount(callee, List.length paramList, List.length args) ]
            else if not validArgs then
                let errors =
                    List.choose
                        (function
                        | Error errs -> Some errs
                        | _ -> None)
                        argResults
                    |> List.concat

                Error errors
            else
                let argTypes =
                    List.map
                        (fun t ->
                            match t with
                            | Ok t -> t
                            | _ -> TInfer)
                        argResults

                let valid =
                    List.forall2 (fun expected actual -> expected = actual || expected = TAny) paramList argTypes

                if valid then
                    Ok returnType
                else
                    Error [ TypeError.InvalidArgumentType(callee, paramList.Head, argTypes.Head) ]
        | Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidCallType(callee, TInfer, TInfer) ] // fix

    // need better type inference here for params, unless params must be typed
    | ELambda(paramList, body, typ) ->
        let returnT, paramT = match typ with
                                | TFunction(parameters, returnType) -> returnType, parameters
                                | _ -> TInfer, List.map (fun _ -> TInfer) paramList
        
        let newEnv =
            List.fold
                (fun acc (param, typ) ->
                    match param.Lexeme with
                    | Identifier _ as id -> Map.add id typ acc
                    | _ -> raise (TypeException([TypeError.UndefinedVariable param])))
                env
                (List.zip paramList paramT)

        let bodyType = checkExpr newEnv body

        match bodyType with
        | Ok bodyType ->
            if bodyType = returnT then
                Ok <| TFunction(paramT, returnT)
            else if returnT = TInfer then
                Ok <| TFunction(paramT, bodyType)
            else
                Error [ TypeError.InvalidFunctionReturn(paramList.Head, returnT, bodyType) ]
        | Error errors -> Error errors
    | EBlock (stmts, _) ->
        let rec checkBlock (env: TypeEnv) (stmts: Stmt list) : Result<TType, TypeErrors> =
            match stmts with
            | [] -> Ok TUnit
            | [ stmt ] ->
                match stmt with
                | SExpression (expr, _) -> checkExpr env expr
                | SVariableDeclaration(_, expr, _) -> checkExpr env expr
                | SPrintStatement (expr, _) -> checkExpr env expr
            | stmt :: rest ->
                let env', _ = checkStmt env stmt
                checkBlock env' rest

        checkBlock env stmts

and checkStmt (env: TypeEnv) (stmt: Stmt) : TypeEnv * Result<TType, TypeErrors> =
    match stmt with
    | SExpression (expr, _) ->
        let exprType = checkExpr env expr

        match exprType with
        | Error errors -> env, Error errors
        | Ok exprType -> env, Ok exprType

    | SVariableDeclaration(token, expr, typ) ->
        let exprType = checkExpr env expr

        match exprType with
        | Error errors -> env, Error errors
        | Ok exprType ->
            if typ = exprType then
                match token.Lexeme with
                | Identifier _ as id -> Map.add id typ env, Ok TUnit
                | _ -> env, Error [ TypeError.UndefinedVariable token ]
            else if typ = TInfer then
                match token.Lexeme with
                | Identifier _ as id -> Map.add id exprType env, Ok TUnit
                | _ -> env, Error [ TypeError.UndefinedVariable token ]
            else
                env, Error [ TypeError.TypeMismatch(token, typ, exprType) ]
    | SPrintStatement(expr, t) -> (env, checkExpr env expr)

let rec checkStmts (env: TypeEnv) (stmts: Stmt list): Result<TypeEnv, TypeErrors> =
    let rec helper env accErrors stmts =
        match stmts with
        | [] -> if accErrors = [] then Ok env else Error accErrors
        | stmt :: rest ->
            let env', result = checkStmt env stmt

            match result with
            | Error errors -> helper env' (accErrors @ errors) rest
            | Ok _ -> helper env' accErrors rest

    helper env [] stmts

let checkProgram (program: Program) =
    match checkStmts defaultTypeEnv program with
    | Ok _ -> ()
    | Error errors -> raise (TypeException errors)


let rec formatTypeError (error: TypeError) : string =
    match error with
    | UndefinedVariable token -> $"Undefined variable {token.Lexeme} at Line: {token.Position.Line}"
    | UndefinedFunction token -> $"Undefined function {token.Lexeme} at Line: {token.Position.Line}"
    | UndefinedType token -> $"Undefined type {token.Lexeme} at Line: {token.Position.Line}"
    | TypeMismatch(token, expected, actual) -> $"Type mismatch at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidAssignment(token, expected, actual) ->
        $"Invalid assignment at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidArgumentCount(expr, expected, actual) ->
        $"Invalid argument count at expr: {expr}, expected {expected}, got {actual}"
    | InvalidArgumentType(expr, expected, actual) ->
        $"Invalid argument type at expr: {expr}, expected {expected}, got {actual}"
    | InvalidReturnType(token, expected, actual) ->
        $"Invalid return type at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidOperandType(token, expected, actual) ->
        $"Invalid operand type at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidOperator(token, typ) -> $"Invalid operator at Line: {token.Position.Line}, got {typ}"
    | InvalidFunctionType(token, typ) -> $"Invalid function type at Line: {token.Position.Line}, got {typ}"
    | InvalidFunction(token, typ) -> $"Invalid function at Line: {token.Position.Line}, got {typ}"
    | InvalidFunctionArgument(token, expected, actual) ->
        $"Invalid function argument at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidFunctionReturn(token, expected, actual) ->
        $"Invalid function return at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidFunctionBody(token, expected, actual) ->
        $"Invalid function body at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidBlock(token, expected, actual) -> $"Invalid block at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidCall(expr, typ) -> $"Invalid call at expr: {expr}, got {typ}"
    | InvalidCallType(expr, expected, actual) ->
        $"Invalid call type at expr: {expr}, expected {expected}, got {actual}"
    | InvalidCallReturn(token, expected, actual) ->
        $"Invalid call return at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | InvalidCallBody(token, expected, actual) ->
        $"Invalid call body at Line: {token.Position.Line}, expected {expected}, got {actual}"
    | NotEnoughInformation(token) -> $"Not enough information at Line: {token.Position.Line}"
    | InvalidOpen(token) -> $"Invalid open statement at Line: {token.Position.Line}"
    | InvalidIf(expr) -> $"Invalid if statement at expr: {expr}"
    | InvalidIndex(expr, typ) -> $"Invalid index at expr: {expr}, got {typ}"
    | InvalidAssert(expr, typ) -> $"Invalid assert at expr: {expr}, got {typ}"

let formatTypeErrors (errors: TypeError list) : string =
    List.map formatTypeError errors |> String.concat "\n"
