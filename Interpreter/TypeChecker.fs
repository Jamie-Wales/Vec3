module Vec3.Interpreter.TypeChecker

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
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

// type Scheme = Forall of TypeVar list * Grammar.Type

// let combineMaps map1 map2 =
//     Map.fold (fun acc key value -> Map.add key value acc) map2 map1

// let freshTypeVar =
//     let counter = ref 0
//     fun () ->
//         counter.Value <- counter.Value + 1
//         TTypeVariable counter.Value

type TypeEnv = Map<string, Grammar.Type>

type TypeError =
    | UndefinedVariable of Token
    | UndefinedFunction of Token
    | UndefinedType of Token
    | TypeMismatch of Token * Grammar.Type * Grammar.Type
    | InvalidAssignment of Token * Grammar.Type * Grammar.Type
    | InvalidArgumentCount of Token * int * int
    | InvalidArgumentType of Token * Grammar.Type * Grammar.Type
    | InvalidReturnType of Token * Grammar.Type * Grammar.Type
    | InvalidOperandType of Token * Grammar.Type * Grammar.Type
    | InvalidOperator of Token * Grammar.Type
    | InvalidFunctionType of Token * Grammar.Type
    | InvalidFunction of Token * Grammar.Type
    | InvalidFunctionArgument of Token * Grammar.Type * Grammar.Type
    | InvalidFunctionReturn of Token * Grammar.Type * Grammar.Type
    | InvalidFunctionBody of Token * Grammar.Type * Grammar.Type
    | InvalidBlock of Token * Grammar.Type * Grammar.Type
    | InvalidCall of Token * Grammar.Type
    | InvalidCallType of Token * Grammar.Type * Grammar.Type
    | InvalidCallReturn of Token * Grammar.Type * Grammar.Type
    | InvalidCallBody of Token * Grammar.Type * Grammar.Type
    | TypeErrors of TypeError list

exception TypeException of TypeError

let BuiltinFunctions =
    [ "print", TFunction([ TAny ], TUnit)
      "input", TFunction([], TString)
      "cos", TFunction([ TFloat ], TFloat)
      "sin", TFunction([ TFloat ], TFloat)
      "tan", TFunction([ TFloat ], TFloat) ]
      

type Result<'T> =
    | Ok of 'T
    | Errors of TypeError list
    

let defaultTypeEnv =
    List.fold (fun acc (name, typ) -> Map.add name typ acc) Map.empty BuiltinFunctions

let checkLiteral (lit: Literal) : Grammar.Type =
    match lit with
    | Literal.TNumber(TNumber.Integer _) -> TInteger
    | Literal.TNumber(TNumber.Float _) -> TFloat
    | Literal.TNumber(TNumber.Rational _) -> TRational
    | Literal.TNumber(TNumber.Complex _) -> TComplex

    | Literal.String _ -> TString
    | Literal.Bool _ -> TBool
    | Literal.Unit -> TUnit

let checkIdentifier (env: TypeEnv) (token: Token) : Result<Grammar.Type> =
    match token.lexeme with
    | Identifier name ->
        match Map.tryFind name env with
        | Some t -> Ok t
        | None -> Errors [ TypeError.UndefinedVariable token ]
    | _ -> Errors [ TypeError.UndefinedVariable token ]

let rec checkExpr (env: TypeEnv) (expr: Expr) : Result<Grammar.Type> =
    match expr with
    | Expr.Literal lit -> Ok <| checkLiteral lit
    | Expr.Identifier token -> checkIdentifier env token
    | Expr.Unary(op, expr) ->
        let exprType = checkExpr env expr

        match exprType with
        | Ok TInteger when op.lexeme = Operator Minus -> Ok TInteger
        | Ok TFloat when op.lexeme = Operator Minus -> Ok TFloat
        | Ok TRational when op.lexeme = Operator Minus -> Ok TRational

        | Ok TInteger when op.lexeme = Operator Plus -> Ok TInteger
        | Ok TFloat when op.lexeme = Operator Plus -> Ok TFloat
        | Ok TRational when op.lexeme = Operator Plus -> Ok TRational

        | Ok TBool when op.lexeme = Operator Bang -> Ok TBool
        | Ok TInteger when op.lexeme = Operator Bang -> Ok TInteger
        | Ok TFloat when op.lexeme = Operator Bang -> Ok TFloat
        | Ok TRational when op.lexeme = Operator Bang -> Ok TRational

        | Ok t -> Errors [ TypeError.InvalidOperator(op, t) ]
        | Errors errors -> Errors errors
    | Expr.Binary(lhs, op, rhs) ->
        let lhsType = checkExpr env lhs
        let rhsType = checkExpr env rhs

        match lhsType, rhsType with
        | Ok TInteger, Ok TInteger when op.lexeme = Operator Plus -> Ok TInteger
        | Ok TFloat, Ok TFloat when op.lexeme = Operator Plus -> Ok TFloat
        | Ok TRational, Ok TRational when op.lexeme = Operator Plus -> Ok TRational
        | Ok TRational, Ok TInteger when op.lexeme = Operator Plus -> Ok TRational

        | Ok TInteger, Ok TInteger when op.lexeme = Operator Minus -> Ok TInteger
        | Ok TFloat, Ok TFloat when op.lexeme = Operator Minus -> Ok TFloat
        | Ok TRational, Ok TRational when op.lexeme = Operator Minus -> Ok TRational
        | Ok TRational, Ok TInteger when op.lexeme = Operator Minus -> Ok TRational

        | Ok TInteger, Ok TInteger when op.lexeme = Operator Star -> Ok TInteger
        | Ok TFloat, Ok TFloat when op.lexeme = Operator Star -> Ok TFloat
        | Ok TRational, Ok TRational when op.lexeme = Operator Star -> Ok TRational
        | Ok TRational, Ok TInteger when op.lexeme = Operator Star -> Ok TRational

        | Ok TInteger, Ok TInteger when op.lexeme = Operator Slash -> Ok TInteger
        | Ok TFloat, Ok TFloat when op.lexeme = Operator Slash -> Ok TFloat
        | Ok TRational, Ok TRational when op.lexeme = Operator Slash -> Ok TRational
        | Ok TRational, Ok TInteger when op.lexeme = Operator Slash -> Ok TRational

        | Ok TInteger, Ok TInteger when op.lexeme = Operator StarStar -> Ok TInteger
        | Ok TFloat, Ok TFloat when op.lexeme = Operator StarStar -> Ok TFloat
        | Ok TRational, Ok TRational when op.lexeme = Operator StarStar -> Ok TRational
        | Ok TRational, Ok TInteger when op.lexeme = Operator StarStar -> Ok TRational

        | Ok TInteger, Ok TInteger when op.lexeme = Operator EqualEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.lexeme = Operator EqualEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.lexeme = Operator EqualEqual -> Ok TBool
        | Ok TBool, Ok TBool when op.lexeme = Operator EqualEqual -> Ok TBool

        | Ok TInteger, Ok TInteger when op.lexeme = Operator BangEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.lexeme = Operator BangEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.lexeme = Operator BangEqual -> Ok TBool
        | Ok TInteger, Ok TInteger when op.lexeme = Operator Less -> Ok TBool

        | Ok TInteger, Ok TInteger when op.lexeme = Operator Less -> Ok TBool
        | Ok TFloat, Ok TFloat when op.lexeme = Operator Less -> Ok TBool
        | Ok TRational, Ok TRational when op.lexeme = Operator Less -> Ok TBool

        | Ok TInteger, Ok TInteger when op.lexeme = Operator LessEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.lexeme = Operator LessEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.lexeme = Operator LessEqual -> Ok TBool

        | Ok TInteger, Ok TInteger when op.lexeme = Operator Greater -> Ok TBool
        | Ok TFloat, Ok TFloat when op.lexeme = Operator Greater -> Ok TBool
        | Ok TRational, Ok TRational when op.lexeme = Operator Greater -> Ok TBool

        | Ok TInteger, Ok TInteger when op.lexeme = Operator GreaterEqual -> Ok TBool
        | Ok TFloat, Ok TFloat when op.lexeme = Operator GreaterEqual -> Ok TBool
        | Ok TRational, Ok TRational when op.lexeme = Operator GreaterEqual -> Ok TBool

        | Errors errors, Errors errors' -> Errors(errors @ errors')
        | Errors errors, Ok _ -> Errors errors
        | Ok _, Errors errors -> Errors errors
        | Ok t, Ok t' -> Errors [ TypeError.InvalidOperandType(op, t, t') ]
    | Expr.Grouping expr -> checkExpr env expr
    | Expr.Assignment(token, expr) ->
        let exprType = checkExpr env expr

        match token.lexeme with
        | Identifier name ->
            match Map.tryFind name env with
            | Some t ->
                match exprType with
                | Ok t' when t = t' -> Ok t
                | Ok t' -> Errors [ TypeError.InvalidAssignment(token, t, t') ]
                | Errors errors -> Errors errors
            | None -> Errors [ TypeError.UndefinedVariable token ]
        | _ -> Errors [ TypeError.UndefinedVariable token ]
    | Expr.Call(callee, args) ->
        let calleeType = checkExpr env (Expr.Identifier callee)

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
                Errors [ TypeError.InvalidArgumentCount(callee, List.length paramList, List.length args) ]
            else if not validArgs then
                let errors =
                    List.choose
                        (function
                        | Errors errs -> Some errs
                        | _ -> None)
                        argResults
                    |> List.concat

                Errors errors
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
                    Errors [ TypeError.InvalidArgumentType(callee, paramList.Head, argTypes.Head) ]
        | Errors errors -> Errors errors
        | _ -> Errors [ TypeError.InvalidCallType(callee, TInfer, TInfer) ] // fix

    // need better type inference here for params, unless params must be typed
    | Expr.Lambda(paramList, returnType, body) ->
        let newEnv =
            List.fold
                (fun acc (param, typ) ->
                    match param.lexeme with
                    | Identifier name -> Map.add name typ acc
                    | _ -> raise (TypeException(TypeError.UndefinedVariable param)))
                env
                paramList

        let bodyType = checkExpr newEnv body

        match bodyType with
        | Ok bodyType ->
            if bodyType = returnType then
                Ok <| TFunction(List.map snd paramList, returnType)
            else if returnType = TInfer then
                Ok <| TFunction(List.map snd paramList, bodyType)
            else
                Errors [ TypeError.InvalidFunctionReturn(fst paramList.Head, returnType, bodyType) ]
        | Errors errors -> Errors errors
    | Expr.Block stmts ->
        let rec checkBlock (env: TypeEnv) (stmts: Stmt list) : Result<Grammar.Type> =
            match stmts with
            | [] -> Ok TUnit
            | [ stmt ] ->
                match stmt with
                | Stmt.Expression expr -> checkExpr env expr
                | Stmt.VariableDeclaration _ -> Ok TUnit
                | Stmt.PrintStatement _ -> Ok TUnit
            | stmt :: rest ->
                let env', _ = checkStmt env stmt
                checkBlock env' rest

        checkBlock env stmts

and checkStmt (env: TypeEnv) (stmt: Stmt) : TypeEnv * Result<Grammar.Type> =
    match stmt with
    | Stmt.Expression expr ->
        let exprType = checkExpr env expr

        match exprType with
        | Errors errors -> env, Errors errors
        | Ok exprType -> env, Ok exprType

    | Stmt.VariableDeclaration(token, typ, expr) ->
        let exprType = checkExpr env expr

        match exprType with
        | Errors errors -> env, Errors errors
        | Ok exprType ->
            if typ = exprType then
                match token.lexeme with
                | Identifier name -> Map.add name typ env, Ok TUnit
                | _ -> env, Errors [ TypeError.UndefinedVariable token ]
            else if typ = TInfer then
                match token.lexeme with
                | Identifier name -> Map.add name exprType env, Ok TUnit
                | _ -> env, Errors [ TypeError.UndefinedVariable token ]
            else
                env, Errors [ TypeError.TypeMismatch(token, typ, exprType) ]
    | PrintStatement(expr) -> (env, checkExpr env expr)

let rec checkStmts (env: TypeEnv) (stmts: Stmt list) =
    let rec helper env accErrors stmts =
        match stmts with
        | [] -> if accErrors = [] then Ok env else Errors accErrors
        | stmt :: rest ->
            let env', result = checkStmt env stmt

            match result with
            | Errors errors -> helper env' (accErrors @ errors) rest
            | Ok _ -> helper env' accErrors rest

    helper env [] stmts

let checkProgram (program: Program) =
    match checkStmts defaultTypeEnv program with
    | Ok _ -> ()
    | Errors errors -> raise (TypeException(TypeErrors errors))


let rec formatTypeError (error: TypeError) : string =
    match error with
    | UndefinedVariable token -> $"Undefined variable {token.lexeme} at Line: {token.line}"
    | UndefinedFunction token -> $"Undefined function {token.lexeme} at Line: {token.line}"
    | UndefinedType token -> $"Undefined type {token.lexeme} at Line: {token.line}"
    | TypeMismatch(token, expected, actual) -> $"Type mismatch at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidAssignment(token, expected, actual) ->
        $"Invalid assignment at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidArgumentCount(token, expected, actual) ->
        $"Invalid argument count at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidArgumentType(token, expected, actual) ->
        $"Invalid argument type at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidReturnType(token, expected, actual) ->
        $"Invalid return type at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidOperandType(token, expected, actual) ->
        $"Invalid operand type at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidOperator(token, typ) -> $"Invalid operator at Line: {token.line}, got {typ}"
    | InvalidFunctionType(token, typ) -> $"Invalid function type at Line: {token.line}, got {typ}"
    | InvalidFunction(token, typ) -> $"Invalid function at Line: {token.line}, got {typ}"
    | InvalidFunctionArgument(token, expected, actual) ->
        $"Invalid function argument at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidFunctionReturn(token, expected, actual) ->
        $"Invalid function return at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidFunctionBody(token, expected, actual) ->
        $"Invalid function body at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidBlock(token, expected, actual) -> $"Invalid block at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidCall(token, typ) -> $"Invalid call at Line: {token.line}, got {typ}"
    | InvalidCallType(token, expected, actual) ->
        $"Invalid call type at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidCallReturn(token, expected, actual) ->
        $"Invalid call return at Line: {token.line}, expected {expected}, got {actual}"
    | InvalidCallBody(token, expected, actual) ->
        $"Invalid call body at Line: {token.line}, expected {expected}, got {actual}"
    | TypeErrors errors -> String.concat "\n" (List.map formatTypeError errors)

let formatTypeErrors (errors: TypeError list) : string =
    List.map formatTypeError errors |> String.concat "\n"