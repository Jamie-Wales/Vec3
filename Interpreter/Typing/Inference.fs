module Vec3.Interpreter.Typing.Inference

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Builtins
open Exceptions

type TypeEnv = Map<string, TType>

type Substitution = Map<TypeVar, TType>

let defaultTypeEnv = BuiltinFunctions

let combineMaps map1 map2 =
    Map.fold (fun acc key value -> Map.add key value acc) map2 map1

let freshTypeVar =
    let counter = ref 0

    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

let checkLiteral (lit: Literal) : TType =
    match lit with
    | LNumber(LInteger _) -> TInteger
    | LNumber(LFloat _) -> TFloat
    | LNumber(LRational _) -> TRational
    | LNumber(LComplex _) -> TComplex

    | LString _ -> TString
    | LBool _ -> TBool
    | LUnit -> TUnit

let checkIdentifier (env: TypeEnv) (token: Token) : Result<TType, TypeErrors> =
    match token.lexeme with
    | Identifier name ->
        match Map.tryFind name env with
        | Some t -> Ok t
        | None -> Error [ TypeError.UndefinedVariable token ]
    | _ -> Error [ TypeError.UndefinedVariable token ]

// attempts to substitute type variables with concrete types
let rec applySubstitution (sub: Substitution) (t: TType) : TType =
    match t with
    | TTypeVariable tv ->
        match Map.tryFind tv sub with
        | Some t' -> applySubstitution sub t'
        | None -> t
    | TFunction(paramsTypes, retType) ->
        let newParams = List.map (applySubstitution sub) paramsTypes
        let newRet = applySubstitution sub retType
        TFunction(newParams, newRet)
    | TTuple types -> TTuple(List.map (applySubstitution sub) types)
    | TList typ -> TList(applySubstitution sub typ)
    | TVector(typ, size) -> TVector(applySubstitution sub typ, size)
    | TMatrix(typ, rows, cols) -> TMatrix(applySubstitution sub typ, rows, cols)
    | TConstrain(var, types) ->
        match Map.tryFind var sub with
        | Some t' -> applySubstitution sub t'
        | None -> TConstrain(var, List.map (applySubstitution sub) types)
    | t -> t

// attempts to substitute type variables with concrete types in an environment
let applySubstitutionToEnv (sub: Substitution) (env: TypeEnv) : TypeEnv =
    Map.map (fun _ -> applySubstitution sub) env

type ResolvedType = Map<TypeVar, TType>

// make this immutable later, as it specialises functions too much
let resolvedTypes: Ref<ResolvedType> = ref Map.empty

// attempts to unify two types
let rec unify (t1: TType) (t2: TType) : Result<Substitution, TypeErrors> =
    match t1, t2 with
    | TInteger, TInteger
    | TFloat, TFloat
    | TRational, TRational
    | TComplex, TComplex

    | TBool, TBool
    | TString, TString
    | TUnit, TUnit

    | TNever, TNever
    | TAny, TAny -> Ok Map.empty

    | TTypeVariable tv, t
    | t, TTypeVariable tv ->
        if t = TTypeVariable tv then
            Ok Map.empty
        else if occursCheck tv t then
            Error [ TypeError.TypeMismatch(Empty, TTypeVariable tv, t) ]
        else
            Ok <| Map.add tv t Map.empty

    | TConstrain(var1, types1), TConstrain(var2, types2) ->
        if var1 <> var2 then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else if List.length types1 <> List.length types2 then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            let results = List.map2 unify types1 types2 // assumes that the constraints are in the same order is this correct? and same length

            List.fold
                (fun acc result ->
                    match acc, result with
                    | Ok sub1, Ok sub2 -> Ok(combineMaps sub1 sub2)
                    | Error errors, Ok _ -> Error errors
                    | Ok _, Error errors -> Error errors
                    | Error errors1, Error errors2 -> Error(errors1 @ errors2))
                (Ok Map.empty)
                results

    | TConstrain(var, types), t
    | t, TConstrain(var, types) ->
        if occursCheck var t then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            match Map.tryFind var resolvedTypes.Value with
            | Some t' when t' <> t -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
            | _ ->
                let resolvedType = List.tryFind (fun ty -> unify t ty |> Result.isOk) types
                
                
                match resolvedType with
                | Some ty ->
                    let sub = unify t ty
                    resolvedTypes.Value <- Map.add var ty resolvedTypes.Value
                    sub
                | None -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
            
            
            // // let results = List.map (unify t) types
            //

    | TFunction(params1, ret1), TFunction(params2, ret2) ->
        if List.length params1 <> List.length params2 then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            let paramResults = List.map2 unify params1 params2
            let retResult = unify ret1 ret2


            let combinedResults =
                List.fold
                    (fun acc result ->
                        match acc, result with
                        | Ok sub1, Ok sub2 -> Ok(combineMaps sub1 sub2)
                        | Error errors, Ok _ -> Error errors
                        | Ok _, Error errors -> Error errors
                        | Error errors1, Error errors2 -> Error(errors1 @ errors2))
                    (Ok Map.empty)
                    paramResults

            match combinedResults, retResult with
            | Ok sub1, Ok sub2 -> Ok(combineMaps sub1 sub2)
            | Error errors, Ok _ -> Error errors
            | Ok _, Error errors -> Error errors
            | Error errors1, Error errors2 -> Error(errors1 @ errors2)

    | TTuple types1, TTuple types2 ->
        let results = List.map2 unify types1 types2

        List.fold
            (fun acc result ->
                match acc, result with
                | Ok sub1, Ok sub2 -> Ok(combineMaps sub1 sub2)
                | Error errors, Ok _ -> Error errors
                | Ok _, Error errors -> Error errors
                | Error errors1, Error errors2 -> Error(errors1 @ errors2))
            (Ok Map.empty)
            results

    | TList typ1, TList typ2 -> unify typ1 typ2
    | TVector(typ1, size1), TVector(typ2, size2) ->
        if size1 = size2 then
            unify typ1 typ2
        else
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
    | TMatrix(typ1, rows1, cols1), TMatrix(typ2, rows2, cols2) ->
        if rows1 = rows2 && cols1 = cols2 then
            unify typ1 typ2
        else
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
    | _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]

and occursCheck (tv: TypeVar) (t: TType) : bool =
    match t with
    | TTypeVariable v -> v = tv
    | TFunction(parameters, ret) -> List.exists (occursCheck tv) parameters || occursCheck tv ret
    | TTuple types -> List.exists (occursCheck tv) types
    | TList typ -> occursCheck tv typ
    | TVector(typ, _) -> occursCheck tv typ
    | TMatrix(typ, _, _) -> occursCheck tv typ
    | TConstrain(var, types) -> List.exists (occursCheck tv) types || var = tv
    | _ -> false

let rec freeTypeVars (typ: TType) : TypeVar list =
    match typ with
    | TTypeVariable tv -> [ tv ]
    | TFunction(paramTypes, retType) -> List.collect freeTypeVars paramTypes @ freeTypeVars retType
    | TTuple types -> List.collect freeTypeVars types
    | TList typ -> freeTypeVars typ
    | TVector(typ, _) -> freeTypeVars typ
    | TMatrix(typ, _, _) -> freeTypeVars typ
    | TConstrain(var, types) -> var :: (List.collect freeTypeVars types)
    | _ -> []

let freeTypeVarsInEnv (env: TypeEnv) : TypeVar list =
    env |> Map.toList |> List.collect (fun (_, typ) -> freeTypeVars typ)

// let defaultTypeEnv =
//     List.fold (fun acc (name, typ) -> Map.add name (Forall([] ,typ)) acc) Map.empty BuiltinFunctions


let rec infer (env: TypeEnv) (expr: Expr) : Result<TType * Substitution * Expr, TypeErrors> =
    match expr with
    | ELiteral (lit, _) ->
        let t = checkLiteral lit
        Ok (t, Map.empty, ELiteral(lit, t))
        
    | EIdentifier (token, _) ->
        match checkIdentifier env token with
        | Ok t -> Ok(t, Map.empty, EIdentifier(token, t))
        | Error errors -> Error errors
    | ELambda(paramList, returnType, body, _) ->
        let paramTypes = List.map snd paramList
        // if List.length paramList > 1 && List.length (List.filter (fun t -> t = TInfer) paramTypes) = List.length paramList then
        //     let token = List.tryFind (fun (_, typ) -> typ = TInfer) paramList
        //     match token with
        //     | Some (t, _) ->
        //         Error [ TypeError.NotEnoughInformation(t) ]
        //     | None -> Error [ TypeError.NotEnoughInformation(Empty) ]
        // else
        let paramTypes =
            List.map
                (fun t ->
                    match t with
                    | TInfer -> TTypeVariable(freshTypeVar ())
                    | _ -> t)
                paramTypes

        let newEnv =
            List.fold2
                (fun acc (param, _) typ ->
                    match param.lexeme with
                    | Identifier name -> Map.add name typ acc
                    | _ -> acc)
                env
                paramList
                paramTypes

        let bodyResult = infer newEnv body

        match bodyResult with
        | Ok(bodyType, sub, expr) ->
            let paramTypes = List.map (applySubstitution sub) paramTypes

            if returnType = TInfer then
                Ok(TFunction(paramTypes, bodyType), sub, ELambda(paramList, bodyType, expr, TFunction(paramTypes, bodyType)))
            else
                let returnResult = unify bodyType returnType

                match returnResult with
                | Ok sub' ->
                    let sub = combineMaps sub sub'
                    let returnType = applySubstitution sub returnType
                    Ok(TFunction(paramTypes, returnType), sub, ELambda(paramList, returnType, expr, TFunction(paramTypes, returnType)))
                | Error errors -> Error errors
        | Error errors -> Error errors

    | ECall(callee, args, _) ->
        let calleeResult = infer env callee

        match calleeResult with
        | Error errors -> Error errors
        | Ok(t, sub, expr) ->
            let t = applySubstitution sub t

            match t with
            | TFunction(paramTypes, ret) ->
                if List.length paramTypes <> List.length args then
                    Error [ TypeError.InvalidArgumentCount(callee, List.length paramTypes, List.length args) ]
                else
                    let argResults = List.map (infer env) args

                    if
                        List.exists
                            (fun result ->
                                match result with
                                | Error _ -> true
                                | _ -> false)
                            argResults
                    then
                        let errors =
                            List.collect
                                (fun result ->
                                    match result with
                                    | Error errors -> errors
                                    | _ -> [])
                                argResults

                        Error errors
                    else
                        let argResults =
                            List.map
                                (fun result ->
                                    match result with
                                    | Ok(t, sub, expr) -> (t, sub, expr)
                                    | _ -> failwith "Impossible")
                                argResults

                        let argTypes = List.map (fun (t, _, _) -> t) argResults
                        let argSubs = List.map (fun (_, sub, _) -> sub) argResults
                        let argExprs = List.map (fun (_, _, expr) -> expr) argResults

                        let paramResults = List.map2 unify paramTypes argTypes

                        if
                            List.exists
                                (fun result ->
                                    match result with
                                    | Error _ -> true
                                    | _ -> false)
                                paramResults
                        then
                            let errors =
                                List.collect
                                    (fun result ->
                                        match result with
                                        | Error errors -> errors
                                        | _ -> [])
                                    paramResults
                            

                            Error errors
                        else
                            let paramResults =
                                List.map
                                    (fun result ->
                                        match result with
                                        | Ok sub -> sub
                                        | _ -> failwith "Impossible")
                                    paramResults
                            
                            let combinedSubs = List.fold combineMaps Map.empty paramResults
                            let combinedSubs = List.fold combineMaps combinedSubs argSubs
                            
                            let returnType = applySubstitution combinedSubs ret
                            
                            match returnType with
                            | TConstrain(_, types) ->
                                let resolvedType = List.tryFind (fun ty -> unify returnType ty |> Result.isOk) types
                                
                                match resolvedType with
                                | Some ty ->
                                    let sub = unify returnType ty
                                    match sub with
                                    | Ok sub' ->
                                        let combinedSubs = combineMaps combinedSubs sub'
                                        let returnType = applySubstitution combinedSubs ty
                                        Ok(returnType, combinedSubs, ECall(expr, argExprs, returnType))
                                    | Error errors -> Error errors
                                | None -> Error [ TypeError.InvalidCall(callee, t) ]
                            | _ -> Ok(returnType, combinedSubs, ECall(expr, argExprs, returnType))
                            
            | _ -> Error [ TypeError.InvalidCall(callee, t) ]

    | EBinary(expr1, op, expr2, _) ->
        let expr1Result = infer env expr1
        let expr2Result = infer env expr2

        match expr1Result, expr2Result with
        | Ok(t1, sub1, expr1), Ok(t2, sub2, expr2) ->
            let sub = combineMaps sub1 sub2
            let typeVar = freshTypeVar ()

            let opType =
                match op.lexeme with
                | Operator Plus
                | Operator Minus
                | Operator Star
                | Operator Slash
                | Operator StarStar ->
                    match t1, t2 with
                    | TInteger, _
                    | _, TInteger -> TInteger
                    | TFloat, _
                    | _, TFloat -> TFloat
                    | TRational, _
                    | _, TRational -> TRational
                    | TComplex, _
                    | _, TComplex -> TComplex
                    | _ -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ])


                | Operator Percent -> TInteger

                | Operator EqualEqual ->
                    TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex; TString; TBool; TUnit ])
                | Operator BangEqual ->
                    TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex; TString; TBool; TUnit ])

                | Operator Less -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ])
                | Operator LessEqual -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ])
                | Operator Greater -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ])
                | Operator GreaterEqual -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ])
                | _ -> TNever

            let returnType =
                match op.lexeme with
                | Operator Plus
                | Operator Minus
                | Operator Star
                | Operator Slash
                | Operator StarStar ->
                    match t1, t2 with
                    | TInteger, _
                    | _, TInteger -> TInteger
                    | TFloat, _
                    | _, TFloat -> TFloat
                    | TRational, _
                    | _, TRational -> TRational
                    | TComplex, _
                    | _, TComplex -> TComplex
                    | _ -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ])

                | Operator Percent -> TInteger

                | Operator EqualEqual
                | Operator BangEqual -> TBool

                | Operator Less
                | Operator LessEqual
                | Operator Greater
                | Operator GreaterEqual -> TBool

                | _ -> TNever

            let opResult = unify opType t1

            match opResult with
            | Ok sub' ->
                let sub = combineMaps sub sub'
                let opResult = unify opType t2

                match opResult with
                | Ok sub'' ->
                    let sub = combineMaps sub'' sub
                    let opResult = unify opType returnType

                    match opResult with
                    | Ok sub''' ->
                        let sub = combineMaps sub sub'''
                        let returnT = applySubstitution sub returnType
                        Ok(returnT, sub, EBinary(expr1, op, expr2, returnT))
                    | Error errors -> Error errors
                | Error errors -> Error errors
            | Error errors -> Error errors
        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors

    | EUnary(op, expr, _) ->
        let exprResult = infer env expr

        match exprResult with
        | Ok(t, sub, expr) ->
            let typeVar = freshTypeVar ()

            let opType =
                match op.lexeme with
                | Operator Minus -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ]) // maybe make
                // this a type var
                // if i have a function (x, y) -> x + y, then x and y should be inferred to the same Constrain type, so that only one type can be passed in
                | Operator Plus -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex ])
                | Operator Bang -> TBool
                | _ -> failwith "todo!!!"

            let opResult = unify opType t

            match opResult with
            | Ok sub' ->
                let sub = combineMaps sub sub'
                let returnT = applySubstitution sub t
                Ok(returnT, sub, EUnary(op, expr, returnT))
            | Error errors -> Error errors
        | Error errors -> Error errors

    | EBlock(stmts, _) ->
        // infer whole block, return type of last statement
        let result = inferProgram env stmts

        match result with
        | Ok(env, sub, _) ->
            let lastStmt = List.last stmts
            let lastStmtResult = inferStmt env lastStmt

            match lastStmtResult with
            | Ok(env, sub', _) ->
                let sub = combineMaps sub sub'

                let lastStmtType =
                    match lastStmt with
                    | SExpression (expr, _) ->
                        match infer env expr with
                        | Ok(t, _, _) -> t
                        | Error _ -> TNever
                    | SVariableDeclaration _ -> TUnit
                    | SPrintStatement _ -> TUnit

                Ok(lastStmtType, sub, EBlock(stmts, lastStmtType))
            | Error errors -> Error errors
        | Error errors -> Error errors

    | EGrouping(expr, _) -> infer env expr
    | EIf(cond, thenBranch, elseBranch, _) ->
        let condResult = infer env cond
        let thenResult = infer env thenBranch
        let elseResult = infer env elseBranch

        match condResult, thenResult, elseResult with
        | Ok(TBool, sub1, expr1), Ok(t1, sub2, expr2), Ok(t2, sub3, expr3) ->
            let sub = combineMaps sub1 sub2
            let sub = combineMaps sub sub3
            let result = unify t1 t2

            match result with
            | Ok sub' ->
                let sub = combineMaps sub sub'
                let returnT = applySubstitution sub t1
                Ok(returnT, sub, EIf(expr1, expr2, expr3, returnT))
            | Error errors -> Error errors
        | Error errors, _, _ -> Error errors
        | _, Error errors, _ -> Error errors
        | _, _, Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidIf(cond) ]
    | ETernary(cond, trueBranch, falseBranch, typ) -> infer env (EIf(cond, trueBranch, falseBranch, typ))
    | _ -> failwith "todo"

and inferStmt (env: TypeEnv) (stmt: Stmt) : Result<TypeEnv * Substitution * Stmt, TypeErrors> =
    resolvedTypes.Value <- Map.empty
    match stmt with
    | SExpression (expr, typ) ->
        let result = infer env expr

        match result with
        | Ok(_, sub, expr) -> Ok(env, sub, SExpression(expr, typ))
        | Error errors -> Error errors
    | SVariableDeclaration(name, typ, expr, _) ->
        let result = infer env expr

        match result with
        | Ok(t, sub, expr) ->
            let typ =
                match typ with
                | TInfer -> t
                | _ -> typ

            let subResult = unify t typ

            match subResult with
            | Ok sub' ->
                let sub = combineMaps sub sub'

                let newEnv =
                    match name with
                    | { lexeme = Identifier name } -> Map.add name (applySubstitution sub typ) env
                    | _ -> env

                Ok(newEnv, sub, SVariableDeclaration(name, typ, expr, typ))
            | Error errors -> Error errors

        | Error errors -> Error errors
    | SPrintStatement _ -> Ok(env, Map.empty, stmt)

and inferProgram (env: TypeEnv) (stmts: Stmt list) : Result<TypeEnv * Substitution * Stmt list, TypeErrors> =
    List.fold
        (fun acc stmt ->
            match acc with
            | Ok(env, sub, stmts) ->
                let result = inferStmt env stmt

                match result with
                | Ok(env', sub', stmt) ->
                    let sub = combineMaps sub sub'
                    Ok(env', sub, stmts @ [ stmt ])
                | Error errors -> Error errors
            | Error errors -> Error errors)
        (Ok(env, Map.empty, []))
        stmts

let quickInferStmt (env: TypeEnv) (stmt: Stmt) =
    match inferStmt env stmt with
    | Ok(env, _, _) -> env
    | Error errors -> raise <| TypeException errors
