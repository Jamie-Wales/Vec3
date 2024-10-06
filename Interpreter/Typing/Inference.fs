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

type ResolvedType = Map<TypeVar, TType>
type ResolvedDims = Map<TypeVar, Dims>

// make this immutable later, as it specialises functions too much
let resolvedTypes: Ref<ResolvedType> = ref Map.empty
let resolvedDims: Ref<ResolvedDims> = ref Map.empty

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
    | TTensor(typ, dims) ->
        let newTyp = applySubstitution sub typ
        match dims with
        | DAny -> TTensor(newTyp, DAny)
        | Dims sizes ->
            TTensor(newTyp, Dims sizes)
        | DVar v ->
            match Map.tryFind v resolvedDims.Value with
            | Some t' -> TTensor(newTyp, t')
            | None -> TTensor(newTyp, DVar v)
    | TConstrain(var, types) ->
        match Map.tryFind var sub with
        | Some t' -> applySubstitution sub t'
        | None -> TConstrain(var, List.map (applySubstitution sub) types)
    | t -> t

// attempts to substitute type variables with concrete types in an environment
let applySubstitutionToEnv (sub: Substitution) (env: TypeEnv) : TypeEnv =
    Map.map (fun _ -> applySubstitution sub) env




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
    
    | TInfer, _
    | _, TInfer -> Ok Map.empty
    
    | TAny, t
    | t, TAny -> Ok Map.empty

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
    
    | TTensor(typ1, sizes1), TTensor(typ2, sizes2) ->
        match sizes1, sizes2 with
        | DAny, DAny -> unify typ1 typ2
        | DAny, Dims sizes
        | Dims sizes, DAny ->
            unify typ1 typ2
        | Dims sizes1, Dims sizes2 ->
            if sizes1 = sizes2 || List.isEmpty sizes1 || List.isEmpty sizes2 then
                unify typ1 typ2
            else
                Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        | DVar v, Dims sizes
        | Dims sizes, DVar v ->
            let resolved = Map.tryFind v resolvedDims.Value
            match resolved with
            | Some (Dims sizes') when sizes = sizes' -> unify typ1 typ2
            | _ ->
                resolvedDims.Value <- Map.add v (Dims sizes) resolvedDims.Value // this is wrong
                unify typ1 typ2
        | DVar v1, DVar v2 ->
            if v1 = v2 then
                Ok Map.empty
            else
                let sub = Map.add v1 (TTensor(typ1, DVar v2)) Map.empty // this is wrong
                Ok sub
        | _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
    | _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]

and occursCheck (tv: TypeVar) (t: TType) : bool =
    match t with
    | TTypeVariable v -> v = tv
    | TFunction(parameters, ret) -> List.exists (occursCheck tv) parameters || occursCheck tv ret
    | TTuple types -> List.exists (occursCheck tv) types
    | TTensor(typ, var) -> occursCheck tv typ || match var with | DVar v -> v = tv | _ -> false
    | TConstrain(var, types) -> List.exists (occursCheck tv) types || var = tv
    | _ -> false

let rec freeTypeVars (typ: TType) : TypeVar list =
    match typ with
    | TTypeVariable tv -> [ tv ]
    | TFunction(paramTypes, retType) -> List.collect freeTypeVars paramTypes @ freeTypeVars retType
    | TTuple types -> List.collect freeTypeVars types
    | TTensor(typ, dims) ->
        match dims with
        | DVar v -> v :: freeTypeVars typ
        | _ -> freeTypeVars typ
    | TConstrain(var, types) -> var :: (List.collect freeTypeVars types)
    | _ -> []

let freeTypeVarsInEnv (env: TypeEnv) : TypeVar list =
    env |> Map.toList |> List.collect (fun (_, typ) -> freeTypeVars typ)

// let defaultTypeEnv =
//     List.fold (fun acc (name, typ) -> Map.add name (Forall([] ,typ)) acc) Map.empty BuiltinFunctions

let rec unifyWithSubstitution paramTypes argTypes currentSubs =
    match (paramTypes, argTypes) with
    | [], [] -> Ok currentSubs 
    | paramType::restParamTypes, argType::restArgTypes ->
        let paramType = applySubstitution currentSubs paramType
        let argType = applySubstitution currentSubs argType

        match unify paramType argType with
        | Error errors -> Error errors 
        | Ok newSub ->
            let combinedSubs = combineMaps currentSubs newSub

            let updatedRestParamTypes = List.map (applySubstitution combinedSubs) restParamTypes
            let updatedRestArgTypes = List.map (applySubstitution combinedSubs) restArgTypes

            unifyWithSubstitution updatedRestParamTypes updatedRestArgTypes combinedSubs

    | _ -> failwith "todo"


let rec infer (env: TypeEnv) (expr: Expr) : Result<TType * Substitution * Expr, TypeErrors> =
    match expr with
    | EAssignment (token, expr, _) ->
        let exprResult = infer env expr

        match exprResult with
        | Ok(t, sub, expr) ->
            let t = applySubstitution sub t
            Ok(t, sub, EAssignment(token, expr, t))
        | Error errors -> Error errors
    | ELiteral (lit, _) ->
        let t = checkLiteral lit
        Ok (t, Map.empty, ELiteral(lit, t))
        
    | EIdentifier (token, _) ->
        match checkIdentifier env token with
        | Ok t -> Ok(t, Map.empty, EIdentifier(token, t))
        | Error errors -> Error errors
    | ELambda(paramList, body, typ) ->
        let returnT, paramTypes = match typ with
                                    | TFunction(parameters, ret) -> ret, parameters
                                    | _ -> TInfer, List.map (fun _ -> TInfer) paramList
                                    
        let paramTypes =
            List.map
                (fun t ->
                    match t with
                    | TInfer -> TTypeVariable(freshTypeVar ())
                    | _ -> t)
                paramTypes
        

        let newEnv =
            List.fold2
                (fun acc param typ ->
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

            if returnT = TInfer then
                Ok(TFunction(paramTypes, bodyType), sub, ELambda(paramList, expr, TFunction(paramTypes, bodyType)))
            else
                let returnResult = unify bodyType returnT

                match returnResult with
                | Ok sub' ->
                    let sub = combineMaps sub sub'
                    let returnType = applySubstitution sub returnT
                    Ok(TFunction(paramTypes, returnType), sub, ELambda(paramList, expr, TFunction(paramTypes, returnType)))
                | Error errors -> Error errors
        | Error errors -> Error errors

    // todo, need to accoutn for type variables as the callee, then sub with a call type based on context
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
                        
                        let paramResults = unifyWithSubstitution paramTypes argTypes Map.empty
                        
                        match paramResults with
                        | Error errors -> Error errors
                        | Ok sub' ->
                            let combinedSubs = List.fold combineMaps sub' argSubs
                            
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
                            | _ ->
                                Ok(returnType, combinedSubs, ECall(expr, argExprs, returnType))
            
            | _ -> Error [ TypeError.InvalidCall(callee, t) ]

    | EBinary(expr1, op, expr2, _) ->
        let expr1Result = infer env expr1
        let expr2Result = infer env expr2

        match expr1Result, expr2Result with
        | Ok(t1, sub1, expr1), Ok(t2, sub2, expr2) ->
            let sub = combineMaps sub1 sub2
            let typeVar = freshTypeVar ()
            let dimsVar = freshTypeVar ()
                
            let vecVar = freshTypeVar ()

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
                    
                    | TTensor(_, DVar v), _
                    | _, TTensor(_, DVar v) -> TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex ]), 
                    DVar v)
                    | TTensor(_, Dims sizes), _
                    | _, TTensor(_, Dims sizes) -> TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex]), Dims sizes)
                    | TTensor(_, DAny), _
                    | _, TTensor(_, DAny) -> TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex ]), 
                    DAny)
                    
                    | _ -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex; TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex; ]), DVar dimsVar) ])


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
                    | TTensor(_, DVar v), _
                    | _, TTensor(_, DVar v) -> TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex ]), DVar v)
                    | TTensor(_, Dims sizes), _
                    | _, TTensor(_, Dims sizes) -> TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex ]),
                     Dims sizes)
                    | TTensor(_, DAny), _
                    | _, TTensor(_, DAny) -> TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex ]), 
                    DAny)
                    | _ -> TConstrain(typeVar, [ TInteger; TFloat; TRational; TComplex; TTensor(TConstrain(vecVar, [ TInteger; TFloat; TRational; TComplex; ]), DVar dimsVar) ])

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
    | ETuple(exprs, typ) -> 
        let results = List.map (infer env) exprs

        if
            List.exists
                (fun result ->
                    match result with
                    | Error _ -> true
                    | _ -> false)
                results
        then
            let errors =
                List.collect
                    (fun result ->
                        match result with
                        | Error errors -> errors
                        | _ -> [])
                    results

            Error errors
        else
            let results =
                List.map
                    (fun result ->
                        match result with
                        | Ok(t, sub, expr) -> (t, sub, expr)
                        | _ -> failwith "Impossible")
                    results

            let types = List.map (fun (t, _, _) -> t) results
            let subs = List.map (fun (_, sub, _) -> sub) results
            let exprs = List.map (fun (_, _, expr) -> expr) results

            let subResults = List.map2 unify types (List.replicate (List.length types - 1) TInfer)

            if
                List.exists
                    (fun result ->
                        match result with
                        | Error _ -> true
                        | _ -> false)
                    subResults
            then
                let errors =
                    List.collect
                        (fun result ->
                            match result with
                            | Error errors -> errors
                            | _ -> [])
                        subResults

                Error errors
            else
                let subResults =
                    List.map
                        (fun result ->
                            match result with
                            | Ok sub -> sub
                            | _ -> failwith "Impossible")
                        subResults

                let combinedSubs = List.fold combineMaps Map.empty subResults
                let combinedSubs = List.fold combineMaps combinedSubs subs

                let returnType = TTuple(List.map (applySubstitution combinedSubs) types)
                Ok(returnType, combinedSubs, ETuple(exprs, returnType))
    | EList(exprs, typ) ->
        // every element in the list must have the same type
        
        let results = List.map (infer env) exprs
        
        if
            List.exists
                (fun result ->
                    match result with
                    | Error _ -> true
                    | _ -> false)
                results
        then
            let errors =
                List.collect
                    (fun result ->
                        match result with
                        | Error errors -> errors
                        | _ -> [])
                    results

            Error errors
        else
            let results =
                List.map
                    (fun result ->
                        match result with
                        | Ok(t, sub, expr) -> (t, sub, expr)
                        | _ -> failwith "Impossible")
                    results

            let types = List.map (fun (t, _, _) -> t) results
            let subs = List.map (fun (_, sub, _) -> sub) results
            let exprs = List.map (fun (_, _, expr) -> expr) results
            
            // check all the types are the same
            let typeVar = freshTypeVar ()
            let subResults = unifyWithSubstitution types (List.replicate (List.length types) (TTypeVariable typeVar)) Map.empty
            
            match subResults with
            | Error errors -> Error errors
            | Ok sub' ->
                let combinedSubs = List.fold combineMaps Map.empty subs
                let combinedSubs = combineMaps combinedSubs sub'
                
                // let returnType = applySubstitution combinedSubs (TTypeVariable typeVar)
                let returnType = TTensor(applySubstitution combinedSubs (List.head types), Dims [List.length types])
                Ok(returnType, combinedSubs, EList(exprs, returnType))
            
    | EIndex(expr, index, _) ->
        let indexResult = infer env index
        let exprResult = infer env expr
        
        match indexResult, exprResult with
        | Ok(TInteger, sub1, expr1), Ok(TTensor(typ, sizes), sub2, expr2) ->
            let sub = combineMaps sub1 sub2
            let returnType = applySubstitution sub typ
            Ok(returnType, sub, EIndex(expr2, expr1, returnType))
        | Ok(TInteger, sub1, expr1), Ok(TTypeVariable n, sub2, expr2) ->
            let typeVar = freshTypeVar ()
            let dimsTypeVar = freshTypeVar ()
            let sub = combineMaps sub1 sub2
            
            let sub2 = Map.add n (TTensor(TTypeVariable typeVar, DVar dimsTypeVar )) sub
            let returnType = TTensor(TTypeVariable typeVar, DVar dimsTypeVar) // this isnt right
            Ok(TTypeVariable typeVar, sub2, EIndex(expr2, expr1, TTypeVariable typeVar))
        | Ok(TInteger, sub1, expr1), Ok(TTuple(types), sub2, expr2) -> failwith "todo"
        | Ok(TInteger, sub1, expr1), Ok(t, s, e) ->
            Error [ TypeError.InvalidIndex(expr, t) ]
        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidIndex(expr, TInfer) ]
        
            
        

and inferStmt (env: TypeEnv) (stmt: Stmt) : Result<TypeEnv * Substitution * Stmt, TypeErrors> =
    // make this immutable later, pass it around, or resolved in substitution
    resolvedTypes.Value <- Map.empty
    
    match stmt with
    | SExpression (expr, typ) ->
        let result = infer env expr

        match result with
        | Ok(_, sub, expr) -> Ok(env, sub, SExpression(expr, typ))
        | Error errors -> Error errors
    | SVariableDeclaration(name, expr, typ) ->
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
                
                let env = applySubstitutionToEnv sub env
                
                let typ = applySubstitution sub typ
                
                let env =
                    match name with
                    | { lexeme = Identifier name } -> Map.add name typ env
                    | _ -> env
                
                Ok(env, sub, SVariableDeclaration(name, expr, typ))
            | Error errors -> Error errors

        | Error errors -> Error errors
    | SPrintStatement _ -> Ok(env, Map.empty, stmt)

and inferProgram (env: TypeEnv) (stmts: Program) : Result<TypeEnv * Substitution * Program, TypeErrors> =
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
