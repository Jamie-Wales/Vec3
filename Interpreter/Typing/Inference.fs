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
        TTypeVariable counter.Value

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
    | t -> t

// attempts to substitute type variables with concrete types in an environment
let applySubstitutionToEnv (sub: Substitution) (env: TypeEnv) : TypeEnv =
    Map.map (fun _ -> applySubstitution sub) env

// attempts to unify two types
let rec unify (t1: TType) (t2: TType): Result<Substitution, TypeErrors> =
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
    
    | TFunction(params1, ret1), TFunction(params2, ret2) ->
        if List.length params1 <> List.length params2 then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            let paramResults = List.map2 unify params1 params2
            let retResult = unify ret1 ret2
        
        
            let combinedResults =
                List.fold(fun acc result ->
                    match acc, result with
                    | Ok sub1, Ok sub2 -> Ok (combineMaps sub1 sub2)
                    | Error errors, Ok _ -> Error errors
                    | Ok _, Error errors -> Error errors
                    | Error errors1, Error errors2 -> Error(errors1 @ errors2)) (Ok Map.empty) paramResults
                    
            match combinedResults, retResult with
            | Ok sub1, Ok sub2 -> Ok (combineMaps sub1 sub2)
            | Error errors, Ok _ -> Error errors
            | Ok _, Error errors -> Error errors
            | Error errors1, Error errors2 -> Error(errors1 @ errors2)
    | TConstrain(types1), TConstrain(types2) ->
        if List.length types1 <> List.length types2 then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            let results = List.map2 unify types1 types2 // assumes that the constraints are in the same order is this correct? and same length
            List.fold(fun acc result ->
                match acc, result with
                | Ok sub1, Ok sub2 -> Ok (combineMaps sub1 sub2)
                | Error errors, Ok _ -> Error errors
                | Ok _, Error errors -> Error errors
                | Error errors1, Error errors2 -> Error(errors1 @ errors2)) (Ok Map.empty) results
        
    | TConstrain(types), t
    | t, TConstrain(types) ->
        let validTypes = List.filter (fun typ -> unify typ t = Ok Map.empty) types
        match validTypes with
        | [] -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        | [singleType] -> unify singleType t
        | _ -> Ok Map.empty
        
    | TTuple types1, TTuple types2 ->
        let results = List.map2 unify types1 types2
        List.fold(fun acc result ->
            match acc, result with
            | Ok sub1, Ok sub2 -> Ok (combineMaps sub1 sub2)
            | Error errors, Ok _ -> Error errors
            | Ok _, Error errors -> Error errors
            | Error errors1, Error errors2 -> Error(errors1 @ errors2)) (Ok Map.empty) results
    
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
    | TConstrain(types) -> List.exists (occursCheck tv) types
    | _ -> false

let rec freeTypeVars (typ: TType) : TypeVar list =
    match typ with
    | TTypeVariable tv -> [ tv ]
    | TFunction (paramTypes, retType) ->
        List.collect freeTypeVars paramTypes @ freeTypeVars retType
    | TTuple types -> List.collect freeTypeVars types
    | TList typ -> freeTypeVars typ
    | TVector(typ, _) -> freeTypeVars typ
    | TMatrix(typ, _, _) -> freeTypeVars typ
    | TConstrain(types) -> List.collect freeTypeVars types // shouldnt have any type variables in constraints
    | _ -> []

let freeTypeVarsInEnv (env: TypeEnv) : TypeVar list =
    env
    |> Map.toList
    |> List.collect (fun (_, typ) -> freeTypeVars typ)

// let defaultTypeEnv =
//     List.fold (fun acc (name, typ) -> Map.add name (Forall([] ,typ)) acc) Map.empty BuiltinFunctions


let rec infer (env: TypeEnv) (expr: Expr) : Result<TType * Substitution, TypeErrors> =
    match expr with
    | ELiteral lit -> Ok (checkLiteral lit, Map.empty)
    | EIdentifier token -> match checkIdentifier env token with
                                | Ok t -> Ok (t, Map.empty)
                                | Error errors -> Error errors
    | ELambda(paramList, returnType, body) ->
        let paramTypes = List.map snd paramList
        let paramTypes = List.map (fun t -> match t with | TInfer -> freshTypeVar() | _ -> t) paramTypes
        
        let newEnv = List.fold2 (fun acc (param, _) typ -> match param.lexeme with
                                                            | Identifier name -> Map.add name typ acc
                                                            | _ -> acc) env paramList paramTypes
        let bodyResult = infer newEnv body
        match bodyResult with
        | Ok (bodyType, sub) ->
            let paramTypes = List.map (applySubstitution sub) paramTypes
            
            if returnType = TInfer then
                Ok (TFunction(paramTypes, bodyType), sub)
            else
                let returnResult = unify bodyType returnType
                match returnResult with
                | Ok sub -> Ok (TFunction(paramTypes, returnType), sub)
                | Error errors -> Error errors
        | Error errors -> Error errors
        
    | ECall(callee, args) ->
        match callee with
        | { lexeme = Identifier name } ->
            match Map.tryFind name env with
            | Some t ->
                match t with
                | TFunction(paramTypes, ret) ->
                    let argResults = List.map (infer env) args
                    if List.exists (fun result -> match result with | Error _ -> true | _ -> false) argResults then
                        let errors = List.collect (fun result -> match result with | Error errors -> errors | _ -> []) argResults
                        Error errors
                    else
                        let argResults = List.map (fun result -> match result with | Ok (t, sub) -> (t, sub) | _ -> failwith "Impossible") argResults
                        let argTypes = List.map fst argResults
                        let argSubs = List.map snd argResults
                        
                        let paramResults = List.map2 unify paramTypes argTypes
                        if List.exists (fun result -> match result with | Error _ -> true | _ -> false) paramResults then
                            let errors = List.collect (fun result -> match result with | Error errors -> errors | _ -> []) paramResults
                            Error errors
                        else
                            let paramResults = List.map (fun result -> match result with | Ok sub -> sub | _ -> failwith "Impossible") paramResults
                        
                            let combinedSubs = List.fold combineMaps Map.empty paramResults
                            let combinedSubs = List.fold combineMaps combinedSubs argSubs
                        
                            let returnType = applySubstitution combinedSubs ret
                            Ok (returnType, combinedSubs)
                | _ -> Error [ TypeError.InvalidCall(callee, t) ]
            | None -> Error [ TypeError.UndefinedVariable callee ]
        | _ -> Error [ TypeError.InvalidCall(callee, TInfer) ]
                    
    | EBinary(expr1, op, expr2) ->
        let expr1Result = infer env expr1
        let expr2Result = infer env expr2
        
        match expr1Result, expr2Result with
        | Ok (t1, sub1), Ok (t2, sub2) ->
            let sub = combineMaps sub1 sub2
            let opType = match op.lexeme with
                            | Operator Plus -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator Minus -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator Star -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator Slash -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator StarStar -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            
                            | Operator EqualEqual -> TConstrain([TInteger; TFloat; TRational; TComplex; TString; TBool; TUnit])
                            | Operator BangEqual -> TConstrain([TInteger; TFloat; TRational; TComplex; TString; TBool; TUnit])
                            
                            | Operator Less -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator LessEqual -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator Greater -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator GreaterEqual -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            
                            | _ -> failwith "Invalid operator type"
            
            let opResult = unify opType t1
            
            match opResult with
            | Ok sub' ->
                let sub = combineMaps sub sub'
                let opResult = unify t1 t2
                match opResult with
                | Ok sub'' ->
                    let sub = combineMaps sub'' sub
                    let returnT = applySubstitution sub t1
                    Ok (returnT, sub)
                | Error errors -> Error errors
            | Error errors -> Error errors
        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors
    
    | EUnary(op, expr) ->
        let exprResult = infer env expr
        match exprResult with
        | Ok (t, sub) ->
            let opType = match op.lexeme with
                            | Operator Minus -> TConstrain([TInteger; TFloat; TRational; TComplex]) // maybe make this a type var
                            // if i have a function (x, y) -> x + y, then x and y should be inferred to the same Constrain type, so that only one type can be passed in
                            | Operator Plus -> TConstrain([TInteger; TFloat; TRational; TComplex])
                            | Operator Bang -> TBool
                            | _ -> failwith "todo!!!"
            
            let opResult = unify opType t
            match opResult with
            | Ok sub' ->
                let sub = combineMaps sub sub'
                let returnT = applySubstitution sub t
                Ok (returnT, sub)
            | Error errors -> Error errors
        | Error errors -> Error errors
    
    | EBlock(stmts) -> failwith "todo!!!"
    | EGrouping(expr) ->
        infer env expr
        
    | _ -> failwith "todo"

and inferStmt (env: TypeEnv) (stmt: Stmt) : Result<TypeEnv * Substitution, TypeErrors> =
    match stmt with
    | SExpression expr ->
        let result = infer env expr
        match result with
        | Ok (_, sub) -> Ok (env, sub)
        | Error errors -> Error errors
    | SVariableDeclaration(name, typ, expr) ->
        let result = infer env expr
        match result with
        | Ok (t, sub) ->
            let typ = match typ with
                        | TInfer -> t
                        | _ -> typ
            
            let subResult = unify t typ
            match subResult with
            | Ok sub' ->
                let sub = combineMaps sub sub'
                let newEnv = match name with
                                | { lexeme = Identifier name } -> Map.add name (applySubstitution sub typ) env
                                | _ -> env
                                
                Ok (newEnv, sub)
            | Error errors -> Error errors
            
        | Error errors -> Error errors
    | SPrintStatement _ ->
        Ok (env, Map.empty)

and inferProgram (env: TypeEnv) (stmts: Stmt list) : Result<TypeEnv * Substitution, TypeErrors> =
    List.fold (fun acc stmt ->
        match acc with
        | Ok (env, sub) ->
            let result = inferStmt env stmt
            match result with
            | Ok (env', sub') ->
                let sub = combineMaps sub sub'
                Ok (env', sub)
            | Error errors -> Error errors
        | Error errors -> Error errors) (Ok (env, Map.empty)) stmts

let quickInferStmt (env: TypeEnv) (stmt: Stmt) =
    match inferStmt env stmt with
    | Ok (env, _) -> env
    | Error errors -> raise <| TypeException errors

