module Vec3.Interpreter.Typing.Inference

open Microsoft.FSharp.Core
open Types
open Substitution
open Generalisation
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Builtins
open Exceptions

// ISSUE: let x = (y) -> y() + 1, should infer x as a function that takes a function of type TFunction([], Interger) and returns an integer
// or let x = (y) -> y() + y() should infer y as a function of TFunction([], TConstrain(var, [ TInteger; TFloat; TRational; TComplex ]))
// but doesnt, very difficult to implement

// let defaultTypeEnv: TypeEnv =
//     builtInFunctionMap |> Map.map (fun _ builtIn ->
//         let typ = BuiltinFunctions[builtIn]
//         Forall([], typ))
let defaultTypeEnv: TypeEnv =
    let funcMap = builtInFunctionMap |> Map.map(fun _ builtIn -> BuiltinFunctions[builtIn])
    // add builin constants
    Map.fold (fun acc name typ -> Map.add name typ acc) funcMap BuiltinConstants
    
    
    

let combineMaps map1 map2 =
    Map.fold (fun acc key value -> Map.add key value acc) map2 map1

let rec occursCheck (tv: TypeVar) (t: TType) : bool =
    match t with
    | TTypeVariable v -> v = tv
    | TFunction(parameters, ret) -> List.exists (occursCheck tv) parameters || occursCheck tv ret
    | TTuple types -> List.exists (occursCheck tv) types
    | TTensor(typ, var) ->
        occursCheck tv typ
        || match var with
           | DVar v -> v = tv
           | _ -> false
    | TConstrain(var, _) -> var = tv
    | TRecord row -> occursCheck tv row
    | TRowExtend(_, typ, row) -> occursCheck tv typ || occursCheck tv row
    | TAlias(_, typ) -> Option.map (occursCheck tv) typ |> Option.defaultValue false
    | _ -> false

let checkLiteral (lit: Literal) : TType =
    match lit with
    | LNumber(LInteger _) -> TInteger
    | LNumber(LFloat _) -> TFloat
    | LNumber(LRational _) -> TRational
    | LNumber(LComplex _) -> TComplex

    | LString _ -> TString
    | LBool _ -> TBool
    | LUnit -> TUnit

let checkIdentifier (env: TypeEnv) (token: Token) : TType TypeResult =
    match Map.tryFind token.Lexeme env with
    | Some t -> Ok t
        // Ok (instantiate t)
    | None -> Error [ TypeError.UndefinedVariable token ]

// attempts to unify two types
let rec unify (aliases: AliasMap) (t1: TType) (t2: TType) : Substitution TypeResult =
    let t1 = resolveAlias t1 aliases
    let t2 = resolveAlias t2 aliases

    let unify = unify aliases

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

    | TAny, _
    | _, TAny -> Ok Map.empty
    
    | TTypeVariable tv, TTypeVariable tv' when tv = tv' -> Ok Map.empty
    
    | TTypeVariable tv, TConstrain(var, f)
    | TConstrain(var, f), TTypeVariable tv ->
        Ok <| Map.add tv (TConstrain(var, f)) Map.empty

    | TTypeVariable tv, t
    | t, TTypeVariable tv ->
        if occursCheck tv t then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            Ok <| Map.add tv t Map.empty
        
    | TConstrain(var1, f1), TConstrain(var2, f2) ->
        if var1 = var2 then
            Ok Map.empty
        else
            let tv = freshTypeVar()
            
            let map = Map.add var1 (TConstrain(tv, fun t -> f1 t && f2 t)) Map.empty
            let map = Map.add var2 (TConstrain(tv, fun t -> f1 t && f2 t)) map
            
            Ok map

    | TConstrain(var, f), t
    | t, TConstrain(var, f) ->
        if occursCheck var t then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else if f t then
            Ok <| Map.add var t Map.empty
        else
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]

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
    | TRecord row1, TRecord row2 -> unify row1 row2
    | TRowEmpty, TRowEmpty -> Ok Map.empty
    | TRowExtend(label1, typ1, rest_rows1), (TRowExtend _ as row2) ->
        let rec rewrite_row (row: Row) (label: Token) (field_typ: TType) =
            match row with
            | TRowEmpty -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
            | TRowExtend(label2, typ2, rest_rows2) when label1.Lexeme = label2.Lexeme -> Ok(typ2, rest_rows2)
            | TRowExtend(label2, typ2, rest_rows2) ->
                let inner = rewrite_row rest_rows2 label field_typ

                match inner with
                | Error _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
                | Ok(typ, rest) -> Ok(TRowExtend(label2, typ2, typ), rest)
            | TTypeVariable v ->
                let resolved = Map.tryFind v resolvedTypes.Value

                match resolved with
                | Some(TRowExtend(label2, typ2, rest_rows2)) when label1.Lexeme = label2.Lexeme -> Ok(typ2, rest_rows2)
                | _ -> Ok(TRowExtend(label1, field_typ, TTypeVariable v), TRowEmpty)
            
            | TConstrain(v, _) -> rewrite_row (TTypeVariable v) label field_typ

            | _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]


        let rest_row_1_var =
            match rest_rows1 with
            | TTypeVariable v -> Some(TTypeVariable v)
            | TConstrain(v, _) -> Some(TTypeVariable v)
            | _ -> None

        let rest_row_2_var =
            match row2 with
            | TRowExtend(_, _, TTypeVariable v) -> Some(TTypeVariable v)
            | TRowExtend(_, _, TConstrain(v, _)) -> Some(TTypeVariable v)
            | _ -> None

        let rest_row_1 =
            match rest_row_1_var with
            | Some(TTypeVariable v) -> TTypeVariable v
            | Some(TConstrain(v, _)) -> TTypeVariable v
            | Some _ -> rest_rows1
            | None -> rest_rows1

        let rest_row_2 =
            match rest_row_2_var with
            | Some(TTypeVariable v) -> TTypeVariable v
            | Some(TConstrain(v, _)) -> TTypeVariable v
            | _ -> row2

        let rewritten = rewrite_row rest_row_2 label1 typ1

        match rewritten with
        | Ok(typ2, rest_rows2) ->
            unify typ1 typ2
            |> Result.bind (fun sub ->
                let sub = combineMaps sub Map.empty

                unify rest_row_1 rest_rows2
                |> Result.bind (fun sub' ->
                    let sub' = combineMaps sub' sub
                    Ok sub'))
        | Error _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]

    | TTensor(typ1, sizes1), TTensor(typ2, sizes2) ->
        match sizes1, sizes2 with
        | DAny, DAny -> unify typ1 typ2
        | DAny, Dims _
        | Dims _, DAny -> unify typ1 typ2
        | Dims sizes1, Dims sizes2 ->
            if sizes1 = sizes2 || List.isEmpty sizes1 || List.isEmpty sizes2 then
                unify typ1 typ2
            else
                Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        | DVar v, Dims sizes
        | Dims sizes, DVar v ->
            let resolved = Map.tryFind v resolvedDims.Value

            match resolved with
            | Some(Dims sizes') when sizes = sizes' -> unify typ1 typ2
            | _ ->
                resolvedDims.Value <- Map.add v (Dims sizes) resolvedDims.Value // this is wrong
                unify typ1 typ2
        | DVar v1, DVar v2 ->
            if v1 = v2 then
                Ok Map.empty
            else
                let sub = Map.add v1 (TTensor(typ1, DVar v2)) Map.empty // this is wrong
                resolvedDims.Value <- Map.add v1 (DVar v2) resolvedDims.Value
                Ok sub
        | DVar v, DAny
        | DAny, DVar v ->
            let resolved = Map.tryFind v resolvedDims.Value

            match resolved with
            | Some(DAny) -> unify typ1 typ2
            | _ ->
                resolvedDims.Value <- Map.add v DAny resolvedDims.Value
                unify typ1 typ2
    | _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]



// let defaultTypeEnv =
//     List.fold (fun acc (name, typ) -> Map.add name (Forall([] ,typ)) acc) Map.empty BuiltinFunctions

let rec unifyWithSubstitution
    (aliases: AliasMap)
    (paramTypes: TType list)
    (argTypes: TType list)
    (currentSubs: Substitution)
    : Substitution TypeResult =
    match (paramTypes, argTypes) with
    | [], [] -> Ok currentSubs
    | paramType :: restParamTypes, argType :: restArgTypes ->
        let paramType = applySubstitution aliases currentSubs paramType
        let argType = applySubstitution aliases currentSubs argType

        unify aliases paramType argType
        |> Result.bind (fun sub ->
            let combinedSubs = combineMaps currentSubs sub

            let updatedRestParamTypes =
                List.map (applySubstitution aliases combinedSubs) restParamTypes

            let updatedRestArgTypes =
                List.map (applySubstitution aliases combinedSubs) restArgTypes

            unifyWithSubstitution aliases updatedRestParamTypes updatedRestArgTypes combinedSubs
            )

    | _ ->
        Error
            [ TypeError.InvalidArgumentCount(EIdentifier(Empty, Some TNever), List.length paramTypes, List.length argTypes) ]


let rec infer (aliases: AliasMap) (env: TypeEnv) (expr: Expr) : (TType * Substitution * Expr) TypeResult =

    let inferArgs (aliases: AliasMap) (env: TypeEnv) (args: Expr list) =
        let argResults = List.map (infer aliases env) args
        let hasErrors = List.exists Result.isError argResults

        if hasErrors then
            let errors =
                List.collect
                    (fun result ->
                        match result with
                        | Error errors -> errors
                        | _ -> [])
                    argResults

            Error errors
        else
            let argTypes =
                List.choose
                    (function
                    | Ok(t, sub, expr) -> Some(t, sub, expr)
                    | _ -> None)
                    argResults

            Ok argTypes

    match expr with
    | ELiteral(lit, _) ->
        let t = checkLiteral lit
        Ok(t, Map.empty, ELiteral(lit, t))
    | EIdentifier(token, _) ->
        checkIdentifier env token
        |> Result.bind (fun t -> Ok(t, Map.empty, EIdentifier(token, Some t)))
    | ELambda(paramList, body, returnT, _) ->
        let paramTypes = List.map snd paramList
        
        let newParamType typ =
            match typ with
            | Some t -> t
            | None -> TTypeVariable(freshTypeVar())
        let paramTypes = List.map newParamType paramTypes
        let paramList = List.map fst paramList
        
        let newEnv =
            List.fold2
                (fun acc param typ ->
                    match param.Lexeme with
                    | Identifier _ as id ->
                        // let typScheme = generalize acc typ
                        Map.add id typ acc
                    | _ -> acc)
                env
                paramList
                paramTypes
        

        infer aliases newEnv body
        |> Result.bind (fun (bodyType, sub, expr) ->
            
            let paramTypes = List.map (applySubstitution aliases sub) paramTypes
            let paramList = List.zip paramList paramTypes
            let paramList = List.map (fun (id, typ) -> (id, Some typ)) paramList

            if Option.isNone returnT then
                Ok(TFunction(paramTypes, bodyType), sub, ELambda(paramList, expr, Some bodyType, Some (TFunction(paramTypes, bodyType))))
            else
                let returnT = Option.defaultValue (TTypeVariable(freshTypeVar())) returnT
                unify aliases bodyType returnT
                |> Result.bind (fun sub' ->
                    let sub = combineMaps sub sub'
                    let returnType = applySubstitution aliases sub returnT

                    Ok(
                        TFunction(paramTypes, returnType),
                        sub,
                        ELambda(paramList, expr, Some returnType, Some (TFunction(paramTypes, returnType)))
                    )))

    | ECall(callee, args, _) ->
        infer aliases env callee
        |> Result.bind (fun (t, sub, expr) ->
            let t = applySubstitution aliases sub t

            match t with
            | TFunction(paramTypes, ret) ->
                if List.length paramTypes <> List.length args then
                    Error [ TypeError.InvalidArgumentCount(callee, List.length paramTypes, List.length args) ]
                else
                    inferArgs aliases env args
                    |> Result.bind (fun argResults ->
                        let argTypes = List.map (fun (t, _, _) -> t) argResults
                        let argSubs = List.map (fun (_, sub, _) -> sub) argResults
                        let argExprs = List.map (fun (_, _, expr) -> expr) argResults

                        unifyWithSubstitution aliases paramTypes argTypes Map.empty
                        |> Result.bind (fun sub' ->
                            let combinedSubs = List.fold combineMaps sub' argSubs
                            let returnType = applySubstitution aliases combinedSubs ret

                            match returnType with
                            | TConstrain(tv, _) ->
                                let resolved = Map.tryFind tv resolvedTypes.Value
                                
                                match resolved with
                                | Some t -> Ok(t, combinedSubs, ECall(expr, argExprs, Some t))
                                | None -> Ok(returnType, combinedSubs, ECall(expr, argExprs, Some returnType))
                                
                            | _ -> Ok(returnType, combinedSubs, ECall(expr, argExprs, Some returnType))))
            | TTypeVariable a ->
                inferArgs aliases env args
                |> Result.bind (fun argResults ->
                    let argTypes = List.map (fun (t, _, _) -> t) argResults
                    let argSubs = List.map (fun (_, sub, _) -> sub) argResults
                    let argExprs = List.map (fun (_, _, expr) -> expr) argResults

                    let returnT = TTypeVariable(freshTypeVar ())
                    let t = TFunction(argTypes, returnT)

                    unify aliases t (TTypeVariable a)
                    |> Result.bind (fun sub ->
                        let t' = applySubstitution aliases sub (TTypeVariable a)

                        let sub = List.fold combineMaps sub argSubs
                        Ok(t', sub, ECall(expr, argExprs, Some t))))
            | _ -> Error [ TypeError.InvalidCall(callee, t) ])
    | EBlock(stmts, _) ->
        inferProgram aliases env stmts
        |> Result.bind (fun (env, aliases, sub, _) ->
            let lastStmt = List.last stmts

            inferStmt aliases env lastStmt
            |> Result.bind (fun (env, aliases, sub', _) ->
                let sub = combineMaps sub sub'

                let lastStmtType =
                    match lastStmt with
                    | SExpression(expr, _) ->
                        match infer aliases env expr with
                        | Ok(t, _, _) -> t
                        | Error _ -> TNever
                    | SVariableDeclaration _ -> TUnit
                    | SAssertStatement _ -> TUnit
                    | STypeDeclaration _ -> TUnit

                Ok(lastStmtType, sub, EBlock(stmts, Some lastStmtType))))

    | EGrouping(expr, _) -> infer aliases env expr
    | EIf(cond, thenBranch, elseBranch, _) ->
        let condResult = infer aliases env cond
        let thenResult = infer aliases env thenBranch
        let elseResult = infer aliases env elseBranch

        match condResult, thenResult, elseResult with
        | Ok(TBool, sub1, expr1), Ok(t1, sub2, expr2), Ok(t2, sub3, expr3) ->
            let sub = combineMaps sub1 sub2
            let sub = combineMaps sub sub3

            unify aliases t1 t2
            |> Result.bind (fun sub' ->
                let sub = combineMaps sub sub'

                let returnT = applySubstitution aliases sub t1
                Ok(returnT, sub, EIf(expr1, expr2, expr3, Some returnT)))
        | Error errors, _, _ -> Error errors
        | _, Error errors, _ -> Error errors
        | _, _, Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidIf(cond) ]
    | ETernary(cond, trueBranch, falseBranch, typ) -> infer aliases env (EIf(cond, trueBranch, falseBranch, typ))
    | ETuple(exprs, _) ->
        inferArgs aliases env exprs
        |> Result.bind (fun results ->
            let types = List.map (fun (t, _, _) -> t) results
            let subs = List.map (fun (_, sub, _) -> sub) results
            let exprs = List.map (fun (_, _, expr) -> expr) results


            let subResults =
                List.map2 (unify aliases) types (List.replicate (List.length types) (TTypeVariable(freshTypeVar ())))

            let hasErrors = List.exists Result.isError subResults

            if hasErrors then
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
                    List.choose
                        (function
                        | Ok(sub) -> Some(sub)
                        | _ -> None)
                        subResults

                let combinedSubs = List.fold combineMaps Map.empty subResults
                let combinedSubs = List.fold combineMaps combinedSubs subs

                let returnType = TTuple(List.map (applySubstitution aliases combinedSubs) types)
                Ok(returnType, combinedSubs, ETuple(exprs, Some returnType)))
    | EList(exprs, _) ->
        inferArgs aliases env exprs
        |> Result.bind (fun results ->
            let types = List.map (fun (t, _, _) -> t) results
            let subs = List.map (fun (_, sub, _) -> sub) results
            let exprs = List.map (fun (_, _, expr) -> expr) results

            // check all the types are the same
            let typeVar = freshTypeVar ()

            unifyWithSubstitution aliases types (List.replicate (List.length types) (TTypeVariable typeVar)) Map.empty
            |> Result.bind (fun sub' ->
                let combinedSubs = List.fold combineMaps Map.empty subs
                let combinedSubs = combineMaps combinedSubs sub'


                let head =
                    if List.length types > 0 then
                        List.head types
                    else
                        TTypeVariable(freshTypeVar ())

                let returnType =
                    TTensor(applySubstitution aliases combinedSubs head, Dims [ List.length types ])

                Ok(returnType, combinedSubs, EList(exprs, Some returnType))))
    | ERange(start, end_, _) ->
        let startResult = infer aliases env start
        let endResult = infer aliases env end_

        match startResult, endResult with
        | Ok(TInteger, sub1, expr1), Ok(TInteger, sub2, expr2) ->
            let sub = combineMaps sub1 sub2
            Ok(TTensor(TInteger, DAny), sub, ERange(expr1, expr2, Some (TTensor(TInteger, DAny))))
        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidRange(start, end_) ]

    | EIndex(expr, index, _) ->
        let indexResult = infer aliases env index
        let exprResult = infer aliases env expr

        match indexResult, exprResult with
        | Ok(TInteger, sub1, expr1), Ok(TTensor(typ, _), sub2, expr2) -> // might be nice to have little dependent types with the dims and index
            let sub = combineMaps sub1 sub2
            let returnType = applySubstitution aliases sub typ
            Ok(returnType, sub, EIndex(expr2, expr1, Some returnType))
        | Ok(TInteger, sub1, expr1), Ok(TTypeVariable n, sub2, expr2) ->
            let typeVar = freshTypeVar ()
            let dimsTypeVar = freshTypeVar ()
            let sub = combineMaps sub1 sub2

            let sub2 = Map.add n (TTensor(TTypeVariable typeVar, DVar dimsTypeVar)) sub
            Ok(TTypeVariable typeVar, sub2, EIndex(expr2, expr1, Some (TTypeVariable typeVar)))

        | Ok(TInteger, sub1, expr1), Ok(TConstrain(n, f), sub2, expr2) -> // is this correct ?
            let sub = combineMaps sub1 sub2
            
            let typeVar = freshTypeVar ()
            let dimsTypeVar = freshTypeVar ()
            
            let sub2 = Map.add n (TTensor(TTypeVariable typeVar, DVar dimsTypeVar)) sub
            Ok(TTypeVariable typeVar, sub2, EIndex(expr2, expr1, Some (TTypeVariable typeVar)))

        | Ok(TInteger, _, _), Ok(TTuple _, _, _) -> failwith "todo"
        | Ok(TInteger, _, _), Ok(t, _, _) -> Error [ TypeError.InvalidIndex(expr, t) ]

        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidIndex(expr, TNever) ]

    // lot of this doesnt work,

    // fails on the following:
    // let x = (x) -> x.a + 4
    // let y = { a = 4.0 }
    // x(y), doesnt infer that a must be an int, so fials on +
    // annoying error stupid hard to fix
    | ERecordEmpty _ -> Ok(TRecord(TRowEmpty), Map.empty, ERecordEmpty(TRecord(TRowEmpty)))
    | ERecordExtend((name, value, _), record, _) ->
        let valueResult = infer aliases env value
        let recordResult = infer aliases env record

        match valueResult, recordResult with
        | Ok(t, sub1, expr1), Ok(TRecord(row), sub2, expr2) ->
            let sub = combineMaps sub1 sub2
            let row = applySubstitution aliases sub row
            let sub = combineMaps sub sub2

            let newType = TRecord(TRowExtend(name, t, row))
            Ok(newType, sub, ERecordExtend((name, expr1, Some t), expr2, Some newType))
        | Ok(t, sub1, expr1), Ok(TTypeVariable n, sub2, expr2) ->
            let sub = combineMaps sub1 sub2

            let sub2 = Map.add n (TRecord(TRowExtend(name, t, TRowEmpty))) sub

            Ok(
                TRecord(TRowExtend(name, t, TRowEmpty)),
                sub2,
                ERecordExtend((name, expr1, Some t), expr2, Some (TRecord(TRowExtend(name, t, TRowEmpty))))
            )
        // if final
        | Ok(t, sub1, expr1), Ok(TRowEmpty, sub2, expr2) ->
            let sub = combineMaps sub1 sub2
            let sub = combineMaps sub sub2

            let newType = TRecord(TRowExtend(name, t, TRowEmpty))
            Ok(newType, sub, ERecordExtend((name, expr1, Some t), expr2, Some newType))
        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidField(name, TNever) ]

    | ERecordSelect(record, name, _) ->
        let recordResult = infer aliases env record

        match recordResult with
        | Ok(TRecord(row), sub, expr) ->
            let row = applySubstitution aliases sub row

            let rec findType (row: TType) (name: Token) : TType =
                match row with
                | TRowEmpty -> TNever
                | TRowExtend(label, typ, _) when label.Lexeme = name.Lexeme -> typ
                | TRowExtend(_, _, rest) -> findType rest name
                | _ -> TNever

            let typ = findType row name

            match typ with
            | TNever -> Error [ TypeError.InvalidField(name, TRecord(row)) ]
            | _ -> Ok(typ, sub, ERecordSelect(expr, name, Some typ))
        | Ok(TTypeVariable n, sub, expr) ->
            let typeVar = freshTypeVar ()

            let sub =
                Map.add n (TRecord(TRowExtend(name, TTypeVariable typeVar, TRowEmpty))) sub

            Ok(TTypeVariable typeVar, sub, ERecordSelect(expr, name, Some(TTypeVariable typeVar)))
        | Ok(TAny, sub, expr) -> Ok(TAny, sub, expr)
        | Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidField(name, TNever) ]
    | ERecordRestrict(record, name, _) ->
        let recordResult = infer aliases env record

        match recordResult with
        | Ok(TRecord(row), sub, expr) ->
            let row = applySubstitution aliases sub row

            let rec findType (row: TType) (name: Token) : TType =
                match row with
                | TRowEmpty -> TNever
                | TRowExtend(label, typ, _) when label.Lexeme = name.Lexeme -> typ
                | TRowExtend(_, _, rest) -> findType rest name
                | _ -> TNever

            let typ = findType row name
            
            match typ with
            | TNever -> Error [ TypeError.InvalidField(name, TRecord(row)) ]
            | _ -> 
                let newType = TRecord(TRowExtend(name, typ, TRowEmpty))
                Ok(newType, sub, ERecordRestrict(expr, name, Some newType))
        | Ok(TTypeVariable n, sub, expr) ->
            let typeVar = freshTypeVar ()

            let sub =
                Map.add n (TRecord(TRowExtend(name, TTypeVariable typeVar, TRowEmpty))) sub

            Ok(
                TRecord(TRowExtend(name, TTypeVariable typeVar, TRowEmpty)),
                sub,
                ERecordRestrict(expr, name, Some (TRecord(TRowExtend(name, TTypeVariable typeVar, TRowEmpty))))
            )
        | Ok(TAny, sub, expr) -> Ok(TAny, sub, expr)

        | Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidField(name, TNever) ]


and inferStmt (aliases: AliasMap) (env: TypeEnv) (stmt: Stmt) : (TypeEnv * AliasMap * Substitution * Stmt) TypeResult =
    // make this immutable later, pass it around, or resolved in substitution
    resolvedTypes.Value <- Map.empty

    match stmt with
    | SExpression(expr, _) ->
        infer aliases env expr
        |> Result.map (fun (t, sub, expr) -> (env, aliases, sub, SExpression(expr, Some t)))
    | SVariableDeclaration(name, expr, typ) ->
        infer aliases env expr
        |> Result.bind (fun (t, sub, expr) ->
            let typ = Option.defaultValue t typ

            unify aliases t typ
            |> Result.bind (fun sub' ->
                let sub = combineMaps sub sub'

                let env = applySubstitutionToEnv aliases sub env

                let typ = applySubstitution aliases sub typ

                let env =
                    match name with
                    | { Lexeme = Identifier _ as id } ->
                        // let scheme = generalize env typ
                        Map.add id typ env
                    | _ -> env

                Ok(env, aliases, sub, SVariableDeclaration(name, expr, Some typ))))
    | SAssertStatement(expr, msg, _) ->
        let exprResult = infer aliases env expr

        match exprResult with
        | Ok(TBool, sub, expr) ->
            let msgResult =
                match msg with
                | Some msg -> infer aliases env msg
                | None -> Ok(TString, Map.empty, ELiteral(LString "", TString))

            match msgResult with
            | Ok(TString, sub', msg) ->
                let sub = combineMaps sub sub'
                Ok(env, aliases, sub, SAssertStatement(expr, Some msg, Some TBool))
            | Ok(t, _, _) -> Error [ TypeError.InvalidAssert(msg.Value, t) ]
            | Error errors -> Error errors

        | Ok(t, _, _) -> Error [ TypeError.InvalidAssert(expr, t) ]
        | Error errors -> Error errors
    | STypeDeclaration(name, typ, _) ->
        let alias = TAlias(name, Some typ)
        let aliases = Map.add name.Lexeme typ aliases
        Ok(env, aliases, Map.empty, STypeDeclaration(name, alias, Some TUnit))

and inferProgram
    (aliases: AliasMap)
    (env: TypeEnv)
    (stmts: Program)
    : (TypeEnv * AliasMap * Substitution * Program) TypeResult =
    List.fold
        (fun acc stmt ->
            match acc with
            | Error errors ->
                let errors' =
                    match inferStmt aliases env stmt with
                    | Ok _ -> []
                    | Error errors -> errors

                Error(errors @ errors')
            | Ok(env, aliases, sub, stmts) ->
                inferStmt aliases env stmt
                |> Result.bind (fun (env, aliases, sub', stmt) ->
                    let sub = combineMaps sub sub'
                    Ok(env, aliases, sub, stmts @ [ stmt ])))
        (Ok(env, aliases, Map.empty, []))
        stmts

let quickInferStmt (aliases: AliasMap) (env: TypeEnv) (stmt: Stmt) : TypeEnv =
    match inferStmt aliases env stmt with
    | Ok(env, _, _, _) -> env
    | Error errors -> raise <| TypeException errors

let rec IsPrimeMultipleTest (bbase: int) (other: int) : bool =
        match bbase with
        | 0 -> false
        | _ when bbase > other -> false
        | _ when bbase = other -> true
        | _ -> IsPrimeMultipleTest (bbase * 2) other 
        
let rec RemoveAllMultiples (list : int list) : int list =
    List.fold (fun acc head -> List.filter (IsPrimeMultipleTest head) acc) list list
    
let GetPrimesUpTo n =
    RemoveAllMultiples [ 2 .. n ]
    