module Vec3.Interpreter.Typing.Inference

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open System
open Builtins

type TType = Vec3.Interpreter.Grammar.Type

type Substitution = Map<TypeVar, TType>
type Scheme = Forall of TypeVar list * TType
type TypeEnv = Map<string, TType>

let defaultTypeEnv = BuiltinFunctions

type TypeError =
    | UndefinedVariable of Token
    | UndefinedFunction of Token
    | UndefinedType of Token
    | TypeMismatch of Token * TType * TType
    | InvalidAssignment of Token * TType * TType
    | InvalidArgumentCount of Token * int * int
    | InvalidArgumentType of Token * TType * TType
    | InvalidReturnType of Token * TType * TType
    | InvalidOperandType of Token * TType * TType
    | InvalidOperator of Token * TType
    | InvalidFunctionType of Token * TType
    | InvalidFunction of Token * TType
    | InvalidFunctionArgument of Token * TType * TType
    | InvalidFunctionReturn of Token * TType * TType
    | InvalidFunctionBody of Token * TType * TType
    | InvalidBlock of Token * TType * TType
    | InvalidCall of Token * TType
    | InvalidCallType of Token * TType * TType
    | InvalidCallReturn of Token * TType * TType
    | InvalidCallBody of Token * TType * TType
    | TypeErrors of TypeError list

exception TypeException of TypeError

type Result<'T> =
    | Ok of 'T
    | Errors of TypeError list

let combineMaps map1 map2 =
     Map.fold (fun acc key value -> Map.add key value acc) map2 map1

let freshTypeVar =
    let counter = ref 0
    fun () ->
        counter.Value <- counter.Value + 1
        TTypeVariable counter.Value

let checkLiteral (lit: Literal) : TType =
    match lit with
    | Literal.TNumber(TNumber.Integer _) -> TInteger
    | Literal.TNumber(TNumber.Float _) -> TFloat
    | Literal.TNumber(TNumber.Rational _) -> TRational
    | Literal.TNumber(TNumber.Complex _) -> TComplex

    | Literal.String _ -> TString
    | Literal.Bool _ -> TBool
    | Literal.Unit -> TUnit

let checkIdentifier (env: TypeEnv) (token: Token) : Result<TType> =
    match token.lexeme with
    | Identifier name ->
        match Map.tryFind name env with
        | Some t -> Ok t
        | None -> Errors [ TypeError.UndefinedVariable token ]
    | _ -> Errors [ TypeError.UndefinedVariable token ]

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

let applySubstitutionToEnv (sub: Substitution) (env: TypeEnv) : TypeEnv =
    Map.map (fun _ t -> applySubstitution sub t) env

let rec unify (t1: TType) (t2: TType): Result<Substitution> =
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
            Errors [ TypeError.TypeMismatch(Empty, TTypeVariable tv, t) ]
        else
            Ok <| Map.add tv t Map.empty
    
    | TFunction(params1, ret1), TFunction(params2, ret2) ->
        if List.length params1 <> List.length params2 then
            Errors [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            let paramResults = List.map2 unify params1 params2
            let retResult = unify ret1 ret2
        
        
            let combinedResults =
                List.fold(fun acc result ->
                    match acc, result with
                    | Ok sub1, Ok sub2 -> Ok (combineMaps sub1 sub2)
                    | Errors errors, Ok _ -> Errors errors
                    | Ok _, Errors errors -> Errors errors
                    | Errors errors1, Errors errors2 -> Errors(errors1 @ errors2)) (Ok Map.empty) paramResults
                    
            match combinedResults, retResult with
            | Ok sub1, Ok sub2 -> Ok (combineMaps sub1 sub2)
            | Errors errors, Ok _ -> Errors errors
            | Ok _, Errors errors -> Errors errors
            | Errors errors1, Errors errors2 -> Errors(errors1 @ errors2)
    | _ -> Errors [ TypeError.TypeMismatch(Empty, t1, t2) ]

and occursCheck (tv: TypeVar) (t: TType) : bool =
    match t with
    | TTypeVariable v -> v = tv
    | TFunction(parameters, ret) -> List.exists (occursCheck tv) parameters || occursCheck tv ret
    | TTuple types -> List.exists (occursCheck tv) types
    | TList typ -> occursCheck tv typ
    | TVector(typ, _) -> occursCheck tv typ
    | TMatrix(typ, _, _) -> occursCheck tv typ
    | _ -> false

let instantiate (scheme: Scheme) : TType =
    match scheme with
    | Forall(vars, typ) ->
        let sub = Map.ofList (List.map (fun v -> v, freshTypeVar()) vars)
        applySubstitution sub typ

let rec freeTypeVars (typ: TType) : TypeVar list =
    match typ with
    | TTypeVariable tv -> [ tv ]
    | TFunction (paramTypes, retType) ->
        List.collect freeTypeVars paramTypes @ freeTypeVars retType
    | TTuple types -> List.collect freeTypeVars types
    | TList typ -> freeTypeVars typ
    | TVector(typ, _) -> freeTypeVars typ
    | TMatrix(typ, _, _) -> freeTypeVars typ
    | _ -> []

let freeTypeVarsInEnv (env: TypeEnv) : TypeVar list =
    env
    |> Map.toList
    |> List.collect (fun (_, typ) -> freeTypeVars typ)

let generalise (env: TypeEnv) (typ: TType) : Scheme =
    let freeVars = freeTypeVars typ |> Set.ofList
    let envFreeVars = freeTypeVarsInEnv env |> Set.ofList
    let generalVars = Set.difference freeVars envFreeVars |> Set.toList
    Forall(generalVars, typ)

// let defaultTypeEnv =
//     List.fold (fun acc (name, typ) -> Map.add name (Forall([] ,typ)) acc) Map.empty BuiltinFunctions


let rec infer (env: TypeEnv) (expr: Expr) : Result<TType * Substitution> =
    match expr with
    | Expr.Literal lit -> Ok (checkLiteral lit, Map.empty)
    | Expr.Identifier token -> match checkIdentifier env token with
                                | Ok t -> Ok (t, Map.empty)
                                | Errors errors -> Errors errors
    | Expr.Lambda(paramList, returnType, body) ->
        let paramTypes = List.map snd paramList
        let paramTypes = List.map (fun t -> match t with | TInfer -> freshTypeVar() | _ -> t) paramTypes
        
        let newEnv = List.fold2 (fun acc (param, _) typ -> match param.lexeme with
                                                            | Identifier name -> Map.add name typ acc
                                                            | _ -> acc) env paramList paramTypes
        let bodyResult = infer newEnv body
        
        match bodyResult with
        | Ok (bodyType, sub) ->
            let paramTypes = List.map (applySubstitution sub) paramTypes
            Ok (TFunction(paramTypes, bodyType), sub)
        | Errors errors -> Errors errors
    | Expr.Call(callee, args) -> failwith "todo!!!"
        
    | _ -> failwith "todo"


