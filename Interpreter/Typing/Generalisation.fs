module Vec3.Interpreter.Typing.Generalisation

open Types
open Substitution
open Vec3.Interpreter.Grammar

let rec freeTypeVars (typ: TType) : TypeVar list =
    match typ with
    | TTypeVariable tv -> [ tv ]
    | TFunction(paramTypes, retType, _) -> List.collect freeTypeVars paramTypes @ freeTypeVars retType
    | TTuple types -> List.collect freeTypeVars types
    | TTensor(typ, dims) ->
        match dims with
        | DVar v -> v :: freeTypeVars typ
        | _ -> freeTypeVars typ
    | TConstrain(var, _) -> [var]
    | TAlias(_, typ) -> Option.map freeTypeVars typ |> Option.defaultValue []
    | _ -> []

let freeTypeVarsInEnv (env: TypeEnv) : TypeVar list =
    env |> Map.toList |> List.collect (fun (_, typ) ->
        match typ with
        | _ -> freeTypeVars typ)
        // | Forall(vars, typ) -> List.filter (fun v -> not (List.contains v vars)) (freeTypeVars typ))

let freeTypeVarsInScheme (scheme: Scheme) : TypeVar list =
    match scheme with
    | Forall(vars, typ) -> List.filter (fun v -> not (List.contains v vars)) (freeTypeVars typ)
    
let generalize (env: TypeEnv) (typ: TType) : Scheme =
    let freeVars = freeTypeVars typ
    let envVars = freeTypeVarsInEnv env
    
    let vars = List.filter (fun v -> not (List.contains v envVars)) freeVars
    Forall(vars, typ)

let instantiate (scheme: Scheme) : TType =
    match scheme with
    | Forall(vars, typ) ->
        let subs = List.map (fun v -> (v, TTypeVariable(freshTypeVar()))) vars
        let sub = Map.ofList subs
        applySubstitution Map.empty sub typ
