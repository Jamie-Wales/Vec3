module Vec3.Interpreter.Typing.Substitution

open Vec3.Interpreter.Typing.Types
open Vec3.Interpreter.Grammar

// make this immutable later, as it specialises functions too much
let resolvedTypes: Ref<ResolvedType> = ref Map.empty
let resolvedDims: Ref<ResolvedDims> = ref Map.empty

let rec resolveAlias (typ: TType) (env: AliasMap) : TType =
    match typ with
    | TAlias(name, _) ->
        let resolved = Map.tryFind name.Lexeme env

        match resolved with
        | Some t -> resolveAlias t env
        | None -> typ
    | TFunction(params', ret, pr, bt) -> TFunction(List.map (fun t -> resolveAlias t env) params', resolveAlias ret 
    env, pr, bt)
    | TTuple types -> TTuple(List.map (fun t -> resolveAlias t env) types)
    | TTensor(typ, dims) -> TTensor(resolveAlias typ env, dims)
    | TRecord row -> TRecord(resolveAlias row env)
    | TRowExtend(label, typ, row) -> TRowExtend(label, resolveAlias typ env, resolveAlias row env)
    | _ -> typ

let rec applySubstitution (env: AliasMap) (sub: Substitution) (t: TType) : TType =
    let t = resolveAlias t env

    match t with
    | TTypeVariable tv ->
        match Map.tryFind tv sub with
        | Some t' -> applySubstitution env sub t'
        | None -> t
    | TFunction(paramsTypes, retType, pr, bt) ->
        let newParams = List.map (applySubstitution env sub) paramsTypes
        let newRet = applySubstitution env sub retType
        TFunction(newParams, newRet, pr, bt)
    | TTuple types -> TTuple(List.map (applySubstitution env sub) types)
    | TTensor(typ, dims) ->
        let newTyp = applySubstitution env sub typ

        match dims with
        | DAny -> TTensor(newTyp, DAny)
        | Dims sizes -> TTensor(newTyp, Dims sizes)
        | DVar v ->
            match Map.tryFind v resolvedDims.Value with
            | Some t' -> TTensor(newTyp, t')
            | None -> TTensor(newTyp, DVar v)
    | TConstrain constrain ->
        match Map.tryFind constrain.TypeVar sub with
        | Some t' -> applySubstitution env sub t'
        | None -> TConstrain constrain
    | TRecord row -> TRecord(applySubstitution env sub row)
    | TRowExtend(label, typ, row) -> TRowExtend(label, applySubstitution env sub typ, applySubstitution env sub row)
    | t -> t

// attempts to substitute type variables with concrete types in an environment
let applySubstitutionToEnv (aliases: AliasMap) (sub: Substitution) (env: TypeEnv) : TypeEnv =
    Map.map (fun _ -> applySubstitution aliases sub) env
