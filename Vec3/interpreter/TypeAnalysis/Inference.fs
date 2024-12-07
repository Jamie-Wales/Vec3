/// <summary>
/// Type inference for the type checker using the Hindley-Milner algorithm with row polymorphism.
/// </summary>

module Vec3.Interpreter.Typing.Inference

// TODO: more on the drawing (trails as rudy said to do them)
// TODO: CLOSURES!!

open System
open Microsoft.FSharp.Collections
open Microsoft.FSharp.Core
open Types
open Substitution
open Vec3.Interpreter
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Builtins
open Exceptions
open Parser

type ResultBuilder() =
    member this.Bind(x, f) = Result.bind f x
    member this.Return(x) = Ok x
    member this.ReturnFrom(x) = x
    member this.Zero() = Ok()

let result = ResultBuilder()


// TODO: ALSO PROBLEM WITH INFERRING RECORD ARGS ??

/// <summary>
/// The default type environment containing all built-in functions and constants.
/// </summary>
let defaultTypeEnv: TypeEnv =
    let funcMap =
        builtInFunctionMap |> Map.map (fun _ builtIn -> BuiltinFunctions[builtIn])

    Map.fold (fun acc name typ -> Map.add name typ acc) funcMap BuiltinConstants


/// <summary>
/// Helper function to combine two maps.
/// </summary>
/// <param name="map1">The first map.</param>
/// <param name="map2">The second map.</param>
/// <returns>The combined map.</returns>
let combineMaps map1 map2 =
    Map.fold (fun acc key value -> Map.add key value acc) map2 map1

/// <summary>
/// Checks if a type variable occurs in a type.
/// This is to stop infinitely recursive types.
/// </summary>
/// <param name="tv">The type variable to check for.</param>
/// <param name="t">The type to check.</param>
/// <returns>True if the type variable occurs in the type, false otherwise.</returns>
let rec occursCheck (tv: TypeVar) (t: TType) : bool =
    match t with
    | TTypeVariable v -> v = tv
    | TFunction(parameters, ret, _, _) -> List.exists (occursCheck tv) parameters || occursCheck tv ret
    | TTuple types -> List.exists (occursCheck tv) types
    | TTensor(typ, var) ->
        occursCheck tv typ
        || match var with
           | DVar v -> v = tv // also check if the dimension is a type variable
           | _ -> false
    | TConstrain constrain -> constrain.TypeVar = tv
    | TRecord row -> occursCheck tv row
    | TRowExtend(_, typ, row) -> occursCheck tv typ || occursCheck tv row
    | TAlias(_, typ) -> Option.map (occursCheck tv) typ |> Option.defaultValue false
    | _ -> false


/// <summary>
/// Helper function to get the type of a literal.
/// </summary>
/// <param name="lit">The literal to check.</param>
/// <returns>The type of the literal.</returns>
let checkLiteral (lit: Literal) : TType =
    match lit with
    | LNumber(LInteger _) -> TInteger
    | LNumber(LFloat _) -> TFloat
    | LNumber(LRational _) -> TRational
    | LNumber(LComplex _) -> TComplex
    | LNumber(LChar _) -> TChar

    | LString _ -> TString
    | LBool _ -> TBool
    | LUnit -> TUnit

/// <summary>
/// Find the type of a variable in the type environment.
/// </summary>
/// <param name="env">The type environment.</param>
/// <param name="token">The token representing the variable.</param>
/// <returns>The type of the variable.</returns>
let checkIdentifier (env: TypeEnv) (token: Token) : TType TypeResult =
    match Map.tryFind token.Lexeme env with
    | Some t -> Ok t
    | None -> Error [ TypeError.UndefinedVariable token ]

/// <summary>
/// Attempts to unify two types.
/// </summary>
/// <param name="aliases">The alias map.</param>
/// <param name="t1">The first type.</param>
/// <param name="t2">The second type.</param>
/// <returns>The substitution that unifies the two types.</returns>
/// <example>
/// unify Map.empty TInteger TInteger
/// Ok Map.empty // the types are the same
///
/// unify Map.empty TInteger TFloat
/// Error [ TypeError.TypeMismatch(Empty, TInteger, TFloat) ] // the types are different
///
/// unify Map.empty TTypeVariable 1 TInteger
/// Ok (Map.singleton 1 TInteger) // the type variable is unified with TInteger
/// </example>
let rec unify (aliases: AliasMap) (t1: TType) (t2: TType) : Substitution TypeResult =
    let t1 = resolveAlias t1 aliases
    let t2 = resolveAlias t2 aliases

    let unify = unify aliases

    match t1, t2 with
    | TInteger, TInteger
    | TInteger, TFloat
    | TFloat, TInteger
    | TRational, TInteger
    | TInteger, TRational
    | TComplex, TInteger
    | TInteger, TComplex
    | TFloat, TFloat
    | TRational, TRational
    | TComplex, TComplex

    | TChar, TChar

    | TBool, TBool
    | TString, TString
    | TUnit, TUnit

    | TNever, TNever -> Ok Map.empty

    // Allows for gradual typing, 'any' can be unified with any type
    | TAny, _
    | _, TAny -> Ok Map.empty

    // Unifying two type variables, when they are the same, return an empty substitution (types are equal)
    | TTypeVariable tv, TTypeVariable tv' when tv = tv' -> Ok Map.empty

    | TTypeVariable tv, TConstrain constrain
    | TConstrain constrain, TTypeVariable tv -> Ok <| Map.add tv (TConstrain constrain) Map.empty

    // When unifying with a type variable, the other type is added as a substitution
    | TTypeVariable tv, t
    | t, TTypeVariable tv ->
        // Check if the type variable occurs in the other type
        // Prevents infinite recursion
        if occursCheck tv t then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            Ok <| Map.add tv t Map.empty

    // Unifying two constraints, when they are the same, return an empty substitution (types are equal)
    // Otherwise, the new type is the union of the two constraints
    | TConstrain constrain1, TConstrain constrain2 ->
        if constrain1.TypeVar = constrain2.TypeVar then
            Ok Map.empty
        else
            let tv = freshTypeVar ()

            let f1 = constrain1.Constrain
            let f2 = constrain2.Constrain

            // Create a new constraint that is the union of the two constraints
            let constrain = TConstrain(Constrain(tv, (fun t -> f1 t && f2 t)))

            let map = Map.add constrain1.TypeVar constrain Map.empty
            let map = Map.add constrain2.TypeVar constrain map

            Ok map

    | TConstrain constrain, t
    | t, TConstrain constrain ->
        // Prevent infinite recursion
        if occursCheck constrain.TypeVar t then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        // Check that the type satisfies the constraint
        else if constrain.Constrain t then
            Ok <| Map.add constrain.TypeVar t Map.empty
        else
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]

    // Check that two functions have the same parameters and return type
    // Disregards the purity of the function, as not relevant in this context
    | TFunction(params1, ret1, _, _), TFunction(params2, ret2, _, _) ->
        if List.length params1 <> List.length params2 then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            let paramResults = List.map2 unify params1 params2
            let retResult = unify ret1 ret2

            // Combine the results of unifying the parameters
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
        // Must have the same number of elements
        if List.length types1 <> List.length types2 then
            Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        else
            // Must have the same types, in the same order
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
    | TRecord row1, TRecord row2 ->
        // Simply unwrap the rows and unify them
        unify row1 row2
    | TRowEmpty, TRowEmpty ->
        // Empty rows are equal
        Ok Map.empty
    | TRowExtend(label1, typ1, rest_rows1), (TRowExtend _ as row2) ->
        // Some logic here is broken, need to rewrite
        // But the idea is from Polymorphic Rows paper
        // Used in the Elm compiler
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

            | TConstrain constrain -> rewrite_row (TTypeVariable constrain.TypeVar) label field_typ

            | _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]


        let rest_row_1_var =
            match rest_rows1 with
            | TTypeVariable v -> Some(TTypeVariable v)
            | TConstrain constrain -> Some(TTypeVariable constrain.TypeVar)
            | _ -> None

        let rest_row_2_var =
            match row2 with
            | TRowExtend(_, _, TTypeVariable v) -> Some(TTypeVariable v)
            | TRowExtend(_, _, TConstrain constrain) -> Some(TTypeVariable constrain.TypeVar)
            | _ -> None

        let rest_row_1 =
            match rest_row_1_var with
            | Some(TTypeVariable v) -> TTypeVariable v
            | Some(TConstrain constrain) -> TTypeVariable constrain.TypeVar
            | Some _ -> rest_rows1
            | None -> rest_rows1

        let rest_row_2 =
            match rest_row_2_var with
            | Some(TTypeVariable v) -> TTypeVariable v
            | Some(TConstrain constrain) -> TTypeVariable constrain.TypeVar
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
        | DAny, _
        | _, DAny -> unify typ1 typ2 // DAny is a wildcard, but infectious
        | Dims sizes1, Dims sizes2 -> // Must have the same dimensions, allows for slightly refined types
            if sizes1 = sizes2 then
                unify typ1 typ2
            else
                Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
        | DVar v, Dims sizes
        | Dims sizes, DVar v ->
            // This isn't correct in every circumstance, but it works for now
            // Represent unknown dimensions as a type variable
            // This allows for function calls to unify to the same type
            // As a function call taking in a list would default to a DimsVar, and so calling it would unify the var to the size of the list

            // This should probably be an immutable field, but it's easier to work with for now due to a lack of
            // monadic state handling in F#
            let resolved = Map.tryFind v resolvedDims.Value

            match resolved with
            | Some(Dims sizes') when sizes = sizes' -> unify typ1 typ2 // This should be handled otherwise and resolved throughout.
            | Some(Dims _) -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]
            | _ ->
                // If the resolved dimensions are unknown, set them to the new dimensions
                resolvedDims.Value <- Map.add v (Dims sizes) resolvedDims.Value
                unify typ1 typ2
        | DVar v1, DVar v2 ->
            // Similar to normal type variables, unify the two dimensions
            if v1 = v2 then
                Ok Map.empty
            else
                // Create a mapping between the two dimensions
                let sub = Map.add v1 (TTensor(typ1, DVar v2)) Map.empty
                resolvedDims.Value <- Map.add v1 (DVar v2) resolvedDims.Value
                Ok sub
    | _ -> Error [ TypeError.TypeMismatch(Empty, t1, t2) ]

/// <summary>
/// Unifies two types and builds a substitution map.
/// Used primarily for unifying function arguments.
/// </summary>
/// <param name="aliases">The alias map.</param>
/// <param name="paramTypes">The types of the parameters.</param>
/// <param name="argTypes">The types of the arguments.</param>
/// <param name="currentSubs">The current substitution map.</param>
/// <returns>The substitution map that unifies the two types.</returns>
let rec unifyWithSubstitution
    (aliases: AliasMap)
    (paramTypes: TType list)
    (argTypes: TType list)
    (currentSubs: Substitution)
    : Substitution TypeResult =
    // Performs a standard fold over the two lists, unifying each pair of types and combining the substitutions
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

            unifyWithSubstitution aliases updatedRestParamTypes updatedRestArgTypes combinedSubs)

    | _ ->
        Error
            [ TypeError.InvalidArgumentCount(
                  EIdentifier(Empty, Some TNever), // Placeholder for the callee, should instead pass the callee
                  List.length paramTypes,
                  List.length argTypes
              ) ]


/// <summary>
/// Infer the type of an expression.
/// </summary>
/// <param name="aliases">The alias map.</param>
/// <param name="env">The type environment.</param>
/// <param name="expr">The expression to infer the type of.</param>
/// <returns>The type of the expression, any substitutions, and the expression with the type attached.</returns>
let rec infer (aliases: AliasMap) (env: TypeEnv) (expr: Expr) : (TType * Substitution * Expr) TypeResult =
    /// <summary>
    /// Helper function to infer the type of a list of statements.
    /// Used primarily for inferring the type of arguments to a function.
    /// </summary>
    /// <param name="aliases">The alias map.</param>
    /// <param name="env">The type environment.</param>
    /// <param name="args">The list of expr's to infer</param>
    /// <returns>The types of the arguments, any substitutions, and the expressions with the types attached.</returns>
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
    | ETail(e, _) -> infer aliases env e // Simply a wrapper
    | ELiteral(lit, _) ->
        // Simple literals have a known type
        let t = checkLiteral lit
        Ok(t, Map.empty, ELiteral(lit, t))
    | EIdentifier(token, _) ->
        // Look up the type of the identifier in the environment
        checkIdentifier env token
        |> Result.bind (fun t -> Ok(t, Map.empty, EIdentifier(token, Some t)))
    | ELambda(paramList, body, returnT, _, _, _) ->
        let paramTypes = List.map snd paramList
        let paramList = List.map fst paramList

        // If the type is given by the programmer, use that, otherwise create a new type variable (which will be unified later)
        let newParamType typ =
            match typ with
            | Some t -> t
            | None -> TTypeVariable(freshTypeVar ())

        let paramTypes = List.map newParamType paramTypes

        // Update the current environment temporarily with the new parameters
        // So that the new parameters can be used in the body
        let newEnv =
            List.fold2
                (fun acc param typ ->
                    match param.Lexeme with
                    | Identifier _ as id -> Map.add id typ acc
                    | _ -> acc)
                env
                paramList
                paramTypes

        // Pure functions are composed of pure builtins
        // The use of this is primarily for plotting functions and optimisation
        // for example,
        // let f = (x) -> x + 1, pure because + is a builtin and pure
        // let f = (x) -> x + x, pure because + is a builtin and pure
        // let f = (x) -> x^2 + 1, pure because + and ^ are builtins and pure
        // let f = (x) -> { x }, not pure due to compound statement
        let rec isPure (body: Expr) : bool =
            List.length paramList = 1
            && match body with
               | ELiteral _ -> true // Literals are pure. However, should consider that maybe string literals are not pure
               | EIdentifier(name, _) ->
                   // Identifiers are pure if they are pure in the environment
                   checkIdentifier newEnv name |> Result.map _.IsPure |> Result.defaultValue false
               | ECall(callee, args, _) ->
                   // Calls are pure if the callee is pure and all arguments are pure
                   // For example, cos(1.0) is pure.
                   let callee = isPure callee
                   let args = List.forall isPure args
                   callee && args
               | EBlock _ -> false // Assume false to simplify
               | EGrouping(expr, _) -> isPure expr // Simple Wrapper
               | _ -> false // We can then assume any other expression is not pure.

        // Infer the body of the lambda
        infer aliases newEnv body
        |> Result.bind (fun (bodyType, sub, expr) ->
            // Substitute the new types into the lambda type
            let paramTypes = List.map (applySubstitution aliases sub) paramTypes
            let paramList = List.zip paramList paramTypes // F# doesn't have zip with unfortunately
            let paramList = List.map (fun (id, typ) -> (id, Some typ)) paramList
            let isPure = isPure expr

            // Return type either must be unified with the body type, or immediately substituted with the body type
            let returnT = Option.defaultValue (TTypeVariable(freshTypeVar ())) returnT

            unify aliases bodyType returnT
            |> Result.bind (fun sub' ->
                let sub = combineMaps sub sub'
                let returnT = applySubstitution aliases sub returnT

                Ok(
                    TFunction(paramTypes, returnT, isPure, false),
                    sub,
                    ELambda(
                        paramList,
                        expr,
                        Some returnT,
                        isPure,
                        Some(TFunction(paramTypes, returnT, isPure, false)),
                        false
                    )
                )))

    | ECall(callee, args, _) ->
        result {
            // Infer the type of the callee
            let! t, sub, expr = infer aliases env callee
            let t = applySubstitution aliases sub t

            // Infer the types of the arguments and extract the types, substitutions, and expressions
            let! argResults = inferArgs aliases env args
            let argTypes = List.map (fun (t, _, _) -> t) argResults
            let argSubs = List.map (fun (_, sub, _) -> sub) argResults
            let argExprs = List.map (fun (_, _, expr) -> expr) argResults

            match t with // t is the type of the callee
            | TFunction(paramTypes, ret, _, _) ->
                // Check that the number of arguments matches the number of parameters
                if List.length paramTypes <> List.length args then
                    return! Error [ TypeError.InvalidArgumentCount(callee, List.length paramTypes, List.length args) ]
                else
                    // Standard unification of the parameter types and argument types
                    // Return the return type of the function
                    let! sub' = unifyWithSubstitution aliases paramTypes argTypes Map.empty
                    let combinedSubs = List.fold combineMaps sub' argSubs
                    let returnType = applySubstitution aliases combinedSubs ret

                    match returnType with
                    | TConstrain constrain ->
                        let resolved = Map.tryFind constrain.TypeVar resolvedTypes.Value

                        match resolved with
                        | Some t -> return (t, combinedSubs, ECall(expr, argExprs, Some t))
                        | None -> return (returnType, combinedSubs, ECall(expr, argExprs, Some returnType))
                    | _ -> return (returnType, combinedSubs, ECall(expr, argExprs, Some returnType))
            | TConstrain constrain ->
                let f = constrain.Constrain
                let returnT = TTypeVariable(freshTypeVar ())
                let! sub = unify aliases t (TConstrain(Constrain(freshTypeVar (), f)))
                let t' = TFunction(argTypes, returnT, false, false)
                let sub = List.fold combineMaps sub argSubs
                // must return the return type of the function
                return (returnT, sub, ECall(expr, argExprs, Some t'))
            | TTypeVariable a ->
                let returnT = TTypeVariable(freshTypeVar ())
                let t = TFunction(argTypes, returnT, false, false)
                let! sub = unify aliases t (TTypeVariable a)
                let t' = applySubstitution aliases sub (TTypeVariable a)
                let sub = List.fold combineMaps sub argSubs
                return (t', sub, ECall(expr, argExprs, Some t))
            | TAny -> return (TAny, Map.empty, ECall(expr, args, Some TAny))
            | _ -> return! Error [ TypeError.InvalidCall(callee, t) ]
        }

    | EBlock(stmts, _) ->
        // A block is a sequence of statements, the type of the block is the type of the last statement
        // This is due to the expressive nature of the language, where the last statement is the return value
        result {
            // infer each of the statements in the block
            let! env, aliases, sub, _ = inferProgram aliases env stmts
            // get the last statement
            let lastStmt = List.last stmts
            // infer the type of the last statement specifically
            let! env, aliases, sub', _ = inferStmt aliases env lastStmt
            let sub = combineMaps sub sub'
            // get the type of the last statement
            let lastStmtType =
                match lastStmt with
                | SExpression(expr, _) ->
                    match infer aliases env expr with
                    | Ok(t, _, _) -> t
                    | Error _ -> TNever
                | SVariableDeclaration _ -> TUnit
                | SAssertStatement _ -> TUnit
                | STypeDeclaration _ -> TUnit
                | SRecFunc _ -> TUnit
                | SAsync _ -> TUnit
                | SImport _ -> TUnit

            return (lastStmtType, sub, EBlock(stmts, Some lastStmtType))
        }

    | EGrouping(expr, _) -> infer aliases env expr // Simply a wrapper
    | EIf(cond, thenBranch, elseBranch, _) ->
        // Infer the types of the condition, then branch, and else branch
        let thenResult = infer aliases env thenBranch
        let elseResult = infer aliases env elseBranch

        match thenResult, elseResult with
        | Ok(t1, sub1, expr1), Ok(t2, sub2, expr2) ->
            let sub = combineMaps sub1 sub2

            result {
                let! cT, sub3, expr3 = infer aliases env cond
                let cT = applySubstitution aliases sub cT
                let sub = combineMaps sub sub3
                // The condition must be a boolean
                let! sub' = unify aliases cT TBool
                let sub = combineMaps sub sub'
                // The types of the then and else branches must be the same
                let! sub' = unify aliases t1 t2
                let sub = combineMaps sub sub'
                let returnType = applySubstitution aliases sub t1
                return (returnType, sub, EIf(expr3, expr1, expr2, Some returnType))
            }
        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors
    | ETernary(cond, trueBranch, falseBranch, typ) -> infer aliases env (EIf(cond, trueBranch, falseBranch, typ)) // Same as if statement
    | ETuple(exprs, _) ->
        // Infer the types of the expressions in the tuple
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
        // Infer the types of the expressions in the list
        inferArgs aliases env exprs
        |> Result.bind (fun results ->
            let types = List.map (fun (t, _, _) -> t) results
            let subs = List.map (fun (_, sub, _) -> sub) results
            let exprs = List.map (fun (_, _, expr) -> expr) results

            // check all the types are the same
            let typeVar = freshTypeVar ()

            // Unify each type against each other (through a type variable)
            // Ensures that all types are the same
            unifyWithSubstitution aliases types (List.replicate (List.length types) (TTypeVariable typeVar)) Map.empty
            |> Result.bind (fun sub' ->
                let combinedSubs = List.fold combineMaps Map.empty subs
                let combinedSubs = combineMaps combinedSubs sub'

                // Default list type of an empty list is a type variable
                let head =
                    if List.length types > 0 then
                        List.head types
                    else
                        TTypeVariable(freshTypeVar ())

                // Calculate the dims based on the number of elements in the list
                let returnType =
                    TTensor(applySubstitution aliases combinedSubs head, Dims(List.length types))

                Ok(returnType, combinedSubs, EList(exprs, Some returnType))))
    | ERange(start, end_, _) ->
        result {
            let! typ1, sub2, expr2 = infer aliases env start
            let! typ2, sub3, expr3 = infer aliases env end_
            let sub = combineMaps sub2 sub3
            let! sub' = unify aliases typ1 TInteger // start must be an integer
            let! sub'' = unify aliases typ2 TInteger // end must be an integer
            let sub = combineMaps sub sub'
            let sub = combineMaps sub sub''

            // Basic sims calculation
            // Only if the expressions are immediately known
            // No variable resolution
            let dims =
                match expr2, expr3 with
                | ELiteral(LNumber(LInteger n1), _), ELiteral(LNumber(LInteger n2), _) -> Dims(n2 - n1)
                | _, _ -> DAny

            let returnType = TTensor(TInteger, dims)
            return (returnType, sub, ERange(expr2, expr3, Some returnType))
        }

    | EIndex(expr, start, _) ->
        let startNum =
            match start with
            | ELiteral(LNumber(LInteger n), _) -> Some n
            | _ -> None

        result {
            let! t, sub, expr = infer aliases env expr
            // check its tensor
            let innerType = TTypeVariable (freshTypeVar())
            let! startType, sub', expr' = infer aliases env start
            let sub = combineMaps sub sub'
            let! sub' = unify aliases startType TInteger
            let sub = combineMaps sub sub'
            
            let t = applySubstitution aliases sub t
            let startType = applySubstitution aliases sub startType
            
            let startNum = Option.defaultValue 0 startNum
            let unifier = TConstrain(Constrain(freshTypeVar(), fun t -> t.hasMinDims startNum))
            let! sub' = unify aliases t unifier
            let sub = combineMaps sub sub'
            let! sub' = unify aliases startType TInteger
            let sub = combineMaps sub sub'
            
            // return type is the inner type
            let returnType = innerType
            return (returnType, sub, EIndex(expr, expr', Some returnType))
        }
    | EIndexRange(expr, start, end_, _) ->
        let startNum =
            match start with
            | ELiteral(LNumber(LInteger n), _) -> Some n
            | _ -> None
            
        let endNum =
            match end_ with
            | ELiteral(LNumber(LInteger n), _) -> Some n
            | _ -> None
            
        let startNum = Option.defaultValue 0 startNum
        let endNum = Option.defaultValue -1 endNum
        
        result {
            let! t, sub, expr = infer aliases env expr
            // check its tensor
            let innerType = TTypeVariable (freshTypeVar())
            let! startType, sub', expr' = infer aliases env start
            let sub = combineMaps sub sub'
            let! sub' = unify aliases startType TInteger
            let sub = combineMaps sub sub'
            
            let! endType, sub', expr'' = infer aliases env end_
            let sub = combineMaps sub sub'
            let! sub' = unify aliases endType TInteger
            let sub = combineMaps sub sub'
            
            let t = applySubstitution aliases sub t
            let startType = applySubstitution aliases sub startType
            let endType = applySubstitution aliases sub endType
            
            let func = if endNum = -1 then fun (t: TType) -> t.hasMinDims startNum else fun t -> t.hasMinDims endNum
            let unifier = TConstrain(Constrain(freshTypeVar(), func))
            let! sub' = unify aliases t unifier
            let sub = combineMaps sub sub'
            let! sub' = unify aliases startType TInteger
            let sub = combineMaps sub sub'
            let! sub' = unify aliases endType TInteger
            let sub = combineMaps sub sub'
            
            // return type is the inner type
            let returnType = TTensor(innerType, Dims(endNum - startNum))
            return (returnType, sub, EIndexRange(expr, expr', expr'', Some returnType))
        }

 
    // lot of this doesnt work,

    // fails on the following:
    // let x = (x) -> x.a + 4
    // let y = { a = 4.0 }
    // x(y), doesnt infer that a must be an int, so fials on +
    // annoying error stupid hard to fix
    // above is fixed, new issue:
    // let x = (x) -> x.a + 4
    // let y = { a = 4, b = 4 }
    // fails
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
                ERecordExtend((name, expr1, Some t), expr2, Some(TRecord(TRowExtend(name, t, TRowEmpty))))
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
        // Infer the type of the record
        let recordResult = infer aliases env record

        match recordResult with
        | Ok(TRecord(row), sub, expr) ->
            let row = applySubstitution aliases sub row

            /// <summary>
            /// Find the type of a field in a record.
            /// </summary>
            /// <param name="row">The row of the record.</param>
            /// <param name="name">The name of the field.</param>
            /// <returns>The type of the field.</returns>
            let rec findType (row: TType) (name: Token) : TType =
                match row with
                | TRowEmpty -> TNever // Field not found
                | TRowExtend(label, typ, _) when label.Lexeme = name.Lexeme -> typ // Field found
                | TRowExtend(_, _, rest) -> findType rest name // TRowExtend is a recursive type
                | TTypeVariable n -> TTypeVariable n // I don't think this should happen
                | TConstrain constrain -> TConstrain constrain // I don't think this should happen
                | _ -> TNever // Should never happen

            let typ = findType row name

            match typ with
            | TNever -> Error [ TypeError.InvalidField(name, TRecord(row)) ]
            | _ -> Ok(typ, sub, ERecordSelect(expr, name, Some typ))
        | Ok(TTypeVariable n, sub, expr) ->
            // Allows for generic records
            let typeVar = freshTypeVar ()

            let sub =
                Map.add n (TRecord(TRowExtend(name, TTypeVariable typeVar, TRowEmpty))) sub

            // Return type is currently unknown
            Ok(TTypeVariable typeVar, sub, ERecordSelect(expr, name, Some(TTypeVariable typeVar)))
        | Ok(TAny, sub, expr) -> Ok(TAny, sub, expr) // Any type is allowed
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
                ERecordRestrict(expr, name, Some(TRecord(TRowExtend(name, TTypeVariable typeVar, TRowEmpty))))
            )
        | Ok(TAny, sub, expr) -> Ok(TAny, sub, expr)

        | Error errors -> Error errors
        | _ -> Error [ TypeError.InvalidField(name, TNever) ]
    | ECodeBlock e -> Ok(TAny, Map.empty, ECodeBlock e) // Code blocks are not typed

    | EMatch(expr, cases, _) ->
        infer aliases env expr
        |> Result.bind (fun (t, sub, expr) ->
            let inferCase (case: Pattern * Expr) =
                let pattern, caseExpr = case

                inferPattern aliases env pattern
                |> Result.bind (fun (patternType, sub', pattern, env) ->
                    let sub = combineMaps sub sub'
                    let patternType = applySubstitution aliases sub patternType

                    // need to bind pattern names
                    let newEnv = applySubstitutionToEnv aliases sub env

                    infer aliases newEnv caseExpr
                    |> Result.bind (fun (caseType, sub'', caseExpr) ->
                        let sub = combineMaps sub sub''

                        unify aliases t patternType
                        |> Result.bind (fun sub''' ->
                            let sub = combineMaps sub sub'''

                            let returnType = applySubstitution aliases sub caseType
                            Ok(returnType, sub, (pattern, caseExpr)))))

            let caseResults = List.map inferCase cases

            let hasErrors = List.exists Result.isError caseResults

            if hasErrors then
                let errors =
                    List.collect
                        (fun result ->
                            match result with
                            | Error errors -> errors
                            | _ -> [])
                        caseResults

                Error errors
            else
                let caseResults =
                    List.choose
                        (function
                        | Ok(t, sub, expr) -> Some(t, sub, expr)
                        | _ -> None)
                        caseResults

                let types = List.map (fun (t, _, _) -> t) caseResults
                let subs = List.map (fun (_, sub, _) -> sub) caseResults
                let exprs = List.map (fun (_, _, expr) -> expr) caseResults

                let subs = List.fold combineMaps Map.empty subs

                let head = List.tryHead types
                let head = Option.defaultValue (TTypeVariable(freshTypeVar ())) head
                let returnType = applySubstitution aliases subs head

                Ok(returnType, subs, EMatch(expr, List.zip (List.map fst cases) (List.map snd exprs), Some returnType)))

// doesnt work
and inferPattern
    (aliases: AliasMap)
    (env: TypeEnv)
    (pattern: Pattern)
    : (TType * Substitution * Pattern * TypeEnv) TypeResult =
    match pattern with
    | PWildcard -> Ok(TAny, Map.empty, PWildcard, env)
    | PIdentifier id ->
        Ok(
            TTypeVariable(freshTypeVar ()),
            Map.empty,
            PIdentifier id,
            Map.add id.Lexeme (TTypeVariable(freshTypeVar ())) env
        )
    | PCons(head, rest) ->
        let headResult = inferPattern aliases env head
        let restResult = inferPattern aliases env rest

        match headResult, restResult with
        | Ok(t1, sub1, expr1, env1), Ok(_, sub2, expr2, env2) ->
            let env = combineMaps env1 env2
            let sub = combineMaps sub1 sub2
            let sub = combineMaps sub sub2

            let newType = TTensor(t1, DAny)
            Ok(newType, sub, PCons(expr1, expr2), env)
        | Error errors, _ -> Error errors
        | _, Error errors -> Error errors
    | PList elems ->
        let results = List.map (inferPattern aliases env) elems

        let hasErrors = List.exists Result.isError results

        if hasErrors then
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
                List.choose
                    (function
                    | Ok(t, sub, expr, env) -> Some(t, sub, expr, env)
                    | _ -> None)
                    results

            let types = List.map (fun (t, _, _, _) -> t) results
            let subs = List.map (fun (_, sub, _, _) -> sub) results
            let exprs = List.map (fun (_, _, expr, _) -> expr) results
            let envs = List.map (fun (_, _, _, env) -> env) results

            let head = List.tryHead types
            let tail = if List.length types > 1 then List.tail types else []
            let head = Option.defaultValue (TTypeVariable(freshTypeVar ())) head

            let subResults = List.map (unify aliases head) tail

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

                let env = List.fold combineMaps Map.empty envs
                let combinedSubs = List.fold combineMaps Map.empty subResults
                let combinedSubs = List.fold combineMaps combinedSubs subs

                // doesnt work
                let returnType = applySubstitution aliases combinedSubs head
                let returnType = TTensor(returnType, DAny)

                Ok(returnType, combinedSubs, PList(exprs), env)
    | PRecordEmpty -> Ok(TRecord(TRowEmpty), Map.empty, PRecordEmpty, env)
    | PLiteral(lit) ->
        let t = checkLiteral lit
        Ok(t, Map.empty, PLiteral(lit), env)
    | PTuple(elems) ->
        let types = List.map (inferPattern aliases env) elems

        let hasErrors = List.exists Result.isError types

        if hasErrors then
            let errors =
                List.collect
                    (fun result ->
                        match result with
                        | Error errors -> errors
                        | _ -> [])
                    types

            Error errors
        else
            let results =
                List.choose
                    (function
                    | Ok(t, sub, expr, env) -> Some(t, sub, expr, env)
                    | _ -> None)
                    types

            let types = List.map (fun (t, _, _, _) -> t) results
            let subs = List.map (fun (_, sub, _, _) -> sub) results
            let exprs = List.map (fun (_, _, expr, _) -> expr) results
            let envs = List.map (fun (_, _, _, env) -> env) results

            let combinedSubs = List.fold combineMaps Map.empty subs
            let env = List.fold combineMaps Map.empty envs

            let returnType = TTuple(types)

            Ok(returnType, combinedSubs, PTuple(exprs), env)

/// <summary>
/// Infer the type of a statement.
/// </summary>
/// <param name="aliases">The type aliases.</param>
/// <param name="env">The type environment.</param>
/// <param name="stmt">The statement to infer.</param>
/// <returns>The type of the statement, the new aliases, the substitution, and the new statement.</returns>
and inferStmt (aliases: AliasMap) (env: TypeEnv) (stmt: Stmt) : (TypeEnv * AliasMap * Substitution * Stmt) TypeResult =
    // make this immutable later, pass it around, or resolved in substitution
    // reset each time to prevent the need for generalisation
    resolvedTypes.Value <- Map.empty

    match stmt with
    | SExpression(expr, _) ->
        // Simple case, infer the type of the expression
        infer aliases env expr
        |> Result.map (fun (t, sub, expr) -> (env, aliases, sub, SExpression(expr, Some t)))
    | SVariableDeclaration(name, expr, typ) ->
        result {
            let! t, sub, expr = infer aliases env expr
            let t = applySubstitution aliases sub t
            let typ = Option.defaultValue t typ // if the variable is typed
            let! sub' = unify aliases t typ // check that the given type is correct
            let sub = combineMaps sub sub'
            let typ = applySubstitution aliases sub typ
            let env = Map.add name.Lexeme typ env // allows for naming of variables not just by identifier (due to user defined ooperators)
            return (env, aliases, sub, SVariableDeclaration(name, expr, Some typ))
        }
    | SAssertStatement(expr, msg, _) ->
        result {
            let! t, sub, expr = infer aliases env expr
            let t = applySubstitution aliases sub t
            let! sub' = unify aliases t TBool
            let sub = combineMaps sub sub'

            let! msgT, sub', msg =
                match msg with
                | Some msg -> infer aliases env msg
                | None -> Ok(TString, Map.empty, ELiteral(LString "", TString))

            let sub = combineMaps sub sub'
            let! sub' = unify aliases msgT TString // check that the message is a string
            let sub = combineMaps sub sub'
            return (env, aliases, sub, SAssertStatement(expr, Some msg, Some TBool))
        }
    | STypeDeclaration(name, typ, _) ->
        // type Vector = { x: Float, y: Float }
        // Allows for aliases of types
        let alias = TAlias(name, Some typ) // Resolve the type later
        let aliases = Map.add name.Lexeme typ aliases // Add the alias to the list of aliases
        Ok(env, aliases, Map.empty, STypeDeclaration(name, alias, Some TUnit))

    | SAsync(name, parameters, body, returnT) ->
        result {
            // Simply call the same as SRecFunc, but with async set to true
            let! env, aliases, sub, stmt = inferStmt aliases env (SRecFunc(name, parameters, body, returnT))

            let stmt =
                match stmt with
                | SRecFunc(name, parameters, body, returnT) -> SAsync(name, parameters, body, returnT)
                | _ -> SAsync(name, parameters, body, returnT)

            return (env, aliases, sub, stmt)
        }

    | SRecFunc(name, parameters, body, returnT) -> // TODO, this is probably wrong
        let paramTypes = List.map snd parameters
        let paramList = List.map fst parameters

        let newParamType typ =
            match typ with
            | Some t -> t
            | None -> TTypeVariable(freshTypeVar ())

        // Default parameter types to type variables if not given
        let paramTypes = List.map newParamType paramTypes

        // Add the parameters to the environment
        let newEnv =
            List.fold2
                (fun acc param typ ->
                    match param.Lexeme with
                    | Identifier _ as id -> Map.add id typ acc
                    | _ -> acc)
                env
                paramList
                paramTypes

        // Add the function itself to the environment
        // Allows it to know about itself
        let newEnv =
            Map.add name.Lexeme (TFunction(paramTypes, TTypeVariable(freshTypeVar ()), false, false)) newEnv

        infer aliases newEnv body
        |> Result.bind (fun (bodyType, sub, expr) ->
            // Apply the substitution to the parameter types
            let paramTypes = List.map (applySubstitution aliases sub) paramTypes
            let paramList = List.zip paramList paramTypes
            let paramList = List.map (fun (id, typ) -> (id, Some typ)) paramList
            let returnT = Option.defaultValue (TTypeVariable(freshTypeVar ())) returnT // if not given default to type var

            unify aliases bodyType returnT
            |> Result.bind (fun sub' ->
                let sub = combineMaps sub sub'

                Ok(
                    newEnv,
                    aliases,
                    sub,
                    SAsync(name, paramList, expr, Some(TFunction(paramTypes, bodyType, false, false)))
                )))
    | SImport(name, path, _) ->
        // todo, binding to name (create record)
        try
            let parsed = parseFile path

            let _, parsed =
                match parsed with
                | Ok parsed -> parsed
                | Error e -> raise <| Exception $"{e}"

            let result = inferProgram aliases env parsed

            match result with
            | Ok(env', aliases', sub, _) ->
                let newEnv = combineMaps env env'
                let newAliases = combineMaps aliases aliases'
                Ok(newEnv, newAliases, sub, SImport(name, path, Some TUnit))
            | Error errors -> Error errors
        with ex ->
            Error [ TypeError.ImportError(name, path, ex.Message) ]


/// <summary>
/// Infer the type of a program (all the statements).
/// </summary>
/// <param name="aliases">The type aliases.</param>
/// <param name="env">The type environment.</param>
/// <param name="stmts">The statements to infer.</param>
/// <returns>The type environment, the new aliases, the substitution, and the new statements.</returns>
and inferProgram
    (aliases: AliasMap)
    (env: TypeEnv)
    (stmts: Program)
    : (TypeEnv * AliasMap * Substitution * Program) TypeResult =
    // Fold all the statemnets, accumulating the environment, aliases, substitution, and new statements (or errors)
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

/// <summary>
/// Quickly infer the type of a statement, without returning a result of the new stuff.
/// Just used to check if the program is valid.
/// </summary>
/// <param name="aliases">The type aliases.</param>
/// <param name="env">The type environment.</param>
/// <param name="stmt">The statement to infer.</param>
/// <returns>The type environment.</returns>
/// <exception cref="TypeException">If the statement is invalid.</exception>
let quickInferStmt (aliases: AliasMap) (env: TypeEnv) (stmt: Stmt) : TypeEnv =
    match inferStmt aliases env stmt with
    | Ok(env, _, _, _) -> env
    | Error errors -> raise <| TypeException errors

/// <summary>
/// Infer the program with a default type environment.
/// </summary>
/// <param name="stmts">The statements to infer.</param>
/// <returns>The type environment, the new aliases, the substitution, and the new statements.</returns>
let inferProgram1 (stmts: Program) : (TypeEnv * AliasMap * Substitution * Program) TypeResult =
    let env = defaultTypeEnv
    let aliases = Map.empty

    inferProgram aliases env stmts
