/// <summary>
/// Dead code elimination.
/// simple dead code elimination, type system stores purity, can eliminate calls to pure functions if result is not used for example,
/// can also eliminate unused variables with ease (also can be done with a liveness analysis)
/// also check standard expressions, if not uses for anything with side effects, can be eliminated
/// also check for unused functions, can be eliminated
/// possibly also inline some calls, maybe not here though
/// </summary>
module Vec3.Interpreter.DCE

open Grammar
open Token


/// <summary>
/// Functions that have side effects.
/// </summary>
let sideEffectFunctions: (Lexeme list) ref =
    builtInFunctionMap
    |> Map.filter (fun _ -> hasSideEffects)
    |> Map.keys
    |> List.ofSeq
    |> ref

/// <summary>
/// True if the expression has side effects.
/// </summary>
/// <param name="expr">The expression.</param>
/// <returns>True if the expression has side effects.</returns>
let rec hasSideEffects (expr: Expr) : bool =
    match expr with
    | ELiteral _ -> false
    | EList(exprs, _) -> List.exists hasSideEffects exprs
    | ETuple(exprs, _) -> List.exists hasSideEffects exprs
    | EGrouping(expr, _) -> hasSideEffects expr
    | EIdentifier(token, _) -> List.exists (fun x -> x = token.Lexeme) sideEffectFunctions.Value
    | ECall(callee, args, _) -> hasSideEffects callee || List.exists hasSideEffects args
    | EIf(cond, thenBranch, elseBranch, _) ->
        hasSideEffects cond || hasSideEffects thenBranch || hasSideEffects elseBranch
    | EIndex(expr, start, _) -> hasSideEffects expr || hasSideEffects start
    | EIndexRange(expr, start, end_, _) -> hasSideEffects expr || hasSideEffects start || hasSideEffects end_
    | EBlock(stmts, _, _) -> true // todo
    | ELambda(_, body, _, _, _, _) -> hasSideEffects body
    | ERecordSelect(expr, _, _) -> hasSideEffects expr
    | ETernary(cond, thenBranch, elseBranch, _) ->
        hasSideEffects cond || hasSideEffects thenBranch || hasSideEffects elseBranch
    | ECodeBlock e -> hasSideEffects e
    | ERecordExtend((_, body, _), fields, _) -> hasSideEffects body || hasSideEffects fields
    | ERecordEmpty _ -> false
    | ERange(start, end_, _) -> hasSideEffects start || hasSideEffects end_
    | ERecordRestrict(expr, _, _) -> hasSideEffects expr
    | ETail(expr, _) -> hasSideEffects expr

/// <summary>
/// True if the statement has side effects.
/// </summary>
/// <param name="stmt">The statement.</param>
/// <returns>True if the statement has side effects.</returns>
let rec hasSideEffectsStmt (stmt: Stmt) : bool =
    match stmt with
    | SExpression(expr, _) -> hasSideEffects expr
    | SAssertStatement _ -> true
    | SVariableDeclaration(name, expr, t) ->
        if hasSideEffects expr then
            // If a variable declaration has side effects, track it as a potential side-effecting function
            match expr with
            | ELambda _ -> sideEffectFunctions.Value <- name.Lexeme :: sideEffectFunctions.Value
            | _ -> ()

        true

    | SRecFunc _ -> true
    | SAsync _ -> true
    | STypeDeclaration _ -> true
    | SImport _ -> true

/// <summary>
/// Eliminates dead code.
/// </summary>
/// <param name="program">The program.</param>
/// <returns>The program with dead code eliminated.</returns>
let rec eliminate (program: Program) : Program =
    let rec pass (stmts: Stmt list) : Stmt list =
        match stmts with
        | [] -> []
        | SVariableDeclaration(name, expr, t) :: rest ->
            let rest = pass rest
            SVariableDeclaration(name, expr, t) :: rest
        | SAsync(name, params', body, t) :: rest ->
            let rest = pass rest

            SAsync(name, params', body, t) :: rest
        | SRecFunc(name, params', body, t) :: rest ->
            let rest = pass rest

            SRecFunc(name, params', body, t) :: rest
        | SAssertStatement _ as stmt :: rest -> stmt :: pass rest
        | SExpression(expr, _) as stmt :: rest -> if hasSideEffects expr then stmt :: pass rest else pass rest
        | STypeDeclaration _ as stmt :: rest -> stmt :: pass rest
        | SImport _ as stmt :: rest -> stmt :: pass rest

    /// <summary>
    /// Multiple passes of dead code elimination.
    /// </summary>
    /// <param name="stmts">The statements.</param>
    /// <returns>The statements with dead code eliminated.</returns>
    let rec refine stmts =
        let refined = pass stmts
        if refined = stmts then refined else refine refined

    program |> List.filter hasSideEffectsStmt |> refine

/// <summary>
/// Checks if a variable is used in a list of statements.
/// </summary>
/// <param name="name">The name of the variable.</param>
/// <param name="stmts">The statements.</param>
/// <returns>True if the variable is used.</returns>
and isUsed (name: Lexeme) (stmts: Stmt list) : bool =
    stmts |> List.exists (isUsedInStmt name)

/// <summary>
/// Checks if a variable is used in a statement.
/// </summary>
/// <param name="name">The name of the variable.</param>
/// <param name="stmt">The statement.</param>
/// <returns>True if the variable is used.</returns>
and isUsedInStmt (name: Lexeme) (stmt: Stmt) : bool =
    match stmt with
    | SExpression(expr, _) -> isUsedE name expr
    | SVariableDeclaration(name_, expr, _) -> name <> name_.Lexeme && isUsedE name expr
    | SAssertStatement(expr, _, _) -> isUsedE name expr
    | SRecFunc(_, _, body, _) -> isUsedE name body
    | SAsync(_, _, body, _) -> isUsedE name body
    | SImport _ -> false
    | STypeDeclaration _ -> false

/// <summary>
/// Checks if a variable is used in an expression.
/// </summary>
/// <param name="name">The name of the variable.</param>
/// <param name="expr">The expression.</param>
/// <returns>True if the variable is used.</returns>
and isUsedE (name: Lexeme) (expr: Expr) : bool =
    match expr with
    | ELiteral _ -> false
    | EList(exprs, _)
    | ETuple(exprs, _) -> List.exists (isUsedE name) exprs
    | EGrouping(expr, _) -> isUsedE name expr
    | EIdentifier(name_, _) -> name = name_.Lexeme
    | ECall(callee, args, _) -> isUsedE name callee || List.exists (isUsedE name) args
    | ETernary(cond, thenBranch, elseBranch, _)
    | EIf(cond, thenBranch, elseBranch, _) -> isUsedE name cond || isUsedE name thenBranch || isUsedE name elseBranch
    | EIndex(expr, start, _) -> isUsedE name expr || isUsedE name start
    | EIndexRange(expr, start, end_, _) -> isUsedE name expr || isUsedE name start || isUsedE name end_
    | EBlock(stmts, _, _) -> isUsed name stmts
    | ELambda(_, body, _, _, _, _) -> isUsedE name body
    | ERecordSelect(expr, _, _) -> isUsedE name expr
    | ECodeBlock e -> isUsedE name e
    | ERecordExtend((_, body, _), fields, _) -> isUsedE name body || isUsedE name fields
    | ERecordEmpty _ -> false
    | ERange(start, end_, _) -> isUsedE name start || isUsedE name end_
    | ERecordRestrict(expr, _, _) -> isUsedE name expr
    | ETail(expr, _) -> isUsedE name expr
