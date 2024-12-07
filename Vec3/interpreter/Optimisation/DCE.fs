module Vec3.Interpreter.DCE

open Grammar
open Token

// simple dead code elimination, type system stores purity, can eliminate calls to pure functions if result is not used for example,
// can also eliminate unused variables with ease (also can be done with a liveness analysis)
// also check standard expressions, if not uses for anything with side effects, can be eliminated
// also check for unused functions, can be eliminated
// possibly also inline some calls, maybe not here though

let sideEffectFunctions =
    builtInFunctionMap
    |> Map.filter (fun _ -> hasSideEffects)
    |> Map.keys
    |> List.ofSeq

let rec hasSideEffects (expr: Expr) : bool =
    match expr with
    | ELiteral _ -> false
    | EList(exprs, _) -> List.exists hasSideEffects exprs
    | ETuple(exprs, _) -> List.exists hasSideEffects exprs
    | EGrouping(expr, _) -> hasSideEffects expr
    | EIdentifier(token, _) -> List.exists (fun x -> x = token.Lexeme) sideEffectFunctions
    | ECall(callee, args, _) -> hasSideEffects callee || List.exists hasSideEffects args
    | EIf(cond, thenBranch, elseBranch, _) ->
        hasSideEffects cond || hasSideEffects thenBranch || hasSideEffects elseBranch
    | EIndex(expr, start, _) -> hasSideEffects expr || hasSideEffects start
    | EIndexRange(expr, start, end_, _) -> hasSideEffects expr || hasSideEffects start || hasSideEffects end_
    | EBlock(stmts, _) -> true // todo
    | ELambda(_, body, _, pr, _, _) -> not pr || hasSideEffects body
    | ERecordSelect(expr, _, _) -> hasSideEffects expr
    | ETernary(cond, thenBranch, elseBranch, _) ->
        hasSideEffects cond || hasSideEffects thenBranch || hasSideEffects elseBranch
    | ECodeBlock e -> hasSideEffects e
    | ERecordExtend((_, body, _), fields, _) -> hasSideEffects body || hasSideEffects fields
    | ERecordEmpty _ -> false
    | ERange(start, end_, _) -> hasSideEffects start || hasSideEffects end_
    | EMatch(expr, cases, _) -> hasSideEffects expr || List.exists (fun (_, e) -> hasSideEffects e) cases
    | ERecordRestrict(expr, _, _) -> hasSideEffects expr
    | ETail(expr, _) -> hasSideEffects expr

let rec hasSideEffectsStmt (stmt: Stmt) : bool =
    match stmt with
    | SExpression(expr, _) -> hasSideEffects expr
    | SAssertStatement _ -> true
    | SVariableDeclaration _ -> true
    | SRecFunc _ -> true
    | SAsync _ -> true
    | STypeDeclaration _ -> true
    | SImport _ -> true

let rec eliminate (program: Program) : Program =
    let rec pass (stmts: Stmt list) : Stmt list =
        match stmts with
        | [] -> []
        | SVariableDeclaration(name, expr, t) :: rest ->
            let rest = pass rest

            if isUsed name.Lexeme rest then
                SVariableDeclaration(name, expr, t) :: rest
            else
                rest
        | SAsync(name, params', body, t) :: rest
        | SRecFunc(name, params', body, t) :: rest ->
            let rest = pass rest

            if isUsed name.Lexeme rest then
                SRecFunc(name, params', body, t) :: rest
            else
                rest
        | SAssertStatement _ as stmt :: rest -> stmt :: pass rest
        | SExpression(expr, _) as stmt :: rest -> if hasSideEffects expr then stmt :: pass rest else pass rest
        | STypeDeclaration _ as stmt :: rest -> stmt :: pass rest
        | SImport _ as stmt :: rest -> stmt :: pass rest

    let rec refine stmts =
        let refined = pass stmts
        if refined = stmts then refined else refine refined

    program |> List.filter hasSideEffectsStmt |> refine

and isUsed (name: Lexeme) (stmts: Stmt list) : bool =
    stmts |> List.exists (isUsedInStmt name)

and isUsedInStmt (name: Lexeme) (stmt: Stmt) : bool =
    match stmt with
    | SExpression(expr, _) -> isUsedE name expr
    | SVariableDeclaration(name_, expr, _) -> name <> name_.Lexeme && isUsedE name expr
    | SAssertStatement(expr, _, _) -> isUsedE name expr
    | SRecFunc(_, _, body, _) -> isUsedE name body
    | SAsync(_, _, body, _) -> isUsedE name body
    | SImport _ -> false
    | STypeDeclaration _ -> false

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
    | EBlock(stmts, _) -> isUsed name stmts
    | ELambda(_, body, _, _, _, _) -> isUsedE name body
    | ERecordSelect(expr, _, _) -> isUsedE name expr
    | ECodeBlock e -> isUsedE name e
    | ERecordExtend((_, body, _), fields, _) -> isUsedE name body || isUsedE name fields
    | ERecordEmpty _ -> false
    | ERange(start, end_, _) -> isUsedE name start || isUsedE name end_
    | EMatch(expr, cases, _) -> isUsedE name expr || List.exists (fun (_, e) -> isUsedE name e) cases
    | ERecordRestrict(expr, _, _) -> isUsedE name expr
    | ETail(expr, _) -> isUsedE name expr
