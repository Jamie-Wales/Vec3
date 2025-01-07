/// <summary>
/// Module to determine whether a given expression is a tail expression.
/// </summary>

module Vec3.Interpreter.SyntaxAnalysis.TailAnalyser

open Vec3.Interpreter.Grammar



/// <summary>
/// Find any tail expressions in a given list of statements.
/// </summary>
let rec analyseStmts (stmt: Stmt list) : Stmt list = List.map analyseStmt stmt

/// <summary>
/// Analyse a single statement to find tail expressions.
/// </summary>
and analyseStmt (stmt: Stmt) : Stmt =
    match stmt with
    | SRecFunc(a, b, body, c) ->
        let newBody = analyseExpr true body
        SRecFunc(a, b, newBody, c)
    | _ -> stmt


/// <summary>
/// Find the tail expression in a given expression.
/// </summary>
and analyseExpr (isTail: bool) (expr: Expr) : Expr =
    match expr with
    // Simple cases where the expression itself is in tail position
    | ELiteral _ -> expr
    | EIdentifier _ -> expr

    // Conditional expressions
    | EIf(cond, thenBranch, elseBranch, ty) ->
        let newThen = ETail(thenBranch, ty)
        let newElse = ETail(elseBranch, ty)
        EIf(analyseExpr false cond, newThen, newElse, ty)

    | ETernary(cond, trueBranch, falseBranch, ty) ->
        let newTrue = analyseExpr isTail trueBranch
        let newFalse = analyseExpr isTail falseBranch
        ETernary(analyseExpr false cond, newTrue, newFalse, ty)

    // Function calls
    | ECall(fn, args, ty) when isTail -> ETail(ECall(fn, args, ty), ty)
    | ECall(fn, args, ty) -> ECall(fn, List.map (analyseExpr false) args, ty)

    // Blocks: Only the last expression in the block can be in tail position
    | EBlock(stmts, t, ty) ->
        let newStmts =
            match List.rev stmts with
            | [] -> []
            | last :: rest ->
                match last with
                | SExpression(expr, _) ->
                    List.rev (analyseStmts (List.rev rest) @ [ SExpression(analyseExpr isTail expr, ty) ])
                | _ -> analyseStmts stmts

        EBlock(newStmts, t, ty)

    | ELambda(args, body, ty, pure', retTy, asyncFlag) ->
        ELambda(args, analyseExpr true body, ty, pure', retTy, asyncFlag)

    | EList(elements, ty) -> EList(List.map (analyseExpr false) elements, ty)
    | ETuple(elements, ty) -> ETuple(List.map (analyseExpr false) elements, ty)
    | EIndex(target, index, ty) -> EIndex(analyseExpr false target, analyseExpr false index, ty)
    | EIndexRange(target, start, stop, ty) ->
        EIndexRange(analyseExpr false target, analyseExpr false start, analyseExpr false stop, ty)
    | ERecordExtend(field, rest, ty) -> ERecordExtend(field, analyseExpr false rest, ty)
    | ERecordRestrict(record, token, ty) -> ERecordRestrict(analyseExpr false record, token, ty)
    | _ -> expr
