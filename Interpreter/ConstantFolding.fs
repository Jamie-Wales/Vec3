module Vec3.Interpreter.ConstantFolding

open Token
open Grammar

let rec GCD a b =
    let r = a / b
    if r = 0 then
        b
    else
        GCD b r

let simplifyRational rat =
    match rat with
    | Rational(a, b) ->
        let gcd = GCD a b
        let a = a / gcd
        let b = b / gcd
        Rational(a, b)
    | _ -> failwith "bad"
    

let foldConstants (program: Program) : Program =
    let rec foldStatements (stmts: Stmt list) : Stmt list = List.map foldStmt stmts

    and foldStmt (stmt: Stmt) : Stmt =
        match stmt with
        | SVariableDeclaration(tok, expr, typ) -> SVariableDeclaration(tok, foldExpr expr, typ)
        | SExpression(expr, typ) -> SExpression(foldExpr expr, typ)
        | SAssertStatement(expr, msg, typ) -> SAssertStatement(foldExpr expr, Option.map foldExpr msg, typ)
        | STypeDeclaration(tok, typ, typ2) -> STypeDeclaration(tok, typ, typ2)

    and foldExpr (expr: Expr) : Expr =
        match expr with
        | ETail(expr, typ) -> ETail(foldExpr expr, typ)
        | ELiteral(lit, typ) -> ELiteral(lit, typ)
        | EBlock(stmts, typ) -> EBlock(foldStatements stmts, typ)
        | EIdentifier(token, typ) -> EIdentifier(token, typ)
        | EGrouping (expr, typ) -> EGrouping(foldExpr expr, typ)
        | ECall (expr, args, typ) -> ECall(foldExpr expr, List.map foldExpr args, typ)
        | EList (elems, typ) -> EList(List.map foldExpr elems, typ)
        | EIndex (expr, index, typ) -> EIndex(foldExpr expr, foldExpr index, typ)
        | ELambda(args, body, rt, pr, typ) -> ELambda(args, foldExpr body, rt, pr, typ)
        | EIf(condEx, thenEx, elseEx, typ) ->
            let cond = foldExpr condEx
            match cond with
            | ELiteral(LBool true, _) -> foldExpr thenEx
            | ELiteral(LBool false, _) -> foldExpr elseEx
            | _ ->
                EIf(cond, foldExpr thenEx, foldExpr elseEx, typ)
        | ETernary(condEx, thenEx, elseEx, typ) -> foldExpr (EIf(condEx, thenEx, elseEx, typ))
        | ETuple(elems, typ) -> ETuple(List.map foldExpr elems, typ)
        | ERecordSelect(record, field, typ) -> ERecordSelect(foldExpr record, field, typ)
        | ERecordExtend((field, value, typ), record, _) -> ERecordExtend((field, foldExpr value, typ), foldExpr record, typ)
        | ERecordRestrict(record, field, typ) -> ERecordRestrict(foldExpr record, field, typ)
        | ERecordEmpty(typ) -> ERecordEmpty(typ)
        | ERange(start, stop, typ) ->
            let start = foldExpr start
            let stop = foldExpr stop
            match start, stop with
            | ELiteral(LNumber(LInteger s), _), ELiteral(LNumber(LInteger e), _) ->
                let range = [for i in s..e -> ELiteral(LNumber(LInteger i), TInteger)]
                EList(range, typ)
            | _ -> ERange(start, stop, typ)
        | ECodeBlock e -> ECodeBlock e

    foldStatements program
