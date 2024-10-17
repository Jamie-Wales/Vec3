module Vec3.Interpreter.ConstantFolding

open Token
open Eval
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
        | SPrintStatement(expr, typ) -> SPrintStatement(foldExpr expr, typ)
        | SAssertStatement(expr, msg, typ) -> SAssertStatement(foldExpr expr, Option.map foldExpr msg, typ)

    and foldExpr (expr: Expr) : Expr =
        match expr with
        | ELiteral(lit, typ) -> ELiteral(lit, typ)
        | EBlock(stmts, typ) -> EBlock(foldStatements stmts, typ)
        | EIdentifier(token, typ) -> EIdentifier(token, typ)
        | EBinary(lhs, opp, rhs, typ) ->
            let lhss = foldExpr lhs
            let rhss = foldExpr rhs
            
            match opp, lhss, rhss with
            | { Lexeme = Operator op }, ELiteral(LNumber lhs, _), ELiteral(LNumber rhs, _) ->
                match op with
                | Operator.Plus ->
                    match evalAddition(lhs, rhs) with
                    | Ok res -> ELiteral(LNumber(res), typ)
                    | Error _ -> EBinary(lhss, opp, rhss, typ)
                 
                | Operator.Minus ->
                    match evalSubtraction(lhs, rhs) with
                    | Ok res -> ELiteral(LNumber(res), typ)
                    | Error _ -> EBinary(lhss, opp, rhss, typ)
                    
                | Operator.Star ->
                    match evalMultiplication(lhs, rhs) with
                    | Ok res -> ELiteral(LNumber(res), typ)
                    | Error _ -> EBinary(lhss, opp, rhss, typ)
                    
                | Operator.Slash ->
                    match evalDivision(lhs, rhs) with
                    | Ok res -> ELiteral(LNumber(res), typ)
                    | Error _ -> EBinary(lhss, opp, rhss, typ)
                | Operator.Caret
                | Operator.StarStar ->
                    match evalPower(lhs, rhs) with
                    | Ok res -> ELiteral(LNumber(res), typ)
                    | Error _ -> EBinary(lhss, opp, rhss, typ)
                    
                | Operator.Percent ->
                    match evalModulo(lhs, rhs) with
                    | Ok res -> ELiteral(LNumber(res), typ)
                    | Error _ -> EBinary(lhss, opp, rhss, typ)
                | Operator.EqualEqual -> ELiteral(LBool(lhs = rhs), typ)
                | Operator.BangEqual -> ELiteral(LBool(lhs <> rhs), typ)
                | Operator.Less -> ELiteral(LBool(lhs < rhs), typ)
                | Operator.LessEqual -> ELiteral(LBool(lhs <= rhs), typ)
                | Operator.Greater -> ELiteral(LBool(lhs > rhs), typ)
                | Operator.GreaterEqual -> ELiteral(LBool(lhs >= rhs), typ)
                | _ -> EBinary(lhss, opp, rhss, typ)
            | { Lexeme = Operator op }, ELiteral(LBool lhs, _), ELiteral(LBool rhs, _) ->
                match op with
                | Operator.EqualEqual -> ELiteral(LBool(lhs = rhs), typ)
                | Operator.BangEqual -> ELiteral(LBool(lhs <> rhs), typ)
                | Operator.AmpersandAmpersand -> ELiteral(LBool(lhs && rhs), typ) // SHORT CIRCUIT
                | Operator.PipePipe -> ELiteral(LBool(lhs || rhs), typ) // SHORT CIRCUIT
                | _ -> EBinary(lhss, opp, rhss, typ)
            | _ -> EBinary(lhss, opp, rhss, typ)
        | EUnary(opp, expr, typ) as un ->
            let valuee = foldExpr expr
            match opp, valuee with
            | { Lexeme = Operator op }, ELiteral(value, _) ->
                match op with
                | Bang ->
                match value with
                    | LNumber(LInteger x) -> ELiteral(LNumber(LInteger(if x = 0 then 1 else 0)), typ)
                    | LNumber(LFloat x) -> ELiteral(LNumber(LFloat(if x = 0.0 then 1.0 else 0.0)), typ)
                    | LBool b -> ELiteral(LBool(not b), typ)
                    | _ -> un
                | Minus ->
                    match value with
                    | LNumber(LInteger x) -> ELiteral(LNumber(LInteger(-x)), typ)
                    | LNumber(LFloat x) -> ELiteral(LNumber(LFloat(-x)), typ)
                    | LNumber(LRational(n, d)) -> ELiteral(LNumber(LRational(-n, d)), typ)
                    | _ -> un

                | Plus ->
                    match value with
                    | LNumber(LInteger x) -> ELiteral(LNumber(LInteger(if x < 0 then 0 - x else x)), typ)
                    | LNumber(LFloat x) -> ELiteral(LNumber(LFloat(if x < 0.0 then 0.0 - x else x)), typ)
                    | LNumber(LRational(n, d)) -> ELiteral(LNumber(LRational(if n < 0 then 0 - n, d else n, d)), typ)
                    | _ -> EUnary(opp, valuee, typ)
                | _ -> EUnary(opp, valuee, typ)
            | _ -> EUnary(opp, valuee, typ)
        | EGrouping (expr, typ) -> EGrouping(foldExpr expr, typ)
        | ECall (expr, args, typ) -> ECall(foldExpr expr, List.map foldExpr args, typ)
        | EList (elems, typ) -> EList(List.map foldExpr elems, typ)
        | EIndex (expr, index, typ) -> EIndex(foldExpr expr, foldExpr index, typ)
        | ELambda(args, body, typ) -> ELambda(args, foldExpr body, typ)
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
        | ERecord(fields, typ) -> ERecord(List.map (fun (f, e, t) -> (f, foldExpr e, t)) fields, typ)
        | ERecordUpdate(record, fields, typ) -> ERecordUpdate(foldExpr record, List.map (fun (f, e, t) -> (f, foldExpr e, t)) fields, typ)

    foldStatements program
