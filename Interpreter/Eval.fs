module Vec3.Interpreter.Eval

open Microsoft.FSharp.Core
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open System

type Env = Map<Lexeme, Expr>

let evalNumber = function 
    | LInteger x -> LInteger x
    | LFloat x -> LFloat x
    | LRational (n, d) -> LRational (n, d)
    | LComplex (r, i) -> LComplex (r, i)
                             
                             
let rec evalAddition = function
    | LInteger x, LInteger y -> LInteger (x + y)
    | LFloat x, LFloat y -> LFloat (x + y)
    | LRational (n1, d1), LRational (n2, d2) -> LRational (n1 * d2 + n2 * d1, d1 * d2)
    | LComplex (r1, i1), LComplex (r2, i2) -> LComplex (r1 + r2, i1 + i2)
    | _ -> failwith "invalid"

let evalSubtraction = function
    | LInteger x, LInteger y -> LInteger (x - y)
    | LFloat x, LFloat y -> LFloat (x - y)
    | LRational (n1, d1), LRational (n2, d2) -> LRational (n1 * d2 - n2 * d1, d1 * d2)
    | LComplex (r1, i1), LComplex (r2, i2) -> LComplex (r1 - r2, i1 - i2)
    | _ -> failwith "invalid"
    
let evalMultiplication = function
    | LInteger x, LInteger y -> LInteger (x * y)
    | LFloat x, LFloat y -> LFloat (x * y)
    | LRational (n1, d1), LRational (n2, d2) -> LRational (n1 * n2, d1 * d2)
    | LComplex (r1, i1), LComplex (r2, i2) -> LComplex (r1 * r2 - i1 * i2, r1 * i2 + r2 * i1)
    | _ -> failwith "invalid"

let evalDivision =
    function
    // this must be integer division as per requirement, not return floating point
    | LInteger x, LInteger y -> LInteger (x / y)
    | LFloat x, LFloat y -> LFloat (x / y)
    | LRational (n1, d1), LRational (n2, d2) -> LRational (n1 * d2, d1 * n2)
    | _ -> failwith "invalid"
    
let evalPower = function
    | LInteger x, LInteger y -> LInteger (int (Math.Pow (float x, float y)))
    | LFloat x, LFloat y -> LFloat (Math.Pow (x, y))
    | _ -> failwith "invalid"

let evalModulo = function
    | LInteger x, LInteger y -> LInteger (x % y)
    | _ -> failwith "invalid"

let evalLiteral = function
    | LNumber x -> LNumber <| evalNumber x
    | LString s -> LString s
    | LBool b -> LBool b
    | LUnit -> LUnit

let rec evalExpr (env: Env) =
    function
    | EBlock (stmts, _) ->
        let rec evalBlock (env: Env) = function
            | [] -> ELiteral (LUnit, TUnit)
            | [stmt] -> fst (evalStmt env stmt)
            | stmt :: rest -> 
                let _, env' = evalStmt env stmt
                evalBlock env' rest

        evalBlock env stmts
    | ETuple (exprs, typ) -> ETuple (List.map (evalExpr env) exprs, typ)
    | EList (exprs, typ) -> EList (List.map (evalExpr env) exprs, typ)
    | EAssignment(_, expr, _) -> evalExpr env expr
    | EGrouping (expr, _) -> evalExpr env expr

    | ELiteral (lit, t) -> ELiteral (evalLiteral lit, t)
    | ECall (expr, args, _) ->
        let expr = evalExpr env expr
        match expr with
        | ELambda(params', body, _) ->
            let rec evalParams (env: Env) (params': Token list) (args: Expr list) =
                match params', args with
                | [], [] -> env
                | p :: ps, a :: as' ->
                    let env' = evalParams env ps as'
                    Map.add p.Lexeme (evalExpr env a) env'
                | _ -> failwith "invalid"

            let env' = evalParams env params' args
            evalExpr env' body
        | EIdentifier ({ Lexeme = name }, _) ->
            match name with
            | Identifier name ->
                match name with
                | "env" ->
                    Map.iter (fun k v -> printfn $"{k}: {v}") env
                    ELiteral(LUnit, TUnit)
                | "print" ->
                    let args = List.map (evalExpr env) args
                    printfn $"{String.Join(' ', args)}"
                    ELiteral(LUnit, TUnit)
                | "input" ->
                    let _ = List.map (evalExpr env) args
                    let input = Console.ReadLine()
                    ELiteral(LString input, TString)
                | "cos" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(Math.Cos(double x))), TFloat)
                    | _ -> failwith "invalid"
                | "sin" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(Math.Sin(double x))), TFloat)
                    | _ -> failwith "invalid"
                | "tan" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(Math.Tan(double x))), TFloat)
                    | _ -> failwith "invalid"
                | _ -> failwith $"function {name} not found"
            | _ -> failwith $"function {name} not found"
        | _ -> failwith "invalid"
    | ELambda(params', body, t') -> ELambda(params', body, t')
    | EIdentifier (name, typ) ->
        match env.TryGetValue name.Lexeme with
        | true, expr -> expr
        | false, _ ->
            match name.Lexeme with
            | Identifier "print"
            | Identifier "input"
            | Identifier "cos"
            | Identifier "sin"
            | Identifier "tan"
            | Identifier "env" -> EIdentifier (name, typ)
            | _ -> failwith $"variable {name} not found"
    | EBinary(lhs, op, rhs, typ) ->
        let lhs = evalExpr env lhs
        let rhs = evalExpr env rhs

        match op, lhs, rhs with
        | { Lexeme = Operator op } , ELiteral (LNumber lhs, _), ELiteral (LNumber rhs, _) ->
            match op with
            | Operator.Plus -> ELiteral (LNumber (evalAddition (lhs, rhs)), typ)
            | Operator.Minus -> ELiteral (LNumber (evalSubtraction (lhs, rhs)), typ)
            | Operator.Star -> ELiteral (LNumber (evalMultiplication (lhs, rhs)), typ)
            | Operator.Slash -> ELiteral (LNumber (evalDivision (lhs, rhs)), typ)
            | Operator.StarStar -> ELiteral (LNumber (evalPower (lhs, rhs)), typ)
            | Operator.Percent -> ELiteral (LNumber (evalModulo (lhs, rhs)), typ)
            | Operator.EqualEqual -> ELiteral (LBool (lhs = rhs), typ)
            | Operator.BangEqual -> ELiteral (LBool (lhs <> rhs), typ)
            | Operator.Less -> ELiteral (LBool (lhs < rhs), typ)
            | Operator.LessEqual -> ELiteral (LBool (lhs <= rhs), typ)
            | Operator.Greater -> ELiteral (LBool (lhs > rhs), typ)
            | Operator.GreaterEqual -> ELiteral (LBool (lhs >= rhs), typ)
            | _ -> failwith "invalid"
        | { Lexeme = Operator op }, ELiteral (LBool lhs, _), ELiteral (LBool rhs, _) ->
            match op with
            | Operator.AmpersandAmpersand -> ELiteral (LBool (lhs && rhs), typ) // should short circuit ?
            | Operator.PipePipe -> ELiteral (LBool (lhs || rhs), typ)
            | _ -> failwith "invalid"
        | { Lexeme = Keyword kw }, ELiteral (LBool lhs, _), ELiteral (LBool rhs, _) ->
            match kw with
            | Keyword.And -> ELiteral (LBool (lhs && rhs), typ) // should short circuit ?
            | Keyword.Or -> ELiteral (LBool (lhs || rhs), typ)
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
    | EUnary(op, expr, typ) ->
        let value = evalExpr env expr

        match op, value with
        | { Lexeme = Operator op }, ELiteral (value, _) ->
            match op with
            | Bang ->
                match value with
                | LNumber(LInteger x) ->
                    ELiteral(LNumber(LInteger(if x = 0 then 1 else 0)), typ)
                | LNumber(LFloat x) ->
                    ELiteral(LNumber(LFloat(if x = 0.0 then 1.0 else 0.0)), typ)
                | LBool b -> ELiteral(LBool(not b), typ)
                | _ -> failwith "invalid"
            | Minus ->
                match value with
                | LNumber (LInteger x) -> ELiteral (LNumber (LInteger (-x)), typ)
                | LNumber (LFloat x) -> ELiteral (LNumber (LFloat (-x)), typ)
                | LNumber (LRational (n, d)) -> ELiteral (LNumber (LRational (-n, d)), typ)
                | _ -> failwith "invalid"

            | Plus ->
                match value with
                | LNumber(LInteger x) ->
                    ELiteral(LNumber(LInteger(if x < 0 then 0 - x else x)), typ)
                | LNumber(LFloat x) ->
                    ELiteral(LNumber(LFloat(if x < 0.0 then 0.0 - x else x)), typ)
                | LNumber(LRational(n, d)) ->
                    ELiteral(LNumber(LRational(if n < 0 then 0 - n, d else n, d)), typ)
                | _ -> failwith "invalid"
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
    | EIf(cond, then', else', _) ->
        let cond = evalExpr env cond
        match cond with
        | ELiteral(LBool true, _) -> evalExpr env then'
        | ELiteral(LBool false, _) -> evalExpr env else'
        | _ -> failwith "invalid"
    | ETernary(cond, then', else', typ) ->
        evalExpr env (EIf(cond, then', else', typ))
    | EIndex(expr, index, _) ->
        let expr = evalExpr env expr
        let index = evalExpr env index
        match expr, index with
        | EList (exprs, _), ELiteral (LNumber (LInteger i), _) -> List.item i exprs
        | _ -> failwith "invalid"

and evalStmt (env: Env) (stmt: Stmt) : Expr * Env =
    match stmt with
    | SExpression (expr, _) -> evalExpr env expr, env

    | SVariableDeclaration(name, expr, _) ->
        let value = evalExpr env expr
        ELiteral (LUnit, TUnit), Map.add name.Lexeme value env
    
    | SPrintStatement (expr, _) ->
        let value = evalExpr env expr
        printfn $"{value}"
        ELiteral (LUnit, TUnit), env

let evalStatement (env: Env) (stmt: Stmt) : Expr * Env =
    match evalStmt env stmt with
    | ELiteral (lit, typ), env -> ELiteral(lit, typ), env
    | EList (exprs, typ), env -> EList(exprs, typ), env
    | _, env -> ELiteral(LUnit, TUnit) , env
    
    

let evalProgram (env: Env) (program: Program) : Expr * Env =
    // return last statement and update env
    List.fold (fun (_, env) -> evalStatement env) (ELiteral(LUnit, TUnit), env) program
