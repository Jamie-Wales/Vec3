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
    | _ -> failwith "invlaid"
    
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
    | EBlock stmts ->
        let rec evalBlock (env: Env) = function
            | [] -> ELiteral LUnit
            | [stmt] -> fst (evalStmt env stmt)
            | stmt :: rest -> 
                let _, env' = evalStmt env stmt
                evalBlock env' rest

        evalBlock env stmts
    | EAssignment(_, expr) -> evalExpr env expr
    | EGrouping expr -> evalExpr env expr

    | ELiteral lit -> ELiteral (evalLiteral lit)
    | ECall (expr, args) ->
        let expr = evalExpr env expr
        match expr with
        | ELambda(params', _, body) ->
            let rec evalParams (env: Env) (params': (Token * Grammar.Type) list) (args: Expr list) =
                match params', args with
                | [], [] -> env
                | (p, _) :: ps, a :: as' ->
                    let env' = evalParams env ps as'
                    Map.add p.lexeme (evalExpr env a) env'
                | _ -> failwith "invalid"

            let env' = evalParams env params' args
            evalExpr env' body
        | EIdentifier { lexeme = name } ->
            match name with
            | Identifier name ->
                match name with
                | "env" ->
                    Map.iter (fun k v -> printfn $"{k}: {v}") env
                    ELiteral(LUnit)
                | "print" ->
                    let args = List.map (evalExpr env) args
                    printfn $"{String.Join(' ', args)}"
                    ELiteral(LUnit)
                | "input" ->
                    let args = List.map (evalExpr env) args
                    let input = Console.ReadLine()
                    ELiteral(LString input)
                | "cos" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LNumber(LFloat x)) ] -> ELiteral(LNumber(LFloat(Math.Cos(double x))))
                    | _ -> failwith "invalid"
                | "sin" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LNumber(LFloat x)) ] -> ELiteral(LNumber(LFloat(Math.Sin(double x))))
                    | _ -> failwith "invalid"
                | "tan" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LNumber(LFloat x)) ] -> ELiteral(LNumber(LFloat(Math.Tan(double x))))
                    | _ -> failwith "invalid"
                | _ -> failwith $"function {name} not found"
            | _ -> failwith $"function {name} not found"
        | _ -> failwith "invalid"
    | ELambda(params', t, body) -> ELambda(params', t, body)
    | EIdentifier name ->
        match env.TryGetValue name.lexeme with
        | true, expr -> expr
        | false, _ ->
            match name.lexeme with
            | Identifier "print"
            | Identifier "input"
            | Identifier "cos"
            | Identifier "sin"
            | Identifier "tan"
            | Identifier "env" -> EIdentifier name
            | _ -> failwith $"variable {name} not found"
    | EBinary(lhs, op, rhs) ->
        let lhs = evalExpr env lhs
        let rhs = evalExpr env rhs

        match op, lhs, rhs with
        | { lexeme = Operator op } , ELiteral (LNumber lhs), ELiteral (LNumber rhs) ->
            match op with
            | Operator.Plus -> ELiteral (LNumber (evalAddition (lhs, rhs)))
            | Operator.Minus -> ELiteral (LNumber (evalSubtraction (lhs, rhs)))
            | Operator.Star -> ELiteral (LNumber (evalMultiplication (lhs, rhs)))
            | Operator.Slash -> ELiteral (LNumber (evalDivision (lhs, rhs)))
            | Operator.StarStar -> ELiteral (LNumber (evalPower (lhs, rhs)))
            | Operator.Percent -> ELiteral (LNumber (evalModulo (lhs, rhs)))
            | Operator.EqualEqual -> ELiteral (LBool (lhs = rhs))
            | Operator.BangEqual -> ELiteral (LBool (lhs <> rhs))
            | Operator.Less -> ELiteral (LBool (lhs < rhs))
            | Operator.LessEqual -> ELiteral (LBool (lhs <= rhs))
            | Operator.Greater -> ELiteral (LBool (lhs > rhs))
            | Operator.GreaterEqual -> ELiteral (LBool (lhs >= rhs))
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
    | EUnary(op, expr) ->
        let value = evalExpr env expr

        match op, value with
        | { lexeme = Operator op }, ELiteral value ->
            match op with
            | Bang ->
                match value with
                | LNumber(LInteger x) ->
                    ELiteral(LNumber(LInteger(if x = 0 then 1 else 0)))
                | LNumber(LFloat x) ->
                    ELiteral(LNumber(LFloat(if x = 0.0 then 1.0 else 0.0)))
                | LBool b -> ELiteral(LBool(not b))
                | _ -> failwith "invalid"
            | Minus ->
                match value with
                | LNumber (LInteger x) -> ELiteral (LNumber (LInteger (if x = 0 then 1 else 0)))
                | LNumber (LFloat x) -> ELiteral (LNumber (LFloat (if x = 0.0 then 1 else 0)))
                | _ -> failwith "invalid"

            | Plus ->
                match value with
                | LNumber(LInteger x) ->
                    ELiteral(LNumber(LInteger(if x < 0 then 0 - x else x)))
                | LNumber(LFloat x) ->
                    ELiteral(LNumber(LFloat(if x < 0.0 then 0.0 - x else x)))
                | LNumber(LRational(n, d)) ->
                    ELiteral(LNumber(LRational(if n < 0 then 0 - n, d else n, d)))
                | _ -> failwith "invalid"
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
    | EIf(cond, then', else') ->
        let cond = evalExpr env cond
        match cond with
        | ELiteral(LBool true) -> evalExpr env then'
        | ELiteral(LBool false) -> evalExpr env else'
        | _ -> failwith "invalid"
    | ETernary(cond, then', else') ->
        evalExpr env (EIf(cond, then', else'))

and evalStmt (env: Env) (stmt: Stmt) : Expr * Env =
    match stmt with
    | SExpression expr -> evalExpr env expr, env

    | SVariableDeclaration(name, _, expr) ->
        let value = evalExpr env expr
        ELiteral LUnit, Map.add name.lexeme value env
    
    | SPrintStatement expr ->
        let value = evalExpr env expr
        printfn $"{value}"
        ELiteral LUnit, env

let evalStatement (env: Env) (stmt: Stmt) : Literal * Env =
    match evalStmt env stmt with
    | ELiteral lit, env -> lit, env
    | _, env -> LUnit , env
    
    

let evalProgram (env: Env) (program: Program) : Literal * Env =
    // return last staement and update env
    match List.fold (fun (lit, env) stmt -> evalStatement env stmt) (LUnit, env) program with
    | lit, env -> lit, env
    | _ -> failwith "invalid"
