module Vec3.Interpreter.Eval

open Microsoft.FSharp.Core
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open System

type Env = Map<Token, Expr>

let evalNumber = function 
    | TNumber.Integer x -> TNumber.Integer x
    | TNumber.Float x -> TNumber.Float x
    | TNumber.Rational (n, d) -> TNumber.Rational (n, d)
    | TNumber.Complex (r, i) -> TNumber.Complex (r, i)
                             
                             
let rec evalAddition = function
    | TNumber.Integer x, TNumber.Integer y -> TNumber.Integer (x + y)
    | TNumber.Float x, TNumber.Float y -> TNumber.Float (x + y)
    | TNumber.Rational (n1, d1), TNumber.Rational (n2, d2) -> TNumber.Rational (n1 * d2 + n2 * d1, d1 * d2)
    | TNumber.Complex (r1, i1), TNumber.Complex (r2, i2) -> TNumber.Complex (r1 + r2, i1 + i2)
    | _ -> failwith "invalid"

let evalSubtraction = function
    | TNumber.Integer x, TNumber.Integer y -> TNumber.Integer (x - y)
    | TNumber.Float x, TNumber.Float y -> TNumber.Float (x - y)
    | TNumber.Rational (n1, d1), TNumber.Rational (n2, d2) -> TNumber.Rational (n1 * d2 - n2 * d1, d1 * d2)
    | TNumber.Complex (r1, i1), TNumber.Complex (r2, i2) -> TNumber.Complex (r1 - r2, i1 - i2)
    | _ -> failwith "invlaid"
    
let evalMultiplication = function
    | TNumber.Integer x, TNumber.Integer y -> TNumber.Integer (x * y)
    | TNumber.Float x, TNumber.Float y -> TNumber.Float (x * y)
    | TNumber.Rational (n1, d1), TNumber.Rational (n2, d2) -> TNumber.Rational (n1 * n2, d1 * d2)
    | TNumber.Complex (r1, i1), TNumber.Complex (r2, i2) -> TNumber.Complex (r1 * r2 - i1 * i2, r1 * i2 + r2 * i1)
    | _ -> failwith "invalid"

let evalDivision =
    function
    // this must be integer division as per requirement, not return floating point
    | TNumber.Integer x, TNumber.Integer y -> TNumber.Integer (x / y)
    | TNumber.Float x, TNumber.Float y -> TNumber.Float (x / y)
    | TNumber.Rational (n1, d1), TNumber.Rational (n2, d2) -> TNumber.Rational (n1 * d2, d1 * n2)
    | _ -> failwith "invalid"
    
let evalPower = function
    | TNumber.Integer x, TNumber.Integer y -> TNumber.Integer (int (Math.Pow (float x, float y)))
    | TNumber.Float x, TNumber.Float y -> TNumber.Float (Math.Pow (x, y))
    | _ -> failwith "invalid"

let rec evalExpr (env: Env) =
    function
    | Block stmts ->
        let rec evalBlock (env: Env) = function
            | [] -> Literal Unit
            | [stmt] -> fst (evalStmt env stmt)
            | stmt :: rest -> 
                let _, env' = evalStmt env stmt
                evalBlock env' rest

        evalBlock env stmts
    | Assignment(_, expr) -> evalExpr env expr
    | Grouping expr -> evalExpr env expr

    | Literal (Literal.String s) -> Literal (Literal.String s)
    | Literal (Literal.Bool b) -> Literal (Literal.Bool b)
    | Literal (Literal.TNumber n) -> Literal (Literal.TNumber (evalNumber n))
    | Literal Unit -> Literal Unit
    | Call (name, args) -> 
        match env.TryGetValue name with
        | true, Lambda(params', _, body) ->
            let rec evalParams (env: Env) (params': (Token * Grammar.Type) list) (args: Expr list) =
                match params', args with
                | [], [] -> env
                | (p, _) :: ps, a :: as' ->
                    let env' = evalParams env ps as'
                    Map.add p (evalExpr env a) env'
                | _ -> failwith "invalid"

            let env' = evalParams env params' args
            evalExpr env' body
        | false, _ ->
            match name.lexeme with
            | Identifier name ->
                match name with
                | "print" ->
                    let args = List.map (evalExpr env) args
                    printfn $"{String.Join(' ', args)}"
                    Literal(Unit)
                | "input" ->
                    let args = List.map (evalExpr env) args
                    let input = Console.ReadLine()
                    Literal(Literal.String input)
                | "cos" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ Literal(Literal.TNumber(TNumber.Float x)) ] -> Literal(Literal.TNumber(TNumber.Float(Math.Cos(double x))))
                    | _ -> failwith "invalid"
                | "sin" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ Literal(Literal.TNumber(TNumber.Float x)) ] -> Literal(Literal.TNumber(TNumber.Float(Math.Sin(double x))))
                    | _ -> failwith "invalid"
                | "tan" ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ Literal(Literal.TNumber(TNumber.Float x)) ] -> Literal(Literal.TNumber(TNumber.Float(Math.Tan(double x))))
                    | _ -> failwith "invalid"
                | _ -> failwith $"function {name} not found"
            | _ -> failwith $"function {name} not found"
        | _ -> failwith $"function {name} not found"
    | Lambda(params', t, body) -> Lambda(params', t, body)
    | Expr.Identifier name ->
        match env.TryGetValue name with
        | true, expr -> expr
        | _ -> failwith $"variable {name} not found"
    | Binary(lhs, op, rhs) ->
        let lhs = evalExpr env lhs
        let rhs = evalExpr env rhs

        match op, lhs, rhs with

        | { lexeme = Operator op } , Expr.Literal (Literal.TNumber lhs), Expr.Literal (Literal.TNumber rhs) ->
            match op with
            | Operator.Plus -> Literal (Literal.TNumber (evalAddition (lhs, rhs)))
            | Operator.Minus -> Literal (Literal.TNumber (evalSubtraction (lhs, rhs)))
            | Operator.Star -> Literal (Literal.TNumber (evalMultiplication (lhs, rhs)))
            | Operator.Slash -> Literal (Literal.TNumber (evalDivision (lhs, rhs)))
            | Operator.StarStar -> Literal (Literal.TNumber (evalPower (lhs, rhs)))
            | Operator.EqualEqual -> Literal (Literal.Bool (lhs = rhs))
            | Operator.BangEqual -> Literal (Literal.Bool (lhs <> rhs))
            | Operator.Less -> Literal (Literal.Bool (lhs < rhs))
            | Operator.LessEqual -> Literal (Literal.Bool (lhs <= rhs))
            | Operator.Greater -> Literal (Literal.Bool (lhs > rhs))
            | Operator.GreaterEqual -> Literal (Literal.Bool (lhs >= rhs))
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
    | Unary(op, expr) ->
        let value = evalExpr env expr

        match op, value with
        | { lexeme = Operator op }, Literal value ->
            match op with
            | Bang ->
                match value with
                | Literal.TNumber(TNumber.Integer x) ->
                    Literal(Literal.TNumber(TNumber.Integer(if x = 0 then 1 else 0)))
                | Literal.TNumber(TNumber.Float x) ->
                    Literal(Literal.TNumber(TNumber.Float(if x = 0.0 then 1.0 else 0.0)))
                | Literal.Bool b -> Literal(Literal.Bool(not b))
                | _ -> failwith "invalid"
            | Minus ->
                match value with
                | Literal.TNumber (TNumber.Integer x) -> Literal (Literal.TNumber (TNumber.Integer (if x = 0 then 1 else 0)))
                | Literal.TNumber (TNumber.Float x) -> Literal (Literal.TNumber (TNumber.Float (if x = 0.0 then 1 else 0)))
                | _ -> failwith "invalid"

            | Plus ->
                match value with
                | Literal.TNumber(TNumber.Integer x) ->
                    Literal(Literal.TNumber(TNumber.Integer(if x < 0 then 0 - x else x)))
                | Literal.TNumber(TNumber.Float x) ->
                    Literal(Literal.TNumber(TNumber.Float(if x < 0.0 then 0.0 - x else x)))
                | Literal.TNumber(TNumber.Rational(n, d)) ->
                    Literal(Literal.TNumber(TNumber.Rational(if n < 0 then 0 - n, d else n, d)))
                | _ -> failwith "invalid"
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"


and evalStmt (env: Env) (stmt: Stmt) : Expr * Env =
    match stmt with
    | Expression expr -> evalExpr env expr, env

    | VariableDeclaration(name, _, expr) ->
        let value = evalExpr env expr
        Literal (Literal.Unit ), Map.add name value env

let evalStatement (env: Env) (stmt: Stmt) : Literal * Env =
    match evalStmt env stmt with
    | Literal lit, env -> lit, env
    | _, env -> Unit , env
    

let evalProgram (env: Env) (program: Program) : Env =
    for stmt in program do
        evalStmt env stmt |> ignore

    env
