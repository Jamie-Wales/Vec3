module Vec3.Interpreter.Eval

open Microsoft.FSharp.Core
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open System

type Env = Map<Token, Expr>

let simplifyRational (n: bigint) (d: bigint) : bigint * bigint =
    let rec gcd (a: bigint) (b: bigint) =
        if b = bigint 0 then a else gcd b (a % b)

    let g = gcd n d
    (n / g, d / g)

let evalNumber =
    function
    | Integer x -> Integer x
    | Float x -> Float x
    | Rational(n, d) -> Rational(simplifyRational n d)
    | Complex(r, i) -> Complex(r, i)


let rec evalAddition =
    function
    | Integer x, Integer y -> Integer(x + y)
    | Float x, Float y -> Float(x + y)
    | Rational(n, d), Integer y -> Rational(simplifyRational (bigint.Add(n, y)) d)
    | _ -> failwith "invalid"

let evalSubtraction =
    function
    | Integer x, Integer y -> Integer(x - y)
    | Float x, Float y -> Float(x - y)
    | Rational(n, d), Integer y -> Rational(simplifyRational (bigint.Subtract(n, y)) d)
    | _ -> failwith "invlaid"

let evalMultiplication =
    function
    | Integer x, Integer y -> Integer(x * y)
    | Float x, Float y -> Float(x * y)
    | Rational(n, d), Integer y -> Rational(simplifyRational (bigint.Multiply(n, y)) d)
    | _ -> failwith "invalid"

let evalDivision =
    function
    // this must be integer division as per requirement, not return floating point
    | Integer x, Integer y -> Integer(x / y)
    | Float x, Float y -> Float(x / y)
    | Rational(n, d), Integer y -> Rational(simplifyRational n (bigint.Multiply(d, y)))
    | _ -> failwith "invalid"

let evalPower =
    function
    | Integer x, Integer y -> Integer(bigint.Pow(x, int y))
    | Float x, Float y -> Float(decimal (double x ** double y))
    | Rational(n, d), Integer y -> Rational(simplifyRational (bigint.Pow(n, int y)) (bigint.Pow(d, int y)))
    | _ -> failwith "invalid"

let rec evalExpr (env: Env) =
    function
    | Block stmts ->
        let rec evalBlock (env: Env) =
            function
            | [] -> Literal(Unit())
            | [ stmt ] -> fst (evalStmt env stmt)
            | stmt :: rest ->
                let _, env' = evalStmt env stmt
                evalBlock env' rest

        evalBlock env stmts
    | Assignment(_, expr) -> evalExpr env expr
    | Grouping expr -> evalExpr env expr
    | Literal(Literal.String s) -> Literal(Literal.String s)
    | Literal(Literal.Bool b) -> Literal(Literal.Bool b)
    | Literal(Literal.Number n) -> Literal(Literal.Number(evalNumber n))
    | Literal(Unit u) -> Literal(Unit u)
    | Call(name, args) ->
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
        | { lexeme = Operator op }, Expr.Literal(Literal.Number lhs), Expr.Literal(Literal.Number rhs) ->
            match op with
            | Operator.Plus -> Literal(Literal.Number(evalAddition (lhs, rhs)))
            | Operator.Minus -> Literal(Literal.Number(evalSubtraction (lhs, rhs)))
            | Operator.Star -> Literal(Literal.Number(evalMultiplication (lhs, rhs)))
            | Operator.Slash -> Literal(Literal.Number(evalDivision (lhs, rhs)))
            | Operator.StarStar -> Literal(Literal.Number(evalPower (lhs, rhs)))
            | Operator.EqualEqual -> Literal(Literal.Bool(lhs = rhs))
            | Operator.BangEqual -> Literal(Literal.Bool(lhs <> rhs))
            | Operator.Less -> Literal(Literal.Bool(lhs < rhs))
            | Operator.LessEqual -> Literal(Literal.Bool(lhs <= rhs))
            | Operator.Greater -> Literal(Literal.Bool(lhs > rhs))
            | Operator.GreaterEqual -> Literal(Literal.Bool(lhs >= rhs))
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
    | Unary(op, expr) ->
        let value = evalExpr env expr

        match op, value with
        | { lexeme = Operator op }, Literal value ->
            match op with
            | Bang ->
                match value with
                | Literal.Number(Integer x) ->
                    Literal(Literal.Number(Integer(if x = bigint 0 then bigint 1 else bigint 0)))
                | Literal.Number(Float x) ->
                    Literal(Literal.Number(Float(if x = decimal 0.0 then decimal 1.0 else decimal 0.0)))
                | Literal.Bool b -> Literal(Literal.Bool(not b))
                | _ -> failwith "invalid"
            | Minus ->
                match value with
                | Literal.Number(Integer x) -> Literal(Literal.Number(Integer(-x)))
                | Literal.Number(Float x) -> Literal(Literal.Number(Float(-x)))
                | Literal.Number(Rational(n, d)) -> Literal(Literal.Number(Rational(-n, d)))
                | _ -> failwith "invalid"
            | Plus ->
                match value with
                | Literal.Number(Integer x) ->
                    Literal(Literal.Number(Integer(if x < bigint 0 then bigint 0 - x else x)))
                | Literal.Number(Float x) ->
                    Literal(Literal.Number(Float(if x < decimal 0.0 then decimal 0.0 - x else x)))
                | Literal.Number(Rational(n, d)) ->
                    Literal(Literal.Number(Rational(if n < bigint 0 then bigint 0 - n, d else n, d)))
                | _ -> failwith "invalid"
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"


and evalStmt (env: Env) (stmt: Stmt) : Expr * Env =
    match stmt with
    | Expression expr -> evalExpr env expr, env

    | VariableDeclaration(name, _, expr) ->
        let value = evalExpr env expr
        Literal(Literal.Unit()), Map.add name value env

let evalStatement (env: Env) (stmt: Stmt) : Literal * Env =
    match evalStmt env stmt with
    | Literal lit, env -> lit, env
    | _, env -> Unit(), env


let evalProgram (env: Env) (program: Program) : Env =
    for stmt in program do
        evalStmt env stmt |> ignore

    env
