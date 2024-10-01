module Vec3.Interpreter.Eval

open Microsoft.FSharp.Core
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open System

type Env = Map<Token, Expr>

let evalNumber = function
    | Integer x -> Integer x
    | Float x -> Float x
                             
                             
let rec evalAddition = function
    | Integer x, Integer y -> Integer (x + y)
    | Float x, Float y -> Float (x + y)
    | _ -> failwith "invalid"

let evalSubtraction = function
    | Integer x, Integer y -> Integer (x - y)
    | Float x, Float y -> Float (x - y)
    | _ -> failwith "invlaid"
    
let evalMultiplication = function
    | Integer x, Integer y -> Integer (x * y)
    | Float x, Float y -> Float (x * y)
    | _ -> failwith "invalid"
    
let evalDivision = function
    // this must be integer division as per requirement, not return floating point
    | Integer x, Integer y -> Integer (x / y)
    | Float x, Float y -> Float (x / y)
    | _ -> failwith "invalid"
    
let evalPower = function
    | Integer x, Integer y -> Integer (int (Math.Pow (float x, float y)))
    | Float x, Float y -> Float (Math.Pow (x, y))
    | _ -> failwith "invalid"
    
let rec evalExpr (env: Env) = function
    | Block stmts ->
        let rec evalBlock (env: Env) = function
            | [] -> Literal (Unit ())
            | [stmt] -> fst (evalStmt env stmt)
            | stmt :: rest -> 
                let _, env' = evalStmt env stmt
                evalBlock env' rest
        evalBlock env stmts
    | Assignment (_, expr) ->
        evalExpr env expr
    | Grouping expr -> evalExpr env expr
    | Literal (Literal.String s) -> Literal (Literal.String s)
    | Literal (Literal.Bool b) -> Literal (Literal.Bool b)
    | Literal (Literal.Number n) -> Literal (Literal.Number (evalNumber n))
    | Literal (Unit u) -> Literal (Unit u)
    | Call (name, args) -> 
        match env.TryGetValue name with
        | true, Lambda (params', body) ->
            let rec evalParams (env: Env) (params': Token list) (args: Expr list) =
                match params', args with
                | [], [] -> env
                | p :: ps, a :: as' -> 
                    let env' = evalParams env ps as'
                    Map.add p (evalExpr env a) env'
                | _ -> failwith "invalid"
            let env' = evalParams env params' args
            evalExpr env' body
        | _ -> failwith $"function {name} not found"
     | Lambda (params', body) -> Lambda (params', body)
    | Expr.Identifier name -> 
        match env.TryGetValue name with
        | true, expr -> expr
        | _ -> failwith $"variable {name} not found"
    | Binary (lhs, op, rhs) ->
        let lhs = evalExpr env lhs
        let rhs = evalExpr env rhs
        match op, lhs, rhs with
        | { lexeme = Operator op } , Expr.Literal (Literal.Number lhs), Expr.Literal (Literal.Number rhs) ->
            match op with
            | Operator.Plus -> Literal (Literal.Number (evalAddition (lhs, rhs)))
            | Operator.Minus -> Literal (Literal.Number (evalSubtraction (lhs, rhs)))
            | Operator.Star -> Literal (Literal.Number (evalMultiplication (lhs, rhs)))
            | Operator.Slash -> Literal (Literal.Number (evalDivision (lhs, rhs)))
            | Operator.StarStar -> Literal (Literal.Number (evalPower (lhs, rhs)))
            | Operator.EqualEqual -> Literal (Literal.Bool (lhs = rhs))
            | Operator.BangEqual -> Literal (Literal.Bool (lhs <> rhs))
            | Operator.Less -> Literal (Literal.Bool (lhs < rhs))
            | Operator.LessEqual -> Literal (Literal.Bool (lhs <= rhs))
            | Operator.Greater -> Literal (Literal.Bool (lhs > rhs))
            | Operator.GreaterEqual -> Literal (Literal.Bool (lhs >= rhs))
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
    | Unary (op, expr) ->
        let value = evalExpr env expr
        match op with
        | { lexeme = Operator op } ->
            match op with
            | Bang -> 
                match value with
                | Literal (Literal.Number (Integer x)) -> Literal (Literal.Number (Integer (if x = 0 then 1 else 0)))
                | Literal (Literal.Number (Float x)) -> Literal (Literal.Number (Float (if x = 0.0 then 1 else 0)))
                | _ -> failwith "invalid"
            | _ -> failwith "invalid"
        | _ -> failwith "invalid"
            
            
and evalStmt (env: Env) (stmt: Stmt): Expr * Env =
    match stmt with
    | Expression expr -> 
        evalExpr env expr, env
        
    | VariableDeclaration (name, expr) ->
        let value = evalExpr env expr
        Literal (Literal.Unit ()), Map.add name value env

let evalStatement (env: Env) (stmt: Stmt): Literal * Env =
    match evalStmt env stmt with
    | Literal lit, env -> lit, env
    | _, env -> Unit (), env
    

let evalProgram (env: Env) (program: Program): Env =
    for stmt in program do
        evalStmt env stmt |> ignore
    env

