module Vec3.Interpreter.Eval

open Microsoft.FSharp.Core
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open System

type Env = Map<Lexeme, Expr>

type EvalError = string

type EvalResult<'a> = Result<'a, EvalError>

let evalNumber =
    function
    | LInteger x -> Ok(LInteger x)
    | LFloat x -> Ok(LFloat x)
    | LRational(n, d) -> Ok(LRational(n, d))
    | LComplex(r, i) -> Ok(LComplex(r, i))


let rec evalAddition =
    function
    | LInteger x, LInteger y -> Ok(LInteger(x + y))
    | LFloat x, LFloat y -> Ok(LFloat(x + y))
    | LRational(n1, d1), LRational(n2, d2) -> Ok(LRational(n1 * d2 + n2 * d1, d1 * d2))
    | LComplex(r1, i1), LComplex(r2, i2) -> Ok(LComplex(r1 + r2, i1 + i2))
    | _ -> Error "Invalid"

let evalSubtraction =
    function
    | LInteger x, LInteger y -> Ok(LInteger(x - y))
    | LFloat x, LFloat y -> Ok(LFloat(x - y))
    | LRational(n1, d1), LRational(n2, d2) -> Ok(LRational(n1 * d2 - n2 * d1, d1 * d2))
    | LComplex(r1, i1), LComplex(r2, i2) -> Ok(LComplex(r1 - r2, i1 - i2))
    | _ -> Error "invalid"

let evalMultiplication =
    function
    | LInteger x, LInteger y -> Ok(LInteger(x * y))
    | LFloat x, LFloat y -> Ok(LFloat(x * y))
    | LRational(n1, d1), LRational(n2, d2) -> Ok(LRational(n1 * n2, d1 * d2))
    | LComplex(r1, i1), LComplex(r2, i2) -> Ok(LComplex(r1 * r2 - i1 * i2, r1 * i2 + r2 * i1))
    | _ -> Error "invalid"

let evalDivision =
    function
    // this must be integer division as per requirement, not return floating point
    | LInteger x, LInteger y -> Ok(LInteger(x / y))
    | LFloat x, LFloat y -> Ok(LFloat(x / y))
    | LRational(n1, d1), LRational(n2, d2) -> Ok(LRational(n1 * d2, d1 * n2))
    | _ -> Error "invalid"

let evalPower =
    function
    | LInteger x, LInteger y -> Ok(LInteger(int (Math.Pow(float x, float y))))
    | LFloat x, LFloat y -> Ok(LFloat(Math.Pow(x, y)))
    | _ -> Error "invalid"

let evalModulo =
    function
    | LInteger x, LInteger y -> Ok(LInteger(x % y))
    | _ -> Error "invalid"
    
let rec addVectors (lhs: Expr list) (rhs: Expr list) =
    match lhs, rhs with
    | [], [] -> []
    | l :: ls, r :: rs ->
        match l, r with
        | ELiteral(LNumber(LInteger x), _), ELiteral(LNumber(LInteger y), _) ->
            ELiteral(LNumber(LInteger(x + y)), TInteger) :: addVectors ls rs
        | ELiteral(LNumber(LFloat x), _), ELiteral(LNumber(LFloat y), _) ->
            ELiteral(LNumber(LFloat(x + y)), TFloat) :: addVectors ls rs
        | ELiteral(LNumber(LRational(n1, d1)), _), ELiteral(LNumber(LRational(n2, d2)), _) ->
            ELiteral(LNumber(LRational(n1 * d2 + n2 * d1, d1 * d2)), TRational)
            :: addVectors ls rs
        | ELiteral(LNumber(LComplex(r1, i1)), _), ELiteral(LNumber(LComplex(r2, i2)), _) ->
            ELiteral(LNumber(LComplex(r1 + r2, i1 + i2)), TComplex) :: addVectors ls rs
        | _ -> failwith "invalid"
    | _ -> failwith "invalid"

let rec subVectors (lhs: Expr list) (rhs: Expr list) =
    match lhs, rhs with
    | [], [] -> []
    | l :: ls, r :: rs ->
        match l, r with
        | ELiteral(LNumber(LInteger x), _), ELiteral(LNumber(LInteger y), _) ->
            ELiteral(LNumber(LInteger(x - y)), TInteger) :: subVectors ls rs
        | ELiteral(LNumber(LFloat x), _), ELiteral(LNumber(LFloat y), _) ->
            ELiteral(LNumber(LFloat(x - y)), TFloat) :: subVectors ls rs
        | ELiteral(LNumber(LRational(n1, d1)), _), ELiteral(LNumber(LRational(n2, d2)), _) ->
            ELiteral(LNumber(LRational(n1 * d2 - n2 * d1, d1 * d2)), TRational)
            :: subVectors ls rs
        | ELiteral(LNumber(LComplex(r1, i1)), _), ELiteral(LNumber(LComplex(r2, i2)), _) ->
            ELiteral(LNumber(LComplex(r1 - r2, i1 - i2)), TComplex) :: subVectors ls rs
        | _ -> failwith "invalid"
    | _ -> failwith "invalid"
    
let rec mulVectors (lhs: Expr list) (rhs: Expr list) =
    match lhs, rhs with
    | [], [] -> []
    | l :: ls, r :: rs ->
        match l, r with
        | ELiteral(LNumber(LInteger x), _), ELiteral(LNumber(LInteger y), _) ->
            ELiteral(LNumber(LInteger(x * y)), TInteger) :: mulVectors ls rs
        | ELiteral(LNumber(LFloat x), _), ELiteral(LNumber(LFloat y), _) ->
            ELiteral(LNumber(LFloat(x * y)), TFloat) :: mulVectors ls rs
        | ELiteral(LNumber(LRational(n1, d1)), _), ELiteral(LNumber(LRational(n2, d2)), _) ->
            ELiteral(LNumber(LRational(n1 * n2, d1 * d2)), TRational) :: mulVectors ls rs
        | ELiteral(LNumber(LComplex(r1, i1)), _), ELiteral(LNumber(LComplex(r2, i2)), _) ->
            ELiteral(LNumber(LComplex(r1 * r2 - i1 * i2, r1 * i2 + r2 * i1)), TComplex)
            :: mulVectors ls rs
        | _ -> failwith "invalid"
    | _ -> failwith "invalid"
    
let rec dotVectors (lhs: Expr list) (rhs: Expr list) =
    match lhs, rhs with
    | [], [] -> 0
    | l :: ls, r :: rs ->
        match l, r with
        | ELiteral(LNumber(LInteger x), _), ELiteral(LNumber(LInteger y), _) -> x * y + dotVectors ls rs
        | _ -> failwith "invalid"
    | _ -> failwith "invalid"
    
let rec crossVectors (lhs: Expr list) (rhs: Expr list) =
    match lhs, rhs with
    | [ ELiteral(LNumber(LInteger x1), _)
        ELiteral(LNumber(LInteger y1), _)
        ELiteral(LNumber(LInteger z1), _) ],
      [ ELiteral(LNumber(LInteger x2), _)
        ELiteral(LNumber(LInteger y2), _)
        ELiteral(LNumber(LInteger z2), _) ] ->
        let x = y1 * z2 - z1 * y2
        let y = z1 * x2 - x1 * z2
        let z = x1 * y2 - y1 * x2

        [ ELiteral(LNumber(LInteger x), TInteger)
          ELiteral(LNumber(LInteger y), TInteger)
          ELiteral(LNumber(LInteger z), TInteger) ]
    | _ -> failwith "invalid"
    
let evalLiteral =
    function
    | LNumber x ->
        match evalNumber x with
        | Ok(num) -> Ok(LNumber num)
        | Error s -> Error s
    | LString s -> Ok(LString s)
    | LBool b -> Ok(LBool b)
    | LUnit -> Ok(LUnit)

let rec evalExpr (env: Env) (expr: Expr) : Expr =
    match expr with
    // extensible rows
    | ERange (start, end_, _) ->
        let start = evalExpr env start
        let end_ = evalExpr env end_
        
        match start, end_ with
        | ELiteral(LNumber(LInteger start), _), ELiteral(LNumber(LInteger end_), _) ->
            let rec range (start: int) (end_: int) =
                if start > end_ then []
                else start :: range (start + 1) end_

            EList(List.map (fun x -> ELiteral(LNumber(LInteger x), TInteger)) (range start end_), Some TInteger)
        | _ -> failwith "invalid"
        
    | ERecordEmpty typ -> ERecordEmpty typ
    
    | ERecordRestrict(record, name, typ) ->
        let record = evalExpr env record

        match record with
        | ERecordExtend((name', _, _), record, _) when name.Lexeme = name'.Lexeme -> record
        | ERecordExtend((_, _, _), record, _) -> evalExpr env (ERecordRestrict(record, name, typ))
        | _ -> failwith "invalid"

    | ERecordExtend((name, value, typ), record, typ2) ->
        let value = evalExpr env value
        ERecordExtend((name, value, typ), evalExpr env record, typ2)
    | ERecordSelect(record, field, typ) ->
        let record = evalExpr env record

        match record with
        | ERecordExtend((name, value, _), _, _) when name.Lexeme = field.Lexeme -> value
        | ERecordExtend((_, _, _), record, _) ->
            let record = evalExpr env record
            evalExpr env (ERecordSelect(record, field, typ))
        | _ -> failwith "invalid"
    | EBlock(stmts, _) ->
        let rec evalBlock (env: Env) =
            function
            | [] -> ELiteral(LUnit, TUnit)
            | [ stmt ] -> fst (evalStmt env stmt)
            | stmt :: rest ->
                let _, env' = evalStmt env stmt
                evalBlock env' rest

        evalBlock env stmts
    | ETuple(exprs, typ) -> ETuple(List.map (evalExpr env) exprs, typ)
    | EList(exprs, typ) -> EList(List.map (evalExpr env) exprs, typ)
    | EGrouping(expr, _) -> evalExpr env expr

    | ELiteral(lit, t) ->
        match evalLiteral lit with
        | Ok(res) -> ELiteral(res, t)
        | Error s -> failwith s
    | ECall(expr, args, _) ->
        let expr = evalExpr env expr

        match expr with
        | ELambda(params', body, _, _) ->
            let params' = List.map fst params'
            let rec evalParams (env: Env) (params': Token list) (args: Expr list) =
                match params', args with
                | [], [] -> env
                | p :: ps, a :: as' ->
                    let env' = evalParams env ps as'
                    Map.add p.Lexeme (evalExpr env a) env'
                | _ -> failwith "invalid"

            let env' = evalParams env params' args
            evalExpr env' body
        | EIdentifier({ Lexeme = name }, _) ->
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
                | "sqrt" ->
                    let args = List.map (evalExpr env) args

                    match args with
                    | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(Math.Sqrt(double x))), TFloat)
                    | _ -> failwith "invalid"
                | "abs" ->
                    let args = List.map (evalExpr env) args
                    
                    match args with
                    | [ ELiteral(LNumber(LInteger x), TInteger) ] -> ELiteral(LNumber(LInteger(Math.Abs(x))), TInteger)
                    | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(Math.Abs(x))), TFloat)
                    | _ -> failwith "invalid"
                | "floor" ->
                    let args = List.map (evalExpr env) args
                    
                    match args with
                    | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(Math.Floor(x))), TFloat)
                    | _ -> failwith "invalid"
                
                | "ceil" ->
                    let args = List.map (evalExpr env) args
                    
                    match args with
                    | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(Math.Ceiling(x))), TFloat)
                    | _ -> failwith "invalid"
                    
                | "fold" ->
                    let args = List.map (evalExpr env) args
                    
                    match args with
                    | [ EList(exprs', _); init; ELambda([listParam; accParam], body, _, _); ] ->
                        let listParam = fst listParam
                        let accParam = fst accParam
                        
                        let rec fold (env: Env) (exprs: Expr list) (acc: Expr) =
                            match exprs with
                            | [] -> acc
                            | e :: es ->
                                let env' = Map.add listParam.Lexeme e env
                                let env' = Map.add accParam.Lexeme acc env'
                                let acc = evalExpr env' body
                                fold env' es acc
                                
                        fold env exprs' init
                    | _ -> failwith "invalid"
                | "exit" ->
                    Environment.Exit(0)
                    ELiteral(LUnit, TUnit)
                    
                | _ -> failwith $"function {name} not found"
            | Operator(Plus, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] ->
                    let res = evalAddition(lhs, rhs)
                                |> Result.bind (fun res -> Ok(ELiteral(LNumber res, TInteger)))
                    
                    match res with
                    | Ok res -> res
                    | Error s -> failwith s
                    
                | [ EList(lhs, _); EList(rhs, _) ] -> EList(addVectors lhs rhs, None)
                | _ -> failwith "invalid"
            | Operator(Minus, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] ->
                    let res = evalSubtraction(lhs, rhs)
                                |> Result.bind (fun res -> Ok(ELiteral(LNumber res, TInteger)))
                    
                    match res with
                    | Ok res -> res
                    | Error s -> failwith s
                    
                | [ EList(lhs, _); EList(rhs, _) ] -> EList(subVectors lhs rhs, None)
                | _ -> failwith "invalid"
            | Operator(Star, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] ->
                    let res = evalMultiplication(lhs, rhs)
                                |> Result.bind (fun res -> Ok(ELiteral(LNumber res, TInteger)))
                    
                    match res with
                    | Ok res -> res
                    | Error s -> failwith s
                    
                | [ EList(lhs, _); EList(rhs, _) ] -> EList(mulVectors lhs rhs, None)
                | _ -> failwith "invalid"
            | Operator(Slash, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] ->
                    let res = evalDivision(lhs, rhs)
                                |> Result.bind (fun res -> Ok(ELiteral(LNumber res, TInteger)))
                    
                    match res with
                    | Ok res -> res
                    | Error s -> failwith s
                | _ -> failwith "invalid"
            | Operator(Caret, Some Infix)
            | Operator(StarStar, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] ->
                    let res = evalPower(lhs, rhs)
                                |> Result.bind (fun res -> Ok(ELiteral(LNumber res, TInteger)))
                    
                    match res with
                    | Ok res -> res
                    | Error s -> failwith s
                | _ -> failwith "invalid"
                
            | Operator(Percent, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] ->
                    let res = evalModulo(lhs, rhs)
                                |> Result.bind (fun res -> Ok(ELiteral(LNumber res, TInteger)))
                    
                    match res with
                    | Ok res -> res
                    | Error s -> failwith s
                | _ -> failwith "invalid"
            | Operator(EqualEqual, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(lhs, _); ELiteral(rhs, _) ] -> ELiteral(LBool(lhs = rhs), TBool)
                | _ -> failwith "invalid"
                
            | Operator(BangEqual, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(lhs, _); ELiteral(rhs, _) ] -> ELiteral(LBool(lhs <> rhs), TBool)
                | _ -> failwith "invalid"
            | Operator(Less, Some Infix) ->
                let args = List.map (evalExpr env) args
                
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] -> ELiteral(LBool(lhs < rhs), TBool)
                | _ -> failwith "invalid"
            | Operator(LessEqual, Some Infix) ->
                let args = List.map (evalExpr env) args
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] -> ELiteral(LBool(lhs <= rhs), TBool)
                | _ -> failwith "invalid"
            | Operator(Greater, Some Infix) ->
                let args = List.map (evalExpr env) args
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] -> ELiteral(LBool(lhs > rhs), TBool)
                | _ -> failwith "invalid"
            | Operator(GreaterEqual, Some Infix) ->
                let args = List.map (evalExpr env) args
                match args with
                | [ ELiteral(LNumber lhs, _); ELiteral(LNumber rhs, _) ] -> ELiteral(LBool(lhs >= rhs), TBool)
                | _ -> failwith "invalid"
            | Operator(Bang, Some Prefix) ->
                let args = List.map (evalExpr env) args
                match args with
                | [ ELiteral(LNumber(LInteger x), TInteger) ] -> ELiteral(LNumber(LInteger(if x = 0 then 1 else 0)), TInteger)
                | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(if x = 0.0 then 1.0 else 0.0)), TFloat)
                | [ ELiteral(LBool b, TBool) ] -> ELiteral(LBool(not b), TBool)
                | _ -> failwith "invalid"
            | Operator(Minus, Some Prefix) ->
                let args = List.map (evalExpr env) args
                match args with
                | [ ELiteral(LNumber(LInteger x), TInteger) ] -> ELiteral(LNumber(LInteger(-x)), TInteger)
                | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(-x)), TFloat)
                | _ -> failwith "invalid"
            | Operator(Plus, Some Prefix) ->
                let args = List.map (evalExpr env) args
                match args with
                | [ ELiteral(LNumber(LInteger x), TInteger) ] -> ELiteral(LNumber(LInteger(if x < 0 then 0 - x else x)), TInteger)
                | [ ELiteral(LNumber(LFloat x), TFloat) ] -> ELiteral(LNumber(LFloat(if x < 0.0 then 0.0 - x else x)), TFloat)
                | _ -> failwith "invalid"
            | Keyword kw ->
                match kw with
                | Keyword.And ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LBool lhs, TBool); ELiteral(LBool rhs, _) ] -> ELiteral(LBool(lhs && rhs), TBool)
                    | _ -> failwith "invalid"
                | Keyword.Or ->
                    let args = List.map (evalExpr env) args
                    match args with
                    | [ ELiteral(LBool lhs, TBool); ELiteral(LBool rhs, _) ] -> ELiteral(LBool(lhs || rhs), TBool)
                    | _ -> failwith "invalid"
                | _ -> failwith "invalid"
            | _ -> failwith $"function {name} not found"
        | _ -> failwith "invalid"
    | ELambda(params', body, rt, t') -> ELambda(params', body, rt, t')
    | EIdentifier(name, typ) ->
        match env.TryGetValue name.Lexeme with
        | true, expr -> expr
        | false, _ ->
            match name.Lexeme with
            | Identifier "print"
            | Identifier "input"
            | Identifier "cos"
            | Identifier "sin"
            | Identifier "tan"
            | Identifier "env"
            | Identifier "exit"
            | Identifier "sqrt"
            | Identifier "abs"
            | Identifier "floor"
            | Identifier "fold"
            | Identifier "plot"
            | Identifier "ceil"
            
            | Operator (Plus, _)
            | Operator (Minus, _)
            | Operator (Star, _)
            | Operator (Slash, _)
            | Operator (Caret, _)
            | Operator (StarStar, _)
            | Operator (Percent, _)
            | Operator (EqualEqual, _)
            | Operator (BangEqual, _)
            | Operator (Less, _)
            | Operator (LessEqual, _)
            | Operator (Greater, _)
            | Operator (GreaterEqual, _)
            | Operator (AmpersandAmpersand, _)
            | Operator (PipePipe, _)
            | Operator (Bang, _)
            | Operator (Minus, _)
            | Operator (Plus, _)
            | Operator (ColonColon, _)
             ->
                EIdentifier(name, typ)
            | _ -> failwith $"variable {name} not found"
    | EIf(cond, then', else', _) ->
        let cond = evalExpr env cond

        match cond with
        | ELiteral(LBool true, _) -> evalExpr env then'
        | ELiteral(LBool false, _) -> evalExpr env else'
        | _ -> failwith "invalid"
    | ETernary(cond, then', else', typ) -> evalExpr env (EIf(cond, then', else', typ))
    | EIndex(expr, index, _) ->
        let expr = evalExpr env expr
        let index = evalExpr env index

        match expr, index with
        | EList(exprs, _), ELiteral(LNumber(LInteger i), _) -> List.item i exprs
        | _ -> failwith "invalid"

and evalStmt (env: Env) (stmt: Stmt) : Expr * Env =
    match stmt with
    | SExpression(expr, _) -> evalExpr env expr, env

    | SVariableDeclaration(name, expr, _) ->
        let value = evalExpr env expr
        ELiteral(LUnit, TUnit), Map.add name.Lexeme value env

    | SAssertStatement(expr, msg, _) ->
        let value = evalExpr env expr

        match value with
        | ELiteral(LBool true, _) -> ELiteral(LUnit, TUnit), env
        | ELiteral(LBool false, _) ->
            match msg with
            | Some msg -> printfn $"Assert failed: {msg}"
            | None -> printfn $"Assertion failed"

            ELiteral(LUnit, TUnit), env
        | _ -> failwith "invalid"
    | STypeDeclaration(name, _, _) -> ELiteral(LUnit, TUnit), Map.add name.Lexeme (ELiteral(LUnit, TUnit)) env

let evalStatement (env: Env) (stmt: Stmt) : Expr * Env =
    match evalStmt env stmt with
    | ELiteral(lit, typ), env -> ELiteral(lit, typ), env
    | EList(exprs, typ), env -> EList(exprs, typ), env
    | ETuple(exprs, typ), env -> ETuple(exprs, typ), env
    | _, env -> ELiteral(LUnit, TUnit), env


let evalProgram (env: Env) (program: Program) : Expr * Env =
    List.fold (fun (_, env) -> evalStatement env) (ELiteral(LUnit, TUnit), env) program
