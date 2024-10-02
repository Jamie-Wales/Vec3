module Vec3.Interpreter.TypeChecker

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open System

type TypeError =
    | UndefinedVariable of Token
    | UndefinedFunction of Token
    | UndefinedType of Token
    | TypeMismatch of Grammar.Type * Grammar.Type
    | InvalidAssignment of Grammar.Type * Grammar.Type
    | InvalidArgumentCount of int * int
    | InvalidArgumentType of Grammar.Type * Grammar.Type
    | InvalidReturnType of Grammar.Type * Grammar.Type
    | InvalidOperandType of Token * Grammar.Type * Grammar.Type
    | InvalidOperator of Token * Grammar.Type
    | InvalidFunctionType of Grammar.Type
    | InvalidFunction of Grammar.Type
    | InvalidFunctionArgument of Grammar.Type * Grammar.Type
    | InvalidFunctionReturn of Grammar.Type * Grammar.Type
    | InvalidFunctionBody of Grammar.Type * Grammar.Type
    | InvalidBlock of Grammar.Type * Grammar.Type
    | InvalidCall of Grammar.Type
    | InvalidCallType of Grammar.Type * Grammar.Type
    | InvalidCallReturn of Grammar.Type * Grammar.Type
    | InvalidCallBody of Grammar.Type * Grammar.Type
    | TypeErrors of TypeError list

exception TypeException of TypeError

type Result<'T> =
    | Ok of 'T
    | Errors of TypeError list
    
// result is monadic, can extract out common code later
let bindR (result: Result<'T>) (f: 'T -> Result<'U>) =
    match result with
    | Ok t -> f t
    | Errors errors -> Errors errors

let mapR (f: 'T -> 'U) (result: Result<'T>) =
    match result with
    | Ok t -> Ok (f t)
    | Errors errors -> Errors errors
    
let (>>=) = bindR
let (>>|) = mapR

let combine (result: Result<'T>) (result': Result<'T>) =
    match result, result' with
    | Ok _, Errors errors -> Errors errors
    | Errors errors, Ok _ -> Errors errors
    | Ok _, Ok _ -> result
    | Errors errors, Errors errors' -> Errors (errors @ errors')

type TypeEnv = Map<string, Grammar.Type>

let checkLiteral (lit: Literal): Grammar.Type =
    match lit with
    | Literal.Number (Integer _) -> Type.Integer
    | Literal.Number (Float _) -> Type.Float
    | Literal.Number (Rational _) -> Type.Rational
    | Literal.Number (Complex _) -> Type.Complex
    
    | Literal.String _ -> Type.String
    | Literal.Bool _ -> Type.Bool
    | Literal.Unit _ -> Type.Unit

let checkIdentifier (env: TypeEnv) (token: Token): Result<Grammar.Type> =
    match token.lexeme with
    | Identifier name ->
        match Map.tryFind name env with
        | Some t -> Ok t
        | None -> Errors [TypeError.UndefinedVariable token]
    | _ -> Errors [TypeError.UndefinedVariable token]

let rec checkExpr (env: TypeEnv) (expr: Expr): Result<Grammar.Type> =
    match expr with
    | Expr.Literal lit -> Ok <| checkLiteral lit
    | Expr.Identifier token -> checkIdentifier env token
    | Expr.Unary (op, expr) ->
        let exprType = checkExpr env expr
        match exprType with
        | Ok Type.Integer when op.lexeme = Operator Minus -> Ok Type.Integer
        | Ok Type.Float when op.lexeme = Operator Minus -> Ok Type.Float
        | Ok Type.Rational when op.lexeme = Operator Minus -> Ok Type.Rational
        
        | Ok Type.Integer when op.lexeme = Operator Plus -> Ok Type.Integer
        | Ok Type.Float when op.lexeme = Operator Plus -> Ok Type.Float
        | Ok Type.Rational when op.lexeme = Operator Plus -> Ok Type.Rational
        
        | Ok Type.Bool when op.lexeme = Operator Bang -> Ok Type.Bool
        | Ok Type.Integer when op.lexeme = Operator Bang -> Ok Type.Integer
        | Ok Type.Float when op.lexeme = Operator Bang -> Ok Type.Float
        | Ok Type.Rational when op.lexeme = Operator Bang -> Ok Type.Rational
        
        | Errors errors -> Errors errors
        | Ok t -> Errors [TypeError.InvalidOperator(op, t)]
    | Expr.Binary (lhs, op, rhs) ->
        let lhsType = checkExpr env lhs
        let rhsType = checkExpr env rhs
        match lhsType, rhsType with
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator Plus -> Ok Type.Integer
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator Plus -> Ok Type.Float
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator Plus -> Ok Type.Rational
        | Ok Type.Rational, Ok Type.Integer when op.lexeme = Operator Plus -> Ok Type.Rational
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator Minus -> Ok Type.Integer
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator Minus -> Ok Type.Float
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator Minus -> Ok Type.Rational
        | Ok Type.Rational, Ok Type.Integer when op.lexeme = Operator Minus -> Ok Type.Rational
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator Star -> Ok Type.Integer
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator Star -> Ok Type.Float
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator Star -> Ok Type.Rational
        | Ok Type.Rational, Ok Type.Integer when op.lexeme = Operator Star -> Ok Type.Rational
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator Slash -> Ok Type.Integer
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator Slash -> Ok Type.Float
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator Slash -> Ok Type.Rational
        | Ok Type.Rational, Ok Type.Integer when op.lexeme = Operator Slash -> Ok Type.Rational
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator StarStar -> Ok Type.Integer
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator StarStar -> Ok Type.Float
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator StarStar -> Ok Type.Rational
        | Ok Type.Rational, Ok Type.Integer when op.lexeme = Operator StarStar -> Ok Type.Rational
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator EqualEqual -> Ok Type.Bool
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator EqualEqual -> Ok Type.Bool
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator EqualEqual -> Ok Type.Bool
        | Ok Type.Bool, Ok Type.Bool when op.lexeme = Operator EqualEqual -> Ok Type.Bool
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator BangEqual -> Ok Type.Bool
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator BangEqual -> Ok Type.Bool
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator BangEqual -> Ok Type.Bool
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator Less -> Ok Type.Bool
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator Less -> Ok Type.Bool
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator Less -> Ok Type.Bool
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator Less -> Ok Type.Bool
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator LessEqual -> Ok Type.Bool
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator LessEqual -> Ok Type.Bool
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator LessEqual -> Ok Type.Bool
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator Greater -> Ok Type.Bool
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator Greater -> Ok Type.Bool
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator Greater -> Ok Type.Bool
        
        | Ok Type.Integer, Ok Type.Integer when op.lexeme = Operator GreaterEqual -> Ok Type.Bool
        | Ok Type.Float, Ok Type.Float when op.lexeme = Operator GreaterEqual -> Ok Type.Bool
        | Ok Type.Rational, Ok Type.Rational when op.lexeme = Operator GreaterEqual -> Ok Type.Bool
        
        | Errors errors, Errors errors' -> Errors (errors @ errors')
        | Errors errors, Ok _ -> Errors errors
        | Ok _, Errors errors -> Errors errors
        | Ok t, Ok t' -> Errors [TypeError.InvalidOperandType(op, t, t')]
    | Expr.Grouping expr -> checkExpr env expr
    | Expr.Assignment (token, expr) ->
        let exprType = checkExpr env expr
        match token.lexeme with
        | Identifier name ->
            match Map.tryFind name env with
            | Some t ->
                match exprType with
                | Ok t' when t = t' -> Ok t
                | Ok t' -> Errors [TypeError.InvalidAssignment(t, t')]
                | Errors errors -> Errors errors
            | None -> Errors [TypeError.UndefinedVariable token]
        | _ -> Errors [TypeError.UndefinedVariable token]
    | Expr.Call (callee, args) ->
        let calleeType = checkExpr env (Expr.Identifier callee)
        match calleeType with
        | Ok (Function (paramList, returnType)) ->
            let argResults = List.map (checkExpr env) args
            let validArgs = List.forall (function Ok _ -> true | _ -> false) argResults
            if List.length paramList <> List.length args then
                 Errors [TypeError.InvalidArgumentCount(List.length paramList, List.length args)]
            else if not validArgs then
                let errors = List.choose (function Errors errs -> Some errs | _ -> None) argResults |> List.concat
                Errors errors
            else
                let argTypes = List.map (fun t -> match t with | Ok t -> t | _ -> Type.Infer) argResults
                let valid = List.forall2 (fun expected actual -> expected = actual) paramList argTypes
                if valid then Ok returnType
                else Errors [TypeError.InvalidArgumentType(paramList.Head, argTypes.Head)]
        | Errors errors -> Errors errors
        | _ -> Errors [TypeError.InvalidCallType(Type.Infer, Type.Infer)] // fix
        
    // need better type inference here for params, unless params must be typed
    | Expr.Lambda (paramList, returnType, body) ->
        let newEnv = List.fold (fun acc (param, typ) ->
            match param.lexeme with
            | Identifier name -> Map.add name typ acc
            | _ -> raise (TypeException (TypeError.UndefinedVariable param))) env paramList
        let bodyType = checkExpr newEnv body
        match bodyType with
        | Ok bodyType ->
            if bodyType = returnType then Ok <| Function (List.map snd paramList, returnType)
            else if returnType = Type.Infer then Ok <| Function (List.map snd paramList, bodyType)
            else Errors [TypeError.InvalidFunctionReturn(returnType, bodyType)]
        | Errors errors -> Errors errors
    | Expr.Block stmts ->
        let rec checkBlock (env: TypeEnv) (stmts: Stmt list): Result<Grammar.Type> =
            match stmts with
            | [] -> Ok Type.Unit
            | [stmt] ->
                match stmt with
                | Stmt.Expression expr -> checkExpr env expr
                | Stmt.VariableDeclaration _ -> Ok Type.Unit
            | stmt :: rest -> 
                let env', _ = checkStmt env stmt
                checkBlock env' rest
                
        checkBlock env stmts

and checkStmt (env: TypeEnv) (stmt: Stmt): TypeEnv * Result<Grammar.Type> =
    match stmt with
    | Stmt.Expression expr ->
        let exprType = checkExpr env expr
        match exprType with
        | Errors errors -> env, Errors errors
        | Ok exprType -> env, Ok exprType
    
    | Stmt.VariableDeclaration (token, typ, expr) ->
        let exprType = checkExpr env expr
        match exprType with
        | Errors errors -> env,  Errors errors
        | Ok exprType ->
            if typ = exprType then
                match token.lexeme with
                | Identifier name -> Map.add name typ env, Ok Type.Unit
                | _ -> env, Errors [TypeError.UndefinedVariable token]
            else if typ = Type.Infer then
                match token.lexeme with
                | Identifier name -> Map.add name exprType env, Ok Type.Unit
                | _ -> env, Errors [TypeError.UndefinedVariable token]
            else env, Errors [TypeError.TypeMismatch(typ, exprType)]

let rec checkStmts (env: TypeEnv) (stmts: Stmt list) =
    let rec helper env accErrors stmts =
        match stmts with
        | [] -> if accErrors = [] then Ok env else Errors accErrors
        | stmt :: rest ->
            let env', result = checkStmt env stmt
            match result with
            | Errors errors -> helper env' (accErrors @ errors) rest
            | Ok _ -> helper env' accErrors rest
            
    helper env [] stmts
        
        
let checkProgram (program: Program) =
    match checkStmts Map.empty program with
    | Ok _ -> ()
    | Errors errors -> raise (TypeException (TypeError.TypeErrors errors))
    
 