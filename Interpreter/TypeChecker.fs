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

exception TypeException of TypeError

type TypeEnv = Map<string, Grammar.Type>

let checkLiteral (lit: Literal): Grammar.Type =
    match lit with
    | Literal.TNumber (TNumber.Integer _) -> Type.Integer
    | Literal.TNumber (TNumber.Float _) -> Type.Float
    | Literal.String _ -> Type.String
    | Literal.Bool _ -> Type.Bool
    | Literal.Unit _ -> Type.Unit

let checkIdentifier (env: TypeEnv) (token: Token): Grammar.Type =
    match token.lexeme with
    | Identifier name ->
        match Map.tryFind name env with
        | Some t -> t
        | None -> raise (TypeException (TypeError.UndefinedVariable token))
    | _ -> raise (TypeException (TypeError.UndefinedVariable token))

let rec checkExpr (env: TypeEnv) (expr: Expr): Grammar.Type =
    match expr with
    | Expr.Literal lit -> checkLiteral lit
    | Expr.Identifier token -> checkIdentifier env token
    | Expr.Unary (op, expr) ->
        let exprType = checkExpr env expr
        match op.lexeme with
        | Operator Minus when exprType = Type.Integer -> Type.Integer
        | Operator Minus when exprType = Type.Float -> Type.Float
        | Operator Bang when exprType = Type.Bool -> Type.Bool
        | _ -> raise (TypeException (TypeError.InvalidOperator(op, exprType)))
    | Expr.Binary (lhs, op, rhs) ->
        let lhsType = checkExpr env lhs
        let rhsType = checkExpr env rhs
        match op.lexeme with
        | Operator Plus when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Integer
        | Operator Plus when lhsType = Type.Float && rhsType = Type.Float -> Type.Float
        | Operator Minus when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Integer
        | Operator Minus when lhsType = Type.Float && rhsType = Type.Float -> Type.Float
        | Operator Star when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Integer
        | Operator Star when lhsType = Type.Float && rhsType = Type.Float -> Type.Float
        | Operator Slash when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Integer
        | Operator Slash when lhsType = Type.Float && rhsType = Type.Float -> Type.Float
        | Operator EqualEqual when lhsType = rhsType -> Type.Bool
        | Operator BangEqual when lhsType = rhsType -> Type.Bool
        | Operator Less when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Bool
        | Operator Less when lhsType = Type.Float && rhsType = Type.Float -> Type.Bool
        | Operator LessEqual when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Bool
        | Operator LessEqual when lhsType = Type.Float && rhsType = Type.Float -> Type.Bool
        | Operator Greater when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Bool
        | Operator Greater when lhsType = Type.Float && rhsType = Type.Float -> Type.Bool
        | Operator GreaterEqual when lhsType = Type.Integer && rhsType = Type.Integer -> Type.Bool
        | Operator GreaterEqual when lhsType = Type.Float && rhsType = Type.Float -> Type.Bool
        | _ -> raise (TypeException (TypeError.InvalidOperandType(op, lhsType, rhsType)))
    | Expr.Grouping expr -> checkExpr env expr
    | Expr.Assignment (token, expr) ->
        let exprType = checkExpr env expr
        match token.lexeme with
        | Identifier name ->
            match Map.tryFind name env with
            | Some t when t = exprType -> t
            | Some t -> raise (TypeException (TypeError.InvalidAssignment(t, exprType)))
            | None -> raise (TypeException (TypeError.UndefinedVariable token))
        | _ -> raise (TypeException (TypeError.UndefinedVariable token))
    | Expr.Call (callee, args) ->
        let calleeType = checkExpr env (Expr.Identifier callee)
        match calleeType with
        | Function (paramList, returnType) ->
            if List.length paramList <> List.length args then
                raise (TypeException (TypeError.InvalidArgumentCount(List.length paramList, List.length args)))
            else
                let argTypes = List.map (checkExpr env) args
                let valid = List.forall2 (fun expected actual -> expected = actual) paramList argTypes
                if valid then returnType
                else raise (TypeException (TypeError.InvalidArgumentType(paramList.Head, argTypes.Head)))
        | _ -> raise (TypeException (TypeError.InvalidCall calleeType))
        
    | Expr.Lambda (paramList, returnType, body) ->
        let newEnv = List.fold (fun acc (param, typ) ->
            match param.lexeme with
            | Identifier name -> Map.add name typ acc
            | _ -> raise (TypeException (TypeError.UndefinedVariable param))) env paramList
        let bodyType = checkExpr newEnv body
        if bodyType = returnType then Function (List.map snd paramList, returnType)
        else raise (TypeException (TypeError.InvalidFunctionBody(bodyType, returnType)))
    | Expr.Block stmts ->
        failwith "not implemented"

and checkStmt (env: TypeEnv) (stmt: Stmt): TypeEnv =
    match stmt with
    | Stmt.Expression expr -> ignore (checkExpr env expr); env
    
    | Stmt.VariableDeclaration (token, typ, expr) ->
        let exprType = checkExpr env expr
        // fix this
        if typ = exprType then
            match token.lexeme with
            | Identifier name -> Map.add name typ env
            | _ -> raise (TypeException (TypeError.UndefinedVariable token))
        else if typ = Type.Infer then
            match token.lexeme with
            | Identifier name -> Map.add name exprType env
            | _ -> raise (TypeException (TypeError.UndefinedVariable token))
        else raise (TypeException (TypeError.TypeMismatch(typ, exprType)))
        
let checkProgram (program: Program) =
    let rec checkStmts (env: TypeEnv) (stmts: Stmt list) =
        List.fold checkStmt env stmts
    
    let _ = checkStmts Map.empty program
    ()
 