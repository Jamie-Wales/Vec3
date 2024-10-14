module Vec3.Interpreter.ConstantFolding

open Token
open Parser
open Eval
open Grammar

let foldConstants (program: Program) : Program =
    let rec foldStatements (stmts: Stmt list) : Stmt list =
        List.map foldStmt stmts
    
    and foldStmt (stmt: Stmt) : Stmt =
        match stmt with
        | SVariableDeclaration (tok, expr, typ) ->
            SVariableDeclaration (tok, expr, typ)
            
            
        stmt
    
    and foldExpr (expr: Expr) : Expr =
        expr
    
    foldStatements program
    



