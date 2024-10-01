module Vec3.Interpreter.Grammar

open Token

type Literal =
    | Number of Number 
    | String of string
    | Bool of bool
    | Unit of unit

type Expr =
    | Literal of Literal
    | Identifier of Token 
    | Unary of Token * Expr
    | Binary of Expr * Token * Expr
    | Grouping of Expr
    | Assignment of Token * Expr
    
    // change this
    | Call of Token * Expr list
    | Lambda of Token list * Expr
    | Block of Stmt list
    
and Stmt =
    | Expression of Expr
    | VariableDeclaration of Token * Expr

type Program = Stmt list

