module Vec3.Interpreter.Grammar

open Token

type Literal =
    | Number of Number 
    | String of string
    | Bool of bool
    | Unit

type Expr =
    | Literal of Literal
    | Unary of Token * Expr
    | Binary of Expr * Token * Expr
    | Grouping of Expr
    | Identifier of string

type Stmt =
    | Expression of Expr
    | Assignment of Token * Expr
    | Print of Expr

type Program = Stmt list

