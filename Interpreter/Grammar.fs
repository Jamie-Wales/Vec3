module Vec3.Interpreter.Grammar

open Token

type Expr =
    | Literal of Literal
    | Unary of Token * Expr
    | Binary of Expr * Token * Expr
    | Grouping of Expr

and Literal =
    | Number of Number 
    | String of string
    | Bool of bool
    | Nil

type Stmt =
    | Expression of Expr
    | Print of Expr

type Program = Stmt list

