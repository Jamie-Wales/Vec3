module Vec3.Interpreter.Grammar

open Token

type Literal =
    | Number of Number 
    | String of string
    | Identifier of string
    | Bool of bool
    | Unit

type Expr =
    | Literal of Literal
    | Variable of Lexeme 
    | Unary of Token * Expr
    | Binary of Expr * Token * Expr
    | Grouping of Expr
    | Assignment of Token * Expr
    

type Stmt =
    | Expression of Expr
    | VariableDecleration of Token * Expr
    | Print of Expr

type Program = Stmt list

