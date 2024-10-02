module Vec3.Interpreter.Grammar

open Token

type Type =
    | Infer
    | Integer
    | Float
    | String
    | Bool
    | Unit
    | Never
    | Function of Type list * Type
    | UserDefined of string

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
    
    | Call of Token * Expr list
    | Lambda of (Token * Type) list * Type * Expr
    | Block of Stmt list
    
and Stmt =
    | Expression of Expr
    | VariableDeclaration of Token * Type * Expr // option

type Program = Stmt list

