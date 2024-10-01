module Vec3.Interpreter.Grammar

open Token

type Literal =
    | Number of Number 
    | String of string
    | Bool of bool
    | Unit

type Expr =
    | Literal of Literal
    | Variable of Lexeme 
    | Unary of Token * Expr
    | Binary of Expr * Token * Expr
    | Grouping of Expr
    | Assignment of Token * Expr
    
    | FunctionCall of Expr * Expr list
    | Function of string list * Expr
    | Block of Stmt list
    
and Stmt =
    | Expression of Expr
    | VariableDeclaration of Token * Expr
    | Print of Expr

type Program = Stmt list

