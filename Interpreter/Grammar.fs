module Vec3.Interpreter.Grammar

open Token

type Type =
    | Infer
    | Integer
    | Float
    | Rational
    | Complex
    | String
    | Bool
    | Unit
    | Never
    | Any
    | Function of Type list * Type
    | UserDefined of string
    
type TNumber =
    | Integer of int
    | Float of float
    | Rational of int * int
    | Complex of float * float
    
type Literal =
    | TNumber of TNumber 
    | String of string
    | Bool of bool
    | Unit

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

let numberToString (n: TNumber) =
    match n with
    | Float f -> $"Float({f})"
    | Integer i -> $"Integer({i})"
    | Rational (n, d) -> $"Rational({n}/{d})"
    | Complex (r, i) -> $"Complex({r}i{i})"

