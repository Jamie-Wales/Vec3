module Vec3.Interpreter.Grammar

open Token

type TypeVar = int

type Type =
    | TInfer
    
    | TInteger
    | TFloat
    | TRational
    | TComplex
    
    | TBool
    
    | TString
    
    | TUnit
    | TNever
    
    | TAny
    
    | TFunction of Type list * Type
    // maybe instead
    // | Function of (Type list) list * Type
    // so that functions can be unionized, but of course need to check whether body is valid
    
    | TTypeVariable of TypeVar
    
    // contrains a type to an allowed set of types
    // i have ideas that this will allow hidler miller inference without type classes
    // gonna take quite a bit of trial and error
    | TConstrain of Type list
    
    // todo
    | TTuple of Type list
    | TList of Type
    
    | TVector of Type * int
    | TMatrix of Type * int * int
    
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

