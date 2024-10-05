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
    
    | TTypeVariable of TypeVar
    
    // contrains a type to an allowed set of types
    // i have ideas that this will allow hidler miller inference without type classes
    // gonna take quite a bit of trial and error
    | TConstrain of TypeVar * Type list
    
    // todo
    | TTuple of Type list
    | TList of Type
    
    | TVector of Type * int
    | TMatrix of Type * int * int

and Constraint = Constraint of Type list
    
type Number =
    | LInteger of int
    | LFloat of float
    | LRational of int * int
    | LComplex of float * float
    
type Literal =
    | LNumber of Number 
    | LString of string
    | LBool of bool
    | LUnit

type Expr =
    | ELiteral of Literal * Type
    | EIdentifier of Token * Type
    | EUnary of Token * Expr * Type
    | EBinary of Expr * Token * Expr * Type
    | EGrouping of Expr * Type
    | EAssignment of Token * Expr * Type
    | EIf of Expr * Expr * Expr * Type
    | ETernary of Expr * Expr * Expr * Type
    
    | ECall of Expr * Expr list * Type
    | ELambda of (Token * Type) list * Type * Expr * Type
    | EBlock of Stmt list * Type
    
and Stmt =
    | SExpression of Expr * Type
    | SVariableDeclaration of Token * Type * Expr * Type
    | SPrintStatement of Expr * Type

type Program = Stmt list

let numberToString (n: Number) =
    match n with
    | LFloat f -> $"Float({f})"
    | LInteger i -> $"Integer({i})"
    | LRational (n, d) -> $"Rational({n}/{d})"
    | LComplex (r, i) -> $"Complex({r}i{i})"

