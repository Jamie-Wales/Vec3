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
    | ELiteral of Literal
    | EIdentifier of Token
    | EUnary of Token * Expr
    | EBinary of Expr * Token * Expr
    | EGrouping of Expr
    | EAssignment of Token * Expr 
    
    | ECall of Token * Expr list
    | ELambda of (Token * Type) list * Type * Expr
    | EBlock of Stmt list
    
and Stmt =
    | SExpression of Expr
    | SVariableDeclaration of Token * Type * Expr // option
    | SPrintStatement of Expr

type Program = Stmt list

let numberToString (n: Number) =
    match n with
    | LFloat f -> $"Float({f})"
    | LInteger i -> $"Integer({i})"
    | LRational (n, d) -> $"Rational({n}/{d})"
    | LComplex (r, i) -> $"Complex({r}i{i})"

