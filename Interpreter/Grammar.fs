module Vec3.Interpreter.Grammar

open Token

type TypeVar = int

let freshTypeVar =
    let counter = ref 0

    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

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
    
    | TConstrain of TypeVar * Type list
    
    | TTuple of Type list
    
    | TTensor of Type * Dims
    
    | TRecord of Row 
    | TRowEmpty
    | TRowExtend of Token * Type * Row
    
    | TAlias of Token * Type

and Dims = Dims of int list | DAny | DVar of TypeVar

and Row = Type // Row | RowEmpty | RowExtend

type TType = Type

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
    | EIf of Expr * Expr * Expr * Type
    | ETernary of Expr * Expr * Expr * Type
    
    | EList of Expr list * Type
    | ETuple of Expr list * Type
    
    | ECall of Expr * Expr list * Type
    | EIndex of Expr * Expr * Type
    
    | ELambda of Token list * Expr * Type
    | EBlock of Stmt list * Type
    | ERange of Expr * Expr * Type
    
    | ERecordSelect of Expr * Token * Type
    | ERecordExtend of (Token * Expr * Type) * Expr * Type
    | ERecordRestrict of Expr * Token * Type
    | ERecordEmpty of Type
    
and Stmt =
    | SExpression of Expr * Type
    | SVariableDeclaration of Token * Expr * Type
    | SAssertStatement of Expr * Expr option * Type
    | STypeDeclaration of Token * Type * Type

type Program = Stmt list

let numberToString (n: Number) =
    match n with
    | LFloat f -> $"Float({f})"
    | LInteger i -> $"Integer({i})"
    | LRational (n, d) -> $"Rational({n}/{d})"
    | LComplex (r, i) -> $"Complex({r}i{i})"

