module Vec3.Interpreter.Grammar

open Token

type TypeVar = int

let freshTypeVar =
    let counter = ref 0

    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

type Type =
    | TInteger
    | TFloat
    | TRational
    | TComplex

    | TBool

    | TString

    | TUnit
    | TNever

    | TAny

    | TFunction of Type list * Type * bool * bool // bool is pure flag, maybe also have builtin flag

    | TTypeVariable of TypeVar

    | TConstrain of TypeVar * (Type -> bool)

    | TTuple of Type list

    | TTensor of Type * Dims

    | TRecord of Row
    | TRowEmpty
    | TRowExtend of Token * Type * Row

    | TAlias of Token * Type option

    member this.IsPrimitive =
        match this with
        | TInteger
        | TFloat
        | TRational
        | TComplex
        | TBool
        | TString
        | TUnit
        | TNever -> true
        | TAlias(_, Some t) -> t.IsPrimitive
        | _ -> false

    member this.IsNumeric =
        match this with
        | TInteger
        | TFloat
        | TRational
        | TComplex -> true
        | TAlias(_, Some t) -> t.IsNumeric
        | _ -> false

    member this.IsArithmetic =
        match this with
        | TInteger
        | TFloat
        | TRational
        | TComplex -> true
        | TAlias(_, Some t) -> t.IsArithmetic
        | TTensor(typ, _) -> typ.IsArithmetic
        | _ -> false

    member this.IsPure =
        match this with
        | TFunction(_, _, pure', _) -> pure'
        | TAlias(_, Some t) -> t.IsPure
        | TInteger
        | TFloat
        | TRational
        | TComplex -> true
        | TTypeVariable _ -> true
        | TConstrain _ -> true
        | _ -> false

    member this.IsPolynomial =
        match this with
        | TFunction _ -> false
        | TTypeVariable _ -> true
        | TConstrain _ -> true
        | TAlias(_, Some t) -> t.IsPolynomial
        | _ -> false

    member this.IsBuiltinFunc =
        match this with
        | TFunction(_, _, _, bt) -> bt
        | _ -> false

and Dims =
    | Dims of int list
    | DAny
    | DVar of TypeVar

and Row = Type // Row | RowEmpty | RowExtend

type TType = Type

let typeMap =
    [ "int", TInteger
      "float", TFloat
      "rational", TRational
      "complex", TComplex
      "bool", TBool
      "string", TString
      "unit", TUnit
      "any", TAny
      "never", TNever ]
    |> Map.ofList

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
    | EIdentifier of Token * Type option
    | EGrouping of Expr * Type option
    | EIf of Expr * Expr * Expr * Type option
    | ETernary of Expr * Expr * Expr * Type option

    | EList of Expr list * Type option
    | ETuple of Expr list * Type option

    | ECall of Expr * Expr list * Type option
    | EIndex of Expr * Expr * Type option

    | ELambda of (Token * Type option) list * Expr * Type option * bool * Type option // bool is pure flag
    | EBlock of Stmt list * Type option
    | ERange of Expr * Expr * Type option

    | ERecordSelect of Expr * Token * Type option
    | ERecordExtend of (Token * Expr * Type option) * Expr * Type option
    | ERecordRestrict of Expr * Token * Type option
    | ERecordEmpty of Type
    

and Stmt =
    | SExpression of Expr * Type option
    | SVariableDeclaration of Token * Expr * Type option
    | SAssertStatement of Expr * Expr option * Type option
    | STypeDeclaration of Token * Type * Type option
    | SRecFunc of Token * (Token * Type option) list * Expr * Type option

type Program = Stmt list
