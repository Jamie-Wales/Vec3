/// <summary>
/// Represents the abstract syntax tree of the language.
/// </summary>
module Vec3.Interpreter.Grammar

open Token

/// <summary>
/// A type variable used for generic types and type inference.
/// </summary>
type TypeVar = int

/// <summary>
/// Generates a fresh type variable.
/// </summary>
let freshTypeVar =
    let counter = ref 0

    fun () ->
        counter.Value <- counter.Value + 1
        counter.Value

/// <summary>
/// Represents a type in the language.
/// </summary>
type Type =
    | TInteger
    | TFloat
    | TRational
    | TComplex
    
    | TChar

    | TBool

    | TString

    /// <summary>
    /// Represents the unit type (a type with only one value).
    /// </summary>
    | TUnit
    
    /// <summary>
    /// The bottom type, unsolvable type.
    /// </summary>
    | TNever

    /// <summary>
    /// The top type, representing any type.
    /// </summary>
    | TAny

    /// <summary>
    /// A function type with a list of argument types, a return type, a pure flag, and a builtin flag.
    /// </summary>
    | TFunction of Type list * Type * bool * bool

    | TTypeVariable of TypeVar

    /// <summary>
    /// A type constraint. Allows constraining a type to a type that satisfies a predicate.
    /// </summary>
    | TConstrain of Constrain

    | TTuple of Type list

    /// <summary>
    /// The list or array type. Contains the element type and the dimensions, allowing for type safe multidimensional arrays.
    /// Can possibly be extended to allow dependant typing due to the dimensions.
    /// </summary>
    | TTensor of Type * Dims

    /// <summary>
    /// Various record types.
    /// Represented by a recursive definition of a row that extends other rows.
    /// </summary>
    | TRecord of Row
    | TRowEmpty
    | TRowExtend of Token * Type * Row

    /// <summary>
    /// An alias type, representing a type that is an alias to another type.
    /// </summary>
    | TAlias of Token * Type option

    /// <summary>
    /// Predicate to check if a type is a primitive type.
    /// </summary>
    /// <returns>True if the type is a primitive type, false otherwise.</returns>
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

    /// <summary>
    /// Predicate to check if a type is a numeric type.
    /// </summary>
    /// <returns>True if the type is a numeric type, false otherwise.</returns>
    member this.IsNumeric =
        match this with
        | TInteger
        | TFloat
        | TRational
        | TChar
        | TComplex -> true
        | TAlias(_, Some t) -> t.IsNumeric
        | _ -> false

    /// <summary>
    /// Predicate to check if a type can be used in arithmetic operations.
    /// </summary>
    /// <returns>True if the type can be used in arithmetic operations, false otherwise.</returns>
    member this.IsArithmetic =
        match this with
        | TInteger
        | TFloat
        | TRational
        | TChar
        | TComplex -> true
        | TAlias(_, Some t) -> t.IsArithmetic
        | TTensor(typ, _) -> typ.IsArithmetic
        | _ -> false

    /// <summary>
    /// Predicate to check if a type is 'pure', i.e. it does not have side effects.
    /// </summary>
    /// <returns>True if the type is pure, false otherwise.</returns>
    member this.IsPure =
        match this with
        | TFunction(_, _, pure', _) -> pure'
        | TAlias(_, Some t) -> t.IsPure
        | TInteger
        | TFloat
        | TRational
        | TChar
        | TComplex -> true
        | TTypeVariable _ -> true
        | TConstrain _ -> true
        | _ -> false

    /// <summary>
    /// Predicate to check if a type is a list type.
    /// </summary>
    /// <returns>True if the type is a list type, false otherwise.</returns>
    member this.IsFunction =
        match this with
        | TFunction _ -> true
        | TAlias(_, Some t) -> t.IsFunction
        | _ -> false

    /// <summary>
    /// Unfinished predicate to check if a type is a polynomial type.
    /// </summary>
    /// <returns>True if the type is a polynomial type, false otherwise.</returns>
    member this.IsPolynomial =
        match this with
        | TFunction _ -> false // fix
        | TTypeVariable _ -> true
        | TConstrain _ -> true
        | TAlias(_, Some t) -> t.IsPolynomial
        | _ -> false

    /// <summary>
    /// Predicate to check if a type is a builtin function type.
    /// </summary>
    /// <returns>True if the type is a builtin function type, false otherwise.</returns>
    member this.IsBuiltinFunc =
        match this with
        | TFunction(_, _, _, bt) -> bt
        | _ -> false

    /// <summary>
    /// Predicate to check if a function type has a specific number of arguments.
    /// </summary>
    /// <param name="num">The number of arguments to check for.</param>
    /// <returns>True if the function type has the specified number of arguments, false otherwise.</returns>
    member this.NumArgsIs num =
        match this with
        | TFunction(args, _, _, _) -> List.length args = num
        | TAlias(_, Some t) -> t.NumArgsIs num
        | _ -> false

    /// <summary>
    /// Predicate to check if a function type has a specific return type.
    /// </summary>
    /// <returns>True if the type is a tensor, false otherwise.</returns>
    member this.IsList =
        match this with
        | TTensor _ -> true
        | TAlias(_, Some t) -> t.IsList
        | _ -> false

    /// <summary>
    /// Predicate to check if a record type has a specific field.
    /// </summary>
    /// <param name="name">The lexeme that is expected in the record</param>
    /// <returns>True if the record type has the specified field, false otherwise.</returns>
    member this.hasField (name: Lexeme) =
        match this with
        | TRecord row ->
            let rec hasField' row =
                match row with
                | TRowEmpty -> false
                | TRowExtend(field, _, rest) -> field.Lexeme = name || hasField' rest
                | _ -> false

            hasField' row
        | TAlias(_, Some t) -> t.hasField name
        | _ -> false

    /// <summary>
    /// Predicate to check if a record type has a specific field with a specific type.
    /// </summary>
    /// <param name="name">The lexeme that is expected in the record</param>
    /// <param name="typ">The type that is expected for the field</param>
    /// <returns>True if the record type has the specified field with the specified type, false otherwise.</returns>
    member this.hasFieldOf (name: Lexeme) (typ: Type) =
        match this with
        | TRecord row ->
            let rec hasField' row =
                match row with
                | TRowEmpty -> false
                | TRowExtend(field, t, rest) -> (field.Lexeme = name && t = typ) || hasField' rest
                | TConstrain constrain -> constrain.Constrain typ
                | _ -> false

            hasField' row
        | TAlias(_, Some t) -> t.hasFieldOf name typ
        | _ -> false

    /// <summary>
    /// Predicate to check if a record type has a specific field that satisfies a predicate.
    /// </summary>
    /// <param name="name">The lexeme that is expected in the record</param>
    /// <param name="constrain">The predicate that the field type must satisfy</param>
    /// <returns>True if the record type has the specified field that satisfies the predicate, false otherwise.</returns>
    member this.hasFieldThat (name: Lexeme) (constrain: Type -> bool) =
        match this with
        | TRecord row ->
            let rec hasField' row =
                match row with
                | TRowEmpty -> false
                | TRowExtend(field, t, rest) -> (field.Lexeme = name && constrain t) || hasField' rest
                | _ -> false

            hasField' row
        | TAlias(_, Some t) -> t.hasFieldThat name constrain
        | _ -> false

    /// <summary>
    /// Predicate to check if a record type has a specific set of fields that all satisfy a predicate.
    /// </summary>
    /// <param name="fields">A list of lexemes and predicates that the fields must satisfy</param>
    /// <returns>True if the record type has the specified fields that all satisfy the predicates, false otherwise.</returns>
    member this.hasFieldsThat (fields: (Lexeme * (Type -> bool)) list) =
        List.forall (fun (name, constrain) -> this.hasFieldThat name constrain) fields

    /// <summary>
    /// Predicate to check if a record type has a specific set of fields.
    /// </summary>
    /// <param name="names">A list of lexemes that the fields must have</param>
    /// <returns>True if the record type has the specified fields, false otherwise.</returns>
    member this.hasFields names = List.forall this.hasField names

    /// <summary>
    /// Predicate to check if a record type has a specific set of fields with specific types.
    /// </summary>
    /// <param name="fields">A list of lexemes and types that the fields must have</param>
    /// <returns>True if the record type has the specified fields with the specified types, false otherwise.</returns>
    member this.hasFieldsOf (fields: (Lexeme * Type) list) =
        List.forall (fun (name, typ) -> this.hasFieldOf name typ) fields
    
    /// <summary>
    /// Predicate to check the minimum number of dimensions of a tensor type.
    /// </summary>
    /// <param name="n">The minimum number of dimensions</param>
    /// <returns>True if the tensor type has at least n dimensions, false otherwise.</returns>
    member this.hasMinDims n =
        match this with
        | TTensor(_, d) -> match d with
                            | Dims dims -> dims >= n
                            | _ -> true
        | TAlias(_, Some t) -> t.hasMinDims n
        | TTuple ts -> List.length ts >= n
        | _ -> false

/// <summary>
/// Represents the dimensions of a tensor type.
/// </summary>
and Dims =
    /// <summary>
    /// A list of integers representing the dimensions of the tensor.
    /// </summary>
    | Dims of int
    /// <summary>
    /// Any dimension.
    /// </summary>
    | DAny
    /// <summary>
    /// A type variable representing an unknown dimension.
    /// </summary>
    | DVar of TypeVar

/// <summary>
/// Alias for a row type (not strictly necessary, but makes the code more readable).
/// </summary>
and Row = Type // Row | RowEmpty | RowExtend

/// <summary>
/// Represents a type constraint (a type variable and a predicate).
/// </summary>
and Constrain(typeVar: TypeVar, constrain: Type -> bool) =
    member this.TypeVar = typeVar
    member this.Constrain = constrain

    /// <summary>
    /// Equality only based on the type variable.
    /// </summary>
    /// <param name="obj">The object to compare to</param>
    /// <returns>True if the objects are equal, false otherwise.</returns>
    override this.Equals(obj: obj) =
        match obj with
        | :? Constrain as c -> c.TypeVar = this.TypeVar
        | _ -> false

    /// <summary>
    /// Hash code only based on the type variable.
    /// </summary>
    /// <returns>The hash code of the object.</returns>
    override this.GetHashCode() = hash this.TypeVar

/// <summary>
/// Alias as Type is also a system type.
/// </summary>
type TType = Type

/// <summary>
/// Map of type names to type values.
/// </summary>
let typeMap =
    [ "int", TInteger
      "float", TFloat
      "rational", TRational
      "complex", TComplex
      "bool", TBool
      "string", TString
      "char", TChar
      "unit", TUnit
      "any", TAny
      "never", TNever ]
    |> Map.ofList

/// <summary>
/// A literal value in the language.
/// </summary>
type Literal =
    | LNumber of Number
    | LString of string
    | LBool of bool
    | LUnit

/// <summary>
/// The AST of the language.
/// </summary>
type Expr =
    | ELiteral of Literal * Type
    | EIdentifier of Token * Type option
    | EGrouping of Expr * Type option
    
    | EIf of Expr * Expr * Expr * Type option
    | ETernary of Expr * Expr * Expr * Type option

    | EList of Expr list * Type option
    | ETuple of Expr list * Type option

    | ECall of Expr * Expr list * Type option
    
    /// <summary>
    /// Indexing operation on a list or tensor.
    /// Expr (list or tensor), (start, end, isRange), type
    /// Allows for indexing in the form l[1] or l[..1] or l[1..2] or l[1..]
    /// </summary>
    | EIndex of Expr * (Expr option * Expr option * bool) * Type option

    /// <summary>
    /// A lambda expression with a list of arguments, a body, a return type, a pure flag, and a type.
    /// </summary>
    | ELambda of (Token * Type option) list * Expr * Type option * bool * Type option * bool // bool is pure flag
    | EBlock of Stmt list * Type option
    | ERange of Expr * Expr * Type option

    | ERecordSelect of Expr * Token * Type option
    
    /// <summary>
    /// Records represented recursively as a row type.
    /// </summary>
    | ERecordExtend of (Token * Expr * Type option) * Expr * Type option
    | ERecordRestrict of Expr * Token * Type option
    | ERecordEmpty of Type

    /// <summary>
    /// Unevaluated code block.
    /// </summary>
    | ECodeBlock of Expr

    /// <summary>
    /// A tail call (for tail recursion).
    /// </summary>
    | ETail of Expr * Type option
    
    /// <summary>
    /// Pattern matching expression.
    /// </summary>
    | EMatch of Expr * (Pattern * Expr) list * Type option
    

/// <summary>
/// A statement in the language (something that does not return a value).
/// </summary>
and Stmt =
    | SExpression of Expr * Type option
    | SVariableDeclaration of Token * Expr * Type option
    | SAssertStatement of Expr * Expr option * Type option
    | STypeDeclaration of Token * Type * Type option
    | SRecFunc of Token * (Token * Type option) list * Expr * Type option
    | SAsync of Token * (Token * Type option) list * Expr * Type option
    | SImport of Token option * string * Type option // maybe binding name, module name (path), type
  
/// <summary>
/// Various patterns for pattern matching.
/// </summary>
and Pattern =
    | PLiteral of Literal
    | PIdentifier of Token
    | PWildcard
    | PList of Pattern list
    | PCons of Pattern * Pattern
    | PTuple of Pattern list
    | PRecordEmpty
    | PRecord of (Token * Pattern) list
    | PType of Type

/// <summary>
/// The program is a list of statements.
/// </summary>
type Program = Stmt list
