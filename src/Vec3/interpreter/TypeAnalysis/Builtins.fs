/// <summary>
/// Contains the types of built-in functions and constants.
/// </summary>

module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

/// <summary>
/// Cons type.
/// </summary>
let consType =
    let listTyp = TTypeVariable(freshTypeVar ())
    let dimsVar1 = DVar(freshTypeVar ())
    let dimsVar2 = DVar(freshTypeVar ())

    TFunction([ listTyp; TTensor(listTyp, dimsVar1) ], TTensor(listTyp, dimsVar2), false, true)


/// <summary>
/// Plot function type.
/// </summary>
let plotFunType =
    let funConstrain =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "Only 1 argument is allowed"))

    TConstrain(
        Constrain(
            freshTypeVar (),
            (fun t ->
                t = TFunction([ TString; funConstrain ], TUnit, false, true)
                || t = TFunction([ TString; funConstrain; TFloat; TFloat ], TUnit, false, true)),
            id,
            Some "Maybe the function is not pure"
        )
    )


/// <summary>
/// Plus function type.
/// </summary>
let plus =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], constrain, true, true)

/// <summary>
/// Minus function type.
/// </summary>
let minus =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], constrain, true, true)

/// <summary>
/// Multiply function type.
/// </summary>
let mul =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], constrain, true, true)

/// <summary>
/// Divide function type.
/// </summary>
let div =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], constrain, true, true)

/// <summary>
/// Power function type.
/// </summary>
let pow =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], constrain, true, true)

/// <summary>
/// Modulus function type.
/// </summary>
let modF = TFunction([ TInteger; TInteger ], TInteger, true, true)

/// <summary>
/// Equal function type.
/// </summary>
let eq = TFunction([ TAny; TAny ], TBool, false, true)

/// <summary>
/// Exponential function type.
/// </summary>
let expType = TFunction([ TFloat ], TFloat, true, true)

/// <summary>
/// Logarithm function type.
/// </summary>
let logType = TFunction([ TFloat; TFloat ], TFloat, false, true)


/// <summary>
/// Less than function type.
/// </summary>
let lt =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], TBool, false, true)

/// <summary>
/// Greater than function type.
/// </summary>
let gt =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], TBool, false, true)

/// <summary>
/// Less than or equal function type.
/// </summary>
let lte =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], TBool, false, true)

/// <summary>
/// Greater than or equal function type.
/// </summary>
let gte =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain; constrain ], TBool, false, true)

/// <summary>
/// And function type.
/// </summary>
let andF = TFunction([ TBool; TBool ], TBool, false, true)

/// <summary>
/// Or function type.
/// </summary>
let orF = TFunction([ TBool; TBool ], TBool, false, true)

/// <summary>
/// <c>Not</c> function type.
/// </summary>
let notF = TFunction([ TBool ], TBool, false, true)

/// <summary>
/// Negate function type.
/// </summary>
let neg =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain ], constrain, true, true)

/// <summary>
/// Unary unnegate function type.
/// </summary>
let unneg =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ constrain ], constrain, true, true)

/// <summary>
/// Cross product function type.
/// </summary>
let crossProduct =
    let tensorTypeVar = TTypeVariable(freshTypeVar ())

    TFunction(
        [ TTensor(tensorTypeVar, Dims 3); TTensor(tensorTypeVar, Dims 3) ],
        TTensor(tensorTypeVar, Dims 3),
        false,
        true
    )

/// <summary>
/// Dot product function type.
/// </summary>
let dotProduct =
    let tensorTypeVar = freshTypeVar ()
    let dimsVar = DVar(freshTypeVar ())
    let constrain = TConstrain(Constrain(tensorTypeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    TFunction([ TTensor(constrain, dimsVar); TTensor(constrain, dimsVar) ], constrain, false, true)

/// <summary>
/// Intelligent cast function type.
/// </summary>
let castType =
    let typ = TConstrain(Constrain(freshTypeVar (), (fun _ -> true), (fun t -> match t with
                                                                                | TTensor(t, _) -> TTensor(t, DAny)
                                                                                | _ -> t), None
                                                                      ))

    TFunction([ TAny; typ ], typ, false, true)

/// <summary>
/// Newton-Raphson function type.
/// </summary>
let newtonRaphsonType =
    let funcT1 =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    let funcT2 =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    TFunction([ funcT1; funcT2; TFloat; TFloat; TInteger ], TFloat, false, true)

/// <summary>
/// Bisection function type.
/// </summary>
let bisectionTyp =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    TFunction([ funcT; TFloat; TFloat; TFloat; TInteger ], TFloat, false, true)

/// <summary>
/// Differentiate function type.
/// </summary>
let differentiateType =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    let retT = TFunction([ TFloat ], TFloat, true, false)

    TFunction([ funcT ], retT, false, true)

/// <summary>
/// Integrate function type.
/// </summary>
let integrateType =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    let retT = TFunction([ TFloat ], TFloat, true, false)

    TFunction([ funcT ], retT, false, true)

/// <summary>
/// Tangent function type.
/// </summary>
let tangentType =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    let retT = TFunction([ TFloat ], TFloat, true, false)

    TFunction([ funcT; TFloat ], retT, false, true)


/// <summary>
/// Taylor series function type.
/// </summary>
let taylorSeriesT =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    let retT = TFunction([ TFloat ], TFloat, true, false)

    TFunction([ funcT; TInteger ], retT, false, true)


/// <summary>
/// Plot functions type.
/// </summary>
let plotFunsType =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1), id, Some "The function must be pure"))

    TFunction([ TString; TTensor(funcT, DAny) ], TUnit, false, true)

/// <summary>
/// Plot type.
/// </summary>
let plotType =
    let func =
        fun (t: TType) ->
            t.hasFieldsThat
                [ (Identifier "x", _.IsList)
                  (Identifier "y", _.IsList)
                  (Identifier "title", fun t -> t = TString)
                  (Identifier "ptype", fun t -> t = TString) ]

    let recTyp = TConstrain(Constrain(freshTypeVar (), func))

    TFunction([ recTyp ], TUnit, false, true)

/// <summary>
/// Draw type.
/// </summary>
let drawType =
    let recTyp =
        TConstrain(
            Constrain(
                freshTypeVar (),
                (fun t ->
                    t.hasFieldsThat
                        [ (Identifier "width", fun t -> t = TFloat)
                          (Identifier "height", fun t -> t = TFloat)
                          (Identifier "x", fun t -> t = TFloat)
                          (Identifier "y", fun t -> t = TFloat)
                          (Identifier "colour", fun t -> t = TString) ]
                    || (t.IsList
                        && match t with
                           | TTensor(t, _) ->
                               t.hasFieldsThat
                                   [ (Identifier "width", fun t -> t = TFloat)
                                     (Identifier "height", fun t -> t = TFloat)
                                     (Identifier "x", fun t -> t = TFloat)
                                     (Identifier "y", fun t -> t = TFloat)
                                     (Identifier "colour", fun t -> t = TString) ]
                           | _ -> false)), id, Some "The type must have the required fields")
            )

    let token =
        { Lexeme = Identifier "id"
          Position = { Column = 0; Line = 0 } }

    let returnT = TRecord(TRowExtend(token, TInteger, TRowEmpty))

    TFunction([ recTyp ], returnT, false, true)

/// <summary>
/// Read type.
/// </summary>
let readTyp = TFunction([ TString ], TAny, false, true)

/// <summary>
/// On type (event handler).
/// </summary>
let onType =
    let idTyp =
        TConstrain(Constrain(freshTypeVar (), (fun t -> t.hasFieldOf (Identifier "id") TInteger), id, Some "The type must have an id field"))

    let eventTyp = TInteger
    // TConstrain(Constrain(freshTypeVar (), (fun t -> t.hasFieldOf (Identifier "event") TInteger)))

    let stateT =
        TConstrain(
            Constrain(freshTypeVar (), (fun t -> t.hasFieldsOf [ (Identifier "x", TFloat); (Identifier "y", TFloat) ]), id, Some "The type must have x and y fields")
        )

    let returnT =
        TConstrain(
            Constrain(freshTypeVar (), (fun t -> t.hasFieldsOf [ (Identifier "x", TFloat); (Identifier "y", TFloat) ]), id, Some "The type must have x and y fields")
        )

    TFunction([ idTyp; eventTyp; TAny ], returnT, false, true)

/// <summary>
/// Plot ellipse type.
/// </summary>
let PlotEllipseType =
    let recTyp =
        TConstrain(
            Constrain(
                freshTypeVar (),
                (fun t ->
                    t.hasFieldsThat
                        [ (Identifier "x", fun t -> t = TFloat)
                          (Identifier "y", fun t -> t = TFloat)
                          (Identifier "rx", fun t -> t = TFloat)
                          (Identifier "ry", fun t -> t = TFloat) ]), id, Some "The type must have x, y, rx, and ry fields"
            )
        )

    TFunction([ recTyp ], TUnit, false, true)

/// <summary>
/// Plot ellipses type.
/// </summary>
let PlotEllipsesType =
    let recTyp =
        TConstrain(
            Constrain(
                freshTypeVar (),
                (fun t ->
                    t.IsList
                    && match t with
                       | TRecord(t) ->
                           t.hasFieldsThat
                               [ (Identifier "x", fun t -> t = TFloat)
                                 (Identifier "y", fun t -> t = TFloat)
                                 (Identifier "rx", fun t -> t = TFloat)
                                 (Identifier "ry", fun t -> t = TFloat) ]
                       | _ -> false), id, Some "The type must have x, y, rx, and ry fields"
            )
        )

    TFunction([ recTyp ], TUnit, false, true)

/// <summary>
/// Append function type.
/// </summary>
let appendType =
    let typeVar = freshTypeVar ()

    let constrain =
        TConstrain(
            Constrain(
                typeVar,
                (fun t -> t.IsList || t = TString),
                (fun t ->
                    match t with
                    | TTensor(t, _) -> TTensor(t, DAny)
                    | _ -> t), Some "The type must be a list or a string"
            )
        )

    TFunction([ constrain; constrain ], constrain, false, true)

/// <summary>
/// Determinate function type.
/// </summary>
let determType =
    let typeVar = freshTypeVar ()
    let dimsVar = DVar(freshTypeVar ())
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    let innerTensor = TTensor(constrain, dimsVar)
    let outerTensor = TTensor(innerTensor, dimsVar)

    TFunction([ outerTensor ], constrain, false, true)

/// <summary>
/// Tranpose function type.
/// </summary>
let transpoteType =
    let typeVar = freshTypeVar ()
    let dimsVar = DVar(freshTypeVar ())
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    let innerTensor = TTensor(constrain, dimsVar)
    let outerTensor = TTensor(innerTensor, dimsVar)

    TFunction([ outerTensor ], outerTensor, false, true)

/// <summary>
/// Inverse function type.
/// </summary>
let inverseType =
    let typeVar = freshTypeVar ()
    let dimsVar = DVar(freshTypeVar ())
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic, id, Some "The type must support arithmetic"))

    let innerTensor = TTensor(constrain, dimsVar)
    let outerTensor = TTensor(innerTensor, dimsVar)

    TFunction([ outerTensor ], outerTensor, false, true)

/// <summary>
/// Map of built-in functions to their types.
/// </summary>
let BuiltinFunctions: Map<BuiltInFunction, TType> =
    [ Print, TFunction([ TAny ], TUnit, false, true)
      Input, TFunction([], TString, false, true)
      Exit, TFunction([], TUnit, false, true)

      Cos, TFunction([ TFloat ], TFloat, true, true)
      Sin, TFunction([ TFloat ], TFloat, true, true)
      Tan, TFunction([ TFloat ], TFloat, true, true)
      ACos, TFunction([ TFloat ], TFloat, true, true)
      ASin, TFunction([ TFloat ], TFloat, true, true)
      ATan, TFunction([ TFloat ], TFloat, true, true)

      Eval, TFunction([ TAny ], TAny, false, true)

      Log, logType
      Exp, expType

      Trunc, TFunction([ TFloat ], TFloat, true, true)

      Read, readTyp

      Env, TFunction([], TUnit, false, true)
      Root, TFunction([ TFloat; TFloat ], TFloat, true, true)
      Abs, TFunction([ TFloat ], TFloat, true, true)
      Floor, TFunction([ TFloat ], TFloat, true, true)

      Plot, plotType
      PlotFunction, plotFunType
      PlotFunctions, plotFunsType

      PlotEllipse, PlotEllipseType
      PlotEllipses, PlotEllipsesType

      Draw, drawType

      Ceil, TFunction([ TFloat ], TFloat, true, true)

      Err, TFunction([ TString ], TAny, false, true)

      Add, plus
      Sub, minus
      Mul, mul
      Div, div
      Mod, modF
      Pow, pow
      And, andF
      Or, orF
      Not, notF
      Neg, neg
      Unneg, unneg
      Eq, eq
      Neq, eq
      Lt, lt
      Lte, lte
      Gt, gt
      Gte, gte

      Append, appendType

      CrossProduct, crossProduct
      DotProduct, dotProduct

      Determinate, determType
      Transpose, transpoteType
      Inverse, inverseType

      Cast, castType

      NewtonRaphson, newtonRaphsonType
      Bisection, bisectionTyp
      Differentiate, differentiateType
      Integrate, integrateType
      Tangent, tangentType

      Cons, consType

      On, onType

      Await, TAny

      TaylorSeries, taylorSeriesT


      Split, TFunction([ TString; TString ], TTensor(TString, DAny), false, true)
      ToLowerCase, TFunction([ TString ], TString, false, true)
      ToUpperCase, TFunction([ TString ], TString, false, true)
      Trim, TFunction([ TString ], TString, false, true)
      Join, TFunction([ TTensor(TString, DAny); TString ], TString, false, true)
      RandomI, TFunction([ TInteger ], TInteger, false, true)
      RandomF, TFunction([ TFloat ], TFloat, false, true)
      Time, TFunction([], TFloat, false, true)
      TypeOf, TFunction([ TAny ], TString, false, true)

      ]
    |> Map.ofList

/// <summary>
/// Map of built-in constants to their types.
/// </summary>
let BuiltinConstants: Map<Lexeme, TType> =
    [ Identifier "PI", TFloat; Identifier "E", TFloat; Identifier "TAU", TFloat ]
    |> Map.ofList
