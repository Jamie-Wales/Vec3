/// <summary>
/// Contains the types of built-in functions and constants.
/// </summary>

module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

let consType =
    let listTyp = TTypeVariable(freshTypeVar ())
    let dimsVar1 = DVar(freshTypeVar ())
    let dimsVar2 = DVar(freshTypeVar ())

    TFunction([ listTyp; TTensor(listTyp, dimsVar1) ], TTensor(listTyp, dimsVar2), false, true)


let plotFunType =
    let funConstrain =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    TConstrain(
        Constrain(
            freshTypeVar (),
            (fun t ->
                t = TFunction([ TString; funConstrain ], TUnit, false, true)
                || t = TFunction([ TString; funConstrain; TFloat; TFloat ], TUnit, false, true))
        )
    )


let plus =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], constrain, true, true)

let minus =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], constrain, true, true)

let mul =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], constrain, true, true)

let div =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], constrain, true, true)

let pow =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], constrain, true, true)

let modF = TFunction([ TInteger; TInteger ], TInteger, true, true)

let eq = TFunction([ TAny; TAny ], TBool, false, true)

let expType = TFunction([ TFloat ], TFloat, true, true)

// later on make this curry
let logType = TFunction([ TFloat; TFloat ], TFloat, false, true)


let lt =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], TBool, false, true)

let gt =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], TBool, false, true)

let lte =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], TBool, false, true)

let gte =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain; constrain ], TBool, false, true)

let andF = TFunction([ TBool; TBool ], TBool, false, true)

let orF = TFunction([ TBool; TBool ], TBool, false, true)

let notF = TFunction([ TBool ], TBool, false, true)

let neg =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain ], constrain, true, true)

let unneg =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(Constrain(typeVar, _.IsArithmetic))

    TFunction([ constrain ], constrain, true, true)

let crossProduct =
    let tensorTypeVar = TTypeVariable(freshTypeVar ())

    TFunction(
        [ TTensor(tensorTypeVar, Dims 3); TTensor(tensorTypeVar, Dims 3) ],
        TTensor(tensorTypeVar, Dims 3),
        false,
        true
    )

let dotProduct =
    let tensorTypeVar = freshTypeVar ()
    let dimsVar = DVar(freshTypeVar ())
    let constrain = TConstrain(Constrain(tensorTypeVar, _.IsArithmetic))

    TFunction([ TTensor(constrain, dimsVar); TTensor(constrain, dimsVar) ], constrain, false, true)

let castType =
    let typ = TTypeVariable(freshTypeVar ())

    TFunction([ TAny; typ ], typ, false, true)

let newtonRaphsonType =
    let funcT1 =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    let funcT2 =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    TFunction([ funcT1; funcT2; TFloat; TFloat; TInteger ], TFloat, false, true)

let bisectionTyp =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    TFunction([ funcT; TFloat; TFloat; TFloat; TInteger ], TFloat, false, true)

let differentiateType =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    let retT = TFunction([ TFloat ], TFloat, true, false)

    TFunction([ funcT ], retT, false, true)

let integrateType =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    let retT = TFunction([ TFloat ], TFloat, true, false)

    TFunction([ funcT ], retT, false, true)


let taylorSeriesT =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    let retT = TFunction([ TFloat ], TFloat, true, false)

    TFunction([ funcT; TInteger ], retT, false, true)


let plotFunsType =
    let funcT =
        TConstrain(Constrain(freshTypeVar (), (fun typ -> typ.IsPure && typ.NumArgsIs 1)))

    TFunction([ TString; TTensor(funcT, DAny) ], TUnit, false, true)

let plotType =
    let tensWithArithFieldsFunc =
        fun (t: TType) ->
            match t with
            | TTensor(typ, _) -> typ.IsArithmetic
            | _ -> false


    let func =
        fun (t: TType) ->
            t.hasFieldsThat
                [ (Identifier "x", tensWithArithFieldsFunc)
                  (Identifier "y", tensWithArithFieldsFunc)
                  (Identifier "title", fun t -> t = TString)
                  (Identifier "ptype", fun t -> t = TString) ]

    let recTyp = TConstrain(Constrain(freshTypeVar (), func))

    TFunction([ recTyp ], TUnit, false, true)

let drawType =
    let recTyp =
        TConstrain(
            Constrain(
                freshTypeVar (),
                fun t ->
                    t.hasFieldsThat
                        [ (Identifier "width", fun t -> t = TFloat)
                          (Identifier "height", fun t -> t = TFloat)
                          (Identifier "x", fun t -> t = TFloat)
                          (Identifier "y", fun t -> t = TFloat)
                          (Identifier "colour", fun t -> t = TString) ]
                    || (t.IsList
                        && match t with
                           | TRecord(t) ->
                               t.hasFieldsThat
                                   [ (Identifier "width", fun t -> t = TFloat)
                                     (Identifier "height", fun t -> t = TFloat)
                                     (Identifier "x", fun t -> t = TFloat)
                                     (Identifier "y", fun t -> t = TFloat)
                                     (Identifier "colour", fun t -> t = TString) ]
                           | _ -> false)
            )
        )

    let token =
        { Lexeme = Identifier "id"
          Position = { Column = 0; Line = 0 } }

    let returnT = TRecord(TRowExtend(token, TInteger, TRowEmpty))

    TFunction([ recTyp ], returnT, false, true)

let readTyp = TFunction([ TString ], TAny, false, true)

let onType =
    let idTyp =
        TConstrain(Constrain(freshTypeVar (), (fun t -> t.hasFieldOf (Identifier "id") TInteger)))

    let eventTyp = TInteger
        // TConstrain(Constrain(freshTypeVar (), (fun t -> t.hasFieldOf (Identifier "event") TInteger)))

    let stateT =
        TConstrain(
            Constrain(freshTypeVar (), (fun t -> t.hasFieldsOf [ (Identifier "x", TFloat); (Identifier "y", TFloat) ]))
        )

    let returnT =
        TConstrain(
            Constrain(freshTypeVar (), (fun t -> t.hasFieldsOf [ (Identifier "x", TFloat); (Identifier "y", TFloat) ]))
        )

    TFunction([ idTyp; eventTyp; TAny ], returnT, false, true)

let PlotEllipseType =
    let recTyp =
        TConstrain(
            Constrain(
                freshTypeVar (),
                fun t ->
                    t.hasFieldsThat
                        [ (Identifier "x", fun t -> t = TFloat)
                          (Identifier "y", fun t -> t = TFloat)
                          (Identifier "rx", fun t -> t = TFloat)
                          (Identifier "ry", fun t -> t = TFloat)
                          ]
            )
        )

    TFunction([ recTyp ], TUnit, false, true)

let PlotEllipsesType =
    let recTyp =
        TConstrain(
            Constrain(
                freshTypeVar (),
                (fun t ->
                    t.IsList && match t with
                                | TRecord(t) ->
                                    t.hasFieldsThat
                                        [ (Identifier "x", fun t -> t = TFloat)
                                          (Identifier "y", fun t -> t = TFloat)
                                          (Identifier "rx", fun t -> t = TFloat)
                                          (Identifier "ry", fun t -> t = TFloat)
                                          ]
                                | _ -> false)
            )
        )
        
    TFunction([ recTyp ], TUnit, false, true)

let appendType =
    let typeVar = freshTypeVar ()
    let constrain = TConstrain(
        Constrain(typeVar,
                  _.IsList,
                  (fun t -> match t with | TTensor(t, _) -> TTensor(t, DAny) | _ -> t)
                  )
        )
    
    TFunction([ constrain; constrain ], constrain, false, true)

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
      
      Cast, castType
      
      NewtonRaphson, newtonRaphsonType
      Bisection, bisectionTyp
      Differentiate, differentiateType
      Integrate, integrateType
      
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
