module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

let foldType =
    let listTyp = TTypeVariable (freshTypeVar())
    let accTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar); accTyp; TFunction([listTyp; accTyp], accTyp, false, false)], accTyp, false, true)
    
let consType =
    let listTyp = TTypeVariable (freshTypeVar())
    let dimsVar1 = DVar (freshTypeVar())
    let dimsVar2 = DVar (freshTypeVar())
    
    TFunction([listTyp; TTensor(listTyp, dimsVar1)], TTensor(listTyp, dimsVar2), false, true)
    
let plotType =
    // Jake plz fix <3
    // let dimsVar = DVar (freshTypeVar ())
    // let constrain = TConstrain(freshTypeVar(), _.IsArithmetic)
    //
    // TFunction([TString; TTensor(constrain, dimsVar); TTensor(constrain, dimsVar)], TUnit, false, true)
    TFunction([TAny;], TUnit, false, true)
let plotFunType =
    let funConstrain = TConstrain(freshTypeVar(), fun typ -> typ.IsPure && typ.NumArgsIs 1)
    
    TFunction([TString; funConstrain], TUnit, false, true)

let plus =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true, true)
    
let minus =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true, true)
    
let mul =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true, true)
    
let div =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true, true)
    
let pow =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true, true)
    
let modF =
    TFunction([TInteger; TInteger], TInteger, true, true)
    
let eq =
    TFunction([TAny; TAny], TBool, false, true)
    
let expType =
    TFunction([TFloat], TFloat, true, true)
    
// later on make this curry
let logType =
    TFunction([TFloat; TFloat], TFloat, false, true)
    
    
let lt =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false, true)
    
let gt =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false, true)
    
let lte =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false, true)
    
let gte =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false, true)
    
let andF =
    TFunction([TBool; TBool], TBool, false, true)
    
let orF =
    TFunction([TBool; TBool], TBool, false, true)
    
let notF =
    TFunction([TBool], TBool, false, true)
    
let neg =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain], constrain, true, true)
    
let unneg =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain], constrain, true, true)
    
let crossProduct =
    let tensorTypeVar = TTypeVariable (freshTypeVar())
    
    TFunction([TTensor(tensorTypeVar, Dims [ 3 ]); TTensor(tensorTypeVar, Dims [ 3 ])], TTensor(tensorTypeVar, Dims[ 3 ]), false, true)
    
let dotProduct =
    let tensorTypeVar = freshTypeVar()
    let dimsVar = DVar (freshTypeVar())
    let constrain = TConstrain(tensorTypeVar, _.IsArithmetic)
    
    TFunction([TTensor(constrain, dimsVar); TTensor(constrain, dimsVar)], constrain, false, true)
        
let mapType =
    let listTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    let accTyp = TTypeVariable (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar); TFunction([listTyp], accTyp, false, false)], TTensor(accTyp, dimsVar), false, true)

let lenType =
    let listTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar)], TInteger, false, true)

let listCast =
    let listType = TTypeVariable (freshTypeVar())
    
    TFunction([listType], TTensor(listType, Dims [ 1 ]), false, true)
    
let tupleCast =
    let listType = TTypeVariable (freshTypeVar())
    
    TFunction([listType], TTuple([listType]), false, true)

let castType =
    let typ = TTypeVariable (freshTypeVar ())
    
    TFunction([TAny; typ], typ, false, true)

let newtonRaphsonType =
    let funcT1 = TConstrain(freshTypeVar(), fun typ -> typ.IsPure && typ.NumArgsIs 1)
    let funcT2 = TConstrain(freshTypeVar(), fun typ -> typ.IsPure && typ.NumArgsIs 1)
    
    TFunction([funcT1; funcT2; TFloat; TFloat; TInteger], TFloat, false, true)

let bisectionTyp =
    let funcT = TConstrain(freshTypeVar(), fun typ -> typ.IsPure && typ.NumArgsIs 1)
    
    TFunction([funcT; TFloat; TFloat; TFloat; TInteger], TFloat, false, true)

let differentiateType =
    let funcT = TConstrain(freshTypeVar(), fun typ -> typ.IsPure && typ.NumArgsIs 1) 
    let retT = TFunction([TFloat], TFloat, true, false)
    
    TFunction([funcT], retT, false, true)

let plotFunsType =
    let funcT = TConstrain(freshTypeVar(), fun typ -> typ.IsPure && typ.NumArgsIs 1)
    
    TFunction([TString; TTensor(funcT, DAny);], TUnit, false, true)

let BuiltinFunctions: Map<BuiltInFunction, TType> =
    [ Print, TFunction([ TAny ], TUnit, false, true)
      Input, TFunction([], TString, false, true)
      Cos, TFunction([ TFloat ], TFloat, true, true)
      Sin, TFunction([ TFloat ], TFloat, true, true)
      Tan, TFunction([ TFloat ], TFloat, true, true)
      ACos, TFunction([ TFloat ], TFloat, true, true)
      ASin, TFunction([ TFloat ], TFloat, true, true)
      ATan, TFunction([ TFloat ], TFloat, true, true)
      Log, logType
      Exp, expType
        
      Len, lenType
      Env, TFunction([], TUnit, false, true) 
      Exit, TFunction([], TUnit, false, true)
      Sqrt, TFunction([ TFloat ], TFloat, true, true)
      Abs, TFunction([ TFloat ], TFloat, true, true)
      Floor, TFunction([ TFloat ], TFloat, true, true)
      Ceil, TFunction([ TFloat ], TFloat, true, true)
      Trunc, TFunction([TFloat], TFloat, true, true)
      Fold, foldType
      Map, mapType
      Plot, plotType
      PlotFunction, plotFunType
      PlotFunctions, plotFunsType
      
      NewtonRaphson, newtonRaphsonType
      Bisection, bisectionTyp
      
      Differentiate, differentiateType
      
      Cons, consType
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
      
      CrossProduct, crossProduct
      DotProduct, dotProduct
      
      BInt, TFunction([ TAny ], TInteger, false, true)
      BFloat, TFunction([ TAny ], TFloat, false, true)
      BComplex, TFunction([ TAny ], TComplex, false, true)
      BRational, TFunction([TAny], TRational, false, true)
      BString, TFunction([TAny], TString, false, true)
      BBool, TFunction([TAny], TBool, false, true)
      BList, listCast
      BTuple, tupleCast
      
      Cast, castType
      
      ]
    |> Map.ofList

let BuiltinConstants: Map<Lexeme, TType> =
    [ Identifier "PI", TFloat 
      Identifier "E", TFloat
      Identifier "TAU", TFloat
      ]
    |> Map.ofList
