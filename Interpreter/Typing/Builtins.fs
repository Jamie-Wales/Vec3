module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Vec3.Interpreter.Typing.Types

    
        

let foldType =
    let listTyp = TTypeVariable (freshTypeVar())
    let accTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar); accTyp; TFunction([listTyp; accTyp], accTyp, false)], accTyp, false)
    
let consType =
    let listTyp = TTypeVariable (freshTypeVar())
    let dimsVar1 = DVar (freshTypeVar())
    let dimsVar2 = DVar (freshTypeVar())
    
    TFunction([listTyp; TTensor(listTyp, dimsVar1)], TTensor(listTyp, dimsVar2), false)
    
let plotType =
    let dimsVar = DVar (freshTypeVar ())
    let constrain = TConstrain(freshTypeVar(), _.IsArithmetic)
    
    TFunction([TString; TTensor(constrain, dimsVar); TTensor(constrain, dimsVar)], TUnit, false)

let plotFunType =
    let funConstrain = TConstrain(freshTypeVar(), _.IsPure)
    
    TFunction([TString; funConstrain], TUnit, false)

let plus =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true)
    
let minus =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true)
    
let mul =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true)
    
let div =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true)
    
let pow =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain, true)
    
let modF =
    TFunction([TInteger; TInteger], TInteger, true)
    
let eq =
    TFunction([TAny; TAny], TBool, false)
    
let expType =
    TFunction([TFloat], TFloat, true)
    
let logType =
    TFunction([TFloat; TFloat], TFloat, true)
    
let log10Type =
    TFunction([TFloat], TFloat, true)
    
let lt =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false)
    
let gt =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false)
    
let lte =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false)
    
let gte =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool, false)
    
let andF =
    TFunction([TBool; TBool], TBool, false)
    
let orF =
    TFunction([TBool; TBool], TBool, false)
    
let notF =
    TFunction([TBool], TBool, false)
    
let neg =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain], constrain, true)
    
let unneg =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain], constrain, true)
    
let crossProduct =
    let tensorTypeVar = TTypeVariable (freshTypeVar())
    
    TFunction([TTensor(tensorTypeVar, Dims [ 3 ]); TTensor(tensorTypeVar, Dims [ 3 ])], TTensor(tensorTypeVar, Dims[ 3 ]), false)
    
let dotProduct =
    let tensorTypeVar = freshTypeVar()
    let dimsVar = DVar (freshTypeVar())
    let constrain = TConstrain(tensorTypeVar, _.IsArithmetic)
    
    TFunction([TTensor(constrain, dimsVar); TTensor(constrain, dimsVar)], constrain, false)
        
let mapType =
    let listTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    let accTyp = TTypeVariable (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar); TFunction([listTyp], accTyp, false)], TTensor(accTyp, dimsVar), false)

let lenType =
    let listTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar)], TInteger, false)

let BuiltinFunctions: Map<BuiltInFunction, TType> =
    [ Print, TFunction([ TAny ], TUnit, false)
      Input, TFunction([], TString, false)
      Cos, TFunction([ TFloat ], TFloat, true)
      Sin, TFunction([ TFloat ], TFloat, true)
      Tan, TFunction([ TFloat ], TFloat, true)
      ACos, TFunction([ TFloat ], TFloat, true)
      ASin, TFunction([ TFloat ], TFloat, true)
      ATan, TFunction([ TFloat ], TFloat, true)
      Log, logType
      Exp, expType
      Log10, log10Type
        
      Len, lenType
      Env, TFunction([], TUnit, false) 
      Exit, TFunction([], TUnit, false)
      Sqrt, TFunction([ TFloat ], TFloat, true)
      Abs, TFunction([ TFloat ], TFloat, true)
      Floor, TFunction([ TFloat ], TFloat, true)
      Ceil, TFunction([ TFloat ], TFloat, true)
      Fold, foldType
      Map, mapType
      Plot, plotType
      PlotFunction, plotFunType
      
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
      
      BInt, TFunction([ TAny ], TInteger, false)
      BFloat, TFunction([ TAny ], TFloat, false)
      BComplex, TFunction([ TAny ], TComplex, false)
      BRational, TFunction([TAny], TRational, false)
      BString, TFunction([TAny], TString, false)
      BBool, TFunction([TAny], TBool, false)
      ]
    |> Map.ofList

let BuiltinConstants: Map<Lexeme, TType> =
    [ Identifier "PI", TFloat 
      Identifier "E", TFloat ]
    |> Map.ofList
