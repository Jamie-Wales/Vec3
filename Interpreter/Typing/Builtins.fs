module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

let foldType =
    let listTyp = TTypeVariable (freshTypeVar())
    let accTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar); accTyp; TFunction([listTyp; accTyp], accTyp)], accTyp)
    
let consType =
    let listTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    
    TFunction([listTyp; TTensor(listTyp, DAny)], TTensor(listTyp, DAny))
    
let plotType =
    let dimsVar = DVar (freshTypeVar ())
    TFunction([TString; TTensor(TInteger, dimsVar); TTensor(TInteger, dimsVar)], TUnit)

let plus =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain)
    
let minus =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain)
    
let mul =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain)
    
let div =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain)
    
let pow =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], constrain)
    
let modF =
    TFunction([TInteger; TInteger], TInteger)
    
let eq =
    TFunction([TAny; TAny], TBool)
    
let lt =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool)
    
let gt =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool)
    
let lte =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool)
    
let gte =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain; constrain], TBool)
    
let andF =
    TFunction([TBool; TBool], TBool)
    
let orF =
    TFunction([TBool; TBool], TBool)
    
let notF =
    TFunction([TBool], TBool)
    
let neg =
    let typeVar = freshTypeVar()
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain], constrain)
    
let unneg =
    let typeVar = (freshTypeVar())
    let constrain = TConstrain(typeVar, _.IsArithmetic)
    
    TFunction([constrain], constrain)
    

let BuiltinFunctions: Map<BuiltInFunction, TType> =
    [ Print, TFunction([ TAny ], TUnit)
      Input, TFunction([], TString)
      Cos, TFunction([ TFloat ], TFloat)
      Sin, TFunction([ TFloat ], TFloat)
      Tan, TFunction([ TFloat ], TFloat)
      Env, TFunction([], TUnit) 
      Exit, TFunction([], TUnit)
      Sqrt, TFunction([ TFloat ], TFloat)
      Abs, TFunction([ TFloat ], TFloat)
      Floor, TFunction([ TFloat ], TFloat)
      Ceil, TFunction([ TFloat ], TFloat)
      Fold, foldType
      Plot, plotType
      
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
      
      CrossProduct, TNever
      DotProduct, TNever // TODO
        
      ]
    |> Map.ofList
