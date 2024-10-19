module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

let foldType =
    let listTyp = TTypeVariable (freshTypeVar())
    let accTyp = TTypeVariable (freshTypeVar())
    let dimsVar = DVar (freshTypeVar())
    
    TFunction([TTensor(listTyp, dimsVar); accTyp; TFunction([listTyp; accTyp], accTyp)], accTyp)
    
    

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
      Fold, foldType
      ]
    |> Map.ofList
