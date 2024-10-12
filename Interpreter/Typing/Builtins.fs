module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token


let BuiltinFunctions: Map<BuiltInFunction, TType> =
    [ Print, TFunction([ TAny ], TUnit)
      Input, TFunction([], TString)
      Cos, TFunction([ TFloat ], TFloat)
      Sin, TFunction([ TFloat ], TFloat)
      Tan, TFunction([ TFloat ], TFloat)
      Env, TFunction([], TUnit) 
      Exit, TFunction([], TUnit) ]
    |> Map.ofList
