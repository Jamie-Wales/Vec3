module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar

type TType = Vec3.Interpreter.Grammar.Type

let BuiltinFunctions: Map<string, TType> =
    [ "print", TFunction([ TAny ], TUnit)
      "input", TFunction([], TString)
      "cos", TFunction([ TFloat ], TFloat)
      "sin", TFunction([ TFloat ], TFloat)
      "tan", TFunction([ TFloat ], TFloat) ]
    |> Map.ofList
