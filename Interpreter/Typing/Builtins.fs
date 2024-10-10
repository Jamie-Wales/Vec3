module Vec3.Interpreter.Typing.Builtins

open Vec3.Interpreter.Grammar


let BuiltinFunctions: Map<string, TType> =
    [ "print", TFunction([ TAny ], TUnit)
      "input", TFunction([], TString)
      "cos", TFunction([ TFloat ], TFloat)
      "sin", TFunction([ TFloat ], TFloat)
      "tan", TFunction([ TFloat ], TFloat)
      "env", TFunction([], TUnit) ]
    |> Map.ofList
