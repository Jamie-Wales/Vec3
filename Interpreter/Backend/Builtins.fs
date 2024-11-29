/// <summary>
/// Built-in functions for the VM.
/// </summary>

module Vec3.Interpreter.Backend.Builtins

open Microsoft.FSharp.Control
open Vec3.Interpreter.Grammar
open Vec3.Interpreter
open System
open Vec3.Interpreter.Token
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Value

/// <summary>
/// For each new draw shape, we need a unique id.
/// </summary>
let getNewDrawId =
    let id = ref 0
    fun () ->
        id.Value <- id.Value + 1
        id.Value

/// <summary>
/// Parse a plot type from a string.
/// </summary>
let parsePlotType =
    function
    | "scatter" ->
        printf "Parsing Scatter"
        Scatter
    | "line" ->
        printf "Parsing line"
        Line
    | "bar" ->
        printf "Parsing Bar"
        Bar
    | name -> raise <| InvalidProgramException $"Unknown plot type: {name}"


let builtins =
    [ Identifier "plot",
      VBuiltin(
          (fun args ->
              match args with
              | [ VList(config, _) ] ->
                  let findField fieldName defaultValue =
                      config
                      |> List.tryFind (function
                          | VList([ VString k; _ ], _) when k = fieldName -> true
                          | _ -> false)
                      |> function
                          | Some(VList([ _; v ], _)) -> v
                          | _ -> defaultValue

                  let title =
                      match findField "title" (VString "Plot") with
                      | VString t -> t
                      | _ -> raise <| InvalidProgramException "title must be a string"

                  let xs =
                      match findField "x" (VList([], LIST)) with
                      | VList(xs, _) -> xs
                      | _ -> raise <| InvalidProgramException $"x must be a list, got {args}"

                  let ys =
                      match findField "y" (VList([], LIST)) with
                      | VList(ys, _) -> ys
                      | _ -> raise <| InvalidProgramException $"y must be a list, got {args}"

                  let plotType =
                      match findField "ptype" (VString "scatter") with
                      | VString t -> parsePlotType (t.ToLowerInvariant())
                      | _ -> raise <| InvalidProgramException "ptype must be a string"

                  VPlotData(title, xs, ys, plotType)


              | _ -> raise <| InvalidProgramException "plot expects a list of configuration options"),
          "Plot"
      )
      Identifier "plotFunc",
      VBuiltin(
          (fun args ->
              match args with
              | [ VString title; VFunction(_, Some f) ] ->
                  let builtin = SymbolicExpression.toBuiltin f

                  VPlotFunction(title, builtin)
              | _ -> raise <| InvalidProgramException "plotFunc expects a title and a function"),
          "PlotFunc"
      )
      Identifier "plotFuncs",
      VBuiltin(
          (fun args ->
              match args with
              | [ VString title; VList(funcs, _) ] ->
                  let funcs =
                      List.map
                          (function
                          | VFunction(_, Some f) -> SymbolicExpression.toBuiltin f
                          | _ -> raise <| InvalidProgramException "plotFuncs expects a list of functions")
                          funcs

                  VPlotFunctions(title, funcs)
              | _ ->
                  raise
                  <| InvalidProgramException "plotFuncs expects a title and a list of functions"),
          "PlotFuncs"
      )

      Identifier "read",
      VBuiltin(
          (fun args ->
              match args with
              | [ VString s ] ->
                  let parsed = Parser.parse s

                  match parsed with
                  | Ok(_, ast) ->
                      let ast = EBlock(ast, None)
                      VBlock ast
                  | _ -> VNil
              | _ -> raise <| InvalidProgramException "Read accepts a string"),
          "Read"
      )

      Identifier "print",
      VBuiltin((fun args -> VOutput $"""{String.concat " " (List.map valueToString args)}"""), "Print")
      
      Identifier "BUILTIN_ROOT",
      VBuiltin(
          (fun args ->
            match args with
            | [VNumber(f); VNumber(VInteger 2)] ->
                VNumber(VFloat(Math.Sqrt(floatValue f)))
            | [VNumber(f); VNumber(r)] ->
                VNumber(VFloat(Math.Pow(floatValue f, 1.0 / floatValue r)))
            | _ -> raise <| InvalidProgramException "root expects a float and an int"
          ), "Root"
          )
      
      Identifier "BUILTIN_ABS",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(abs f))
              | [ VNumber(VInteger i) ] -> VNumber(VInteger(abs i))
              | [ VNumber(VComplex(r, i)) ] -> VNumber(VFloat(sqrt (r * r + i * i)))
              | [ VNumber(VRational(n, d)) ] -> VNumber(VRational(abs n, abs d))
              | _ -> raise <| InvalidProgramException "abs expects a number"),
          "Abs"
      )
      Identifier "BUILTIN_FLOOR",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(floor f))
              | [ VNumber(VInteger i) ] -> VNumber(VInteger(i))
              | _ -> raise <| InvalidProgramException "floor expects a float"),
          "Floor"
      )
      Identifier "BUILTIN_COS",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(cos f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(cos (float i)))
              | _ -> raise <| InvalidProgramException "cos expects a float"),
          "Cos"
      )
      Identifier "BUILTIN_SIN",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(sin f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(sin (float i)))
              | _ -> raise <| InvalidProgramException "sin expects a float"),
          "Sin"
      )
      Identifier "BUILTIN_TAN",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(tan f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(tan (float i)))
              | _ -> raise <| InvalidProgramException "tan expects a float"),
          "Tan"
      )
      Identifier "BUILTIN_ACOS",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(acos f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(acos (float i)))
              | _ -> raise <| InvalidProgramException "acos expects a float"),
          "Acos"
      )

      Identifier "BUILTIN_ATAN",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(atan f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(atan (float i)))
              | _ -> raise <| InvalidProgramException "atan expects a float"),
          "Atan"
      )

      Identifier "BUILTIN_ASIN",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(asin f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(asin (float i)))
              | _ -> raise <| InvalidProgramException "asin expects a float"),
          "Asin"
      )

      Identifier "BUILTIN_EXP",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(exp f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(exp (float i)))
              | _ -> raise <| InvalidProgramException "exp expects a float"),
          "Exp"
      )

      Identifier "BUILTIN_LOG",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f); VNumber(VFloat f1) ] -> VNumber(VFloat((log f1) / (log f)))
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(log f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(log (float i)))
              | _ -> raise <| InvalidProgramException "log expects two floats"),
          "Log"
      )

      Identifier "BUILTIN_LOG10",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat f) ] -> VNumber(VFloat(log10 f))
              | [ VNumber(VInteger i) ] -> VNumber(VFloat(log10 (float i)))
              | _ -> raise <| InvalidProgramException "log10 expects a float"),
          "Log10"
      )

      Identifier "input",
      VBuiltin(
          (fun _ ->
              let input = Console.ReadLine()
              VString input),
          "Input"
      )
      Identifier "cons",
      VBuiltin(
          (fun args ->
              match args with
              | [ value; VList(l, t) ] -> VList(value :: l, t)
              | _ -> raise <| InvalidProgramException "cons expects a value and a list"),
          "Cons"
      )
      Identifier "exit",
      VBuiltin(
          (fun _ ->
              Environment.Exit(0)
              VNil),
          "Exit"
      )
      
      Identifier "append",
        VBuiltin(
            (fun args ->
                match args with
                | [ VList(l1, t1); VList(l2, t2) ] -> VList(l1 @ l2, t1)
                | _ -> raise <| InvalidProgramException "append expects two lists"),
            "Append"
        )
      
      Identifier "dotProduct",
      VBuiltin(
          (fun args ->
              match args with
              | [ VList(l1', _) as l1; VList(l2', _) as l2 ] when List.length l1' = List.length l2' ->
                  dotProduct l1 l2
              | _ ->
                  raise
                  <| InvalidProgramException "dotProduct expects two lists of the same length"),
          "DotProduct"
      )
      Identifier "crossProduct",
      VBuiltin(
          (fun args ->
              match args with
              | [ VList(l1', _) as l1; VList(l2', _) as l2 ] when List.length l1' = 3 && List.length l2' = 3 ->
                  crossProduct l1 l2
              | _ -> raise <| InvalidProgramException "crossProduct expects two lists of length 3"),
          "CrossProduct"
      )

      Identifier "BUILTIN_TRUNC",
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VFloat num) ] -> VNumber(VFloat(truncate num))
              | [ VNumber(VInteger num) ] -> VNumber(VFloat(truncate num))
              | _ -> raise <| InvalidProgramException "truncate expects a float"),
          "Truncate"
      )

      Identifier "PI", VNumber(VFloat(Math.PI))

      Identifier "E", VNumber(VFloat(Math.E))

      Identifier "TAU", VNumber(VFloat(Math.Tau))

      Identifier "cast",
      VBuiltin(
          (fun args ->
              let org = args.Head
              let castTyp = List.item 1 args

              cast org castTyp),
          "Cast"
      )

      Identifier "newtonRaphson",
      VBuiltin(
          (fun args ->
              match args with
              | [ VFunction(_, Some f1)
                  VFunction(_, Some f2)
                  VNumber(VFloat init)
                  VNumber(VFloat tol)
                  VNumber(VInteger it) ] ->
                  let builtin1 = SymbolicExpression.toBuiltin f1
                  let builtin2 = SymbolicExpression.toBuiltin f2

                  let res = newtonRaphson builtin1 builtin2 init tol it
                  VNumber(VFloat(res))
              | [ VFunction(_, Some f1)
                  VFunction(_, Some f2)
                  VNumber(VInteger init)
                  VNumber(VFloat tol)
                  VNumber(VInteger it) ] ->
                  let builtin1 = SymbolicExpression.toBuiltin f1
                  let builtin2 = SymbolicExpression.toBuiltin f2

                  let res = newtonRaphson builtin1 builtin2 init tol it
                  VNumber(VFloat(res))
              | [ VFunction(_, Some f1)
                  VFunction(_, Some f2)
                  VNumber(VFloat init)
                  VNumber(VInteger tol)
                  VNumber(VInteger it) ] ->
                  let builtin1 = SymbolicExpression.toBuiltin f1
                  let builtin2 = SymbolicExpression.toBuiltin f2

                  let res = newtonRaphson builtin1 builtin2 init tol it
                  VNumber(VFloat(res))
              | [ VFunction(_, Some f1)
                  VFunction(_, Some f2)
                  VNumber(VInteger init)
                  VNumber(VInteger tol)
                  VNumber(VInteger it) ] ->
                  let builtin1 = SymbolicExpression.toBuiltin f1
                  let builtin2 = SymbolicExpression.toBuiltin f2

                  let res = newtonRaphson builtin1 builtin2 init tol it
                  VNumber(VFloat(res))
              | _ ->
                  raise
                  <| InvalidProgramException
                      "newtonRaphson expects two functions, an initial guess, a tolerance, and a maximum number of iterations"),
          "NewtonRaphson"
      )

      Identifier "bisection",
      VBuiltin(
          (fun args ->
              match args with
              | [ VFunction(_, Some f)
                  VNumber(a)
                  VNumber(b)
                  VNumber(tol)
                  VNumber(VInteger it) ] ->
                  let a = floatValue a
                  let b = floatValue b
                  let tol = floatValue tol
                  
                  let builtin = SymbolicExpression.toBuiltin f
                  let res = bisection builtin a b tol it
                  VNumber(VFloat(res))
              | _ ->
                  raise
                  <| InvalidProgramException
                      "bisection expects a function, a lower bound, an upper bound, a tolerance, and a maximum number of iterations"),
          "Bisection"
      )

      Identifier "assert", // could do this in language
      VBuiltin(
          (fun args ->
              match args with
              | [ msg; cond ] ->
                  if not (isTruthy cond) then
                      raise <| InvalidProgramException $"Assertion failed: {valueToString msg}"
                  else
                      VNil
              | [ cond ] ->
                  if not (isTruthy cond) then
                      raise <| InvalidProgramException $"Assertion failed: {valueToString cond}"
                  else
                      VNil
              | _ -> raise <| InvalidProgramException "assert expects a condition"),
          "Assert"
      )

      Identifier "draw",
      VBuiltin(
          (fun args ->
              match args with
              | [ VList(elems, RECORD) ] ->
                  let width =
                      elems
                      |> List.tryFind (function
                          | VList([ VString "width"; VNumber(VFloat _) ], _) -> true
                          | _ -> false)

                  let width =
                      match width with
                      | Some(VList([ VString "width"; VNumber(VFloat w) ], _)) -> w
                      | _ -> raise <| InvalidProgramException "draw expects a width"

                  let height =
                      elems
                      |> List.tryFind (function
                          | VList([ VString "height"; VNumber(VFloat _) ], _) -> true
                          | _ -> false)

                  let height =
                      match height with
                      | Some(VList([ VString "height"; VNumber(VFloat h) ], _)) -> h
                      | _ -> raise <| InvalidProgramException "draw expects a height"

                  let x =
                      elems
                      |> List.tryFind (function
                          | VList([ VString "x"; VNumber(VFloat _) ], _) -> true
                          | _ -> false)

                  let x =
                      match x with
                      | Some(VList([ VString "x"; VNumber(VFloat x) ], _)) -> x
                      | _ -> raise <| InvalidProgramException "draw expects an x"

                  let y =
                      elems
                      |> List.tryFind (function
                          | VList([ VString "y"; VNumber(VFloat _) ], _) -> true
                          | _ -> false)

                  let y =
                      match y with
                      | Some(VList([ VString "y"; VNumber(VFloat y) ], _)) -> y
                      | _ -> raise <| InvalidProgramException "draw expects a y"

                  let colour =
                      elems
                      |> List.tryFind (function
                          | VList([ VString "colour"; VString _ ], _) -> true
                          | _ -> false)

                  let colour =
                      match colour with
                      | Some(VList([ VString "colour"; VString c ], _)) -> c
                      | _ -> raise <| InvalidProgramException "draw expects a colour"

                  let typ =
                      elems
                      |> List.tryFind (function
                          | VList([ VString "shape"; VString _ ], _) -> true
                          | _ -> false)

                  let typ =
                      match typ with
                      | Some(VList([ VString "shape"; VString c ], _)) -> c
                      | _ -> "circle"
                  
                  let trace =
                      elems
                      |> List.tryFind (function
                          | VList([ VString "trace"; VBoolean _ ], _) -> true
                          | _ -> false)
                  
                  let trace =
                      match trace with
                      | Some(VList([ VString "trace"; VBoolean t ], _)) -> t
                      | _ -> false

                  VShape(width, height, x, y, colour, typ, getNewDrawId(), trace)
              | [ VList(elems, LIST) ] ->
                  let res =
                      elems
                      |> List.map (fun elems ->
                          match elems with
                          | VList(elems, RECORD) ->

                              let width =
                                  elems
                                  |> List.tryFind (function
                                      | VList([ VString "width"; VNumber(VFloat _) ], _) -> true
                                      | _ -> false)

                              let width =
                                  match width with
                                  | Some(VList([ VString "width"; VNumber(VFloat w) ], _)) -> w
                                  | _ -> raise <| InvalidProgramException "draw expects a width"

                              let height =
                                  elems
                                  |> List.tryFind (function
                                      | VList([ VString "height"; VNumber(VFloat _) ], _) -> true
                                      | _ -> false)

                              let height =
                                  match height with
                                  | Some(VList([ VString "height"; VNumber(VFloat h) ], _)) -> h
                                  | _ -> raise <| InvalidProgramException "draw expects a height"

                              let x =
                                  elems
                                  |> List.tryFind (function
                                      | VList([ VString "x"; VNumber(VFloat _) ], _) -> true
                                      | _ -> false)

                              let x =
                                  match x with
                                  | Some(VList([ VString "x"; VNumber(VFloat x) ], _)) -> x
                                  | _ -> raise <| InvalidProgramException "draw expects an x"



                              let y =
                                  elems
                                  |> List.tryFind (function
                                      | VList([ VString "y"; VNumber(VFloat _) ], _) -> true
                                      | _ -> false)

                              let y =
                                  match y with
                                  | Some(VList([ VString "y"; VNumber(VFloat y) ], _)) -> y
                                  | _ -> raise <| InvalidProgramException "draw expects a y"

                              let colour =
                                  elems
                                  |> List.tryFind (function
                                      | VList([ VString "colour"; VString _ ], _) -> true
                                      | _ -> false)

                              let colour =
                                  match colour with
                                  | Some(VList([ VString "colour"; VString c ], _)) -> c
                                  | _ -> raise <| InvalidProgramException "draw expects a colour"

                              let typ =
                                  elems
                                  |> List.tryFind (function
                                      | VList([ VString "shape"; VString _ ], _) -> true
                                      | _ -> false)

                              let typ =
                                  match typ with
                                  | Some(VList([ VString "shape"; VString c ], _)) -> c
                                  | _ -> "circle"

                              (width, height, x, y, colour, typ)
                          | _ -> raise <| InvalidProgramException("draw expects a list of records"))

                  VShapes (res, getNewDrawId())
              | _ -> raise <| InvalidProgramException "draw expects a title and a list of functions"),
          "Draw"
      )

      Operator(Plus, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> add a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for +"),
          "Add"
      )

      Operator(Minus, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> subtract a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for -"),
          "Subtract"
      )

      Operator(Star, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> multiply a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for *"),
          "Multiply"
      )
      
      Operator(PlusPlus, Some Infix),
        VBuiltin(
            (fun args ->
                match args with
                | [ VString a; VString b ] -> VString(a + b)
                | [ VList(a, _); VList(b, _) ] -> VList(a @ b, LIST)
                | _ -> raise <| InvalidProgramException "Expected two strings for ++"),
            "Concat"
        )

      Operator(Slash, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> divide a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for /"),
          "Divide"
      )

      Operator(Percent, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ VNumber(VInteger a); VNumber(VInteger b) ] -> VNumber(VInteger(a % b))
              | [ VNumber(a); VNumber(b) ] -> VNumber(VFloat(floatValue a % floatValue b))
              | _ -> raise <| InvalidProgramException "Expected two integers or floats for %"),
          "Mod"
      )

      Operator(StarStar, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> power a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for **"),
          "Power"
      )

      Operator(EqualEqual, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> valuesEqual a b |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for =="),
          "Equal"
      )

      Operator(BangEqual, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> valuesEqual a b |> not |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for !="),
          "NotEqual"
      )

      Operator(Less, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> compare a b < 0 |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for <"),
          "Less"
      )

      Operator(LessEqual, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> compare a b <= 0 |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for <="),
          "LessEqual"
      )

      Operator(Greater, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> compare a b > 0 |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for >"),
          "Greater"
      )

      Operator(GreaterEqual, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> compare a b >= 0 |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for >="),
          "GreaterEqual"
      )

      Operator(AmpersandAmpersand, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> (isTruthy a && isTruthy b) |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for &&"),
          "And"
      )

      Operator(PipePipe, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> (isTruthy a || isTruthy b) |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected two arguments for ||"),
          "Or"
      )

      Operator(Bang, Some Prefix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a ] -> not (isTruthy a) |> VBoolean
              | _ -> raise <| InvalidProgramException "Expected one argument for !"),
          "Not"
      )

      Operator(Minus, Some Prefix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a ] -> negate a
              | _ -> raise <| InvalidProgramException "Expected one argument for -"),
          "Negate"
      )

      Operator(Plus, Some Prefix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a ] -> unnegate a
              | _ -> raise <| InvalidProgramException "Expected one argument for +"),
          "Unnegate"
      )

      Operator(Caret, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> power a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for ^"),
          "Power"
      )

      Operator(DotStar, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> dotProduct a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for .*"),
          "DotProduct"
      )

      Operator(Cross, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; b ] -> crossProduct a b
              | _ -> raise <| InvalidProgramException "Expected two arguments for cross"),
          "CrossProduct"
      )

      Operator(ColonColon, Some Infix),
      VBuiltin(
          (fun args ->
              match args with
              | [ a; VList(l, _) ] -> VList(a :: l, LIST)
              | _ -> raise <| InvalidProgramException "Expected a value and a list for ::"),
          "Cons"
      )

      // Identifier "tail",
      // VBuiltin(
      //     (fun args ->
      //         match args with
      //         | [ VList(l, _) ] ->
      //             let tail = List.tail l
      //             VList(tail, LIST)
      //         | _ -> raise <| InvalidProgramException "Expected a list for tail"),
      //     "Tail"
      // )

      Identifier "error",
      VBuiltin(
          (fun args ->
              match args with
              | [ VString s ] -> raise <| InvalidProgramException s
              | _ -> raise <| InvalidProgramException "Expected a string for error"),
          "Error"
      )

      Identifier "index",
      VBuiltin(
          (fun args ->
              match args with
              | [ VList(l, _); VNumber(VInteger i) ] -> List.item i l // is index
              | [ VList(l, _); VNumber(VInteger 0); VNumber(VInteger i) ] ->
                  let res = List.take i l in VList(res, LIST) // is range
              | [ VList(l, _); VNumber(VInteger i); VNumber(VInteger 0) ] ->
                  let res = List.skip i l in VList(res, LIST) // is range
              | [ VList(l, _); VNumber(VInteger i); VNumber(VInteger j) ] ->
                  let res = List.skip i l |> List.take (j - i) in VList(res, LIST) // is range
              | _ -> raise <| InvalidProgramException $"Expected a list and an integer for index, {args}"),
          "Index"
      )

      Identifier "select",
      VBuiltin(
          (fun args ->
              match args with
              | [ VList(l, _); VString s ] ->
                  let res =
                      l
                      |> List.tryFind (function
                          | VList([ VString k; v ], _) when k = s -> true
                          | _ -> false)

                  match res with
                  | Some(VList([ _; v ], _)) -> v
                  | _ -> raise <| InvalidProgramException "Key not found"
              | _ -> raise <| InvalidProgramException "Expected a list and a string for select"),
          "Select"
      )
      Identifier "match",
        VBuiltin((fun args ->
            match args with
            | [ pattern; expr ] ->
                match pattern, expr with
                | VBoolean true, _ -> VBoolean true
                | VBoolean false, _ -> VBoolean false
                | p, x -> valuesEqual p x |> VBoolean
            | _ -> raise <| InvalidProgramException "Expected a pattern and an expression for match"), "Match")
      Identifier "on",
        VBuiltin((fun args ->
            match args with
            | [ VList([VList([VString "id"; VNumber(VInteger shapeId)], _)], RECORD); VList([VList([VString "event"; VNumber(VInteger eventId)], _)], RECORD); VFunction(func, _) ] ->
                VEventListener(shapeId, eventId, func)
            | _ -> raise <| InvalidProgramException $"Expected a shape id, an event id, and a function for on {args}"),
             "On")
      
      Identifier "await",
        VBuiltin((fun args ->
            match args with
            | [ VPromise(task) ] ->
                Async.RunSynchronously task
            | _ -> failwith "not a promise"
            ), "Async")
      ]


    |> List.map (fun (key, value) -> lexemeToString key, value)
    |> Map.ofList
