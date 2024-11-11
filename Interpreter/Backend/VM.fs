module Vec3.Interpreter.Backend.VM

open System
open Microsoft.FSharp.Collections
open Vec3.Interpreter
open Vec3.Interpreter.Backend.Instructions
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Token
open Grammar

let loadFunction (vm: VM) (func: Function) : VM =
    let frame = {
        Function = func
        IP = 0
        StackBase = vm.Stack.Count
        Locals = [||]
    }
    vm.Frames.Add(frame)
    vm

let createOutputStreams () =
    { ConstantPool = Seq.empty
      Disassembly = Seq.empty
      Execution = Seq.empty
      StandardOutput = Seq.empty
      Globals = Seq.empty }

let getCurrentFrame (vm: VM) = vm.Frames[vm.Frames.Count - 1]

let readByte (vm: VM) =
    let frame = getCurrentFrame vm
    let byte = frame.Function.Chunk.Code[frame.IP]
    let updatedFrame = { frame with IP = frame.IP + 1 }
    vm.Frames[vm.Frames.Count - 1] <- updatedFrame
    (vm, byte)

let readConstant (vm: VM) =
    let vm, byte = readByte vm
    let frame = getCurrentFrame vm

    if int byte >= frame.Function.Chunk.ConstantPool.Count then
        raise <| InvalidProgramException $"Constant index out of range: {byte} (pool size: {frame.Function.Chunk.ConstantPool.Count})"

    let constant = frame.Function.Chunk.ConstantPool[int byte]
    (constant, vm)

let readConstantLong (vm: VM) =
    let vm, byte1 = readByte vm
    let vm, byte2 = readByte vm
    let vm, byte3 = readByte vm
    let frame = getCurrentFrame vm
    let index = (int byte1) ||| ((int byte2) <<< 8) ||| ((int byte3) <<< 16)

    if index >= frame.Function.Chunk.ConstantPool.Count then
        raise <| InvalidProgramException $"Long constant index out of range: %d{index} (pool size: %d{frame.Function.Chunk.ConstantPool.Count})"

    let constant = frame.Function.Chunk.ConstantPool[index]
    (constant, vm)


let saveVMState (vm: VM) = vm.ExecutionHistory.Add(vm)

let appendToStream (stream: seq<string>) (str: string) = Seq.append stream [ str ]

let appendOutput (vm: VM) (streamType: StreamType) (str: string) =
    let updatedStreams =
        match streamType with
        | ConstantPool ->
            { vm.Streams with
                ConstantPool = appendToStream vm.Streams.ConstantPool str }
        | Disassembly ->
            { vm.Streams with
                Disassembly = appendToStream vm.Streams.Disassembly str }
        | Execution ->
            { vm.Streams with
                Execution = appendToStream vm.Streams.Execution str }
        | StandardOutput ->
            { vm.Streams with
                StandardOutput = appendToStream vm.Streams.StandardOutput str }
        | Globals ->
            { vm.Streams with
                Globals = appendToStream vm.Streams.Globals str }

    { vm with Streams = updatedStreams }

let push (vm: VM) (value: Value) =
    vm.Stack.Add(value)
    vm

let pop (vm: VM) =
    let value = vm.Stack[vm.Stack.Count - 1]
    vm.Stack.RemoveAt(vm.Stack.Count - 1)
    (value, vm)

let peek (vm: VM) offset = vm.Stack[vm.Stack.Count - 1 - offset]


let defineGlobal (vm: VM) (name: string) (value: Value) =
    let updatedGlobals = Map.add name value vm.Globals
    let updatedVM = { vm with Globals = updatedGlobals }
    let globalOutput = $"{name} = {valueToString value}"
    appendOutput updatedVM Globals globalOutput

let getGlobal (vm: VM) (name: string) =
    printfn $"Getting global: {name}"
    Map.iter (fun k v -> printfn $"Global: {k} = {valueToString v}") vm.Globals
    Map.tryFind name vm.Globals

let binaryOp (vm: VM) (op: Value -> Value -> Value) =
    let b, vm = pop vm
    let a, vm = pop vm
    let result = op a b
    push vm result

let parsePlotType = function
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
    
let rec createNewVM (mainFunc: Function) : VM =
    let constantPool =
        mainFunc.Chunk.ConstantPool
        |> Seq.indexed
        |> Seq.map (fun (i, value) -> $"[{i}] {valueToString value}")
    let disassembly =
        disassembleChunkToString mainFunc.Chunk mainFunc.Name
        |> fun s -> s.Split(Environment.NewLine) |> Seq.ofArray
    let vm =
        { Frames = ResizeArray<CallFrame>()
          Stack = ResizeArray<Value>(256)
          ScopeDepth = 0
          Globals = builtins()
          Streams =
            { ConstantPool = constantPool
              Disassembly = disassembly
              Execution = Seq.empty
              StandardOutput = Seq.empty
              Globals = Seq.empty }
          ExecutionHistory = ResizeArray<VM>()
          Plots = ResizeArray<Value>()
          Canvas = ResizeArray<Value>() 
          }  
    let mainFrame =
        { Function = mainFunc
          IP = 0
          StackBase = 0
          Locals = [||] }
    vm.Frames.Add(mainFrame)
    vm
    
and builtins () =
    [ Identifier "plot",
      VBuiltin(fun args vm ->
      match args with
      | [ VList(config, _) ] ->
          let findField fieldName defaultValue = 
              config 
              |> List.tryFind (function 
                  | VList([VString k; _], _) when k = fieldName -> true 
                  | _ -> false)
              |> function
                  | Some(VList([_; v], _)) -> v
                  | _ -> defaultValue

          let title = 
              match findField "title" (VString "Plot") with
              | VString t -> t
              | _-> raise <| InvalidProgramException "title must be a string"

          let xs = 
              match findField "x" (VList([], LIST)) with
              | VList(xs, _) -> xs
              | _ -> raise <| InvalidProgramException "x must be a list"

          let ys = 
              match findField "y" (VList([], LIST)) with
              | VList(ys, _) -> ys
              | _ -> raise <| InvalidProgramException "y must be a list"

          let plotType = 
              match findField "ptype" (VString "scatter") with
               | VString t -> parsePlotType(t.ToLowerInvariant())
                | _ -> raise <| InvalidProgramException "ptype must be a string"
          let plotData = VPlotData(title, xs, ys, plotType)
          vm.Plots.Add(plotData)
          push vm VNil

      | _ ->
          raise <| InvalidProgramException "plot expects a list of configuration options")
      Identifier "plotFunc",
      VBuiltin(fun args vm ->
          match args with
          | [ VString title; VFunction(_, Some f) ] ->
              let builtin = SymbolicExpression.toBuiltin f
              
              let plotData = VPlotFunction(title, builtin)
              vm.Plots.Add(plotData)  
              push vm VNil  
          | _ ->
              raise <| InvalidProgramException "plotFunc expects a title and a function")
      Identifier "plotFuncs",
        VBuiltin(fun args vm ->
        match args with
        | [ VString title; VList(funcs, _) ] ->
            let funcs = List.map (function
                | VFunction(_, Some f) -> SymbolicExpression.toBuiltin f
                | _ -> raise <| InvalidProgramException "plotFuncs expects a list of functions") funcs
            
            let plotData = VPlotFunctions(title, funcs)
            vm.Plots.Add(plotData)
            push vm VNil
        | _ ->
            raise <| InvalidProgramException "plotFuncs expects a title and a list of functions")
      
      Identifier "read",
      VBuiltin(fun args vm ->
          match args with
          | [ VString s ] ->
              let parsed = Parser.parse s
              match parsed with
              | Ok(_, ast) ->
                  let ast = EBlock(ast, None)
                  let value = VBlock ast
                  push vm value
              | _ ->
                  push vm VNil
          | _ -> raise <| InvalidProgramException ("Read accepts a string")
          )
      
      Identifier "print",
      VBuiltin(fun args vm ->
          let vm =
              appendOutput vm StandardOutput $"""{String.concat " " (List.map valueToString args)}"""

          push vm VNil)
      Identifier "BUILTIN_SQRT",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] ->
              let result = VNumber(VFloat(sqrt f))
              push vm result
          | _ -> raise <| InvalidProgramException "sqrt expects a float")
      Identifier "BUILTIN_ABS",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(abs f)))
          | [ VNumber(VInteger i) ] -> push vm (VNumber(VInteger(abs i)))
          | [ VNumber(VComplex(r, i)) ] -> push vm (VNumber(VFloat(sqrt(r * r + i * i))))
          | [ VNumber(VRational(n, d)) ] -> push vm (VNumber(VRational(abs n, abs d)))
          | _ -> raise <| InvalidProgramException "abs expects a number")
      Identifier "power",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat x); VNumber(VFloat y) ] ->
              let result = VNumber(VFloat(pown x (int y)))
              push vm result
          | [ VNumber(VFloat x); VNumber(VInteger y) ] ->
              let result = VNumber(VFloat(pown x y))
              push vm result
          | [ VNumber(VInteger x); VNumber(VFloat y) ] ->
              let result = VNumber(VFloat(pown x (int y)))
              push vm result
          | [ VNumber(VInteger x); VNumber(VInteger y) ] ->
              let result = VNumber(VInteger(pown x y))
              push vm result
          | _ -> raise <| InvalidProgramException "power expects two numbers")
      Identifier "BUILTIN_FLOOR",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(floor f)))
          | _ -> raise <| InvalidProgramException "floor expects a float")
      Identifier "BUILTIN_COS",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(cos f)))
          | _ -> raise <| InvalidProgramException "cos expects a float")
      Identifier "BUILTIN_SIN",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(sin f)))
          | _ -> raise <| InvalidProgramException "sin expects a float")
      Identifier "BUILTIN_TAN",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(tan f)))
          | _ -> raise <| InvalidProgramException "tan expects a float")
      Identifier "BUILTIN_ACOS",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(acos f)))
          | _ -> raise <| InvalidProgramException "acos expects a float")

      Identifier "BUILTIN_ATAN",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(atan f)))
          | _ -> raise <| InvalidProgramException "atan expects a float")

      Identifier "BUILTIN_ASIN",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(asin f)))
          | _ -> raise <| InvalidProgramException "asin expects a float")

      Identifier "BUILTIN_EXP",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(exp f)))
          | _ -> raise <| InvalidProgramException "exp expects a float")

      Identifier "BUILTIN_LOG",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f); VNumber(VFloat f1) ] -> push vm (VNumber(VFloat((log f1) / (log f))))
          | _ -> raise <| InvalidProgramException "log expects two floats")

      Identifier "BUILTIN_LOG10",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat f) ] -> push vm (VNumber(VFloat(log10 f)))
          | _ -> raise <| InvalidProgramException "log10 expects a float")

      Identifier "env",
      VBuiltin(fun _ vm ->
          let globals =
              vm.Globals
              |> Map.map (fun k v -> $"{k} = {valueToString v}")
              |> Map.toSeq
              |> Seq.map snd
              |> Seq.toList

          let globalsString = String.concat Environment.NewLine globals
          printfn $"Globals: {globalsString}"
          push vm (VString globalsString))
      Identifier "input",
      VBuiltin(fun _ vm ->
          let input = Console.ReadLine()
          push vm (VString input))
      Identifier "cons",
      VBuiltin(fun args vm ->
          match args with
          | [ value; VList(l, t) ] ->
              let list = VList(value :: l, t)
              push vm list
          | _ ->
              raise <| InvalidProgramException "cons expects a value and a list")
      Identifier "exit",
      VBuiltin(fun _ vm ->
          Environment.Exit(0)
          vm)
      Identifier "fold",
      VBuiltin(fun args vm ->
          match args with
          | [ VList(l, _); acc; VFunction(f, None) ] ->
              let rec fold acc vm =
                  function
                  | [] -> acc, vm
                  | x :: xs ->

                      let vm = push vm x
                      let vm = push vm acc

                      let frame =
                          { Function = f
                            IP = 0
                            StackBase = vm.Stack.Count - 2
                            Locals = Array.zeroCreate f.Locals.Length }

                      vm.Frames.Add(frame)
                      let result, vm = runCurrentFrame vm
                      fold result vm xs
              let result, vm = fold acc vm l
              push vm result
          | [ VList(l, _); acc; VBuiltin f ] ->
              let rec fold acc vm =
                  function
                  | [] -> acc, vm
                  | x :: xs ->
                      let vm = f [ x; acc ] vm
                      let result, vm = pop vm
                      fold result vm xs

              let result, vm = fold acc vm l
              push vm result
          | _ ->
              raise <| InvalidProgramException "fold expects a list, an accumulator, and a function")

      Identifier "map",
      VBuiltin(fun args vm ->
          match args with
          | [ VList(l', _); VFunction(f, None) ] ->
              let rec map acc vm =
                  function
                  | [] -> acc, vm
                  | x :: xs ->

                      let vm = push vm x

                      let frame =
                          { Function = f
                            IP = 0
                            StackBase = vm.Stack.Count - 1
                            Locals = Array.zeroCreate f.Locals.Length }

                      vm.Frames.Add(frame)

                      let result, vm = runCurrentFrame vm

                      let vm = push vm result
                      map (result :: acc) vm xs

              let result, vm = map [] vm l'
              push vm (VList(List.rev result, LIST))

          | [ VList(l', _); VBuiltin f ] ->
              let rec map acc vm =
                  function
                  | [] -> acc, vm
                  | x :: xs ->
                      let vm = f [ x ] vm
                      let result, vm = pop vm
                      map (result :: acc) vm xs

              let result, vm = map [] vm l'
              push vm (VList(List.rev result, LIST))
          | _ ->
              raise <| InvalidProgramException "map expects a list and a function")
      Identifier "dotProduct",
      VBuiltin(fun args vm ->
          match args with
          | [ VList(l1', _) as l1; VList(l2', _) as l2 ] when List.length l1' = List.length l2' ->
              let result = dotProduct l1 l2
              push vm result
          | _ -> raise <| InvalidProgramException "dotProduct expects two lists of the same length")
      Identifier "crossProduct",
      VBuiltin(fun args vm ->
          match args with
          | [ VList(l1', _) as l1; VList(l2', _) as l2 ] when List.length l1' = 3 && List.length l2' = 3 ->
              let result = crossProduct l1 l2
              push vm result
          | _ -> raise <| InvalidProgramException "crossProduct expects two lists of length 3")

      Identifier "BUILTIN_TRUNC",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VFloat num) ] -> push vm (VNumber(VFloat(truncate num)))
          | _ -> raise <| InvalidProgramException "truncate expects a float")

      Identifier "range",
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VInteger start); VNumber(VInteger stop) ] ->
              let range = [ for i in start..stop -> VNumber(VInteger i) ]
              let list = VList(range, LIST)
              push vm list
            | [ VNumber(VInteger start); VNumber(VInteger stop); VNumber(VInteger step) ] ->
                let range = [ for i in start..step..stop -> VNumber(VInteger i) ]
                let list = VList(range, LIST)
                push vm list
            | _ -> raise <| InvalidProgramException "range expects two integers")

      Identifier "BUILTIN_LEN",
      VBuiltin(fun args vm ->
          match args with
          | [ VList(l, _) ] -> push vm (VNumber(VInteger(List.length l)))
          | [ VString s ] -> push vm (VNumber(VInteger(s.Length)))
          | _ -> raise <| InvalidProgramException "len expects a list or a string")

      Identifier "PI", VNumber(VFloat(Math.PI))

      Identifier "E", VNumber(VFloat(Math.E))

      Identifier "TAU", VNumber(VFloat(Math.Tau))

      Identifier "cast",
      VBuiltin(fun args vm ->
          let org = args.Head
          let castTyp = List.item 1 args
          
          cast org castTyp |> push vm
      )

      Identifier "newtonRaphson",
      VBuiltin(fun args vm ->
          match args with
          | [ VFunction(_, Some f1)
              VFunction(_, Some f2)
              VNumber(VFloat init)
              VNumber(VFloat tol)
              VNumber(VInteger it) ] ->
              let builtin1 = SymbolicExpression.toBuiltin f1
              let builtin2 = SymbolicExpression.toBuiltin f2
              
              let res = newtonRaphson builtin1 builtin2 init tol it
              push vm (VNumber(VFloat(res)))
          | _ -> raise <| InvalidProgramException "newtonRaphson expects two functions, an initial guess, a tolerance, and a maximum number of iterations")

      Identifier "bisection",
      VBuiltin(fun args vm ->
          match args with
          | [ VFunction(_, Some f); VNumber(VFloat(a)); VNumber(VFloat(b)); VNumber(VFloat(tol)); VNumber(VInteger(it)) ] ->
              let builtin = SymbolicExpression.toBuiltin f
              let res = bisection builtin a b tol it
              push vm (VNumber(VFloat(res)))
          | _ -> raise <| InvalidProgramException "bisection expects a function, a lower bound, an upper bound, a tolerance, and a maximum number of iterations")
      
      Identifier "eval",
      VBuiltin(fun args vm ->
          match args with
          | [VBlock e] ->
              match e with
              | EBlock (stmts, _) ->
                  let compiled = Compiler.compileProgram stmts
                  match compiled with
                  | Ok(func, _) ->
                      let block = createNewVM(func)
                      let vm' = run block
                      let lst = vm'.Stack[vm'.Stack.Count - 1]
                      push vm lst
                  | Error err -> raise <| InvalidProgramException $"{err}"
              | _ -> raise <| InvalidProgramException "eval expects a block"
          | _ -> raise <| InvalidProgramException "eval expects a block")
      
      Identifier "differentiate",
      VBuiltin(fun args vm ->
          match args with
            | [ VFunction(_, Some f) ] ->
                let diff = SymbolicExpression.differentiate f
                let expr = SymbolicExpression.toExpr diff
                
                let param = { Lexeme = Identifier "x"; Position = { Line = 0; Column = 0 } }
                let expr = SExpression(ELambda([(param, None)], expr, None, true, None), None)
                
                let compiled = Compiler.compileProgram [expr]
                match compiled with
                | Ok(func, _) ->
                    let block = createNewVM(func)
                    let vm' = run block
                    let lst = vm'.Stack[vm'.Stack.Count - 1]
                    push vm lst
                | Error err -> raise <| InvalidProgramException $"{err}"
                
            | _ -> raise <| InvalidProgramException "differentiate expects a function")
      
      Identifier "integrate",
      VBuiltin(fun args vm ->
          match args with
          | [ VFunction(_, Some f) ] ->
              let integral = SymbolicExpression.integrate f
              let expr = SymbolicExpression.toExpr integral
              
              let param = { Lexeme = Identifier "x"; Position = { Line = 0; Column = 0 } }
              let expr = SExpression(ELambda([(param, None)], expr, None, true, None), None)
              
              let compiled = Compiler.compileProgram [expr]
              match compiled with
              | Ok(func, _) ->
                    let block = createNewVM(func)
                    let vm' = run block
                    let lst = vm'.Stack[vm'.Stack.Count - 1]
                    push vm lst
              | Error err -> raise <| InvalidProgramException $"{err}"
            | _ -> raise <| InvalidProgramException "integrate expects a function")
      
      Identifier "assert",
      VBuiltin(fun args vm ->
          match args with
          | [msg; cond] ->
              if not (isTruthy cond) then
                  raise <| InvalidProgramException $"Assertion failed: {valueToString msg}"
              else
                  push vm VNil
          | [cond] ->
              if not (isTruthy cond) then
                  raise <| InvalidProgramException $"Assertion failed: {valueToString cond}"
              else
                  push vm VNil
            | _ -> raise <| InvalidProgramException "assert expects a condition")
      
      Identifier "draw",
        VBuiltin(fun args vm ->
            match args with
            | [ VList (elems, RECORD) ] ->
                let width = elems |> List.tryFind (function
                    | VList([VString "width"; VNumber(VFloat _)], _) -> true
                    | _ -> false)
                
                let width = match width with
                            | Some(VList([VString "width"; VNumber(VFloat w)], _)) -> w
                            | _ -> raise <| InvalidProgramException "draw expects a width"
                
                let height = elems |> List.tryFind (function
                    | VList([VString "height"; VNumber(VFloat _)], _) -> true
                    | _ -> false)
                
                let height = match height with
                                | Some(VList([VString "height"; VNumber(VFloat h)], _)) -> h
                                | _ -> raise <| InvalidProgramException "draw expects a height"
                
                let x = elems |> List.tryFind (function
                    | VList([VString "x"; VNumber(VFloat _)], _) -> true
                    | _ -> false)
                
                let x = match x with
                            | Some(VList([VString "x"; VNumber(VFloat x)], _)) -> x
                            | _ -> raise <| InvalidProgramException "draw expects an x"
                
                
                
                let y = elems |> List.tryFind (function
                    | VList([VString "y"; VNumber(VFloat _)], _) -> true
                    | _ -> false)
                
                let y = match y with
                            | Some(VList([VString "y"; VNumber(VFloat y)], _)) -> y
                            | _ -> raise <| InvalidProgramException "draw expects a y"
                
                let colour = elems |> List.tryFind (function
                    | VList([VString "colour"; VString _], _) -> true
                    | _ -> false)
                
                let colour = match colour with
                                | Some(VList([VString "colour"; VString c], _)) -> c
                                | _ -> raise <| InvalidProgramException "draw expects a colour"
                                
                let typ = elems |> List.tryFind (function
                    | VList([VString "shape"; VString _], _) -> true
                    | _ -> false
                    )
                
                let typ = match typ with
                           | Some(VList([VString "shape"; VString c], _)) -> c
                           | _ -> "circle"
                
                let value = VShape(width, height, x, y, colour, typ)
                vm.Canvas.Add(value)
                push vm VNil
            | [VList(elems, LIST)] ->
                elems |>
                List.iter (fun elems ->
                    match elems with
                    | VList(elems, RECORD) ->
                        
                        let width = elems |> List.tryFind (function
                            | VList([VString "width"; VNumber(VFloat _)], _) -> true
                            | _ -> false)
                        
                        let width = match width with
                                    | Some(VList([VString "width"; VNumber(VFloat w)], _)) -> w
                                    | _ -> raise <| InvalidProgramException "draw expects a width"
                        
                        let height = elems |> List.tryFind (function
                            | VList([VString "height"; VNumber(VFloat _)], _) -> true
                            | _ -> false)
                        
                        let height = match height with
                                        | Some(VList([VString "height"; VNumber(VFloat h)], _)) -> h
                                        | _ -> raise <| InvalidProgramException "draw expects a height"
                        
                        let x = elems |> List.tryFind (function
                            | VList([VString "x"; VNumber(VFloat _)], _) -> true
                            | _ -> false)
                        
                        let x = match x with
                                    | Some(VList([VString "x"; VNumber(VFloat x)], _)) -> x
                                    | _ -> raise <| InvalidProgramException "draw expects an x"
                        
                        
                        
                        let y = elems |> List.tryFind (function
                            | VList([VString "y"; VNumber(VFloat _)], _) -> true
                            | _ -> false)
                        
                        let y = match y with
                                    | Some(VList([VString "y"; VNumber(VFloat y)], _)) -> y
                                    | _ -> raise <| InvalidProgramException "draw expects a y"
                        
                        let colour = elems |> List.tryFind (function
                            | VList([VString "colour"; VString _], _) -> true
                            | _ -> false)
                        
                        let colour = match colour with
                                        | Some(VList([VString "colour"; VString c], _)) -> c
                                        | _ -> raise <| InvalidProgramException "draw expects a colour"
                        
                        let typ = elems |> List.tryFind (function
                            | VList([VString "shape"; VString _], _) -> true
                            | _ -> false
                            )
                        
                        let typ = match typ with
                                   | Some(VList([VString "shape"; VString c], _)) -> c
                                   | _ -> "circle"
                        
                        let value = VShape(width, height, x, y, colour, typ)
                        vm.Canvas.Add(value)
                        push vm VNil |> ignore
                    | _ -> raise <| InvalidProgramException("draw expects a list of records")
                )
                vm
            | _ -> raise <| InvalidProgramException "draw expects a title and a list of functions")
      
      Identifier "findIntegral",
      VBuiltin(fun args vm ->
          match args with
          | [ VFunction(_, Some f); VNumber(VFloat b); VNumber(VFloat a)  ] ->
                let res = SymbolicExpression.findIntegral f a b
                push vm (VNumber(VFloat res))
            | _ -> raise <| InvalidProgramException "findIntegral expects a function, a lower bound, and an upper bound")

      Operator(Plus, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> add a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for +")

      Operator(Minus, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> subtract a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for -")

      Operator(Star, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> multiply a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for *")

      Operator(Slash, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> divide a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for /")

      Operator(Percent, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ VNumber(VInteger a); VNumber(VInteger b) ] -> push vm (VNumber(VInteger(a % b)))
          | [ VNumber(VFloat a); VNumber(VFloat b) ] -> push vm (VNumber(VFloat(a % b)))
          | _ -> raise <| InvalidProgramException "Expected two integers or floats for %")

      Operator(StarStar, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> power a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for **")

      Operator(EqualEqual, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> valuesEqual a b |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for ==")

      Operator(BangEqual, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> valuesEqual a b |> not |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for !=")

      Operator(Less, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b < 0 |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for <")

      Operator(LessEqual, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b <= 0 |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for <=")

      Operator(Greater, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b > 0 |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for >")

      Operator(GreaterEqual, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> compare a b >= 0 |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for >=")

      Operator(AmpersandAmpersand, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> (isTruthy a && isTruthy b) |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for &&")

      Operator(PipePipe, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> (isTruthy a || isTruthy b) |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for ||")

      Operator(Bang, Some Prefix),
      VBuiltin(fun args vm ->
          match args with
          | [ a ] -> not (isTruthy a) |> VBoolean |> push vm
          | _ -> raise <| InvalidProgramException "Expected one argument for !")

      Operator(Minus, Some Prefix),
      VBuiltin(fun args vm ->
          match args with
          | [ a ] -> negate a |> push vm
          | _ -> raise <| InvalidProgramException "Expected one argument for -")

      Operator(Plus, Some Prefix),
      VBuiltin(fun args vm ->
          match args with
          | [ a ] -> unnegate a |> push vm
          | _ -> raise <| InvalidProgramException "Expected one argument for +")

      Operator(Caret, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> power a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for ^")

      Operator(DotStar, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> dotProduct a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for .*")

      Operator(Cross, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; b ] -> crossProduct a b |> push vm
          | _ -> raise <| InvalidProgramException "Expected two arguments for cross")

      Operator(ColonColon, Some Infix),
      VBuiltin(fun args vm ->
          match args with
          | [ a; VList(l, _) ] -> VList(a :: l, LIST) |> push vm
          | _ -> raise <| InvalidProgramException "Expected a value and a list for ::")
      ]

    |> List.map (fun (key, value) -> lexemeToString key, value)
    |> Map.ofList

and runFrameRecursive vm =
    let frame = getCurrentFrame vm
    if frame.IP >= frame.Function.Chunk.Code.Count then
        VNil, vm
    else
        let vm, instruction = readByte vm
        let opcode = byteToOpCode instruction
        match opcode with
        | CALL ->
            let vm, argCount  = readByte vm
            let vm, recursive = readByte vm
            let callee = peek vm (int argCount)
            callee, vm
        | _ ->
            let vm = executeOpcode vm opcode
            runCurrentFrame vm
            
and runCurrentFrame vm =
    let frame = getCurrentFrame vm

    if frame.IP >= frame.Function.Chunk.Code.Count then
        VNil, vm
    else
        let vm, instruction = readByte vm
        let opcode = byteToOpCode instruction

        match opcode with
        | RETURN ->
            let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
            vm.Frames.RemoveAt(vm.Frames.Count - 1)
            result, vm
        | _ ->
            let vm = executeOpcode vm opcode
            runCurrentFrame vm


and executeOpcode (vm: VM) (opcode: OP_CODE) =
    printfn $"{opCodeToString opcode}"
    let vm = appendOutput vm Execution $"Executing: {opCodeToString opcode}"

    match opcode with
    | CONSTANT ->
        let constant, vm = readConstant vm
        let vm = push vm constant

        let _ =
            appendOutput vm Execution $"Pushed constant onto stack: {valueToString constant}"

        vm
    | CONSTANT_LONG ->
        let constant, vm = readConstantLong vm
        let vm = push vm constant

        let _ =
            appendOutput vm Execution $"Pushed long constant onto stack: {valueToString constant}"

        vm
    | GET_LOCAL ->
        let vm, slot = readByte vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot

        if index >= vm.Stack.Count then
            raise <| InvalidProgramException $"GET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"

        let value = vm.Stack[index]
        let vm = push vm value
        vm
    | SET_LOCAL ->
        let vm, slot = readByte vm
        let value, vm = pop vm
        let frame = getCurrentFrame vm
        let index = frame.StackBase + int slot

        if index >= vm.Stack.Count then
            raise <| InvalidProgramException $"SET_LOCAL: Stack index out of range. Index: {index}, Stack size: {vm.Stack.Count}"

        vm.Stack[index] <- value
        vm
    | TRUE -> push vm (VBoolean true)
    | FALSE -> push vm (VBoolean false)
    | NIL -> push vm VNil
    | POP ->
        let _, vm = pop vm
        vm
    | DEFINE_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            let value, vm = pop vm

            let vm =
                appendOutput vm Execution $"Defining global variable: {name} = {valueToString value}"

            defineGlobal vm name value
        | _ -> raise <| InvalidProgramException "Expected string constant for variable name in DEFINE_GLOBAL"
    | GET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            match getGlobal vm name with
            | Some value ->
                push vm value
            | None ->
                raise <| InvalidProgramException $"Undefined variable '{name}'"
        | _ -> raise <| InvalidProgramException "Expected string constant for variable name in GET_GLOBAL"
    | SET_GLOBAL ->
        let constant, vm = readConstant vm

        match constant with
        | VString name ->
            let value, vm = pop vm

            if vm.Globals.ContainsKey(name) then
                let vm = defineGlobal vm name value
                vm
            else
                raise <| InvalidProgramException $"Undefined variable '{name}'"
        | _ -> raise <| InvalidProgramException "Expected string constant for variable name in SET_GLOBAL"
    | CALL ->
        let vm, argCount  = readByte vm
        let vm, recursive = readByte vm
        callValue vm (int argCount) (int recursive)
    | RETURN ->
        let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
        vm.Frames.RemoveAt(vm.Frames.Count - 1)

        if vm.Frames.Count = 0 then
            let vm = push vm result
            vm
        else
            let callerFrame = getCurrentFrame vm
            vm.Stack.RemoveRange(callerFrame.StackBase, vm.Stack.Count - callerFrame.StackBase)
            let vm = push vm result
            runLoop vm
    | CLOSURE ->
        let constant, vm = readConstant vm

        match constant with
        | VFunction(func, _) ->
            let upValues =
                func.Locals
                |> Seq.filter (fun local -> local.Depth > 0)
                |> Seq.map (fun local -> vm.Stack[local.Index])
                |> Seq.toList

            let closure = VClosure { Function = func; UpValues = upValues }
            let vm = push vm closure
            vm
        | _ -> raise <| InvalidProgramException "Expected function constant for closure"
    | JUMP ->
        printfn "here"
        let vm, byte1 = readByte vm
        let vm, byte2 = readByte vm
        let jump = int  ((byte1 <<< 8) ||| byte2)
        
        let frame = getCurrentFrame vm
        let frame = { frame with IP = frame.IP + jump }
        vm.Frames[vm.Frames.Count - 1] <- frame
        vm
    | JUMP_IF_FALSE ->
        let vm, byte1 = readByte vm
        let vm, byte2 = readByte vm
        let jump = int  ((byte1 <<< 8) ||| byte2)
        let condition, vm = pop vm
        
        if not (isTruthy condition) then
            let frame = getCurrentFrame vm
            let frame = { frame with IP = frame.IP + jump }
            vm.Frames[vm.Frames.Count - 1] <- frame
            vm
        else
            vm
    | COMPOUND_CREATE ->
        let structure, vm = pop vm
        let count, vm = pop vm

        match (structure, count) with
        | VList(values, typ), VNumber(VInteger n) when n >= 0 ->
            let values' =
                [ 0 .. n - 1 ] |> List.map (fun _ -> let value, _ = pop vm in value) |> List.rev

            let list = VList(List.append values values', typ)
            let vm = push vm list
            vm
        | _ -> raise <| InvalidProgramException("Expected list and integer for list create")

    | COMPOUND_GET ->
        let key, vm = pop vm
        let structure, vm = pop vm

        match (structure, key) with
        | VList(values, _), VNumber(VInteger i) when i >= 0 && i < List.length values ->
            let value = List.item i values
            let vm = push vm value
            vm
        | VList(values, _), VString key ->
            let value =
                values
                |> Seq.tryFind (fun value ->
                    match value with
                    | VList([ VString k; _ ], _) when k = key -> true
                    | _ -> false)

            match value with
            | Some(VList([ _; v ], _)) -> push vm v
            | _ -> push vm VNil
        | _ -> raise <| InvalidProgramException("Expected list and integer for list get")
    | _ -> raise <| InvalidProgramException($"Unimplemented opcode: {opCodeToString opcode}")

and runLoop vm =
    if vm.Frames.Count = 0 then
        vm
    else
        let frame = getCurrentFrame vm
        if frame.IP >= frame.Function.Chunk.Code.Count then
            if vm.Frames.Count > 1 then
                let result, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm 
                vm.Frames.RemoveAt(vm.Frames.Count - 1)
                let callerFrame = getCurrentFrame vm
                vm.Stack.RemoveRange(callerFrame.StackBase, vm.Stack.Count - callerFrame.StackBase)
                let vm = push vm result
                runLoop vm
            else
                vm.Frames.RemoveAt(vm.Frames.Count - 1)
                let vm = if vm.Stack.Count = 0 then push vm VNil else vm
                vm 
        else
            saveVMState vm
            let vm, instruction = readByte vm

            let vm =
                let opcode = byteToOpCode instruction
                executeOpcode vm opcode

            runLoop vm


and run (vm: VM) = runLoop vm

and callValue (vm: VM) (argCount: int) (recursive: int): VM =
    let callee = peek vm argCount

    match recursive with
    | 0 ->
        match callee with
        | VFunction(func, _) ->
            if argCount <> func.Arity then
                raise <| InvalidProgramException $"Expected {func.Arity} arguments but got {argCount}"

            let frame =
                { Function = func
                  IP = 0
                  StackBase = vm.Stack.Count - argCount
                  Locals = Array.zeroCreate func.Locals.Length }

            vm.Frames.Add(frame)
            vm
        | VClosure closure ->
            if argCount <> closure.Function.Arity then
                raise <| InvalidProgramException $"Expected {closure.Function.Arity} arguments but got {argCount}"

            let frame =
                { Function = closure.Function
                  IP = 0
                  StackBase = vm.Stack.Count - argCount
                  Locals = Array.zeroCreate closure.Function.Locals.Length }

            vm.Frames.Add(frame)
            let frame = getCurrentFrame vm
            closure.UpValues |> Seq.iteri (fun i upValue -> frame.Locals[i] <- upValue)
            vm
        | VBuiltin func ->
            printfn $"Calling builtin function: {func}"

            let args =
                [ 0 .. argCount - 1 ]
                |> List.map (fun _ -> let value, _ = pop vm in value)
                |> List.rev

            let vm = func args vm
            let res, vm = pop vm
            let _, vm = pop vm
            push vm res
        | _ -> raise <| InvalidProgramException $"Can only call functions and closures"
    | 1 ->
        match callee with
        | VFunction(func, _) ->
            if argCount <> func.Arity then
                raise <| InvalidProgramException $"Expected {func.Arity} arguments but got {argCount}"
            let frame =
                { Function = func
                  IP = 0
                  StackBase = vm.Stack.Count - argCount
                  Locals = Array.zeroCreate func.Locals.Length }
            vm.Frames.Add(frame)
            match runFrameRecursive vm with
            | VFunction(f, e),  vm  ->
                vm.Frames.RemoveAt(vm.Frames.Count- 1)
                callValue vm argCount 1
            | v, vm ->
                let _ = push vm v in
                vm
        | _ -> raise <| InvalidProgramException $"Can only call functions and closures"




// let interpretWithMode (func: Function) (vm: VM option) (isRepl: bool) =
//     let vm =
//         match vm with
//         | Some existingVM ->
//             let newFrame =
//                 { Function = func
//                   IP = 0
//                   StackBase = existingVM.Stack.Count
//                   Locals = [||] }
//
//             existingVM.Frames.Add(newFrame)
//             existingVM
//         | None ->
//             let newVM = createVM func
//             appendOutput newVM ConstantPool "=== Constant Pool ==="
//
//     let vm = appendOutput vm Execution "\n=== Program Execution ==="
//     let finalVm = run vm
//     (finalVm, finalVm.Streams)
//
// let interpret (func: Function) (vm: VM option) = interpretWithMode func vm false

// let replExecute (func: Function) (vm: VM option) = interpretWithMode func vm true

let getStreamContent (stream: seq<string>) =
    String.concat Environment.NewLine (Seq.toArray stream)

let resetStreams (vm: VM) =
    { vm with
        Streams = createOutputStreams () }

let stepVM (vm: VM) =
    let currentFrame = getCurrentFrame vm

    if currentFrame.IP >= currentFrame.Function.Chunk.Code.Count then
        if vm.Frames.Count > 1 then
            vm.Frames.RemoveAt(vm.Frames.Count - 1)
            vm
        else
            vm
    else
        saveVMState vm
        let vm, instruction = readByte vm

        let updatedVM =
            match byteToOpCode instruction with
            | opcode ->
                let vm = appendOutput vm Execution $"Executing: {opCodeToString opcode}"

                match opcode with
                | GET_LOCAL ->
                    let vm, slot = readByte vm
                    let frame = getCurrentFrame vm
                    let value = vm.Stack[frame.StackBase + int slot]
                    let _ = push vm value
                    vm
                | SET_LOCAL ->
                    let vm, slot = readByte vm
                    let value, vm = pop vm
                    let frame = getCurrentFrame vm
                    vm.Stack[frame.StackBase + int slot] <- value
                    vm
                | CONSTANT ->
                    let constant, vm = readConstant vm
                    push vm constant
                | CONSTANT_LONG ->
                    let constant, vm = readConstantLong vm
                    push vm constant
                | TRUE -> push vm (VBoolean true)
                | FALSE -> push vm (VBoolean false)
                | POP ->
                    let _, vm = pop vm
                    vm
                | CALL ->
                    let _, argCount = readByte vm
                    let vm, recursive = readByte vm 
                    callValue vm (int argCount) (int recursive)
                | DEFINE_GLOBAL ->
                    let constant, vm = readConstant vm

                    match constant with
                    | VString name ->
                        let value, vm = pop vm

                        let vm =
                            appendOutput vm Execution $"Defining global variable: {name} = {valueToString value}"

                        defineGlobal vm name value
                    | _ -> raise <| InvalidProgramException("Expected string constant for variable name in DEFINE_GLOBAL")
                | GET_GLOBAL ->
                    let constant, vm = readConstant vm

                    match constant with
                    | VString name ->
                        match getGlobal vm name with
                        | Some value -> push vm value
                        | None -> raise <| InvalidProgramException($"Undefined variable '{name}'")
                    | _ -> raise <| InvalidProgramException("Expected string constant for variable name in GET_GLOBAL")
                | RETURN ->
                    if vm.Frames.Count > 1 then
                        let returnValue, vm = if vm.Stack.Count > 0 then pop vm else VNil, vm
                        vm.Frames.RemoveAt(vm.Frames.Count - 1)
                        push vm returnValue
                    else
                        vm

                | _ -> raise <| InvalidProgramException($"Unimplemented opcode: {opCodeToString opcode}")

        updatedVM

let stepBackVM (vm: VM) =
    if vm.ExecutionHistory.Count > 0 then
        let previousState = vm.ExecutionHistory[vm.ExecutionHistory.Count - 1]
        vm.ExecutionHistory.RemoveAt(vm.ExecutionHistory.Count - 1)
        previousState
    else
        vm
        


