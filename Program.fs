namespace Vec3

open System
open Avalonia
open Vec3.Interpreter.Repl
open Vec3.Interpreter
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Typing.Exceptions

module Program =
    
    let IsPrimeMultipleTest n x =
        x = n || x % n <> 0
        
    let RemoveAllMultiples listn =
        List.fold (fun acc x -> List.filter (fun el -> el = x || el % x <> 0) acc) listn listn
    
    let rec RemoveAllMultiples2 listn =
        match listn with
        | head :: tail ->
            let filterd = List.filter (IsPrimeMultipleTest head) tail
            head :: RemoveAllMultiples2 filterd
        | [] -> []
    
    let GetPrimesUpTo n =
        RemoveAllMultiples2 [2..n]
        

    // args, -r to repl, -f to file, -g or no args for GUI
    let usg_msg = "Usage: vec3 [-r | -f <filename> | -g]"

    [<CompiledName "BuildAvaloniaApp">]
    let buildAvaloniaApp () =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)

    [<EntryPoint; STAThread>]
    let main (argv: string array) : int =
        GetPrimesUpTo 100
        |> List.iter (fun x -> printfn $"{x}")
        match argv with
        | [||]
        | [| "-g" |] -> buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
        | [| "-r" |] ->
            evalRepl
            0
        | [| "-f"; filename |] ->
            let content = System.IO.File.ReadAllText filename
            let content = Preprocessor.preprocessContent content

            match parse content with
            | Ok(_, program) ->
                let typeCheck =
                    Typing.Inference.inferProgram Map.empty Typing.Inference.defaultTypeEnv program

                match typeCheck with
                | Ok(_, _, _, program) ->
                    let program = ConstantFolding.foldConstants program
                    let value, _ = evalProgram defaultEnv program
                    printfn $"{exprToString value}"
                    0
                | Error errors ->
                    printfn $"{formatTypeErrors errors}"
                    1
            | Error(e, s) ->
                printfn $"{formatParserError e s}"
                1
        | _ ->
            printfn $"{usg_msg}"
            1
