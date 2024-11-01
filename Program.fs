namespace Vec3

open System
open Avalonia
open Vec3.Interpreter.Repl
open Vec3.Interpreter
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Typing.Exceptions

module Program =

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
