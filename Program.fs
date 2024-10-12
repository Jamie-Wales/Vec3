namespace Vec3

open System
open Avalonia
open Vec3.Interpreter.Repl
open Vec3.Interpreter
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Typing.Checker

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
    let main argv =
        match argv with
        | [||] | [|"-g"|] ->
            // Start the GUI
            evalRepl
            0
            // buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
        | [|"-r"|] ->
            // Explicit REPL mode
            startRepl ()
            0
        | [|"-f"; filename|] ->
            // File execution mode
            let _ = NotImplementedException "File execution not implemented yet."
            0
        | _ ->
            printfn $"{usg_msg}"
            1
