namespace Vec3

open System
open Avalonia
open Vec3.Interpreter.Repl
open Vec3.Interpreter
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Typing.Checker

module Program =
    
    // args, -r to repl, -f to file, standard input otherwise
    let usg_msg = "Usage: vec3 [-r | -f l<filename>]"
    
    [<CompiledName "BuildAvaloniaApp">] 
    let buildAvaloniaApp () = 
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)

    [<EntryPoint; STAThread>]
    let main argv =
        if argv.Length = 0 then
            // Start the REPL
            startRepl ()
            0
        else if argv.[0] = "-r" then
            // Explicit REPL mode
            startRepl ()
            0
        else if argv.[0] = "-f" then
            if argv.Length < 2 then
                printfn $"{usg_msg}"
                1
            else
                let _ = NotImplementedException "File execution not implemented yet."
                0
        else
            printfn $"{usg_msg}"
            1
