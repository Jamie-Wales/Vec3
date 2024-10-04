namespace Vec3

open System
open Avalonia
open Interpreter.Repl
open Vec3.Interpreter
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Typing.Checker

module Program =
    
    // args, -r to repl, -f to file, standard input otherwise
    let usg_msg = "Usage: vec3 [-r | -f <filename>]"
    

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
            // buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
            // repl
            // 0
            evalRepl
            0
        else if argv.[0] = "-r" then
            repl
            0
        else if argv.[0] = "-f" then
            if argv.Length < 2 then
                printfn $"{usg_msg}"
                1
            else
                0
        else
            1