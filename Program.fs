namespace Vec3

open System
open Avalonia
open Interpreter.Repl
open Vec3.Interpreter

module Program =

    [<CompiledName "BuildAvaloniaApp">] 
    let buildAvaloniaApp () = 
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)

    [<EntryPoint; STAThread>]
    let main argv =
        // buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
        repl Map.empty
        0