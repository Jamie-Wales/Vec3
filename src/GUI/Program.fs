open System
open Avalonia
open GUI.App

/// <summary>
/// The main program module (entry point).
/// </summary>
module Program =
    /// <summary>
    /// Creates an Avalonia application.
    /// </summary>
    [<CompiledName "BuildAvaloniaApp">]
    let buildAvaloniaApp () =
        AppBuilder
            .Configure<Main>()
            .UsePlatformDetect()
            .WithInterFont()
            .LogToTrace(areas = Array.empty)

    [<EntryPoint; STAThread>]
    let main (argv: string array) : int = buildAvaloniaApp().StartWithClassicDesktopLifetime(argv)
