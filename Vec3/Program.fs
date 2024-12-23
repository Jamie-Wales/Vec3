namespace Vec3

open System
open System.IO
open Avalonia
open Vec3.Transpiler.Transpiler

/// <summary>
/// The main program module (entry point).
/// </summary>
module Program =

    /// <summary>
    /// Simple read file function.
    /// </summary>
    /// <param name="filePath">The file path.</param>
    /// <returns>The file contents.</returns>
    let readFile (filePath: string) : string =
        if File.Exists(filePath) then
            File.ReadAllText(filePath)
        else
            failwithf $"File not found: %s{filePath}"
            
    /// <summary>
    /// args, -r to repl, -f to file, -g or no args for GUI
    /// </summary>
    let usg_msg = "Usage: vec3 [-r | -f <filename> | -g]"

    /// <summary>
    /// Creates an Avalonia application.
    /// </summary>
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
        | [| "-c"; filename |] ->
            let config =
                if argv.Length > 2 then
                    createConfig (Some argv[2])
                else
                    defaultConfig

            printfn $"Transpiling %s{filename}..."

            match transpile filename config with
            | Ok exePath ->
                printfn $"Successfully compiled to %s{exePath}"
                0
            | Error err ->
                eprintfn $"Transpilation failed: %s{formatError err}"
                1
        | _ ->
            printfn $"{usg_msg}"
            1
