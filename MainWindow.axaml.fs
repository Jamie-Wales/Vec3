namespace Vec3

open System
open System.IO
open System.Text.Encodings.Web
open Avalonia.Controls
open Avalonia.Markup.Xaml
open AvaloniaEdit
open Avalonia.Media
open TextMateSharp.Grammars
open AvaloniaEdit.TextMate
open Vec3.Interpreter
open Vec3.Interpreter.Repl
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Typing
open System.Threading.Tasks
open System.Threading

type MainWindow () as this =
    inherit Window ()

    let mutable textEditor: TextEditor = null
    let mutable textMateInstallation: TextMate.Installation = null
    let mutable executeButton: Button = null
    let mutable replInput: TextEditor = null
    let mutable loadButton: Button = null
    let mutable standardOutput: TextBlock = null
    let mutable replState: VM = createNewVM(initFunction("Main"))
    
    let debounceTime = 500
    let mutable debounceTimer = None : Timer option
    
    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
    member private this.InitializeComponent() =
        textEditor <- this.FindControl<TextEditor>("Editor")
        executeButton <- this.FindControl<Button>("ExecuteButton")
        loadButton <- this.FindControl<Button>("LoadButton")
        standardOutput <- this.FindControl<TextBlock>("StandardOutput")
        replInput <- this.FindControl<TextEditor>("ReplInput")

        let registryOptions = RegistryOptions(ThemeName.QuietLight)
        
        if textEditor <> null then
            textEditor.ShowLineNumbers <- true
            textEditor.Options.ShowTabs <- false 
            textEditor.Options.ShowSpaces <- false 
            textEditor.Options.ShowEndOfLine <- false 
            textEditor.Options.HighlightCurrentLine <- false 

            textMateInstallation <- TextMate.Installation(textEditor, registryOptions)
            
            let fsharpLanguage = registryOptions.GetLanguageByExtension(".fs")
            let scopeName = registryOptions.GetScopeByLanguageId(fsharpLanguage.Id)
            textMateInstallation.SetGrammar(scopeName)

            textEditor.Text <- """// Vec3 Editor Example
    let x: int = 5
    print(x)

    let f = (x) -> x^2.0 - 2.0
    let a = 1.0
    let b = 2.0
    let tolerance = 1e-6
    let max = 100

    let root = bisection(f, a, b, tolerance, max)

    print(root)

    plotFunc("test", f)
    """

            this.ApplyColorScheme()
                
        if replInput <> null then
            replInput.ShowLineNumbers <- false
            replInput.Options.ShowTabs <- false 
            replInput.Options.ShowSpaces <- false 
            replInput.Options.ShowEndOfLine <- false 
            replInput.Options.HighlightCurrentLine <- false
                
            // Add syntax highlighting to REPL input with matching color scheme
            let replTextMate = TextMate.Installation(replInput, registryOptions)
            let fsharpLanguage = registryOptions.GetLanguageByExtension(".fs")
            let scopeName = registryOptions.GetScopeByLanguageId(fsharpLanguage.Id)
            replTextMate.SetGrammar(scopeName)
                
            // Apply the same color scheme
            let mutable colorString = ""
            if textMateInstallation.TryGetThemeColor("editor.background", &colorString) then
                match Color.TryParse(colorString) with
                | true, color -> replInput.Background <- SolidColorBrush(color)
                | _ -> ()
                    
            if textMateInstallation.TryGetThemeColor("editor.foreground", &colorString) then
                match Color.TryParse(colorString) with
                | true, color -> replInput.Foreground <- SolidColorBrush(color)
                | _ -> ()
                
        executeButton.Click.AddHandler(fun _ _ -> this.ExecuteCode())
        loadButton.Click.AddHandler(fun _ _ -> this.LoadCode())
        textEditor.TextChanged.AddHandler(fun _ _ -> this.TextChanged())
    member private this.LoadCode() =
        replState <- createNewVM(initFunction("Main"))
        let code = this.GetEditorText()
        match parseAndCompile code replState with
        | Some vm ->
            replState <- vm
            standardOutput.Foreground <- SolidColorBrush(Colors.GreenYellow)
            standardOutput.Text <- "Vec3 code -> Loaded"
        | None -> 
            standardOutput.Foreground <- SolidColorBrush(Colors.Red)
            standardOutput.Text <- "Failed to compile code"
            
    member private this.TextChanged() =
        debounceTimer |> Option.iter (_.Dispose())
        
        // probably be better to not reparse old code, but would involve diffing, complex
        let callback = fun _ ->
            Task.Run(fun () ->
                Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                    let code = textEditor.Text
                    if String.IsNullOrEmpty(code) then
                        standardOutput.Foreground <- SolidColorBrush(Colors.White)
                        standardOutput.Text <- ""
                    else
                        match parse code with
                        | Ok (_, ast) ->
                            let env, aliases, _, _ = Prelude.preludeChecked
                            
                            match Inference.inferProgram aliases env ast with
                            | Ok _ ->
                                    standardOutput.Foreground <- SolidColorBrush(Colors.White)
                                    standardOutput.Text <- "No errors"
                            | Error err ->
                                    standardOutput.Foreground <- SolidColorBrush(Colors.Yellow)
                                    standardOutput.Text <- Exceptions.formatTypeErrors err
                        | Error (err, state) ->
                            standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                            standardOutput.Text <- formatParserError err state
                ) |> ignore
            ) |> ignore
        
        debounceTimer <- Some(new Timer(callback, null, debounceTime, Timeout.Infinite))
    
    member private this.ApplyColorScheme() =
        if textMateInstallation <> null then
            let applyColor colorKey (action: IBrush -> unit) =
                let mutable colorString = ""
                if textMateInstallation.TryGetThemeColor(colorKey, &colorString) then
                    match Color.TryParse(colorString) with
                    | true, color ->
                        let brush = SolidColorBrush(color)
                        action(brush)
                    | _ -> ()

            applyColor "editor.background" (fun brush -> textEditor.Background <- brush)
            applyColor "editor.foreground" (fun brush -> textEditor.Foreground <- brush)
            applyColor "editorLineNumber.foreground" (fun brush -> textEditor.LineNumbersForeground <- brush)

    member private this.ExecuteCode() =
        replState <- run replState 
        standardOutput.Foreground <- SolidColorBrush(Colors.White)
        let outputText = String.concat "\n" replState.Streams.StandardOutput
        standardOutput.Text <- sprintf "Output:\n%s" outputText
        if replState.Stack.Count > 0 then
            let topValue = replState.Stack[replState.Stack.Count - 1]
            match topValue with
                | VPlotData (title, xs, ys) ->
                    let xValues = 
                        xs |> List.choose (function
                            | VNumber (VFloat f) -> Some f
                            | VNumber (VInteger i) -> Some (float i)
                            | _ -> None)
                        |> Array.ofList
                    let yValues = 
                        ys |> List.choose (function
                            | VNumber (VFloat f) -> Some f
                            | VNumber (VInteger i) -> Some (float i)
                            | _ -> None)
                        |> Array.ofList
                    let plotWindow = PlotWindow()
                    plotWindow.PlotControl.Plot.Clear()
                    plotWindow.PlotControl.Plot.Add.Scatter(xValues, yValues) |> ignore
                    plotWindow.PlotControl.Plot.Title(title)
                    plotWindow.PlotControl.Refresh()
                    plotWindow.Show()
                | VPlotFunction (title, f) ->
                    let plotWindow = PlotWindow()
                    plotWindow.PlotControl.Plot.Clear()
                    plotWindow.PlotControl.Plot.Add.Function(f) |> ignore
                    plotWindow.PlotControl.Plot.Title(title)
                    plotWindow.PlotControl.Refresh()
                    plotWindow.Show()
                | _ -> ()

    member private this.GetEditorText() : string =
        if textEditor <> null then
            textEditor.Text
        else
            ""

    member this.SetEditorText(text: string) =
        if textEditor <> null then
            textEditor.Text <- text
