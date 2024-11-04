namespace Vec3

open System
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

open ScottPlot.Collections
type MainWindow () as this =
    inherit Window ()

    let mutable textEditor: TextEditor = null
    let mutable textMateInstallation: TextMate.Installation = null
    let mutable replInput: TextEditor = null
    let mutable loadButton: Button = null
    let mutable runButton: Button = null
    let mutable standardOutput: TextBlock = null
    let mutable replState: VM = createNewVM(initFunction("Main"))
    
    let debounceTime = 500
    let mutable debounceTimer = None : Timer option
    
    let welcomeMessage = """Welcome to Vec3 Editor!
Use the editor on the left to write your Vec3 code.
Click 'Load' or 'Shift+L' to execute the code.
REPL for quick commands click 'Play' or Shift+Enter.
Type 'help()' in the REPL for more information."""

    let initialCode = """// Vec3 Editor Example
let x: int = 5
print(x)

let f = (x) -> x^2.0 - 2.0

let y = (z) -> cos(z)

let r = (t) -> 2.9 * tan(t) 
let a = 1.0
let b = 2.0
let tolerance = 1e-6
let max = 100

let root = bisection(f, a, b, tolerance, max)

print(root)

plotFunc("test", f)
plotFunc("cosTest", y)
plotFunc("tanTest", r)

let data = {
    title="My Plot",
    x=[1, 2, 3, 4],
    y=[1, 4, 9, 16],
    ptype="bar" 
}
plot(data)
"""
    
    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()

    member private this.SetupEditor(registryOptions: RegistryOptions) =
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
            textEditor.Text <- initialCode
            this.ApplyColorScheme()

    member private this.SetupReplInput(registryOptions: RegistryOptions) =
        if replInput <> null then
            replInput.ShowLineNumbers <- false
            replInput.Options.ShowTabs <- false 
            replInput.Options.ShowSpaces <- false 
            replInput.Options.ShowEndOfLine <- false 
            replInput.Options.HighlightCurrentLine <- false
                
            let replTextMate = TextMate.Installation(replInput, registryOptions)
            let fsharpLanguage = registryOptions.GetLanguageByExtension(".fs")
            let scopeName = registryOptions.GetScopeByLanguageId(fsharpLanguage.Id)
            replTextMate.SetGrammar(scopeName)
            
            // Apply color scheme
            let mutable colorString = ""
            if textMateInstallation.TryGetThemeColor("editor.background", &colorString) then
                match Color.TryParse(colorString) with
                | true, color -> replInput.Background <- SolidColorBrush(color)
                | _ -> ()
                    
            if textMateInstallation.TryGetThemeColor("editor.foreground", &colorString) then
                match Color.TryParse(colorString) with
                | true, color -> replInput.Foreground <- SolidColorBrush(color)
                | _ -> ()

    member private this.InitializeComponent() =
        textEditor <- this.FindControl<TextEditor>("Editor")
        loadButton <- this.FindControl<Button>("LoadButton")
        standardOutput <- this.FindControl<TextBlock>("StandardOutput")
        replInput <- this.FindControl<TextEditor>("ReplInput")
        runButton <- this.FindControl<Button>("RunButton")
        standardOutput.Foreground <- SolidColorBrush(Colors.White)
        standardOutput.Text <- welcomeMessage

        let registryOptions = RegistryOptions(ThemeName.QuietLight)
        this.SetupEditor(registryOptions)
        this.SetupReplInput(registryOptions)
                
        loadButton.Click.AddHandler(fun _ _ -> this.LoadCode())
        textEditor.TextChanged.AddHandler(fun _ _ -> this.TextChanged())
        runButton.Click.AddHandler(fun _ _ -> this.run() |> ignore)

    member private this.CreatePlotWindow(title: string) =
        let plotWindow = PlotWindow()
        plotWindow.Title <- title
        plotWindow.PlotControl.Plot.Clear()
        plotWindow

    member private this.HandlePlotOutput(vm: VM) =
        for value in vm.Plots do
            match value with
            | VPlotData (title, xs, ys, plotType) ->
                let plotWindow = PlotWindow()
                plotWindow.Title <- title
                
                let extractNumber = function
                    | VNumber (VFloat f) -> f
                    | VNumber (VInteger i) -> float i
                    | _ -> 0.0
                
                let x = List.map extractNumber xs |> Array.ofList
                let y = List.map extractNumber ys |> Array.ofList
                
                match plotType with
                | Scatter -> 
                    plotWindow.PlotControl.Plot.Add.Scatter(x, y) |> ignore
                | Line ->
                    plotWindow.PlotControl.Plot.Add.Line(x.[0], y.[0], x.[1], y.[1]) |> ignore
                | Bar ->
                    plotWindow.PlotControl.Plot.Add.Bars(y, x) |> ignore
                | Histogram -> failwith "todo"
                    
                plotWindow.PlotControl.Plot.Title(title)
                plotWindow.PlotControl.Refresh()
                plotWindow.Show()
                        
            | VPlotFunction (title, f) ->
                let plotWindow = PlotWindow()
                plotWindow.Title <- title
                plotWindow.PlotControl.Plot.Add.Function(f) |> ignore
                plotWindow.PlotControl.Plot.Title(title)
                plotWindow.PlotControl.Refresh()
                plotWindow.Show()
                        
            | _ -> ()
                
        vm.Plots.Clear()
        
    member private this.LoadCode() =
        replState <- createNewVM(initFunction("Main"))
        let code = this.GetEditorText()
        match parseAndCompile code replState with
        | Some vm ->
            replState <- run vm 
            standardOutput.Foreground <- SolidColorBrush(Colors.White)
            let outputText = String.concat "\n" replState.Streams.StandardOutput
            standardOutput.Text <- $"Vec3 code loaded and executed:\n%s{outputText}"
            this.HandlePlotOutput(replState)  
        | None -> 
            standardOutput.Foreground <- SolidColorBrush(Colors.Red)
            standardOutput.Text <- "Failed to compile code"
            
    member private this.TextChanged() =
        debounceTimer |> Option.iter (_.Dispose())
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
                            printfn $"{err}"
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

    member this.run() =
        if replInput <> null then
            let code = replInput.Text.Trim()
            if code = "help()" then
                let syntaxWindow = SyntaxWindow()
                syntaxWindow.Show()
                standardOutput.Foreground <- SolidColorBrush(Colors.White)
                standardOutput.Text <- $"%s{standardOutput.Text}\n<Vec3> help()\nOpening syntax guide window..."
                replInput.Text <- ""
            else
                match noTcParseAndCompile code replState with
                | Some vm ->
                    let previousOutput = standardOutput.Text  
                    let oldOutputLength = Seq.length replState.Streams.StandardOutput
                    replState <- run vm
                    standardOutput.Foreground <- SolidColorBrush(Colors.White)
                    
                    let newOutput = replState.Streams.StandardOutput 
                                  |> Seq.skip oldOutputLength 
                                  |> String.concat "\n"
                    if not (String.IsNullOrWhiteSpace(newOutput)) then
                        standardOutput.Text <- $"%s{previousOutput}\n<Vec3> %s{code}\n%s{newOutput}"
                    else
                        standardOutput.Text <- $"%s{previousOutput}\n<Vec3> %s{code}"
                            
                    this.HandlePlotOutput(replState)
                    replInput.Text <- ""  
                | None -> 
                    standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                    standardOutput.Text <- $"%s{standardOutput.Text}\nFailed to compile: %s{replInput.Text}"
        true

    member private this.GetEditorText() : string =
        if textEditor <> null then
            textEditor.Text
        else
            ""

    member this.SetEditorText(text: string) =
        if textEditor <> null then
            textEditor.Text <- text