namespace Vec3

open System
open System.Text.Encodings.Web
open Avalonia.Controls
open Avalonia.Markup.Xaml
open AvaloniaEdit
open Avalonia.Media
open TextMateSharp.Grammars
open AvaloniaEdit.TextMate
open Vec3.Interpreter.Repl
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Parser
open Vec3.Interpreter.PrettyPrinter
open Vec3.Interpreter.Typing
open System.Threading.Tasks
open System.Threading
open System.Threading.Tasks


type MainWindow () as this =
    inherit Window ()

    let mutable textEditor: TextEditor = null
    let mutable textMateInstallation: TextMate.Installation = null
    let mutable switchToReplButton: Button = null
    let mutable executeButton: Button = null
    let mutable switchToStandardButton: Button = null
    let mutable exitDebugButton: Button = null
    let mutable debugButton: Button = null
    let mutable stepBackButton: Button = null
    let mutable stepForwardButton: Button = null
    let mutable constantPoolOutput: TextBlock = null
    let mutable disassemblyOutput: TextBlock = null
    let mutable executionOutput: TextBlock = null
    let mutable standardOutput: TextBlock = null
    let mutable globalsOutput: TextBlock = null
    let mutable replState = createInitialState()
    let mutable debugVM: VM option = None
    
    let debounceTime = 500
    let mutable debounceTimer = None : Timer option

    
    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()

    member private this.InitializeComponent() =
        textEditor <- this.FindControl<TextEditor>("Editor")
        switchToReplButton <- this.FindControl<Button>("SwitchToReplButton")
        switchToStandardButton <- this.FindControl<Button>("SwitchToStandardButton")
        executeButton <- this.FindControl<Button>("ExecuteButton")
        debugButton <- this.FindControl<Button>("DebugButton")
        stepBackButton <- this.FindControl<Button>("StepBackButton")
        stepForwardButton <- this.FindControl<Button>("StepForwardButton")
        constantPoolOutput <- this.FindControl<TextBlock>("ConstantPoolOutput")
        disassemblyOutput <- this.FindControl<TextBlock>("DisassemblyOutput")
        executionOutput <- this.FindControl<TextBlock>("ExecutionOutput")
        standardOutput <- this.FindControl<TextBlock>("StandardOutput")
        exitDebugButton <- this.FindControl<Button>("ExitDebugButton")
        globalsOutput <- this.FindControl<TextBlock>("GlobalsOutput")
            
        if textEditor <> null then
            textEditor.ShowLineNumbers <- true
            textEditor.Options.ShowTabs <- false 
            textEditor.Options.ShowSpaces <- false 
            textEditor.Options.ShowEndOfLine <- false 
            textEditor.Options.HighlightCurrentLine <- false 

            let registryOptions = RegistryOptions(ThemeName.DarkPlus)
            textMateInstallation <- TextMate.Installation(textEditor, registryOptions)
            
            let fsharpLanguage = registryOptions.GetLanguageByExtension(".fs")
            let scopeName = registryOptions.GetScopeByLanguageId(fsharpLanguage.Id)
            textMateInstallation.SetGrammar(scopeName)

            textEditor.Text <- """// Vec3 Editor Example
let x: int = 5
let y: float = 3.14
if x > 0 then
    // Do something
    ()"""

            this.ApplyColorScheme()

        // Wire the event handlers
        switchToReplButton.Click.AddHandler(fun _ _ -> this.SwitchToReplMode())
        switchToStandardButton.Click.AddHandler(fun _ _ -> this.SwitchToStandardMode())
        executeButton.Click.AddHandler(fun _ _ -> this.ExecuteCode())
        debugButton.Click.AddHandler(fun _ _ -> this.StartDebugMode())
        stepBackButton.Click.AddHandler(fun _ _ -> this.StepBack())
        stepForwardButton.Click.AddHandler(fun _ _ -> this.StepForward())
        exitDebugButton.Click.AddHandler(fun _ _ -> this.ExitDebugMode())
        // trigger text changed on new line
        textEditor.TextChanged.AddHandler(fun _ _ -> this.TextChanged())
    
    member private this.TextChanged() =
        debounceTimer |> Option.iter (_.Dispose())
        
        let callback = fun _ ->
            Task.Run(fun () ->
                Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                    let code = textEditor.Text
                    if String.IsNullOrEmpty(code) then
                        standardOutput.Foreground <- SolidColorBrush(Colors.Black)
                        standardOutput.Text <- ""
                    else
                        match parse code with
                        | Ok (_, ast) ->
                            match Inference.inferProgram1 ast with
                            | Ok _ ->
                                    standardOutput.Foreground <- SolidColorBrush(Colors.Black)
                                    standardOutput.Text <- ""
                            | Error err ->
                                    standardOutput.Foreground <- SolidColorBrush(Colors.Red)
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

    member private this.SwitchToReplMode() =
        textEditor.Text <- "// REPL Mode"
        replState <- createInitialState()
        debugVM <- None
        this.UpdateOutputStreams()
        this.SetButtonStates(isRepl = true, isDebug = false)

    member private this.SwitchToStandardMode() =
        textEditor.Text <- "// Standard Mode"
        replState <- createInitialState()
        debugVM <- None
        this.UpdateOutputStreams()
        this.SetButtonStates(isRepl = false, isDebug = false)
        

    member private this.ExecuteCode() =
        let code = this.GetEditorText()
        let vm =
            match replState.VM with
            | Some vm -> vm
            | None -> createVM (initFunction "Main")
        match parseAndCompile code with
        | Some func ->
            let newVM, _ = interpretWithMode func (Some vm) false  // Use false for isRepl
            replState <- { replState with VM = Some newVM }
        | None -> printfn "Failed to compile code"
        
        match replState.VM with
        | Some vm when vm.Stack.Count > 0 ->
            let topValue = vm.Stack.[vm.Stack.Count - 1]
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
        | _ -> ()

        this.UpdateOutputStreams()

    member private this.StartDebugMode() =
        let code = this.GetEditorText()

        match parseAndCompile code with
        | Some func ->
            let vm = createVM func
            debugVM <- Some vm
            this.UpdateOutputStreams()
            this.SetButtonStates(isRepl = false, isDebug = true)
            switchToReplButton.IsEnabled <- false
            switchToStandardButton.IsEnabled <- false
            exitDebugButton.IsEnabled <- true
        | None ->
            printfn "Failed to compile code for debugging"
            
    member private this.ExitDebugMode() =
        debugVM <- None
        this.SetButtonStates(isRepl = false, isDebug = false)
        switchToReplButton.IsEnabled <- true
        switchToStandardButton.IsEnabled <- true
        exitDebugButton.IsEnabled <- false
        
    member private this.StepForward() =
        match debugVM with
        | Some vm ->
            let steppedVM = stepVM vm
            debugVM <- Some steppedVM
            let currentFrame = getCurrentFrame steppedVM
            if currentFrame.IP >= currentFrame.Function.Chunk.Code.Count then
                this.SetButtonStates(isRepl = false, isDebug = true, canStepForward = false)
            this.UpdateOutputStreams()
        | None -> ()

    member private this.StepBack() =
        match debugVM with
        | Some vm ->
            let previousVM = stepBackVM vm
            debugVM <- Some previousVM
            let currentFrame = getCurrentFrame previousVM
            if currentFrame.IP = 0 then
                this.SetButtonStates(isRepl = false, isDebug = true, canStepBack = false)
            else
                this.SetButtonStates(isRepl = false, isDebug = true, canStepForward = true)
            this.UpdateOutputStreams()
        | None -> ()

    member private this.UpdateOutputStreams() =
        let vm = 
            match debugVM with
            | Some vm -> vm
            | None -> 
                match replState.VM with
                | Some vm -> vm
                | None -> createVM (initFunction "Main")
        constantPoolOutput.Text <- getStreamContent vm.Streams.ConstantPool
        disassemblyOutput.Text <- getStreamContent vm.Streams.Disassembly
        executionOutput.Text <- getStreamContent vm.Streams.Execution
        standardOutput.Text <- getStreamContent vm.Streams.StandardOutput
        globalsOutput.Text <- getStreamContent vm.Streams.Globals
        if vm.Frames.Count > 0 then
            let currentFrame = getCurrentFrame vm
            this.HighlightCurrentInstruction(currentFrame.IP)
        else
            ()
    member private this.SetButtonStates(isRepl: bool, isDebug: bool, ?canStepBack: bool, ?canStepForward: bool) =
        switchToReplButton.IsEnabled <- not isRepl && not isDebug
        switchToStandardButton.IsEnabled <- not isDebug
        executeButton.IsEnabled <- true
        debugButton.IsEnabled <- not isDebug
        stepBackButton.IsEnabled <- isDebug && (defaultArg canStepBack true)
        stepForwardButton.IsEnabled <- isDebug && (defaultArg canStepForward true)
        exitDebugButton.IsEnabled <- isDebug
        
    member private this.HighlightCurrentInstruction(ip: int) =
        ()

    member this.GetEditorText() : string =
        if textEditor <> null then
            textEditor.Text
        else
            ""

    member this.SetEditorText(text: string) =
        if textEditor <> null then
            textEditor.Text <- text