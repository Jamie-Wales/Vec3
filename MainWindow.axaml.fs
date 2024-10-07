namespace Vec3

open Avalonia.Controls
open Avalonia.Markup.Xaml
open AvaloniaEdit
open Avalonia.Media
open TextMateSharp.Grammars
open AvaloniaEdit.TextMate
open Avalonia.Interactivity
open Vec3.Interpreter.Repl
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Backend.Compiler


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

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
    member private this.InitializeComponent() =
        textEditor <- this.FindControl<TextEditor>("Editor")
        switchToReplButton <- this.FindControl<Button>("SwitchToReplButton")
        switchToStandardButton <- this.FindControl<Button>("SwitchToStandardButton")  // Button for switching to standard mode
        executeButton <- this.FindControl<Button>("ExecuteButton")
        debugButton <- this.FindControl<Button>("DebugButton")
        stepBackButton <- this.FindControl<Button>("StepBackButton")
        stepForwardButton <- this.FindControl<Button>("StepForwardButton")
        constantPoolOutput <- this.FindControl<TextBlock>("ConstantPoolOutput")
        disassemblyOutput <- this.FindControl<TextBlock>("DisassemblyOutput")
        executionOutput <- this.FindControl<TextBlock>("ExecutionOutput")
        standardOutput <- this.FindControl<TextBlock>("StandardOutput")
        exitDebugButton <- this.FindControl<Button>("ExitDebugButton")  // Button for exiting debug mode
        globalsOutput <- this.FindControl<TextBlock>("GlobalsOutput")

        // Verify that all controls are not null
        if textEditor = null then
            raise (System.Exception("TextEditor not found"))
        if switchToReplButton = null then
            raise (System.Exception("SwitchToReplButton not found"))
        if switchToStandardButton = null then
            raise (System.Exception("SwitchToStandardButton not found"))
        if executeButton = null then
            raise (System.Exception("ExecuteButton not found"))
        if debugButton = null then
            raise (System.Exception("DebugButton not found"))
        if stepBackButton = null then
            raise (System.Exception("StepBackButton not found"))
        if stepForwardButton = null then
            raise (System.Exception("StepForwardButton not found"))
        if constantPoolOutput = null then
            raise (System.Exception("ConstantPoolOutput not found"))
        if disassemblyOutput = null then
            raise (System.Exception("DisassemblyOutput not found"))
        if executionOutput = null then
            raise (System.Exception("ExecutionOutput not found"))
        if standardOutput = null then
            raise (System.Exception("StandardOutput not found"))
        if exitDebugButton = null then
            raise (System.Exception("ExitDebugButton not found"))
        if globalsOutput = null then
            raise (System.Exception("GlobalsOutput not found"))
            
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
        match replState.VM with
        | Some vm ->
            match debugVM with
            | Some debugVm ->
                let finalVM = run debugVm
                debugVM <- Some finalVM
            | None ->
                match parseAndCompile code with
                | Some chunk ->
                    let (newVM, _) = replExecute chunk (Some vm)
                    replState <- { replState with VM = Some newVM }
                | None -> printfn "Failed to compile code"
        | None ->
            match parseAndCompile code with
            | Some chunk ->
                let (newVM, _) = replExecute chunk None
                replState <- { replState with VM = Some newVM }
            | None -> printfn "Failed to compile code"
        
        this.UpdateOutputStreams()

    member private this.StartDebugMode() =
        let code = this.GetEditorText()

        match parseAndCompile code with
        | Some chunk ->
            let vm = createVM chunk
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
            if steppedVM.IP >= steppedVM.Chunk.Code.Count then
                this.SetButtonStates(isRepl = false, isDebug = true, canStepForward = false)
            this.UpdateOutputStreams()
        | None -> ()

    member private this.StepBack() =
        match debugVM with
        | Some vm ->
            let previousVM = stepBackVM vm
            debugVM <- Some previousVM
            if previousVM.IP = 0 then
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
                | None -> createVM (emptyChunk())

        constantPoolOutput.Text <- getStreamContent vm.Streams.ConstantPool
        disassemblyOutput.Text <- getStreamContent vm.Streams.Disassembly
        executionOutput.Text <- getStreamContent vm.Streams.Execution
        standardOutput.Text <- getStreamContent vm.Streams.StandardOutput
        globalsOutput.Text <- getStreamContent vm.Streams.Globals
        this.HighlightCurrentInstruction(vm.IP)

    member private this.SetButtonStates(isRepl: bool, isDebug: bool, ?canStepBack: bool, ?canStepForward: bool) =
        switchToReplButton.IsEnabled <- not isRepl && not isDebug
        switchToStandardButton.IsEnabled <- not isDebug
        executeButton.IsEnabled <- true
        debugButton.IsEnabled <- not isDebug
        stepBackButton.IsEnabled <- isDebug && (defaultArg canStepBack true)
        stepForwardButton.IsEnabled <- isDebug && (defaultArg canStepForward true)
        exitDebugButton.IsEnabled <- isDebug
        
    member private this.HighlightCurrentInstruction(ip: int) =
        // Placeholder for highlighting current instruction
        ()

    member this.GetEditorText() : string =
        if textEditor <> null then
            textEditor.Text
        else
            ""

    member this.SetEditorText(text: string) =
        if textEditor <> null then
            textEditor.Text <- text