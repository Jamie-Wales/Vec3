namespace GUI.App.Windows

open System
open System.IO
open Avalonia.Controls
open Avalonia.Markup.Xaml
open AvaloniaEdit
open Avalonia.Media
open TextMateSharp.Grammars
open AvaloniaEdit.TextMate
open Vec3
open Vec3.Interpreter.Repl
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Typing
open System.Threading.Tasks
open System.Threading
open Vec3.Transpiler.Transpiler
open Vec3.Interpreter.SymbolicExpression

/// <summary>
/// The main UI window used for the Vec3 editor.
/// </summary>
type MainWindow() as this =
    inherit Window()

    let mutable textEditor: TextEditor = null
    
    let mutable transpileButton: Button = null
    let mutable openFileButton: Button = null
    let mutable textMateInstallation: TextMate.Installation = null
    let mutable replInput: TextEditor = null

    let mutable openNotebookButton: Button = null
    let mutable loadButton: Button = null
    let mutable runButton: Button = null
    let mutable standardOutput: TextBlock = null
    let mutable replState: VM = createNewVM (initFunction "Main")

    let debounceTime = 500
    let mutable debounceTimer = None: Timer option

    /// <summary>
    /// First message displayed to the user.
    /// </summary>
    let welcomeMessage =
        """Welcome to Vec3 Editor!
Use the editor on the left to write your Vec3 code.
Click 'Load' or 'Shift+L' to execute the code.
REPL for quick commands click 'Play' or Shift+Enter.
Type 'help()' in the REPL for more information."""

    /// <summary>
    /// Initial editor code
    /// </summary>
    let initialCode =
        """// Vec3 Editor Example
let f = (x) -> x^2.0 - 2.0

// Differentiate the function
let diff = differentiate(f)
// Integrate the function
let integral = integrate(f)
// Find the tangent at x = -3
let tang = tangentFunc(f, -3.0)

print(diff)

plotFuncs("Function differential", [f, diff, tang, integral])

let areaUnder = findIntegral(f, 0.0, 10.0)

print(areaUnder)

let y = (z) -> cos(z)

let r = (t) -> 2.9 * tan(t) 
let a = 1.0
let b = 2.0
let tolerance = 1e-6
let max = 100

// Find the root of the function
let root = bisection(f, a, b, tolerance, max)

print(root)

// Function plotting
plotFunc("Polynomial", f)
plotFunc("Cos", cos)
plotFunc("Tan", r)

// Data plotting
let data = {
    title="My Plot",
    x=[1, 2, 3, 4],
    y=[1, 4, 9, 16],
    ptype="bar" 
}

plot(data)

// Drawing shapes
let data = {
    x = 100.0,
    y = 100.0,
    width = 50.0,
    height = 50.0,
    colour = "red",
    trace = true
}

let id = draw(data)

// Event handling
on(id, Keys.Right, (state) -> { x = state.x + 10.0, y = cos(state.x) * 10.0 + 100.0 })
on(id, Keys.Left, (state) -> { x = state.x - 10.0, y = cos(state.x) * 10.0 + 100.0 })
on(id, Keys.Down, (state) -> { x = state.x, y = state.y + 20.0 })
on(id, Keys.Up, (state) -> { x = state.x, y = state.y - 20.0 })

"""

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()

    /// <summary>
    /// Sets up the editor with the given registry options.
    /// </summary>
    /// <param name="registryOptions">The registry options</param>
    /// <returns>Unit</returns>
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

    /// <summary>
    /// Sets up the REPL input with the given registry options.
    /// </summary>
    /// <param name="registryOptions">The registry options</param>
    /// <returns>Unit</returns>
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

    /// <summary>
    /// Initializes the components of the window.
    /// </summary>
    member private this.InitializeComponent() =
        textEditor <- this.FindControl<TextEditor>("Editor")
        loadButton <- this.FindControl<Button>("LoadButton")
        standardOutput <- this.FindControl<TextBlock>("StandardOutput")
        replInput <- this.FindControl<TextEditor>("ReplInput")
        runButton <- this.FindControl<Button>("RunButton")
        openNotebookButton <- this.FindControl<Button>("OpenNotebookButton")
        openFileButton <- this.FindControl<Button>("OpenFileButton")
        openFileButton.Click.AddHandler(fun _ _ -> this.OpenFileAsync())
        standardOutput.Foreground <- SolidColorBrush(Colors.White)
        standardOutput.Text <- welcomeMessage
        transpileButton <- this.FindControl<Button>("TranspileButton")
        transpileButton.Click.AddHandler(fun _ _ -> this.TranspileCodeAsync())  

        let registryOptions = RegistryOptions(ThemeName.QuietLight)
        this.SetupEditor(registryOptions)
        this.SetupReplInput(registryOptions)

        loadButton.Click.AddHandler(fun _ _ -> this.LoadCode())
        textEditor.TextChanged.AddHandler(fun _ _ -> this.TextChanged())
        runButton.Click.AddHandler(fun _ _ -> this.run ())
        openNotebookButton.Click.AddHandler(fun _ _ -> this.OpenNotebook())
        

    /// <summary>
    /// Open the notebook window
    /// </summary>
    member private this.OpenNotebook() =
        let notebookWindow = NotebookWindow()
        notebookWindow.Show()

    /// <summary>
    /// Initializes the plot window with the given title.
    /// </summary>
    member private this.CreatePlotWindow(title: string) =
        let plotWindow = PlotWindow()
        plotWindow.Title <- title
        plotWindow.PlotControl.Plot.Clear()
        plotWindow

    /// <summary>
    /// Open a file in the editor asynchronously.
    /// </summary>
    member private this.OpenFileAsync() =
        task {
            try
                let dialog = OpenFileDialog()
                // Add .vec3 as the primary file type
                dialog.Filters.Add(FileDialogFilter(Name = "Vec3 Files", Extensions = ResizeArray["vec3"]))
                // Add other supported file types
                dialog.Filters.Add(FileDialogFilter(Name = "Text Files", Extensions = ResizeArray["txt"; "fs"; "fsx"]))
                // Add an "All Files" option
                dialog.Filters.Add(FileDialogFilter(Name = "All Files", Extensions = ResizeArray["*"]))
                
                let! result = dialog.ShowAsync(this)
                match result with
                | null -> ()  // User cancelled
                | files ->
                    // Use Task.Run for file operations to avoid blocking UI
                    let! fileContent = 
                        Task.Run(fun () ->
                            let filePath = files[0]  // Take first selected file
                            File.ReadAllText(filePath))
                    
                    // Update UI on the UI thread
                    do! Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                        if textEditor <> null then
                            textEditor.Text <- fileContent
                            standardOutput.Foreground <- SolidColorBrush(Colors.White)
                            standardOutput.Text <- "File loaded successfully")
            with ex ->
                do! Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                    standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                    standardOutput.Text <- $"Error loading file: {ex.Message}")
        } |> ignore
        
    member private this.GenPoints (f: Expression, start: float, end_: float, step: float): (float * float) list =
        let f = toBuiltin f
        let rec loop pos acc =
            if pos > end_ then
                acc
            else
                let y = f pos
                loop (pos + step) ((pos, y) :: acc)
        loop start []
    
    /// <summary>
    /// Plot the given data currently in the VM.
    /// </summary>
    /// <param name="vm">The current vm</param>
    /// <returns>Unit</returns>
    /// <summary>
    /// Plot the given data currently in the VM.
    /// </summary>
    /// <param name="vm">The current vm</param>
    /// <returns>Unit</returns>
    member private this.HandlePlotOutput(vm: VM) =
        let shapes =
            vm.Canvas
            |> Seq.choose (function
                | VShape _ as shape -> Some shape
                | VShapes _ as shapes -> Some shapes
                | _ -> None)
            |> Seq.toList

        if not (List.isEmpty shapes) then
            let drawWindow = DrawWindow()
            drawWindow.Show()

            Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                for shape in shapes do
                    drawWindow.DrawShape(vm, shape)

                for eventListener in vm.EventListeners do
                    let shapeId, listenerId, func = eventListener
                    drawWindow.AddEventListener(vm, shapeId, listenerId, func)

                vm.EventListeners.Clear())
            |> ignore

        // Handle other plot types
        for value in vm.Plots do
            match value with
            | VShape _ ->
                // Already handled above
                ()
            | VPlotData(title, xs, ys, plotType) ->
                let plotWindow = PlotWindow()
                plotWindow.PlotControl.Plot.Title(title)
                plotWindow.SetVM(vm)  
                let yL = plotWindow.PlotControl.Plot.Add.VerticalLine(0.0)
                yL.Color <- ScottPlot.Color(byte 0, byte 0, byte 0)
                let xL = plotWindow.PlotControl.Plot.Add.HorizontalLine(0.0)
                xL.Color <- ScottPlot.Color(byte 0, byte 0, byte 0)
                let extractNumber =
                    function
                    | VNumber(VFloat f) -> f
                    | VNumber(VInteger i) -> float i
                    | _ -> 0.0

                let x = List.map extractNumber xs |> Array.ofList
                let y = List.map extractNumber ys |> Array.ofList

                match plotType with
                | Scatter -> plotWindow.PlotControl.Plot.Add.Scatter(x, y) |> ignore
                | Line -> plotWindow.PlotControl.Plot.Add.Line(x[0], y[0], x[1], y[1]) |> ignore
                | Bar -> plotWindow.PlotControl.Plot.Add.Bars(y, x) |> ignore
                | Histogram -> failwith "todo"
                | Signal -> plotWindow.PlotControl.Plot.Add.Signal(y) |> ignore

                plotWindow.PlotControl.Plot.Title(title)
                plotWindow.PlotControl.Refresh()
                plotWindow.Show()
            | VPlotFunction(title, sf, start, end_, area) ->
                let f = toBuiltin sf
                let plotWindow = PlotWindow()
                plotWindow.Title <- title
                plotWindow.SetVM(vm)
                
                // Draw x and y axis
                let yL = plotWindow.PlotControl.Plot.Add.VerticalLine(0.0)
                yL.Color <- ScottPlot.Color(byte 0, byte 0, byte 0)
                let xL = plotWindow.PlotControl.Plot.Add.HorizontalLine(0.0)
                xL.Color <- ScottPlot.Color(byte 0, byte 0, byte 0)
                
                // Add function plot
                let p = plotWindow.PlotControl.Plot.Add.Function(f)
                p.LegendText <- $"f(x) = {toString sf}"
                
                match start, end_, area with
                | Some start, Some end_, Some area ->
                    let startHeight = f start
                    let endHeight = f end_

                    plotWindow.PlotControl.Plot.Add.Line(start, 0.0, start, startHeight) |> ignore
                    plotWindow.PlotControl.Plot.Add.Line(end_, 0.0, end_, endHeight) |> ignore
                    plotWindow.PlotControl.Plot.Add.Annotation($"Area: %f{area}") |> ignore
                | _ -> ()
                
                plotWindow.PlotControl.Plot.XLabel("x")
                plotWindow.PlotControl.Plot.YLabel("y")
                plotWindow.PlotControl.Plot.Title(title)
                plotWindow.PlotControl.Plot.Legend.Padding <- ScottPlot.PixelPadding(10.0f, 10.0f, 10.0f,30.0f)
                plotWindow.PlotControl.Plot.Legend.Location <- ScottPlot.Alignment.UpperRight
                plotWindow.PlotControl.Plot.Legend.FontSize <- 12f
                plotWindow.PlotControl.Plot.Legend.Orientation <- ScottPlot.Orientation.Horizontal
                plotWindow.PlotControl.Plot.Legend.Margin <- ScottPlot.PixelPadding(10.0f, 10.0f, 10.0f, 30.0f)
                plotWindow.PlotControl.Plot.ShowLegend() |> ignore
                plotWindow.PlotControl.Refresh()
                plotWindow.Show()

            | VPlotFunctions(title, fs) ->
                let plotWindow = PlotWindow()
                plotWindow.Title <- title
                
                let yL = plotWindow.PlotControl.Plot.Add.VerticalLine(0.0)
                yL.Color <- ScottPlot.Color(byte 0, byte 0, byte 0)
                let xL = plotWindow.PlotControl.Plot.Add.HorizontalLine(0.0)
                xL.Color <- ScottPlot.Color(byte 0, byte 0, byte 0)
                
                for sf in fs do
                    let f = toBuiltin sf
                    let p = plotWindow.PlotControl.Plot.Add.Function(f)
                    p.LegendText <- $"f(x) = {toString sf}"
                
                // Configure legend
                plotWindow.PlotControl.Plot.Legend.Padding <- ScottPlot.PixelPadding(10.0f, 10.0f, 10.0f, 30.0f)
                plotWindow.PlotControl.Plot.Legend.Location <- ScottPlot.Alignment.UpperRight
                plotWindow.PlotControl.Plot.Legend.FontSize <- 11f
                plotWindow.PlotControl.Plot.Legend.Orientation <- ScottPlot.Orientation.Horizontal
                plotWindow.PlotControl.Plot.Legend.Margin <- ScottPlot.PixelPadding(10.0f, 10.0f, 10.0f, 30.0f)
                plotWindow.PlotControl.Plot.XLabel("x")
                plotWindow.PlotControl.Plot.YLabel("y")
                plotWindow.PlotControl.Plot.Title(title)
                plotWindow.PlotControl.Plot.ShowLegend() |> ignore
                plotWindow.PlotControl.Refresh()
                plotWindow.Show()
            | _ -> ()

        vm.Plots.Clear()

    /// <summary>
    /// Run the code currently in the editor.
    /// </summary>
    member private this.LoadCode() =
        replState <- createNewVM (initFunction "Main")
        let code = this.GetEditorText()

        try
            match parseAndCompile code replState with
            | Some vm ->
                replState <- run vm
                standardOutput.Foreground <- SolidColorBrush(Colors.White)
                let outputText = String.concat "\n" output.Value
                standardOutput.Text <- $"Vec3 code loaded and executed:\n%s{outputText}"
                this.HandlePlotOutput(replState)
                vm.Streams.StandardOutput.Value <- Seq.empty
                vm.Stack.Clear()
            | None ->
                standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                standardOutput.Text <- "Failed to compile code"
        with ex ->
            printfn $"Error: {ex}"
            standardOutput.Foreground <- SolidColorBrush(Colors.Red)
            standardOutput.Text <- $"Error: %s{ex.Message}"

    /// <summary>
    /// Feedback for the user when the text changes.
    /// </summary>
    member private this.TextChanged() =
        // Dispose of the previous timer
        debounceTimer |> Option.iter _.Dispose()

        let callback =
            fun _ ->
                Task.Run(fun () ->
                    Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                        let code = textEditor.Text

                        if String.IsNullOrEmpty(code) then
                            standardOutput.Foreground <- SolidColorBrush(Colors.White)
                            standardOutput.Text <- ""
                        else
                            match parse code true with
                            | Ok(_, ast) ->
                                match Inference.inferProgram1 ast with
                                | Ok _ ->
                                    standardOutput.Foreground <- SolidColorBrush(Colors.White)
                                    standardOutput.Text <- "No errors"
                                | Error err ->
                                    standardOutput.Foreground <- SolidColorBrush(Colors.Yellow)
                                    standardOutput.Text <- Exceptions.formatTypeErrors err (if Seq.length err > 3 then 3 else Seq.length err)
                            | Error(err, state) ->
                                standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                                standardOutput.Text <- formatParserError err state)
                    |> ignore)
                |> ignore

        // Timer runs the callback after the debounce time
        debounceTimer <- Some(new Timer(callback, null, debounceTime, Timeout.Infinite))

    /// <summary>
    /// Transpile the code in the editor to C.
    /// </summary>
    member private this.TranspileCodeAsync() =
        task {
            try
                let dialog = OpenFolderDialog()
                dialog.Title <- "Select Output Directory"
                
                let! result = dialog.ShowAsync(this)
                match result with
                | null -> ()  // User cancelled
                | outputDir ->
                    let config = createConfig (Some outputDir)
                    
                    // Create temporary file for the current editor content
                    let tempFile = Path.Combine(Path.GetTempPath(), "temp.vec3")
                    File.WriteAllText(tempFile, this.GetEditorText())
                    
                    match transpile tempFile config with
                    | Ok exePath -> 
                        let mainCPath = Path.Combine(outputDir, "vec3_program/src", "main.c")
                        let! mainCContent = Task.Run(fun () -> File.ReadAllText(mainCPath))
                        do! Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                            standardOutput.Foreground <- SolidColorBrush(Colors.White)
                            standardOutput.Text <- $"Transpilation successful!\nOutput Directory: {outputDir}\nExecutable: {exePath}\n\nGenerated main.c in {mainCPath}:\n{mainCContent}")
                    | Error err ->
                        do! Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                            standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                            standardOutput.Text <- $"Transpilation error: {err}")
                    
                    // Clean up temporary file
                    File.Delete(tempFile)
                    
            with ex ->
                do! Avalonia.Threading.Dispatcher.UIThread.InvokeAsync(fun () ->
                    standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                    standardOutput.Text <- $"Transpilation error: {ex.Message}")
        } |> ignore
    
    /// <summary>
    /// Initializes the color scheme of the editor.
    /// </summary>
    member private this.ApplyColorScheme() =
        if textMateInstallation <> null then
            let applyColor colorKey (action: IBrush -> unit) =
                let mutable colorString = ""

                if textMateInstallation.TryGetThemeColor(colorKey, &colorString) then
                    match Color.TryParse(colorString) with
                    | true, color ->
                        let brush = SolidColorBrush(color)
                        action brush
                    | _ -> ()

            applyColor "editor.background" (fun brush -> textEditor.Background <- brush)
            applyColor "editor.foreground" (fun brush -> textEditor.Foreground <- brush)
            applyColor "editorLineNumber.foreground" (fun brush -> textEditor.LineNumbersForeground <- brush)

    /// <summary>
    /// Runs the repl input.
    /// </summary>
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
                try
                    match noTcParseAndCompile code replState false with
                    | Some vm ->
                        let previousOutput = standardOutput.Text
                        let oldOutputLength = Seq.length replState.Streams.StandardOutput.Value
                        replState <- run vm
                        standardOutput.Foreground <- SolidColorBrush(Colors.White)
                        let topOfStack = vm.Stack[Seq.length vm.Stack - 1] |> valueToString

                        let newOutput =
                            replState.Streams.StandardOutput.Value
                            |> Seq.skip oldOutputLength
                            |> String.concat "\n"

                        if not (String.IsNullOrWhiteSpace(newOutput)) then
                            standardOutput.Text <- $"%s{previousOutput}\n<Vec3> %s{code}\n%s{newOutput}\n%s{topOfStack}"
                        else
                            standardOutput.Text <- $"%s{previousOutput}\n<Vec3> %s{code}\n%s{topOfStack}"

                        this.HandlePlotOutput(replState)
                        replInput.Text <- ""
                    | None ->
                        standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                        standardOutput.Text <- $"%s{standardOutput.Text}\nFailed to compile: %s{replInput.Text}"
                with ex ->
                    printfn $"Error: {ex}"
                    standardOutput.Foreground <- SolidColorBrush(Colors.Red)
                    standardOutput.Text <- $"%s{standardOutput.Text}\nError: %s{ex.Message}"
                    replInput.Text <- ""

    /// <summary>
    /// Helper function to get the text from the editor.
    /// </summary>
    member private this.GetEditorText() : string =
        if textEditor <> null then textEditor.Text else ""

    /// <summary>
    /// Sets the text of the editor.
    /// </summary>
    member this.SetEditorText(text: string) =
        if textEditor <> null then
            textEditor.Text <- text
