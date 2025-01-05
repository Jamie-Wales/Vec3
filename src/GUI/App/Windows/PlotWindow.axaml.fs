namespace GUI.App.Windows

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open ScottPlot
open ScottPlot.Avalonia
open Avalonia.Input
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Repl
open Vec3.Interpreter

/// <summary>
/// Represents a window for plotting graphs.
/// </summary>
type PlotWindow() as this =
    inherit Window()
    
    let mutable plotControl: AvaPlot = null
    let mutable inputBox: TextBox = null
    let mutable currentVM: VM option = None
    static let mutable windowCount = 0
    static let offset = 30
    
    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        windowCount <- windowCount + 1
        this.Position <- PixelPoint(offset * windowCount, offset * windowCount)
        this.Width <- 800.0
        this.Height <- 600.0
        
    /// <summary>
    /// Initializes the components of the window.
    /// </summary>
    member private this.InitializeComponent() =
        plotControl <- this.FindControl<AvaPlot>("PlotControl")
        inputBox <- this.FindControl<TextBox>("InputBox")
        
        if plotControl <> null then
            plotControl.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Stretch
            plotControl.VerticalAlignment <- Avalonia.Layout.VerticalAlignment.Stretch
            
        if inputBox <> null then
            inputBox.KeyDown.Add(fun e ->
                if e.Key = Key.Enter then
                    this.HandleInput(inputBox.Text)
                    inputBox.Text <- ""
            )
    
    /// <summary>
    /// Refreshes the plots in the window.
    /// </summary>
    /// <param name="vm">The virtual machine currently in use</param>
    /// <returns>Unit</returns>
    member private this.UpdatePlots(vm: VM) =
        for value in vm.Plots do
            match value with
            | VPlotFunction(title, sf, start, end_, area) ->
                let f = SymbolicExpression.toBuiltin sf
                let p = plotControl.Plot.Add.Function(f)
                p.LegendText <- $"f(x) = {SymbolicExpression.toString sf}"
                plotControl.Plot.Title(title)
                match start, end_, area with
                | Some s, Some e, Some a ->
                    let startHeight = f s
                    let endHeight = f e
                    plotControl.Plot.Add.Line(s, 0.0, s, startHeight) |> ignore
                    plotControl.Plot.Add.Line(e, 0.0, e, endHeight) |> ignore
                    plotControl.Plot.Add.Annotation($"Area: {a}") |> ignore
                | _ -> ()
            | VPlotData(title, xs, ys, plotType) ->
                let extractNumber = function
                    | VNumber(VFloat f) -> f
                    | VNumber(VInteger i) -> float i
                    | _ -> 0.0
                    
                let x = List.map extractNumber xs |> Array.ofList
                let y = List.map extractNumber ys |> Array.ofList
                
                match plotType with
                | Scatter -> plotControl.Plot.Add.Scatter(x, y) |> ignore
                | Line -> plotControl.Plot.Add.Line(x[0], y[0], x[1], y[1]) |> ignore
                | Bar -> plotControl.Plot.Add.Bars(y, x) |> ignore
                | Signal -> plotControl.Plot.Add.Signal(y) |> ignore
                | _ -> ()
                
                plotControl.Plot.Title(title)
            | VPlotFunctions(title, fs) ->
                for sf in fs do
                    let f = SymbolicExpression.toBuiltin sf
                    let p = plotControl.Plot.Add.Function(f)
                    p.LegendText <- $"f(x) = {SymbolicExpression.toString sf}"
                plotControl.Plot.Title(title)
            | _ -> ()
            
        plotControl.Refresh()
        
    /// <summary>
    /// Handles the input from the user.
    /// </summary>
    /// <param name="input">The input from the user</param>
    /// <returns>Unit</returns>
    member private this.HandleInput(input: string) =
        try
            match currentVM with
            | Some vm ->
                match noTcParseAndCompile input vm false with
                | Some compiledVM ->
                    currentVM <- Some (run compiledVM)
                    let topOfStack = compiledVM.Stack[Seq.length vm.Stack - 1]
                    
                    // allows for plotting functions directly
                    match topOfStack with
                    | VClosure(_, Some f)
                    | VFunction(_, Some f) ->
                        let plotData = VPlotFunction ("", f, None, None, None)
                        compiledVM.Plots.Add(plotData)
                    | _ -> ()
                    this.UpdatePlots(compiledVM)
                | None -> ()
            | None -> ()
        with ex ->
            printfn $"Error: %s{ex.Message}"
            
    /// <summary>
    /// The plot control of the window.
    /// </summary>
    member this.PlotControl = plotControl
    
    /// <summary>
    /// Sets the virtual machine of the window.
    /// </summary>
    /// <param name="vm">The virtual machine</param>
    /// <returns>Unit</returns>
    member this.SetVM(vm: VM) =
        currentVM <- Some vm
    
    /// <summary>
    /// On closed event handler.
    /// </summary>
    /// <param name="e">The event arguments</param>
    /// <returns>Unit</returns>
    override this.OnClosed(e) =
        windowCount <- windowCount - 1
        if windowCount = 0 then
            windowCount <- 0
        base.OnClosed(e)