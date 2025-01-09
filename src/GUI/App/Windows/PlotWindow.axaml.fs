namespace GUI.App.Windows

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open ScottPlot
open ScottPlot.Avalonia
open Avalonia.Input
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Repl
open Vec3.Interpreter
open Vec3.Interpreter.Parser
open Vec3.Interpreter.SymbolicExpression

type PlotWindow() as this =
    inherit Window()
    
    let mutable plotControl: AvaPlot = null
    let mutable inputBox: TextBox = null
    let mutable currentVM: VM option = None
    
    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        this.Width <- 800.0
        this.Height <- 600.0
        
    member private this.InitializeComponent() =
        plotControl <- this.FindControl<AvaPlot>("PlotControl")
        inputBox <- this.FindControl<TextBox>("InputBox")
        
        if plotControl <> null then
            plotControl.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Stretch
            plotControl.VerticalAlignment <- Avalonia.Layout.VerticalAlignment.Stretch
            
            let yL = plotControl.Plot.Add.VerticalLine(0.0)
            yL.Color <- Color(byte 0, byte 0, byte 0)
            let xL = plotControl.Plot.Add.HorizontalLine(0.0)
            xL.Color <- Color(byte 0, byte 0, byte 0)
            plotControl.Plot.XLabel("x")
            plotControl.Plot.YLabel("y")
            plotControl.Plot.Legend.Location <- Alignment.UpperRight
            plotControl.Plot.ShowLegend() |> ignore
            plotControl.Refresh()
            
        if inputBox <> null then
            inputBox.KeyDown.Add(fun e ->
                if e.Key = Key.Enter then
                    this.HandleInput(inputBox.Text)
                    inputBox.Text <- ""
            )
    
    member private this.HandleInput(input: string) =
        try
            let addToPlot symExpr =
                let f = toBuiltin symExpr
                let p = plotControl.Plot.Add.Function(f)
                p.LegendText <- $"f(x) = {toString symExpr}"
                plotControl.Refresh()
            
            match parse input false with
            | Ok(_, [SExpression(expr, _)]) ->
                let symExpr = fromExpr expr
                addToPlot symExpr
            | _ -> ()

        with ex ->
            printfn $"Error: %s{ex.Message}"
            
    member this.PlotControl = plotControl
    
    member this.SetVM(vm: VM) =
        currentVM <- Some vm

    member this.Clear() =
        if plotControl <> null then
            plotControl.Plot.Clear()
            
            // Re-add axes
            let yL = plotControl.Plot.Add.VerticalLine(0.0)
            yL.Color <- Color(byte 0, byte 0, byte 0)
            let xL = plotControl.Plot.Add.HorizontalLine(0.0)
            xL.Color <- Color(byte 0, byte 0, byte 0)
            
            plotControl.Plot.XLabel("x")
            plotControl.Plot.YLabel("y")
            plotControl.Plot.Legend.Location <- Alignment.UpperRight
            plotControl.Plot.ShowLegend() |> ignore
            plotControl.Refresh()