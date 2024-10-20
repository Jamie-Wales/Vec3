namespace Vec3

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open ScottPlot.Avalonia

type PlotWindow() as this =
    inherit Window()

    let mutable plotControl: AvaPlot = null

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()

    member private this.InitializeComponent() =
        plotControl <- this.FindControl<AvaPlot>("PlotControl")

    member this.PlotControl
        with get() = plotControl