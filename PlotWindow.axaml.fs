namespace Vec3

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open ScottPlot.Avalonia

type PlotWindow() as this =
    inherit Window()

    let mutable plotControl: AvaPlot = null
    static let mutable windowCount = 0
    static let offset = 30

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        windowCount <- windowCount + 1
        this.Position <- PixelPoint(offset * windowCount, offset * windowCount)

        this.Width <- 420.0  
        this.Height <- 360.0 

    member private this.InitializeComponent() =
        plotControl <- this.FindControl<AvaPlot>("PlotControl")
        if plotControl <> null then
            plotControl.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Center

    member this.PlotControl 
        with get() = plotControl

    override this.OnClosed(e) =
        windowCount <- windowCount - 1
        if windowCount = 0 then
            windowCount <- 0  
        base.OnClosed(e)