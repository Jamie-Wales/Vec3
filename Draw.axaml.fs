namespace Vec3

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Markup.Xaml
open Avalonia.Media

type DrawWindow() as this =
    inherit Window()

    let mutable plotControl: Canvas = null
    static let mutable windowCount = 0
    static let offset = 30

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        windowCount <- windowCount + 1
        this.Position <- PixelPoint(offset * windowCount, offset * windowCount)

    member private this.InitializeComponent() =
        plotControl <- this.FindControl<Canvas>("Canvas")
        
        let rectangle = Rectangle()
        rectangle.Width <- 100.0
        rectangle.Height <- 50.0
        rectangle.Fill <- SolidColorBrush(Colors.Blue)
        
        Canvas.SetLeft(rectangle, 50.0)
        Canvas.SetTop(rectangle, 100.0)
        
        plotControl.Children.Add(rectangle)
    member this.PlotControl 
        with get() = plotControl

    override this.OnClosed(e) =
        windowCount <- windowCount - 1
        if windowCount = 0 then
            windowCount <- 0  // Reset position when all windows are closed
        base.OnClosed(e)
