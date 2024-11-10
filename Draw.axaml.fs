namespace Vec3

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml



type DrawWindow() as this =
    inherit Window()
    let mutable canvasControl: Canvas = null
    static let mutable windowCount = 0
    static let offset = 30
    
    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        windowCount <- windowCount + 1
        this.Position <- PixelPoint(offset * windowCount, offset * windowCount)
        
    member private this.InitializeComponent() =
        canvasControl <- this.FindControl<Canvas>("Canvas")
        
    member this.Canvas
        with get() = canvasControl
        
    override this.OnClosed(e) =
        windowCount <- windowCount - 1
        if windowCount = 0 then
            windowCount <- 0
        base.OnClosed(e)