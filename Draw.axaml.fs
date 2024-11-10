namespace Vec3

open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Media
open Avalonia.Controls.Shapes
open Vec3.Interpreter.Backend.Types

type DrawWindow() as this =
    inherit Window()
    
    let mutable canvasControl: Canvas = null
    static let mutable windowCount = 0
    static let offset = 30
    let mutable currentVm: VM option = None
    
    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        windowCount <- windowCount + 1
        this.Position <- PixelPoint(offset * windowCount, offset * windowCount)
        this.Title <- "Vec3 Drawing"
        this.Width <- 800.0
        this.Height <- 600.0
        
    member private this.InitializeComponent() =
        canvasControl <- this.FindControl<Canvas>("Canvas")
        canvasControl.Background <- SolidColorBrush(Colors.White)
        
    member private this.ParseColor(colorStr: string) =
        try
            match colorStr.ToLowerInvariant() with
            | "red" -> Colors.Red
            | "blue" -> Colors.Blue
            | "green" -> Colors.Green
            | "yellow" -> Colors.Yellow
            | "black" -> Colors.Black
            | "white" -> Colors.White
            | _ -> 
                match Color.TryParse(colorStr) with
                | true, color -> color
                | false, _ -> Colors.Black
        with _ -> Colors.Black
        
    member this.DrawShape(vm: VM, shape) =
        currentVm <- Some vm
        let (w, h, x, y, color) = match shape with VShape(x, y, w, h, c) -> (x, y, w, h, c)
        
        let colorBrush = SolidColorBrush(this.ParseColor(color))
        
        // Determine shape type based on parameters and create appropriate shape
        let shape =
            if abs(w - h) < 0.001 then
                // Circle/Ellipse
                let circle = Ellipse()
                circle.Width <- abs w
                circle.Height <- abs h
                circle.Fill <- colorBrush
                Canvas.SetLeft(circle, x - w/2.0)  // Center the circle
                Canvas.SetTop(circle, y - h/2.0)
                circle :> Shape
            else
                let rect = Rectangle()
                rect.Width <- abs w
                rect.Height <- abs h
                rect.Fill <- colorBrush
                Canvas.SetLeft(rect, x)
                Canvas.SetTop(rect, y)
                rect :> Shape
                
        canvasControl.Children.Add(shape)
        
    member this.Clear() =
        canvasControl.Children.Clear()
        
    member this.Canvas
        with get() = canvasControl
        
    override this.OnClosed(e) =
        windowCount <- windowCount - 1
        if windowCount = 0 then
            windowCount <- 0
        base.OnClosed(e)
