namespace GUI.App.Windows

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Input
open Avalonia.Media
open Avalonia.Controls.Shapes
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.VM

/// <summary>
/// Information about a shape to be drawn.
/// </summary>
type ShapeInfo =
    { x: float
      y: float
      w: float
      h: float
      color: string
      typ: string
      id: int
      trace: bool // Whether to draw a trail behind the shape when it moves
    }

/// <summary>
/// The window used for drawing shapes.
/// </summary>
type DrawWindow() as this =
    inherit Window()

    let mutable canvasControl: Canvas = null
    static let mutable windowCount = 0
    static let offset = 30
    let mutable currentVm: VM option = None

    let mutable shapes: Map<int, Shape> = Map.empty
    let mutable eventListeners: Map<int, (int * Function) list> = Map.empty
    let mutable shapeInfo: Map<int, ShapeInfo> = Map.empty

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        windowCount <- windowCount + 1
        this.Position <- PixelPoint(offset * windowCount, offset * windowCount)
        this.Title <- "Vec3 Drawing"
        this.Width <- 800.0
        this.Height <- 600.0
        this.Focusable <- true

        // Listen for arrow keys
        this.KeyDown.Add(fun e ->
            let listenerId = this.keyToInt (e.Key)
            this.handleEvent listenerId)

    /// <summary>
    /// Convert key to int
    /// </summary>
    member private this.keyToInt key =
        match key with
        | Key.Left -> 0
        | Key.Right -> 1
        | Key.Up -> 2
        | Key.Down -> 3
        | _ -> -1

    /// <summary>
    /// Initialize the component
    /// </summary>
    member private this.InitializeComponent() =
        canvasControl <- this.FindControl<Canvas>("Canvas")
        canvasControl.Background <- SolidColorBrush(Colors.White)
    
    /// <summary>
    /// Parse color string
    /// </summary>
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
        with _ ->
            Colors.Black

    /// <summary>
    /// Handle events for shapes
    /// </summary>
    member private this.handleEvent listenerId =
        let listeners = eventListeners.TryFind(listenerId)
        
        // Get canvas bounds
        let canvasWidth = canvasControl.Bounds.Width
        let canvasHeight = canvasControl.Bounds.Height
        
        // Ensure coordinates stay within bounds
        let clampX x width = 
            Math.Max(width/2.0, Math.Min(canvasWidth - width/2.0, x))
        
        let clampY y height = 
            Math.Max(height/2.0, Math.Min(canvasHeight - height/2.0, y))

        match listeners with
        | Some lst ->
            lst
            |> List.iter (fun (shapeId, func) ->
                let shape = shapes.TryFind(shapeId)
                let shapeInfo = shapeInfo.TryFind(shapeId)

                if Option.isNone shape || Option.isNone shapeInfo then
                    raise <| Exception $"Shape with id {shapeId} not found"
                else
                    let shape = Option.get shape
                    let info = Option.get shapeInfo

                    let prevX = Canvas.GetLeft(shape) + info.w/2.0
                    let prevY = Canvas.GetTop(shape) + info.h/2.0

                    let args =
                        VList(
                            [ VList([ VString "x"; VNumber(VFloat prevX) ], LIST)
                              VList([ VString "y"; VNumber(VFloat prevY) ], LIST) ],
                            RECORD
                        )

                    match currentVm with
                    | Some vm ->
                        let newVal = runFunction vm func [ args ]

                        match newVal with
                        | VList([ VList([ VString "y"; VNumber(VFloat y) ], _)
                                  VList([ VString "x"; VNumber(VFloat x) ], _) ],
                                _) ->
                            let boundedX = clampX x info.w
                            let boundedY = clampY y info.h
                            
                            if info.trace then
                                let trail = Avalonia.Controls.Shapes.Line()
                                trail.StartPoint <- Point(prevX, prevY)
                                trail.EndPoint <- Point(boundedX, boundedY)
                                trail.Stroke <- SolidColorBrush(Colors.Black)
                                trail.StrokeThickness <- 1.0
                                canvasControl.Children.Add(trail)

                            match shape with
                            | :? Rectangle as rect ->
                                Canvas.SetLeft(rect, boundedX - info.w/2.0)
                                Canvas.SetTop(rect, boundedY - info.h/2.0)
                            | :? Ellipse as circle ->
                                Canvas.SetLeft(circle, boundedX - info.w/2.0)
                                Canvas.SetTop(circle, boundedY - info.h/2.0)
                            | :? Avalonia.Controls.Shapes.Line as line ->
                                Canvas.SetLeft(line, boundedX)
                                Canvas.SetTop(line, boundedY)
                            | _ -> ()
                        | _ -> ()
                    | _ -> ())
        | None -> ()

    /// <summary>
    /// Draw a shape
    /// </summary>
    member this.DrawShape(vm: VM, shape: Value) =
        currentVm <- Some vm

        let w, h, x, y, color, typ, id, trail =
            match shape with
            | VShape(x, y, w, h, c, t, id, trail) -> (x, y, w, h, c, t, id, trail)
            | _ -> raise <| InvalidProgramException("Unknown shape value")

        let colorBrush = SolidColorBrush(this.ParseColor(color))
        
        // Get canvas bounds
        let canvasWidth = canvasControl.Bounds.Width
        let canvasHeight = canvasControl.Bounds.Height
        
        // Ensure coordinates stay within bounds
        let clampX x width = 
            Math.Max(width/2.0, Math.Min(canvasWidth - width/2.0, x))
        
        let clampY y height = 
            Math.Max(height/2.0, Math.Min(canvasHeight - height/2.0, y))

        // Determine shape type based on parameters and create appropriate shape
        let shape =
            match typ with
            | "rectangle" ->
                let rect = Rectangle()
                rect.Width <- abs w
                rect.Height <- abs h
                rect.Fill <- colorBrush
                let boundedX = clampX x (abs w)
                let boundedY = clampY y (abs h)
                Canvas.SetLeft(rect, boundedX - w/2.0) // Center the rectangle
                Canvas.SetTop(rect, boundedY - h/2.0)
                rect :> Shape
                
            | "circle" ->
                let circle = Ellipse()
                circle.Width <- abs w
                circle.Height <- abs h
                circle.Fill <- colorBrush
                let boundedX = clampX x (abs w)
                let boundedY = clampY y (abs h)
                Canvas.SetLeft(circle, boundedX - w/2.0) // Center the circle
                Canvas.SetTop(circle, boundedY - h/2.0)
                circle :> Shape
                
            | "line" ->
                let line = Avalonia.Controls.Shapes.Line()
                line.StartPoint <- Point(0, 0)
                line.EndPoint <- Point(w, h)
                line.Stroke <- colorBrush
                line.StrokeThickness <- 2.0
                let boundedX = clampX x w
                let boundedY = clampY y h
                Canvas.SetLeft(line, boundedX)
                Canvas.SetTop(line, boundedY)
                line :> Shape
            | _ -> 
                let rect = Rectangle() // Default to rectangle
                rect.Width <- abs w
                rect.Height <- abs h
                rect.Fill <- colorBrush
                Canvas.SetLeft(rect, clampX x (abs w))
                Canvas.SetTop(rect, clampY y (abs h))
                rect :> Shape

        shapes <- shapes.Add(id, shape)

        // Save shape information for later use
        shapeInfo <-
            shapeInfo.Add(
                id,
                { x = x
                  y = y
                  w = w
                  h = h
                  color = color
                  typ = typ
                  id = id
                  trace = trail }
            )

        canvasControl.Children.Add(shape)

    /// <summary>
    /// Add event listener
    /// </summary>
    member this.AddEventListener(vm: VM, shapeId: int, listenerId: int, func: Function) =
        currentVm <- Some vm
        let listeners = eventListeners.TryFind(listenerId)

        let listeners =
            match listeners with
            | Some lst -> lst
            | None -> []

        eventListeners <- eventListeners.Add(listenerId, (shapeId, func) :: listeners)
    
    /// <summary>
    /// Clear all shapes
    /// </summary>
    member this.Clear() = 
        canvasControl.Children.Clear()
        shapes <- Map.empty
        eventListeners <- Map.empty
        shapeInfo <- Map.empty

    /// <summary>
    /// Get the canvas control
    /// </summary>
    member this.Canvas = canvasControl

    /// <summary>
    /// Handle window closing
    /// </summary>
    override this.OnClosed(e) =
        windowCount <- windowCount - 1
        if windowCount = 0 then
            windowCount <- 0
        base.OnClosed(e)