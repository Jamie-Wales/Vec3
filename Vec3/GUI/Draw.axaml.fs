namespace Vec3

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open Avalonia.Input
open Avalonia.Media
open Avalonia.Controls.Shapes
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.VM

type ShapeInfo =
    { x: float
      y: float
      w: float
      h: float
      color: string
      typ: string
      id: int
      trace: bool }

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


    // listen for arrow keys
    do
        this.KeyDown.Add(fun e ->
            let listenerId = this.keyToInt (e.Key)
            this.handleEvent listenerId)


    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()
        windowCount <- windowCount + 1
        this.Position <- PixelPoint(offset * windowCount, offset * windowCount)
        this.Title <- "Vec3 Drawing"
        this.Width <- 800.0
        this.Height <- 600.0
        this.Focusable <- true

    member private this.handleEvent listenerId =
        let listeners = eventListeners.TryFind(listenerId)

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
                    let shapeInfo = Option.get shapeInfo

                    let prevX = Canvas.GetLeft(shape) + 25.0
                    let prevY = Canvas.GetTop(shape) + 25.0

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
                            if shapeInfo.trace then
                                let trail = Avalonia.Controls.Shapes.Line()
                                trail.StartPoint <- Point(prevX, prevY)
                                trail.EndPoint <- Point(x, y)
                                trail.Stroke <- SolidColorBrush(Colors.Black)
                                trail.StrokeThickness <- 1.0
                                canvasControl.Children.Add(trail)

                            match shape with
                            | :? Rectangle as rect ->
                                Canvas.SetLeft(rect, x)
                                Canvas.SetTop(rect, y)
                            | :? Ellipse as circle ->
                                Canvas.SetLeft(circle, x - circle.Width / 2.0)
                                Canvas.SetTop(circle, y - circle.Height / 2.0)
                            | :? Avalonia.Controls.Shapes.Line as line ->
                                Canvas.SetLeft(line, x)
                                Canvas.SetTop(line, y)
                            | _ -> ()
                        | _ -> ()
                    | _ -> ())
        | None -> ()

    member private this.keyToInt key =
        match key with
        | Key.Left -> 0
        | Key.Right -> 1
        | Key.Up -> 2
        | Key.Down -> 3
        | _ -> -1

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
        with _ ->
            Colors.Black

    member this.AddEventListener(vm: VM, shapeId: int, listenerId: int, func: Function) =
        currentVm <- Some vm
        let listeners = eventListeners.TryFind(listenerId)

        let listeners =
            match listeners with
            | Some lst -> lst
            | None -> []

        eventListeners <- eventListeners.Add(listenerId, (shapeId, func) :: listeners)

    member this.DrawShape(vm: VM, shape: Value) =
        currentVm <- Some vm

        let w, h, x, y, color, typ, id, trail =
            match shape with
            | VShape(x, y, w, h, c, t, id, trail) -> (x, y, w, h, c, t, id, trail)
            | _ -> raise <| InvalidProgramException("Unknown shape value")

        let colorBrush = SolidColorBrush(this.ParseColor(color))

        // Determine shape type based on parameters and create appropriate shape
        let shape =
            match typ with
            | "rectangle" ->
                let rect = Rectangle()
                rect.Width <- abs w
                rect.Height <- abs h
                rect.Fill <- colorBrush
                Canvas.SetLeft(rect, x)
                Canvas.SetTop(rect, y)
                rect :> Shape
            | "circle" ->
                let circle = Ellipse()
                circle.Width <- abs w
                circle.Height <- abs h
                circle.Fill <- colorBrush
                Canvas.SetLeft(circle, x - w / 2.0) // Center the circle
                Canvas.SetTop(circle, y - h / 2.0)
                circle :> Shape
            | "line" ->
                let line = Avalonia.Controls.Shapes.Line()
                line.Width <- abs w
                line.Height <- abs h
                line.Fill <- colorBrush
                Canvas.SetLeft(line, x)
                Canvas.SetTop(line, y)
                line :> Shape

        shapes <- shapes.Add(id, shape)

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

    member this.Clear() = canvasControl.Children.Clear()

    member this.Canvas = canvasControl

    override this.OnClosed(e) =
        windowCount <- windowCount - 1

        if windowCount = 0 then
            windowCount <- 0

        base.OnClosed(e)
