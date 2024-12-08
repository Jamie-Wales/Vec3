namespace Vec3

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Markup.Xaml
open AvaloniaEdit
open Avalonia.Media
open ScottPlot.Avalonia
open TextMateSharp.Grammars
open AvaloniaEdit.TextMate
open Vec3.Interpreter
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Typing
open Vec3.Interpreter.Repl
open QuestPDF.Infrastructure

type CellType = 
    | Code = 0
    | Text = 1

type NotebookCell = {
    Type: CellType
    Content: string
    Output: string option
    [<JsonIgnore>]
    Plots: byte[] list // We don't serialize plots in .vec3nb format
}

type NotebookData = {
    Version: string
    Cells: NotebookCell list
}

/// <summary>
/// The notebook window.
/// </summary>
type NotebookWindow() as this =
    inherit Window()

    let mutable cellsContainer: StackPanel = null
    let mutable addCodeButton: Button = null
    let mutable addTextButton: Button = null
    let mutable exportButton: Button = null
    let mutable importButton: Button = null
    let mutable vm: VM = createNewVM (initFunction ("Main"))
    let mutable typeEnv: Types.TypeEnv = Inference.defaultTypeEnv

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()

    member private this.SetupTextMateEditor(editor: TextEditor) =
        let registryOptions = RegistryOptions(ThemeName.QuietLight)
        editor.ShowLineNumbers <- true
        editor.Options.ShowTabs <- false
        editor.Options.ShowSpaces <- false
        editor.Options.ShowEndOfLine <- false
        editor.Options.HighlightCurrentLine <- false

        let installation = TextMate.Installation(editor, registryOptions)
        let fsharpLanguage = registryOptions.GetLanguageByExtension(".fs")
        let scopeName = registryOptions.GetScopeByLanguageId(fsharpLanguage.Id)
        installation.SetGrammar(scopeName)

        // Apply theme colors
        let mutable colorString = ""

        if installation.TryGetThemeColor("editor.background", &colorString) then
            match Color.TryParse(colorString) with
            | true, color -> editor.Background <- SolidColorBrush(color)
            | _ -> ()

        if installation.TryGetThemeColor("editor.foreground", &colorString) then
            match Color.TryParse(colorString) with
            | true, color -> editor.Foreground <- SolidColorBrush(color)
            | _ -> ()

    member private this.CreateRunButton() =
        let runButton = Button()
        runButton.Classes.Add("cell-button")
        runButton.Background <- SolidColorBrush(Colors.ForestGreen)
        runButton.Margin <- Thickness(5.0, 0.0)

        let icon = PathIcon()
        icon.Data <- Application.Current.FindResource("PlayRegular") :?> StreamGeometry
        icon.Foreground <- SolidColorBrush(Colors.White)

        runButton.Content <- icon
        runButton

    member private this.CreateDeleteButton() =
        let deleteButton = Button()
        deleteButton.Classes.Add("cell-button")
        deleteButton.Background <- SolidColorBrush(Colors.Crimson)

        let icon = PathIcon()
        icon.Data <- Application.Current.FindResource("DeleteRegular") :?> StreamGeometry
        icon.Foreground <- SolidColorBrush(Colors.White)

        deleteButton.Content <- icon
        deleteButton

    member private this.AddCodeCell() =
        let cellBorder = Border()
        cellBorder.Classes.Add("cell")

        let grid = Grid()
        grid.RowDefinitions <- RowDefinitions("Auto,*,Auto,Auto") // Added row for plots

        // Buttons panel
        let buttonsPanel = StackPanel()
        buttonsPanel.Orientation <- Avalonia.Layout.Orientation.Horizontal
        buttonsPanel.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Right
        buttonsPanel.Margin <- Thickness(5.0)

        let runButton = this.CreateRunButton()
        let deleteButton = this.CreateDeleteButton()

        // Editor setup
        let editor = TextEditor()
        editor.FontFamily <- "Cascadia Code,Consolas,Menlo,Monospace"
        editor.FontSize <- 14.0
        editor.Background <- SolidColorBrush(Colors.White)
        editor.Foreground <- SolidColorBrush(Colors.Black)
        editor.Margin <- Thickness(5.0)
        this.SetupTextMateEditor(editor)

        // Output area
        let output = TextBlock()
        output.Margin <- Thickness(10.0)
        output.FontFamily <- "Cascadia Code,Consolas,Menlo,Monospace"
        output.Foreground <- SolidColorBrush(Colors.Black)

        let plotsPanel = StackPanel()
        plotsPanel.Orientation <- Avalonia.Layout.Orientation.Vertical
        plotsPanel.Margin <- Thickness(10.0)

        runButton.Click.Add(fun _ ->
            try
                match noTcParseAndCompile editor.Text vm with
                | Some newVM ->
                    let oldOutputLength = Seq.length vm.Streams.StandardOutput.Value
                    vm <- run newVM
                    output.Foreground <- SolidColorBrush(Colors.Black)

                    let newOutput =
                        vm.Streams.StandardOutput.Value
                        |> Seq.skip oldOutputLength
                        |> String.concat "\n"
                        
                    let topOfStack = newVM.Stack[Seq.length newVM.Stack - 1] |> valueToString

                    let newOutput = if newOutput = "" then topOfStack else newOutput
                    
                    output.Text <- newOutput

                    // Clear existing plots
                    plotsPanel.Children.Clear()

                    // Handle any plots that were generated
                    for value in vm.Plots do
                        match value with
                        | VPlotData(title, xs, ys, plotType) ->
                            let plotControl = AvaPlot()
                            plotControl.Height <- 300
                            plotControl.Width <- 400
                            plotControl.Margin <- Thickness(0, 10, 0, 10)
                            plotControl.Plot.Title(title)

                            let extractNumber =
                                function
                                | VNumber(VFloat f) -> f
                                | VNumber(VInteger i) -> float i
                                | _ -> 0.0

                            let x = List.map extractNumber xs |> Array.ofList
                            let y = List.map extractNumber ys |> Array.ofList

                            match plotType with
                            | Scatter -> plotControl.Plot.Add.Scatter(x, y) |> ignore
                            | Line -> plotControl.Plot.Add.Line(x[0], y[0], x[1], y[1]) |> ignore
                            | Bar -> plotControl.Plot.Add.Bars(y, x) |> ignore
                            | Histogram -> failwith "todo"
                            | Signal -> plotControl.Plot.Add.Signal(y) |> ignore

                            plotControl.Refresh()
                            plotsPanel.Children.Add(plotControl)

                        | VPlotFunction(title, f, start, end_, area) ->
                            let plotControl = AvaPlot()
                            plotControl.Height <- 300
                            plotControl.Width <- 400
                            plotControl.Margin <- Thickness(0, 10, 0, 10)
                            plotControl.Plot.Title(title)
                            plotControl.Plot.Add.Function(f) |> ignore

                            match start, end_, area with
                            | Some start, Some end_, Some area ->
                                let startHeight = f start
                                let endHeight = f end_

                                plotControl.Plot.Add.Line(start, 0.0, start, startHeight) |> ignore
                                plotControl.Plot.Add.Line(end_, 0.0, end_, endHeight) |> ignore

                                plotControl.Plot.Add.Annotation($"Area: %f{area}") |> ignore
                            | _ -> ()

                            plotControl.Refresh()
                            plotsPanel.Children.Add(plotControl)

                        | VPlotFunctions(title, fs) ->
                            let plotControl = AvaPlot()
                            plotControl.Height <- 300
                            plotControl.Width <- 400
                            plotControl.Margin <- Thickness(0, 10, 0, 10)
                            plotControl.Plot.Title(title)

                            for f in fs do
                                plotControl.Plot.Add.Function(f) |> ignore

                            plotControl.Refresh()
                            plotsPanel.Children.Add(plotControl)
                        | _ -> ()

                    vm.Plots.Clear()
                | None ->
                    output.Foreground <- SolidColorBrush(Colors.Red)
                    output.Text <- "Failed to compile code"
                    plotsPanel.Children.Clear()
            with ex ->
                output.Foreground <- SolidColorBrush(Colors.Red)
                output.Text <- ex.Message
                printfn $"Error: %s{ex.Message}"
                plotsPanel.Children.Clear())

        deleteButton.Click.Add(fun _ -> cellsContainer.Children.Remove(cellBorder) |> ignore)

        // Assemble the cell
        buttonsPanel.Children.Add(runButton)
        buttonsPanel.Children.Add(deleteButton)

        Grid.SetRow(buttonsPanel, 0)
        Grid.SetRow(editor, 1)
        Grid.SetRow(output, 2)
        Grid.SetRow(plotsPanel, 3)

        grid.Children.Add(buttonsPanel)
        grid.Children.Add(editor)
        grid.Children.Add(output)
        grid.Children.Add(plotsPanel)

        cellBorder.Child <- grid
        cellsContainer.Children.Add(cellBorder)

    member private this.AddTextCell() =
        let cellBorder = Border()
        cellBorder.Classes.Add("cell")

        let grid = Grid()
        grid.RowDefinitions <- RowDefinitions("Auto,*")

        let deleteButton = this.CreateDeleteButton()
        deleteButton.HorizontalAlignment <- Avalonia.Layout.HorizontalAlignment.Right
        deleteButton.Margin <- Thickness(5.0)

        // Text editor setup
        let editor = TextEditor()
        editor.FontFamily <- "Cascadia Code,Consolas,Menlo,Monospace"
        editor.FontSize <- 14.0
        editor.Background <- SolidColorBrush(Colors.White)
        editor.Foreground <- SolidColorBrush(Colors.Black)
        editor.Margin <- Thickness(5.0)
        editor.HorizontalScrollBarVisibility <- Avalonia.Controls.Primitives.ScrollBarVisibility.Disabled
        editor.VerticalScrollBarVisibility <- Avalonia.Controls.Primitives.ScrollBarVisibility.Auto

        deleteButton.Click.Add(fun _ -> cellsContainer.Children.Remove(cellBorder) |> ignore)

        Grid.SetRow(deleteButton, 0)
        Grid.SetRow(editor, 1)

        grid.Children.Add(deleteButton)
        grid.Children.Add(editor)

        cellBorder.Child <- grid
        cellsContainer.Children.Add(cellBorder)

    member private this.SaveNotebookAsync() =
        task {
            try
                let dialog = SaveFileDialog()
                dialog.DefaultExtension <- "vec3nb"
                dialog.Filters.Add(FileDialogFilter(Name = "Vec3 Notebook", Extensions = ResizeArray["vec3nb"]))
                
                let! path = dialog.ShowAsync(this)
                match path with
                | null -> ()  // User cancelled
                | path ->
                    let cells = 
                        cellsContainer.Children 
                        |> Seq.cast<Border>
                        |> Seq.map (fun border ->
                            let grid = border.Child :?> Grid
                            match grid.RowDefinitions.Count with
                            | 4 -> // Code cell
                                let editor = grid.Children[1] :?> TextEditor
                                let output = grid.Children[2] :?> TextBlock
                                {
                                    Type = CellType.Code
                                    Content = editor.Text
                                    Output = Some(output.Text)
                                    Plots = []
                                }
                            | 2 -> // Text cell
                                let editor = grid.Children[1] :?> TextEditor
                                {
                                    Type = CellType.Text
                                    Content = editor.Text
                                    Output = None
                                    Plots = []
                                }
                            | _ -> failwith "Invalid cell type")
                        |> Seq.toList
                    
                    let notebook = {
                        Version = "1.0"
                        Cells = cells
                    }
                    
                    let options = JsonSerializerOptions()
                    options.WriteIndented <- true
                    let json = JsonSerializer.Serialize(notebook, options)
                    do! File.WriteAllTextAsync(path, json)
            with ex ->
                eprintfn $"Failed to save notebook: %s{ex.Message}"
        } |> ignore

    member private this.LoadNotebookAsync() =
            task {
                try
                    let dialog = OpenFileDialog()
                    dialog.Filters.Add(FileDialogFilter(Name = "Vec3 Notebook", Extensions = ResizeArray["vec3nb"]))
                    
                    let! result = dialog.ShowAsync(this)
                    match result with
                    | null -> ()  // User cancelled
                    | files ->
                        let! content = File.ReadAllTextAsync(files.[0])
                        let notebook = JsonSerializer.Deserialize<NotebookData>(content)
                        
                        cellsContainer.Children.Clear()
                        
                        vm <- createNewVM (initFunction "Main")
                        
                        for cell in notebook.Cells do
                            match cell.Type with
                            | CellType.Text ->
                                this.AddTextCell()
                                let border = cellsContainer.Children[cellsContainer.Children.Count - 1] :?> Border
                                let grid = border.Child :?> Grid
                                let editor = grid.Children[1] :?> TextEditor
                                editor.Text <- cell.Content
                                this.SetupTextMateEditor(editor)  
                                
                            | CellType.Code ->
                                this.AddCodeCell()
                                let border = cellsContainer.Children[cellsContainer.Children.Count - 1] :?> Border
                                let grid = border.Child :?> Grid
                                let editor = grid.Children[1] :?> TextEditor
                                let output = grid.Children[2] :?> TextBlock
                                editor.Text <- cell.Content
                                this.SetupTextMateEditor(editor) 
                                match cell.Output with
                                | Some text -> output.Text <- text
                                | None -> ()
                            | _ -> ()
                with ex ->
                    eprintfn $"Failed to load notebook: %s{ex.Message}"
            } |> ignore
    member private this.ExportNotebook() =
        task {
            try
                QuestPDF.Settings.License <- QuestPDF.Infrastructure.LicenseType.Community
                let dialog = SaveFileDialog()
                let extensions = ResizeArray<string>([ "pdf" ])
                dialog.Filters.Add(FileDialogFilter(Name = "PDF Document", Extensions = extensions))
                let! path = dialog.ShowAsync(this)

                match path with
                | null -> ()
                | path ->
                    let cells = NotebookPdfExport.extractCellsData cellsContainer
                    do! Task.Run(fun () -> NotebookPdfExport.exportToPdf cells path)
            with ex ->
                eprintfn $"Failed to export PDF: %s{ex.Message}"
        }
        |> ignore
    member private this.InitializeComponent() =
        cellsContainer <- this.FindControl<StackPanel>("CellsContainer")
        addCodeButton <- this.FindControl<Button>("AddCodeButton")
        addTextButton <- this.FindControl<Button>("AddTextButton")
        exportButton <- this.FindControl<Button>("ExportButton")
        importButton <- this.FindControl<Button>("ImportButton")
        
        addCodeButton.Click.AddHandler(fun _ _ -> this.AddCodeCell())
        addTextButton.Click.AddHandler(fun _ _ -> this.AddTextCell())
        
        exportButton.Click.AddHandler(fun _ _ -> 
            let menu = ContextMenu()
            let pdfItem = MenuItem(Header = "Export as PDF")
            let notebookItem = MenuItem(Header = "Save as .vec3nb")
            
            pdfItem.Click.Add(fun _ -> this.ExportNotebook())
            notebookItem.Click.Add(fun _ -> this.SaveNotebookAsync())
            
            menu.Items.Add(pdfItem) |> ignore
            menu.Items.Add(notebookItem) |> ignore
            menu.Open(exportButton))
            
        importButton.Click.AddHandler(fun _ _ -> this.LoadNotebookAsync())
