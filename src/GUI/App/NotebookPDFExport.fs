namespace GUI.App

open QuestPDF.Fluent
open QuestPDF.Helpers
open QuestPDF.Infrastructure
open System
open Avalonia
open Avalonia.Controls
open AvaloniaEdit
open ScottPlot.Avalonia
open Avalonia.Media.Imaging
open System.IO

/// <summary>
/// Represents the data of a cell in a notebook.
/// </summary>
type CellData =
    | TextCell of string
    | CodeCell of
        {| Code: string
           Output: string
           Plots: byte[] list |}

/// <summary>
/// Exports a notebook to a PDF file.
/// </summary>
module NotebookPdfExport =
    /// <summary>
    /// Saves the plots in a plot panel to image files.
    /// </summary>
    /// <param name="plot">The plot panel to capture.</param>
    /// <returns>The image data of the plot panel.</returns>
    let private capturePlotImage (plot: AvaPlot) =
        let pixelSize = PixelSize(int plot.Width, int plot.Height)
        let bitmap = new RenderTargetBitmap(pixelSize)

        plot.Measure(Size(plot.Width, plot.Height))
        plot.Arrange(Rect(0.0, 0.0, plot.Width, plot.Height))
        bitmap.Render(plot)

        use stream = new MemoryStream()
        bitmap.Save(stream)
        stream.ToArray()

    /// <summary>
    /// Extracts the data of all cells in a notebook.
    /// </summary>
    /// <param name="cellsContainer">The container of the cells.</param>
    /// <returns>The data of all cells in the notebook.</returns>
    let extractCellsData (cellsContainer: StackPanel) =
        if cellsContainer = null then
            []
        else
            cellsContainer.Children
            |> Seq.cast<Border>
            |> Seq.map (fun border ->
                let grid = border.Child :?> Grid

                match grid.RowDefinitions.Count with
                | 4 -> // Code cell (has buttons, editor, output, and plots)
                    let editor = grid.Children[1] :?> TextEditor
                    let output = grid.Children[2] :?> TextBlock
                    let plotsPanel = grid.Children[3] :?> StackPanel

                    // Capture all plots in the cell
                    let plotImages =
                        plotsPanel.Children
                        |> Seq.cast<AvaPlot>
                        |> Seq.map capturePlotImage
                        |> Seq.toList

                    CodeCell
                        {| Code = editor.Text.Trim()
                           Output = output.Text.Trim()
                           Plots = plotImages |}
                | 2 -> // Text cell (has button and editor)
                    let editor = grid.Children[1] :?> TextEditor
                    TextCell(editor.Text.Trim())
                | _ -> TextCell "")
            |> Seq.filter (function
                | TextCell text -> not (String.IsNullOrWhiteSpace(text))
                | CodeCell data ->
                    not (String.IsNullOrWhiteSpace(data.Code))
                    || not (String.IsNullOrWhiteSpace(data.Output))
                    || not (List.isEmpty data.Plots))
            |> Seq.toList

    /// <summary>
    /// Exports the cells of a notebook to a PDF file.
    /// </summary>
    /// <param name="cells">The data of the cells in the notebook.</param>
    /// <param name="path">The path of the PDF file to save.</param>
    /// <returns>Unit</returns>
    let exportToPdf (cells: CellData list) (path: string) =
        Document
            .Create(fun container ->
                container.Page(fun page ->
                    page.Size(PageSizes.A4)
                    page.Margin(2.0f, Unit.Centimetre)
                    page.DefaultTextStyle(fun text -> text.FontFamily("Cascadia Code").FontSize(11.0f))

                    page
                        .Content()
                        .Column(fun column ->
                            // Title Section
                            column
                                .Item()
                                .Border(1.0f)
                                .BorderColor("#E0E0E0")
                                .Background("#F8F9FA")
                                .Padding(20.0f)
                                .Row(fun row ->
                                    row
                                        .RelativeItem()
                                        .PaddingRight(10.0f)
                                        .Text("Vec3 Notebook")
                                        .FontSize(28.0f)
                                        .Bold()
                                        .FontColor("#2D3748")
                                    |> ignore

                                    row
                                        .ConstantItem(140.0f)
                                        .Text(DateTime.Now.ToString("yyyy-MM-dd HH:mm"))
                                        .FontSize(10.0f)
                                        .FontColor("#718096")
                                    |> ignore)

                            column.Spacing(25.0f)

                            // Cells
                            for cell in cells do
                                match cell with
                                | TextCell text ->
                                    column
                                        .Item()
                                        .Padding(5.0f)
                                        .Column(fun textCol ->
                                            textCol.Spacing(8.0f)

                                            for line in text.Split('\n') do
                                                if line.StartsWith("# ") then
                                                    textCol
                                                        .Item()
                                                        .Text(line.Replace("# ", ""))
                                                        .FontSize(24.0f)
                                                        .Bold()
                                                        .FontColor("#1A365D")
                                                    |> ignore
                                                elif line.StartsWith("## ") then
                                                    textCol
                                                        .Item()
                                                        .Text(line.Replace("## ", ""))
                                                        .FontSize(20.0f)
                                                        .Bold()
                                                        .FontColor("#2C5282")
                                                    |> ignore
                                                elif line.StartsWith("### ") then
                                                    textCol
                                                        .Item()
                                                        .Text(line.Replace("### ", ""))
                                                        .FontSize(16.0f)
                                                        .Bold()
                                                        .FontColor("#2B6CB0")
                                                    |> ignore
                                                elif not (String.IsNullOrWhiteSpace(line)) then
                                                    textCol.Item().Text(line).FontColor("#2D3748") |> ignore)

                                | CodeCell data ->
                                    column
                                        .Item()
                                        .Column(fun codeSection ->
                                            // Code Section
                                            codeSection
                                                .Item()
                                                .Border(1.0f)
                                                .BorderColor("#E2E8F0")
                                                .Column(fun codeCol ->
                                                    codeCol
                                                        .Item()
                                                        .Background("#EDF2F7")
                                                        .Padding(8.0f)
                                                        .Text("Code")
                                                        .Bold()
                                                        .FontSize(12.0f)
                                                        .FontColor("#4A5568")
                                                    |> ignore

                                                    codeCol
                                                        .Item()
                                                        .Background("#F7FAFC")
                                                        .Padding(12.0f)
                                                        .Text(data.Code)
                                                        .FontColor("#2D3748")
                                                    |> ignore)

                                            // Output Section (if any)
                                            if not (String.IsNullOrWhiteSpace(data.Output)) then
                                                codeSection
                                                    .Item()
                                                    .PaddingTop(5.0f)
                                                    .Border(1.0f)
                                                    .BorderColor("#E2E8F0")
                                                    .Column(fun outputCol ->
                                                        outputCol
                                                            .Item()
                                                            .Background("#F0FFF4")
                                                            .Padding(8.0f)
                                                            .Text("Output")
                                                            .Bold()
                                                            .FontSize(12.0f)
                                                            .FontColor("#276749")
                                                        |> ignore

                                                        outputCol
                                                            .Item()
                                                            .Background("#F0FFF4")
                                                            .Padding(12.0f)
                                                            .Text(data.Output)
                                                            .FontColor("#2F855A")
                                                        |> ignore)

                                            // Plots Section (if any)
                                            if not (List.isEmpty data.Plots) then
                                                codeSection
                                                    .Item()
                                                    .PaddingTop(5.0f)
                                                    .Border(1.0f)
                                                    .BorderColor("#E2E8F0")
                                                    .Column(fun plotsCol ->
                                                        plotsCol
                                                            .Item()
                                                            .Background("#EBF8FF")
                                                            .Padding(8.0f)
                                                            .Text("Plots")
                                                            .Bold()
                                                            .FontSize(12.0f)
                                                            .FontColor("#2C5282")
                                                        |> ignore

                                                        for plotImage in data.Plots do
                                                            plotsCol
                                                                .Item()
                                                                .Row(fun row ->
                                                                    // Add left spacing
                                                                    row.ConstantItem(20.0f).Height(0.0f) |> ignore

                                                                    // Center content
                                                                    row
                                                                        .RelativeItem()
                                                                        .AlignCenter()
                                                                        .Image(plotImage)
                                                                        .WithCompressionQuality(
                                                                            ImageCompressionQuality.Medium
                                                                        )
                                                                        .FitWidth()
                                                                    |> ignore

                                                                    // Add right spacing
                                                                    row.ConstantItem(20.0f).Height(0.0f) |> ignore)))

                                column.Item().MinHeight(15.0f) |> ignore))
                |> ignore)
            .GeneratePdf(path)
