namespace Vec3
open Avalonia
open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Markup.Xaml
open System
open Interpreter.Token
open Interpreter.Parser
open Interpreter.Printer
open Interpreter.Scanner

type MainWindow () as this =
    inherit Window()
    let mutable inputBox: TextBox = null
    let mutable lineNumbers: TextBlock = null
    let mutable outputBlock: TextBlock = null
    let mutable evaluateButton: Button = null
    do
        this.InitializeComponent()
        inputBox <- this.FindControl<TextBox>("InputBox")
        lineNumbers <- this.FindControl<TextBlock>("LineNumbers")
        outputBlock <- this.FindControl<TextBlock>("OutputBlock")
        evaluateButton <- this.FindControl<Button>("EvaluateButton")
        evaluateButton.Click.AddHandler(this.Evaluate_Click)
        inputBox.TextChanged.Add(this.UpdateLineNumbers)

    member private this.InitializeComponent() =
#if DEBUG
        this.AttachDevTools()
#endif
        AvaloniaXamlLoader.Load(this)

    member private this.UpdateLineNumbers(_) =
        let lines = inputBox.Text.Split(Environment.NewLine)
        let lineCount = lines.Length
        lineNumbers.Text <- String.Join(Environment.NewLine, [1..lineCount])

    member private this.AppendOutput(text: string) =
        outputBlock.Text <- outputBlock.Text + text + Environment.NewLine

    member private this.ClearOutput() =
        outputBlock.Text <- ""

    
    member private this.ParseAndPrintExpression(tokens: Token list) =
        try
            this.AppendOutput "Parsed Expression:"
            let expr = parseTokens tokens
            let astString = $"{expr}"
            this.AppendOutput astString
        with
        | ex -> this.AppendOutput $"Error parsing expression: {ex.Message}"

    member private this.Evaluate_Click(_sender: obj) (_e: RoutedEventArgs) =
        let input = inputBox.Text
        if not (String.IsNullOrWhiteSpace input) then
            this.ClearOutput()  
            this.AppendOutput $"> {input}"
            try
                let tokens = tokenize input
                this.AppendOutput "Tokens:"
                tokens |> List.iter (fun token -> this.AppendOutput $"{tokenToString token}")
                this.ParseAndPrintExpression tokens
            with
            | ex -> this.AppendOutput $"Error: {ex.Message}"
