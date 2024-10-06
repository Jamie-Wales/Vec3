namespace Vec3

open Avalonia.Controls
open Avalonia.Interactivity
open Avalonia.Markup.Xaml
open System
open Vec3.Interpreter.Backend
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Chunk
open Vec3.Interpreter.Preprocessor

type MainWindow () as this =
    inherit Window()

    let mutable inputBox: TextBox = null
    let mutable lineNumbers: TextBlock = null
    let mutable outputBlock: TextBlock = null
    let mutable constantPoolBlock: TextBlock = null
    let mutable disassembledChunkBlock: TextBlock = null
    let mutable vmInstructionsBlock: TextBlock = null
    let mutable vmStackBlock: TextBlock = null
    let mutable vmPrintOutputBlock: TextBlock = null
    let mutable evaluateButton: Button = null
    let mutable switchModeButton: Button = null
    let mutable isReplMode = false

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()

    member private this.InitializeComponent() =
        inputBox <- this.FindControl<TextBox>("InputBox")
        lineNumbers <- this.FindControl<TextBlock>("LineNumbers")
        outputBlock <- this.FindControl<TextBlock>("OutputBlock")
        constantPoolBlock <- this.FindControl<TextBlock>("ConstantPoolBlock")
        disassembledChunkBlock <- this.FindControl<TextBlock>("DisassembledChunkBlock")
        vmInstructionsBlock <- this.FindControl<TextBlock>("VMInstructionsBlock")
        vmStackBlock <- this.FindControl<TextBlock>("VMStackBlock")
        vmPrintOutputBlock <- this.FindControl<TextBlock>("VMPrintOutputBlock")
        evaluateButton <- this.FindControl<Button>("EvaluateButton")
        switchModeButton <- this.FindControl<Button>("SwitchModeButton")

        if evaluateButton <> null then
            evaluateButton.Click.AddHandler(this.Evaluate_Click)
        if switchModeButton <> null then
            switchModeButton.Click.AddHandler(this.SwitchMode_Click)
        if inputBox <> null then
            inputBox.TextChanged.Add(this.UpdateLineNumbers)

    member private this.UpdateLineNumbers(_) =
        if inputBox <> null && lineNumbers <> null then
            let lines = inputBox.Text.Split(Environment.NewLine)
            let lineCount = lines.Length
            lineNumbers.Text <- String.Join(Environment.NewLine, [1..lineCount] |> List.map (fun i -> i.ToString().PadLeft(3)))

    member private this.AppendOutput(text: string) =
        if outputBlock <> null then
            outputBlock.Text <- outputBlock.Text + text + Environment.NewLine

    member private this.ClearOutput() =
        if outputBlock <> null then outputBlock.Text <- ""
        if constantPoolBlock <> null then constantPoolBlock.Text <- ""
        if disassembledChunkBlock <> null then disassembledChunkBlock.Text <- ""
        if vmInstructionsBlock <> null then vmInstructionsBlock.Text <- ""
        if vmStackBlock <> null then vmStackBlock.Text <- ""
        if vmPrintOutputBlock <> null then vmPrintOutputBlock.Text <- ""

    member private this.ProcessVMOutput (vmOutput: string) =
        let sections = vmOutput.Split([|"=== Constant Pool ==="; "=== Disassembled Chunk ==="; "=== Program Execution ==="|], StringSplitOptions.RemoveEmptyEntries)
        
        if sections.Length >= 3 then
            if constantPoolBlock <> null then
                constantPoolBlock.Text <- "=== Constant Pool ===\n" + sections.[0].Trim()

            if disassembledChunkBlock <> null then
                disassembledChunkBlock.Text <- "=== Disassembled Chunk ===\n" + sections.[1].Trim()

            let executionOutput = sections.[2].Trim()
            let executionLines = executionOutput.Split('\n')
            
            let mutable instructions = new System.Text.StringBuilder()
            let mutable stack = new System.Text.StringBuilder()
            let mutable printOutput = new System.Text.StringBuilder()
            let mutable globals = new System.Text.StringBuilder()

            for line in executionLines do
                if line.StartsWith("IP:") || line.StartsWith("Executing") then
                    instructions.AppendLine(line) |> ignore
                elif line.StartsWith("Stack:") then
                    stack.AppendLine(line) |> ignore
                elif line.StartsWith("Printed value:") then
                    printOutput.AppendLine(line) |> ignore
                elif line.StartsWith("Defining global variable:") || line.StartsWith("=== Globals ===") then
                    globals.AppendLine(line) |> ignore

            if vmInstructionsBlock <> null then
                vmInstructionsBlock.Text <- instructions.ToString()
            if vmStackBlock <> null then
                vmStackBlock.Text <- stack.ToString()
            if vmPrintOutputBlock <> null then
                vmPrintOutputBlock.Text <- printOutput.ToString()
            if outputBlock <> null then
                outputBlock.Text <- globals.ToString()

        else
            // If we can't split the output as expected, just display it all in the output block
            if outputBlock <> null then
                outputBlock.Text <- vmOutput

    member private this.Evaluate_Click(_sender: obj) (_e: RoutedEventArgs) =
        if inputBox <> null then
            let input = inputBox.Text
            if not (String.IsNullOrWhiteSpace input) then
                this.ClearOutput()
                this.AppendOutput $"> {input}"
                try
                    let preprocessedInput = preprocessContent input
                    let parsed = parse preprocessedInput
                    match parsed with
                    | Ok(program, _) ->
                        match compileProgram program with
                        | Ok (chunk, _) ->
                            let vmOutput = interpret chunk
                            this.ProcessVMOutput(vmOutput)
                        | Error (msg, _) ->
                            this.AppendOutput $"Compilation error: {msg}"
                    | Error (msg, _) ->
                        this.AppendOutput $"Parsing error: {msg}"
                with
                | e -> this.AppendOutput $"An error occurred: {e.Message}"

    member private this.SwitchMode_Click(_sender: obj) (_e: RoutedEventArgs) =
        isReplMode <- not isReplMode
        if switchModeButton <> null then
            switchModeButton.Content <- if isReplMode then "Switch to Multi-line Mode" else "Switch to REPL Mode"
        if inputBox <> null then
            inputBox.AcceptsReturn <- not isReplMode
            this.ClearOutput()
            inputBox.Text <- ""
            this.UpdateLineNumbers(null)