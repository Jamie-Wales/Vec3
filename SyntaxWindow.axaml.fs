namespace Vec3

open Avalonia.Controls
open Avalonia.Markup.Xaml
open AvaloniaEdit
open Avalonia.Media
open TextMateSharp.Grammars
open AvaloniaEdit.TextMate

type SyntaxWindow () as this =
    inherit Window ()

    let mutable syntaxEditor: TextEditor = null
    let mutable textMateInstallation: TextMate.Installation = null

    let syntaxExample = """// Vec3 Syntax Guide

// 1. Basic Types
let integer: int = 42
let float_num: float = 3.14
let rational: rational = 22/7
let complex: complex = 1 + 2i
let text: string = "Hello Vec3"
let boolean: bool = true
let unit_value: unit = ()

// 2. Variable Declaration
let x = 5              // Type inference
let y: float = 2.5     // Explicit type

// 3. Functions
// Lambda with inferred types
let add = (a, b) -> a + b

// Lambda with explicit types
let typed_add = (a: int, b: int): int -> a + b

// Block syntax
let factorial = (n: int) {
    if n <= 1 then
        1
    else
        n * factorial(n - 1)
}

// 4. Control Flow
// If expression
let abs = (x: float) -> if x < 0.0 then -x else x

// Ternary expression
let sign = (x) -> 1 if x > 0 else -1 if x < 0 else 0

// 5. Lists and Operations
let numbers = [1, 2, 3, 4, 5]
let first = numbers[0]
let mapped = map((x) -> x * 2, numbers)

// 6. Built-in Functions
print("Output to console")
len([1, 2, 3])        // List length
map((x) -> x * x, [1, 2, 3])

// 7. Mathematical Functions
let f = (x) -> x^2 - 2*x + 1
plotFunc("quadratic", f)

// 8. Advanced Examples
// Function composition
let compose = (f, g) -> (x) -> f(g(x))
let double = (x) -> x * 2
let square = (x) -> x * x
let doubleSquare = compose(double, square)

// Numerical methods
let derivative = (f, h) -> (x) -> (f(x + h) - f(x)) / h
let newton = (f, x0, tolerance, maxIter) {
    let df = derivative(f, 1e-7)
    let rec iterate = (x, iter) {
        let fx = f(x)
        if abs(fx) < tolerance || iter >= maxIter then
            x
        else
            let dfx = df(x)
            iterate(x - fx/dfx, iter + 1)
    }
    iterate(x0, 0)
}"""

    do
        AvaloniaXamlLoader.Load(this)
        this.InitializeComponent()

    member private this.InitializeComponent() =
        syntaxEditor <- this.FindControl<TextEditor>("SyntaxEditor")
        
        let registryOptions = RegistryOptions(ThemeName.DarkPlus)
        
        if syntaxEditor <> null then
            syntaxEditor.ShowLineNumbers <- true
            syntaxEditor.Options.ShowTabs <- false 
            syntaxEditor.Options.ShowSpaces <- false 
            syntaxEditor.Options.ShowEndOfLine <- false 
            syntaxEditor.Options.HighlightCurrentLine <- true
            syntaxEditor.Text <- syntaxExample

            textMateInstallation <- TextMate.Installation(syntaxEditor, registryOptions)
            
            let fsharpLanguage = registryOptions.GetLanguageByExtension(".fs")
            let scopeName = registryOptions.GetScopeByLanguageId(fsharpLanguage.Id)
            textMateInstallation.SetGrammar(scopeName)

            this.ApplyColorScheme()

    member private this.ApplyColorScheme() =
        if textMateInstallation <> null then
            let applyColor colorKey (action: IBrush -> unit) =
                let mutable colorString = ""
                if textMateInstallation.TryGetThemeColor(colorKey, &colorString) then
                    match Color.TryParse(colorString) with
                    | true, color ->
                        let brush = SolidColorBrush(color)
                        action(brush)
                    | _ -> ()

            applyColor "editor.background" (fun brush -> syntaxEditor.Background <- brush)
            applyColor "editor.foreground" (fun brush -> syntaxEditor.Foreground <- brush)
            applyColor "editorLineNumber.foreground" (fun brush -> syntaxEditor.LineNumbersForeground <- brush)