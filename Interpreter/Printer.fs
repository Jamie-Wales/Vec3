module Vec3.Interpreter.Printer

open System.Text
open Vec3.Interpreter.Token
open Vec3.Interpreter.Grammar
let rec printAST program =
    let sb = StringBuilder()
    printASTWithIndent program 0 sb
    sb.ToString()

and printASTWithIndent expr indent (sb: StringBuilder) =
    let indentStr = String.replicate indent "  "
    match expr with
    | ELiteral lit -> printLiteral lit indentStr sb
    | EUnary (op, expr) -> printUnary op expr indentStr indent sb
    | EBinary (left, op, right) -> printBinary left op right indentStr indent sb
    | EGrouping expr -> printGrouping expr indentStr indent sb
    | EAssignment (name, expr) -> 
        sb.AppendLine($"{indentStr}Assignment") |> ignore
        sb.AppendLine($"{indentStr}Name: {name.lexeme}") |> ignore
        sb.AppendLine($"{indentStr}Expression:") |> ignore
        printASTWithIndent expr (indent + 1) sb
        

and printLiteral (lit: Literal) indentStr (sb: StringBuilder) =
    match lit with
    | LNumber n -> sb.AppendLine($"{indentStr}{numberToString n}") |> ignore
    | LString s -> sb.AppendLine($"{indentStr}String(\"{s}\")") |> ignore
    | LBool b -> sb.AppendLine($"{indentStr}Bool({b})") |> ignore
    | LUnit -> sb.AppendLine($"{indentStr}Nil") |> ignore

and printUnary op expr indentStr indent (sb: StringBuilder) =
    sb.AppendLine($"{indentStr}Unary") |> ignore
    sb.AppendLine($"{indentStr}Operator: {tokenToString op}") |> ignore
    printASTWithIndent expr (indent + 1) sb

and printBinary left op right indentStr indent (sb: StringBuilder) =
    sb.AppendLine($"{indentStr}Binary") |> ignore
    sb.AppendLine($"{indentStr}Left:") |> ignore
    printASTWithIndent left (indent + 2) sb
    sb.AppendLine($"{indentStr} Operator: {tokenToString op}") |> ignore
    sb.AppendLine($"{indentStr} Right:") |> ignore
    printASTWithIndent right (indent + 2) sb

and printGrouping expr indentStr indent (sb: StringBuilder) =
    sb.AppendLine($"{indentStr}Grouping") |> ignore
    printASTWithIndent expr (indent + 1) sb


let printStmt stmt indent =
    let sb = StringBuilder()
    let indentStr = String.replicate indent "  "
    match stmt with
    | SExpression expr ->
        sb.AppendLine($"{indentStr}Expression Statement:") |> ignore
        printASTWithIndent expr (indent + 1) sb
    | SPrintStatement expr ->
        sb.AppendLine($"{indentStr}Print Statement:") |> ignore
        printASTWithIndent expr (indent + 1) sb
    | SVariableDeclaration (name, _, expr) ->
        sb.AppendLine($"{indentStr}Variable Declaration:") |> ignore
        sb.AppendLine($"{indentStr}Name: {name.lexeme}") |> ignore
    sb.ToString()

let printProgram (program: Program) =
    let sb = StringBuilder()
    sb.AppendLine("Program:") |> ignore
    List.iteri (fun i stmt -> 
        sb.AppendLine($"Statement {i + 1}:") |> ignore
        sb.Append(printStmt stmt 1) |> ignore
    ) program
    sb.ToString()