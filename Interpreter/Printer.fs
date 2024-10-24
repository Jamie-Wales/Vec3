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
    | ELiteral (lit, _) -> printLiteral lit indentStr sb
    | EUnary (op, expr, _) -> printUnary op expr indentStr indent sb
    | EBinary (left, op, right, _) -> printBinary left op right indentStr indent sb
    | EGrouping (expr, _) -> printGrouping expr indentStr indent sb
    | EIdentifier (name, _) -> sb.AppendLine($"{indentStr}Identifier: {name.Lexeme}") |> ignore
    | ECall (callee, args, _) ->
        sb.AppendLine($"{indentStr}Call") |> ignore
        sb.AppendLine($"{indentStr}Callee:") |> ignore
        printASTWithIndent callee (indent + 1) sb
        sb.AppendLine($"{indentStr}Arguments:") |> ignore
        List.iter (fun arg -> printASTWithIndent arg (indent + 1) sb) args
    | EBlock (stmts, _) ->
        sb.AppendLine($"{indentStr}Block") |> ignore
        
        
        
        

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


and printStmt stmt indent =
    let sb = StringBuilder()
    let indentStr = String.replicate indent "  "
    match stmt with
    | SExpression (expr, _) ->
        sb.AppendLine($"{indentStr}Expression Statement:") |> ignore
        printASTWithIndent expr (indent + 1) sb
    | SVariableDeclaration (name, expr, _) ->
        sb.AppendLine($"{indentStr}Variable Declaration:") |> ignore
        sb.AppendLine($"{indentStr}Name: {name.Lexeme}") |> ignore
        sb.AppendLine($"{indentStr}Initializer:") |> ignore
        printASTWithIndent expr (indent + 1) sb
    | SAssertStatement (expr, msg, _) ->
        sb.AppendLine($"{indentStr}Assert Statement:") |> ignore
        printASTWithIndent expr (indent + 1) sb
        sb.AppendLine($"{indentStr}Message:") |> ignore
    sb.ToString()

let printProgram (program: Program) =
    let sb = StringBuilder()
    sb.AppendLine("Program:") |> ignore
    List.iteri (fun i stmt -> 
        sb.AppendLine($"Statement {i + 1}:") |> ignore
        sb.Append(printStmt stmt 1) |> ignore
    ) program
    sb.ToString()