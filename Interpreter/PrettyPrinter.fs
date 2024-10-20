module Vec3.Interpreter.PrettyPrinter

open Grammar
open Token

let printNumber = function
    | LFloat f -> $"{f}"
    | LInteger i -> $"{i}"
    | LRational (n, d) -> $"{n}/{d}"
    | LComplex (r, i) -> $"{r} + {i}i"

let printLiteral = function
    | LNumber n -> printNumber n
    | LString s -> $"\"{s}\""
    | LBool b -> $"{b}"
    | LUnit -> "()"

let lexemeToString = function
    | Operator op -> operatorToString op
    | Keyword kw -> keywordToString kw
    | Identifier id -> id
    | Number num -> numberToString num
    | String str -> $"\"{str}\""

let rec printExpr = function
    | ELiteral (lit, _) -> printLiteral lit
    | EList (exprs, _) -> $"""[{String.concat ", " (List.map printExpr exprs)}]"""
    | ETuple (exprs, _) -> $"""({String.concat ", " (List.map printExpr exprs)})"""
    | EBinary (left, op, right, _) -> $"""{printExpr left} {lexemeToString op.Lexeme} {printExpr right}"""
    | EUnary (op, expr, _) -> $"""{lexemeToString op.Lexeme}{printExpr expr}"""
    | EGrouping (expr, _) -> $"""({printExpr expr})"""
    | EIdentifier (name, _) -> lexemeToString name.Lexeme
    | ECall (callee, args, _) -> $"""{printExpr callee}({String.concat ", " (List.map printExpr args)})"""
    | EIf (cond, thenBranch, elseBranch, _) -> $"""if {printExpr cond} then {printExpr thenBranch} else {printExpr elseBranch}"""
    | EIndex (expr, index, _) -> $"""{printExpr expr}[{printExpr index}]"""
    | EBlock (stmts, _) -> $"""{{ {String.concat "\n" (List.map printStmt stmts)} }}"""
    | ELambda (params', body, _) -> $"""({String.concat ", " (List.map (fun (param) -> lexemeToString param.Lexeme) params')}) -> {printExpr body}"""
    | ERecordSelect (expr, field, _) -> $"""{printExpr expr}.{lexemeToString field.Lexeme}"""
    | ETernary (cond, thenBranch, elseBranch, _) -> $"""{printExpr thenBranch} if {printExpr cond} else {printExpr elseBranch}"""
    
and printStmt = function
    | SExpression (expr, _) -> printExpr expr
    | SVariableDeclaration (name, expr, _) -> $"""let {lexemeToString name.Lexeme} = {printExpr expr}"""
    | SAssertStatement (expr, msg, _) -> $"""assert {printExpr expr}, "{msg}" """
    
    
    
let printProgram (program: Program) =
    String.concat "\n" (List.map printStmt program)