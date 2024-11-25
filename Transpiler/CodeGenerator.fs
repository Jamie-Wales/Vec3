module Vec3.Transpiler.CodeGenerator

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

/// Helper function to escape strings for C
let escapeString (s: string) =
    s.Replace("\\", "\\\\").Replace("\"", "\\\"")

/// Generate C code from an expression
let rec generateExpr (expr: Expr) : string =
    match expr with
    | ELiteral(LNumber(LInteger i), _) -> 
        $"vec3_new_number(number_from_int(%d{i}))"
    | ELiteral(LNumber(LFloat f), _) -> 
        $"vec3_new_number(number_from_float(%f{f}))"
    | ELiteral(LString s, _) -> 
        $"vec3_new_string(\"%s{escapeString s}\")"
    | ELiteral(LBool b, _) -> 
        $"vec3_new_bool(%b{b})"
    | ELiteral(LUnit, _) -> 
        "vec3_new_nil()"
    | EIdentifier(id, _) -> 
        match id.Lexeme with
        | Identifier name -> name
        | _ -> failwith "Expected identifier"
    | EList(exprs, _) ->
            sprintf "({ Vec3Value* list = vec3_new_list(%d);\n%s\n    list; })"
                exprs.Length
                (exprs 
                 |> List.map (fun e -> $"    vec3_list_append(list, %s{generateExpr e});")
                 |> String.concat "\n")
    | ECall(func, args, _) ->
        match func with
        | EIdentifier(id, _) ->
            match id.Lexeme with
            | Operator(Plus, _) -> 
                match args with
                | [left; right] -> $"vec3_add(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for +"
            | Operator(Minus, _) ->
                match args with
                | [left; right] -> $"vec3_subtract(%s{generateExpr left}, %s{generateExpr right})"
                | [arg] -> $"vec3_negate(%s{generateExpr arg})"
                | _ -> failwith "Invalid number of arguments for -"
            | Identifier "print" ->
                match args with
                | [arg] -> 
                    let exprStr = generateExpr arg
                    sprintf "({ Vec3Value* tmp = %s; vec3_print(tmp); printf(\"\\n\"); tmp; })" exprStr
                | _ -> failwith "Invalid number of arguments for print"
            | _ ->
                let funcName = lexemeToString id.Lexeme
                let argsStr = args |> List.map generateExpr |> String.concat ", "
                $"%s{funcName}(%s{argsStr})"
        | _ ->
            let funcName = generateExpr func
            let argsStr = args |> List.map generateExpr |> String.concat ", "
            $"%s{funcName}(%s{argsStr})"
    | _ -> failwith "Unsupported expression type"

/// Generate C code from a statement
let generateStmt (stmt: Stmt) : string =
    match stmt with
    | SVariableDeclaration(name, expr, _) ->
        match name.Lexeme with
        | Identifier id ->
            $"Vec3Value* %s{id} = %s{generateExpr expr};"
        | _ -> failwith "Expected identifier in variable declaration"
    | SExpression(expr, _) ->
        sprintf "{ Vec3Value* result = %s; vec3_decref(result); }" (generateExpr expr)
    | _ -> failwith "Unsupported statement type"

/// Generate the complete C code from a program
let generateCCode (program: Program) : string =
    let stmts = program |> List.map generateStmt |> String.concat "\n    "
    let headers = """
#include "number.h"
#include "value.h"
#include "vec3_math.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
"""
    sprintf "%s\n\nint main(void) {\n    %s\n    return 0;\n}\n" headers stmts