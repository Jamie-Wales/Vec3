module Vec3.Transpiler.CodeGenerator

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

/// Helper function to escape strings for C
let escapeString (s: string) =
    s.Replace("\\", "\\\\").Replace("\"", "\\\"")

/// Generate C code from an expression
let rec generateExpr (expr: Expr) : string =
    match expr with
    | ELiteral(LNumber(LInteger i), _) -> $"vec3_new_number(number_from_int(%d{i}))"
    | ELiteral(LNumber(LFloat f), _) -> $"vec3_new_number(number_from_float(%f{f}))"
    | ELiteral(LString s, _) -> $"vec3_new_string(\"%s{escapeString s}\")"
    | ELiteral(LBool b, _) -> $"vec3_new_bool(%b{b})"
    | ELiteral(LUnit, _) -> "vec3_new_nil()"
    | EIdentifier(id, _) ->
        match id.Lexeme with
        | Identifier name -> name
        | _ -> failwith "Expected identifier"
    | EList(exprs, _) ->
        let args =
            (exprs
             |> List.map (fun e ->
                 if (List.rev exprs).Head = e then
                     $"%s{generateExpr e}"
                 else
                     $"%s{generateExpr e},")
             |> String.concat "\n") in

        $"vec3_new_list({exprs.Length}, {args});"
    | ECall(func, args, _) ->
        match func with
        | EIdentifier(id, _) ->
            match id.Lexeme with
            | Operator(Plus, _) ->
                match args with
                | [ left; right ] -> $"vec3_add(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for +"
            | Operator(Minus, _) ->
                match args with
                | [ left; right ] -> $"vec3_subtract(%s{generateExpr left}, %s{generateExpr right})"
                | [ arg ] -> $"vec3_negate(%s{generateExpr arg})"
                | _ -> failwith "Invalid number of arguments for -"
            | Operator(ColonColon, _) ->
                match args with
                | [ left; right ] ->
                    let leftExpr = generateExpr left
                    let rightExpr = generateExpr right in
                    sprintf $"vec3_cons(%s{leftExpr}, %s{rightExpr})"
                | _ -> failwith "Invalid number of arguments for print"
            | Identifier "print" ->
                match args with
                | [ arg ] ->
                    let exprStr = generateExpr arg
                    $"vec3_print({exprStr});"
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
        | Identifier id -> $"Vec3Value* %s{id} = %s{generateExpr expr}"
        | _ -> failwith "Expected identifier in variable declaration"
    | SExpression(expr, _) -> $"{generateExpr expr}"
    | _ -> failwith "Unsupported statement type"

/// Generate the complete C code from a program
let generateCCode (program: Program) : string =
    let stmts = program |> List.map generateStmt |> String.concat "\n    "

    let headers =
        """
#include "number.h"
#include "value.h"
#include "vec3_math.h"
#include "vec3_list.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
"""

    $"%s{headers}\n\nint main(void) {{\n    %s{stmts}\n    return 0;\n}}\n"
