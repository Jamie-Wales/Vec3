module Vec3.Transpiler.CodeGenerator

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

let headers = """
#include "number.h"
#include "value.h"
#include "vec3_math.h"
#include "vec3_list.h"
#include "env.h"
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
"""

let initCode = """
    // Create global environment
    Vec3Env* env = vec3_new_environment(NULL);
    
    // Register builtin functions
    vec3_register_builtins(env);
"""

let cleanupCode = """
    // Cleanup environment
    vec3_destroy_environment(env);
    return 0;
"""
let escapeString (s: string) =
    s.Replace("\\", "\\\\")
     .Replace("\"", "\\\"")
     .Replace("\n", "\\n")
     .Replace("\t", "\\t")

let rec generateExpr (expr: Expr) : string =
    match expr with
    | ELiteral(lit, _) ->
        match lit with
        | LNumber(LInteger i) -> $"vec3_new_number(number_from_int(%d{i}))"
        | LNumber(LFloat f) -> $"vec3_new_number(number_from_float(%f{f}))"
        | LNumber(LRational(n, d)) -> $"vec3_new_number(number_from_rational(%d{n}, %d{d}))"
        | LNumber(LComplex(r, i)) -> $"vec3_new_number(number_from_complex(%f{r}, %f{i}))"
        | LNumber(LChar c) -> $"vec3_new_number(number_from_int(%d{int c}))"
        | LString s -> $"vec3_new_string(\"%s{escapeString s}\")"
        | LBool b -> $"vec3_new_bool(%b{b})"
        | LUnit -> "vec3_new_nil()"
    
    | EIdentifier(id, _) ->
        match id.Lexeme with
        | Identifier name -> $"vec3_env_get(env, \"{name}\")"
        | Operator(op, _) -> $"vec3_env_get(env, \"{lexemeToString id.Lexeme}\")"
    
    | EList(exprs, _) ->
        let args = exprs 
                  |> List.map generateExpr 
                  |> String.concat ", "
        $"vec3_new_list(%d{List.length exprs}, %s{args})"
    
    | ETuple(exprs, _) ->
        let args = exprs 
                  |> List.map generateExpr 
                  |> String.concat ", "
        $"vec3_new_list(%d{List.length exprs}, %s{args})"
        
    | EIndex(list, index, _) ->
        let listCode = generateExpr list
        let indexCode = generateExpr index
        $"vec3_list_get(%s{listCode}, %s{indexCode})"

    | EGrouping(e, _) -> 
        $"(%s{generateExpr e})"
        
    | ECall(func, args, _) ->
        let funcExpr = generateExpr func
        let argExprs = args
                       |> List.map generateExpr
                       |> String.concat ", "

        $"vec3_call_function(%s{funcExpr}, (Vec3Value*[]){{%s{argExprs}}}, %d{List.length args})"
            
    | EBlock(stmts, _) ->
        let stmtCodes = stmts |> List.map generateStmt |> String.concat "\n    "
        $"{{\n    Vec3Env* block_env = vec3_new_environment(env);\n    %s{stmtCodes}\n    vec3_destroy_environment(block_env);\n}}"
        
    | EIf(cond, thenExpr, elseExpr, _) ->
        let condCode = generateExpr cond
        let thenCode = generateExpr thenExpr
        let elseCode = generateExpr elseExpr
        $"vec3_is_truthy(%s{condCode}) ? %s{thenCode} : %s{elseCode}"
    
    | _ -> failwith $"Unsupported expression type: {expr}"
/// Generate C code from a statement
and generateStmt (stmt: Stmt) : string =
    match stmt with
    | SVariableDeclaration(name, expr, _) ->
        match name.Lexeme with
        | Identifier id -> 
            let exprCode = generateExpr expr
            $"vec3_env_define(env, \"{id}\", %s{exprCode});"
        | _ -> failwith "Expected identifier in variable declaration"
    
    | SExpression(expr, _) -> 
        $"%s{generateExpr expr};"
        
    | SAssertStatement(expr, msg, _) ->
        match msg with
        | Some msgExpr ->
            $"if (!vec3_is_truthy(%s{generateExpr expr})) {{ vec3_error(%s{generateExpr msgExpr}); }}"
        | None ->
            $"if (!vec3_is_truthy(%s{generateExpr expr})) {{ vec3_error(vec3_new_string(\"Assertion failed\")); }}"
    
    | _ -> failwith $"Unsupported statement type: {stmt}"

/// Generate the complete C code from a program
let generateCCode (program: Program) : string =
    let stmts = program 
                |> List.map generateStmt 
                |> String.concat "\n    "
    
    $"""{headers}

int main(void) {{
{initCode}
    
    // Program statements
    %s{stmts}
    
{cleanupCode}
}}
"""