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
        | _ -> failwith "Expected identifier"
    
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
        match func with
        | EIdentifier(id, _) ->
            match id.Lexeme with
            | Operator(Plus, _) ->
                match args with
                | [left; right] -> $"vec3_add(%s{generateExpr left}, %s{generateExpr right})"
                | [arg] -> generateExpr arg  // Unary plus
                | _ -> failwith "Invalid number of arguments for +"
            
            | Operator(Minus, _) ->
                match args with
                | [left; right] -> $"vec3_subtract(%s{generateExpr left}, %s{generateExpr right})"
                | [arg] -> $"vec3_negate(%s{generateExpr arg})"
                | _ -> failwith "Invalid number of arguments for -"
            
            | Operator(Star, _) ->
                match args with
                | [left; right] -> $"vec3_multiply(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for *"
                
            | Operator(Slash, _) ->
                match args with
                | [left; right] -> $"vec3_divide(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for /"
                
            | Operator(Percent, _) ->
                match args with
                | [left; right] -> $"vec3_modulo(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for %"
            
            | Operator(StarStar, _) ->
                match args with
                | [left; right] -> $"vec3_power(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for **"
            
            | Operator(ColonColon, _) ->
                match args with
                | [left; right] -> $"vec3_cons(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for ::"
                
            | Operator(EqualEqual, _) ->
                match args with
                | [left; right] -> $"vec3_equal(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for =="
                
            | Operator(BangEqual, _) ->
                match args with
                | [left; right] -> $"vec3_not_equal(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for !="
                
            | Operator(Less, _) ->
                match args with
                | [left; right] -> $"vec3_less(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for <"
                
            | Operator(LessEqual, _) ->
                match args with
                | [left; right] -> $"vec3_less_equal(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for <="
                
            | Operator(Greater, _) ->
                match args with
                | [left; right] -> $"vec3_greater(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for >"
                
            | Operator(GreaterEqual, _) ->
                match args with
                | [left; right] -> $"vec3_greater_equal(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for >="
                
            | Operator(AmpersandAmpersand, _) ->
                match args with
                | [left; right] -> $"vec3_and(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for &&"
                
            | Operator(PipePipe, _) ->
                match args with
                | [left; right] -> $"vec3_or(%s{generateExpr left}, %s{generateExpr right})"
                | _ -> failwith "Invalid number of arguments for ||"
                
            | Operator(Bang, _) ->
                match args with
                | [arg] -> $"vec3_not(%s{generateExpr arg})"
                | _ -> failwith "Invalid number of arguments for !"
            
            | Identifier "print" ->
                let argsStr = args |> List.map generateExpr |> String.concat ", "
                $"vec3_print(%s{argsStr}, true)"
                
            | Identifier "input" ->
                if List.isEmpty args then
                    "vec3_input()"
                else
                    failwith "input takes no arguments"
            
            | _ ->
                let funcName = lexemeToString id.Lexeme
                let argsStrs = args |> List.map generateExpr
                let argsArray = $"(Vec3Value*[]){{{argsStrs}}}"
                $"vec3_call_function(vec3_env_get(env, \"{funcName}\"), %s{argsArray}, %d{List.length args})"
        
        | _ ->
            let funcExpr = generateExpr func
            let argsStrs = args |> List.map generateExpr
            let argsArray = $"(Vec3Value*[]){{{argsStrs}}}"
            $"vec3_call_function(%s{funcExpr}, %s{argsArray}, %d{List.length args})"
            
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