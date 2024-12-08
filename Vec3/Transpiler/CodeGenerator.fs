module Vec3.Transpiler.CodeGenerator

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token


open Vec3.Interpreter.Token

let builtInFunctionMap =
    [ 
      Operator(Plus, Some Infix), BuiltInFunction.Add
      Operator(Minus, Some Infix), BuiltInFunction.Sub
      Operator(Star, Some Infix), BuiltInFunction.Mul
      Operator(Slash, Some Infix), BuiltInFunction.Div
      Operator(Percent, Some Infix), BuiltInFunction.Mod
      Operator(StarStar, Some Infix), BuiltInFunction.Pow
      Operator(Caret, Some Infix), BuiltInFunction.Pow
      Operator(AmpersandAmpersand, Some Infix), BuiltInFunction.And
      Operator(PipePipe, Some Infix), BuiltInFunction.Or
      Operator(Bang, Some Prefix), BuiltInFunction.Not
      Operator(Minus, Some Prefix), BuiltInFunction.Neg
      Operator(Plus, Some Prefix), BuiltInFunction.Unneg
      Operator(EqualEqual, Some Infix), BuiltInFunction.Eq
      Operator(BangEqual, Some Infix), BuiltInFunction.Neq
      Operator(Less, Some Infix), BuiltInFunction.Lt
      Operator(LessEqual, Some Infix), BuiltInFunction.Lte
      Operator(Greater, Some Infix), BuiltInFunction.Gt
      Operator(GreaterEqual, Some Infix), BuiltInFunction.Gte
      Operator(Cross, Some Infix), BuiltInFunction.CrossProduct
      Operator(DotStar, Some Infix), BuiltInFunction.DotProduct
      Operator(ColonColon, Some Infix), BuiltInFunction.Cons
      Operator(PlusPlus, Some Infix), BuiltInFunction.Concat
    ]
    |> Map.ofList
let tryGetBuiltInFunction lexeme =
    if Map.containsKey lexeme builtInFunctionMap then
        Some(builtInFunctionMap.[lexeme])
    else
        None
type GeneratedFunction = {
    Name: string
    Implementation: string
}

let functionDefinitions : GeneratedFunction list ref = ref []
let functionNames : Set<string> ref = ref Set.empty 

let generateUniqueName baseName =
    let rec findUnique i =
        let name = if i = 0 then baseName else $"%s{baseName}_%d{i}"
        if Set.contains name !functionNames then
            findUnique (i + 1)
        else
            functionNames := Set.add name !functionNames
            name
    findUnique 0
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
    
     | ELambda(parameters, body, _, _, _, isAsync) ->
            let paramNames = parameters |> List.map (fun (token, _) -> 
                match token.Lexeme with 
                | Identifier name -> name 
                | _ -> failwith "Expected identifier in parameter list")
                
            let funcName = 
                let baseName = 
                    match paramNames with
                    | [] -> "lambda_anonymous"
                    | [single] -> sprintf "lambda_%s" single
                    | multiple -> sprintf "lambda_%s_et_al" multiple.Head
                
                let rec findUnique i =
                    let name = if i = 0 then baseName else sprintf "%s_%d" baseName i
                    if Set.contains name !functionNames then
                        findUnique (i + 1)
                    else
                        functionNames := Set.add name !functionNames
                        name
                findUnique 0
            
            let funcImpl = $"""
            Vec3Value* %s{funcName}(Vec3Value** args) {{
                // Create function environment
                Vec3Env* func_env = vec3_new_environment(env);
                
                // Bind parameters to arguments
                %s{
                    paramNames 
                    |> List.mapi (fun i name -> 
                        $"vec3_env_define(func_env, \"{name}\", args[{i}]);")
                    |> String.concat "\n    "
                }
                
                // Execute function body
                Vec3Value* result = %s{generateExpr body};
                
                // Cleanup and return
                Vec3Value* final_result = result;
                vec3_incref(final_result);
                vec3_destroy_environment(func_env);
                vec3_decref(result);
                return final_result;
            }}
            """
            
            functionDefinitions := { Name = funcName; Implementation = funcImpl } :: !functionDefinitions
            
            // Create function value
            $"vec3_new_user_function(\"%s{funcName}\", %d{List.length parameters}, %s{funcName}, env)"
    | EIdentifier(id, _) ->
        match id.Lexeme with
        | Identifier name -> $"vec3_env_get(env, \"{name}\")"
    
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
        
    | ECall((EIdentifier(id, _)), args, _) ->
        let argExprs = args |> List.map generateExpr |> String.concat ", "
        match tryGetBuiltInFunction id.Lexeme with
        | Some BuiltInFunction.Cons ->
            // Directly call vec3_cons
            $"vec3_cons((Vec3Value*[]){{%s{argExprs}}})"
        | _ ->
            let funcExpr = generateExpr (EIdentifier(id, None))
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
    // Reset state
    functionDefinitions := []
    functionNames := Set.empty
    
    let stmts = program 
                |> List.map generateStmt 
                |> String.concat "\n    "
    
    // Generate forward declarations and implementations
    let forwardDecls = 
        !functionDefinitions
        |> List.map (fun f -> $"Vec3Value* %s{f.Name}(Vec3Value** args);")
        |> String.concat "\n"
        
    let funcImplementations = 
        !functionDefinitions
        |> List.rev  // Preserve definition order
        |> List.map (fun f -> f.Implementation)
        |> String.concat "\n\n"
    
    $"""{headers}

// Forward declarations
%s{forwardDecls}

// Function implementations
%s{funcImplementations}

int main(void) {{
{initCode}
    
    // Program statements
    %s{stmts}
    
{cleanupCode}
}}

"""