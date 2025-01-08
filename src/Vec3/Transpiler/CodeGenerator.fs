module Vec3.Transpiler.CodeGenerator

open System
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

type GeneratedFunction =
    { Name: string; Implementation: string }

let functionDefinitions: GeneratedFunction list ref = ref []
let functionNames: Set<string> ref = ref Set.empty

let generateUniqueName baseName =
    let rec findUnique i =
        let name = if i = 0 then baseName else $"{baseName}_{i}"

        if Set.contains name functionNames.Value then
            findUnique (i + 1)
        else
            functionNames.Value <- Set.add name functionNames.Value
            name

    findUnique 0

let builtInFunctionMap =
    [ Operator(Plus, Some Infix), BuiltInFunction.Add
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
      Operator(ColonColon, Some Infix), BuiltInFunction.Cons]
    |> Map.ofList

let tryGetBuiltInFunction lexeme =
    if Map.containsKey lexeme builtInFunctionMap then
        Some builtInFunctionMap[lexeme]
    else
        None

let headers = $"""
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

Vec3Env* env;
"""

let initCode = $"""
    // Create global environment
    env = vec3_new_environment(NULL);
    
    // Register builtin functions
    vec3_register_builtins(env);
"""

let cleanupCode = $"""
    return 0;
"""

let escapeString (s: string) =
    s.Replace("\\", "\\\\")
     .Replace("\"", "\\\"")
     .Replace("\n", "\\n")
     .Replace("\t", "\\t")

let rec generateExpr (expr: Expr) : string =
    try
        match expr with
        | ELiteral(lit, _) ->
            match lit with
            | LNumber(LInteger i) -> $"vec3_new_number(number_from_int({i}))"
            | LNumber(LFloat f) -> $"vec3_new_number(number_from_float({f}))"
            | LNumber(LRational(n, d)) -> $"vec3_new_number(number_from_rational({n}, {d}))"
            | LNumber(LComplex(r, i)) -> $"vec3_new_number(number_from_complex({r}, {i}))"
            | LNumber(LChar c) -> $"vec3_new_number(number_from_int({int c}))"
            | LString s -> $"vec3_new_string(\"{escapeString s}\")"
            | LBool b -> $"vec3_new_bool({b})"
            | LUnit -> "vec3_new_nil()"

        | ELambda(parameters, body, _, _, _, _) ->
            let paramNames =
                parameters 
                |> List.map (fun (token, _) ->
                    match token.Lexeme with
                    | Identifier name -> name
                    | _ -> failwith "Expected identifier in parameter list")

            let funcName = generateUniqueName "lambda"
            let paramBindings = 
                paramNames 
                |> List.mapi (fun i name -> 
                    $"vec3_env_define(func_env, \"{name}\", args[{i + 1}]);")
                |> String.concat "\n    "

            let funcImpl = $"""
                Vec3Value* {funcName}(Vec3Value** args) {{
                    Vec3Value* result = NULL;
                    Vec3Function* func = ((Vec3Value*)args[0])->as.function;
                    Vec3Env* func_env = vec3_new_environment(func->env);
                    Vec3Env* prev_env = env;
                    env = func_env;
                    
                    // Bind parameters
                    {paramBindings}
                    
                    // Execute body
                    result = {generateExpr body};
                    if (result != NULL) {{
                        vec3_incref(result);
                    }}
                    
                    env = prev_env;
                    return result;
                }}
                """

            functionDefinitions.Value <- { Name = funcName; Implementation = funcImpl } :: functionDefinitions.Value
            $"vec3_new_function(\"{funcName}\", {List.length parameters}, {funcName}, env)"

        | EIdentifier(id, _) ->
            match id.Lexeme with
            | Identifier name -> $"vec3_env_get(env, \"{name}\")"
            | _ -> raise <| InvalidProgramException "Expected identifier in expression"

        | EList(exprs, _) ->
            let args = exprs |> List.map generateExpr |> String.concat ", "
            $"vec3_new_list({List.length exprs}, {args})"

        | ETuple(exprs, _) ->
            let args = exprs |> List.map generateExpr |> String.concat ", "
            $"vec3_new_list({List.length exprs}, {args})"

        | EIndex(list, index, _) ->
            let listCode = generateExpr list
            let indexCode = generateExpr index
            let func = $"vec3_env_get(env, \"index\")"
            $"vec3_call_function({func}, (Vec3Value*[]){{{func}, {listCode}, {indexCode}}}, 3)"

        | EGrouping(e, _) -> $"({generateExpr e})"

        | ECall(callee, args, _) ->
            let argExprs = args |> List.map generateExpr |> String.concat ", "
            match callee with
            | EIdentifier(id, _) ->
                match id.Lexeme with
                | Identifier name ->
                    let func = $"vec3_env_get(env, \"{name}\")"
                    $"vec3_call_function({func}, (Vec3Value*[]){{{func}, {argExprs}}}, {List.length args + 1})"
                | Operator(op, _) ->
                    let funcName =
                        match op with
                        | StarStar | Caret -> "power"
                        | Plus -> "add"
                        | Minus -> "sub"
                        | Star -> "mul"
                        | Slash -> "div"
                        | Percent -> "mod"
                        | AmpersandAmpersand -> "and"
                        | PipePipe -> "or"
                        | Bang -> "not"
                        | EqualEqual -> "eq"
                        | BangEqual -> "neq"
                        | Less -> "lt"
                        | LessEqual -> "lte"
                        | Greater -> "gt"
                        | GreaterEqual -> "gte"
                        | Cross -> "cross"
                        | DotStar -> "dot"
                        | ColonColon -> "cons"
                        | PlusPlus -> "concat"
                        | _ -> failwithf $"Unknown operator: {op}"

                    let func = $"vec3_env_get(env, \"{funcName}\")"
                    $"vec3_call_function({func}, (Vec3Value*[]){{{func}, {argExprs}}}, {List.length args + 1})"
                | _ -> failwithf $"Unknown identifier type: {id.Lexeme}"
            | _ ->
                let funcExpr = generateExpr callee
                $"vec3_call_function({funcExpr}, (Vec3Value*[]){{{funcExpr}, {argExprs}}}, {List.length args + 1})"

        | EBlock(stmts, _, _) ->
            let stmtCodes = stmts |> List.map generateStmt |> String.concat "\n    "
            $"({{ 
                Vec3Value* result = NULL;
                Vec3Env* block_env = vec3_new_environment(env);
                Vec3Env* prev_env = env;
                env = block_env;
                
                {stmtCodes}
                
                if (result != NULL) {{
                    vec3_incref(result);
                }}
                
                env = prev_env;
                result; 
            }})"

        | EIf(cond, thenExpr, elseExpr, _) ->
            let condCode = generateExpr cond
            let thenCode = generateExpr thenExpr
            let elseCode = generateExpr elseExpr
            $"vec3_is_truthy({condCode}) ? {thenCode} : {elseCode}"

        | ETail(expr, _) -> generateExpr expr

        | ERange(start, end_, _) ->
            let startCode = generateExpr start
            let endCode = generateExpr end_
            let func = $"vec3_env_get(env, \"range\")"
            $"vec3_call_function({func}, (Vec3Value*[]){{{func}, {startCode}, {endCode}}}, 3)"

        | EIndexRange(list, start, end_, _) ->
            let listCode = generateExpr list
            let startCode = generateExpr start
            let endCode = generateExpr end_
            let func = $"vec3_env_get(env, \"slice\")"
            $"vec3_call_function({func}, (Vec3Value*[]){{{func}, {listCode}, {startCode}, {endCode}}}, 4)"

        | EMatch(expr, _, _) -> failwithf $"Match expressions not implemented for: {expr}"

        | ERecordEmpty _ -> "vec3_new_record()"

        | ERecordExtend((name, value, _), record, _) ->
            let recordCode = generateExpr record
            let valueCode = generateExpr value
            match name.Lexeme with
            | Identifier fieldName -> 
                $"vec3_record_extend({recordCode}, \"{fieldName}\", {valueCode})"
            | _ -> failwith "Expected identifier in record field"

        | ERecordSelect(record, field, _) ->
            let recordCode = generateExpr record
            match field.Lexeme with
            | Identifier fieldName -> 
                $"vec3_record_get({recordCode}, \"{fieldName}\")"
            | _ -> failwith "Expected identifier in record field"

        | ERecordRestrict(record, field, _) ->
            let recordCode = generateExpr record
            match field.Lexeme with
            | Identifier fieldName -> 
                $"vec3_record_restrict({recordCode}, \"{fieldName}\")"
            | _ -> failwith "Expected identifier in record field"

    with ex ->
        printf $"{ex.Message}"
        failwithf $"Error generating code for expression {expr}:\n{ex.Message}\nStackTrace: {ex.StackTrace}"

and generateStmt (stmt: Stmt) : string =
    try
        match stmt with
        | SVariableDeclaration(name, expr, _) ->
            match name.Lexeme with
            | Identifier id ->
                let exprCode = generateExpr expr
                $"""{{
                    Vec3Value* value = {exprCode};
                    vec3_env_define(env, "{id}", value);
                }}"""
            | _ -> failwith "Expected identifier in variable declaration"

        | SExpression(expr, _) -> 
            let exprCode = generateExpr expr
            $"result = {exprCode};"

        | SAssertStatement(expr, msg, _) ->
            match msg with
            | Some msgExpr -> 
                $"if (!vec3_is_truthy({generateExpr expr})) {{ vec3_error({generateExpr msgExpr}); }}"
            | None ->
                $"if (!vec3_is_truthy({generateExpr expr})) {{ vec3_error(vec3_new_string(\"Assertion failed\")); }}"

        | STypeDeclaration _ -> ""
        | SImport _ -> ""
        | SRecFunc(name, parameters, body, _) -> failwithf $"Recursive functions not implemented for: {name}"
        | SAsync(name, parameters, body, _) -> failwithf $"Async functions not implemented for: {name}"

    with ex ->
        printf $"{ex.Message}"
        failwithf $"Error generating code for statement {stmt}:\n{ex.Message}\nStackTrace: {ex.StackTrace}"
and generateTopLevelStmt (stmt: Stmt) : string =
    try
        match stmt with
        | SVariableDeclaration(name, expr, _) ->
            match name.Lexeme with
            | Identifier id ->
                let exprCode = generateExpr expr
                $"""{{
                    Vec3Value* value = {exprCode};
                    vec3_env_define(env, "{id}", value);
                }}"""
            | _ -> failwith "Expected identifier in variable declaration"
            
        | SExpression(expr, _) ->
            let exprCode = generateExpr expr
            $"{{ Vec3Value* temp = {exprCode}; }}"
            
        | SAssertStatement(expr, msg, _) ->
            match msg with
            | Some msgExpr -> 
                $"if (!vec3_is_truthy({generateExpr expr})) {{ vec3_error({generateExpr msgExpr}); }}"
            | None ->
                $"if (!vec3_is_truthy({generateExpr expr})) {{ vec3_error(vec3_new_string(\"Assertion failed\")); }}"

        | STypeDeclaration _ -> ""
        | SImport _ -> ""
        | SRecFunc(name, parameters, body, _) -> failwithf $"Recursive functions not implemented for: {name}"
        | SAsync(name, parameters, body, _) -> failwithf $"Async functions not implemented for: {name}"

    with ex ->
        printf $"{ex.Message}"
        failwithf $"Error generating code for statement {stmt}:\n{ex.Message}\nStackTrace: {ex.StackTrace}"

let generateCCode (program: Program) : string =
    try
        functionDefinitions.Value <- []
        functionNames.Value <- Set.empty

        let stmts = program |> List.map generateTopLevelStmt |> String.concat "\n    "

        let forwardDecls =
            functionDefinitions.Value
            |> List.map (fun f -> $"Vec3Value* {f.Name}(Vec3Value** args);")
            |> String.concat "\n"

        let funcImplementations =
            functionDefinitions.Value
            |> List.rev
            |> List.map (fun f -> f.Implementation)
            |> String.concat "\n\n"

        $"""
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

Vec3Env* env;

// Forward declarations
{forwardDecls}

// Function implementations 
{funcImplementations}

int main(void) {{
    // Create global environment
    env = vec3_new_environment(NULL);
    
    // Register builtin functions
    vec3_register_builtins(env);
    
    // Program statements
    {stmts}
    
    return 0;
}}
"""
    with ex ->
        failwithf $"Error generating program code:\n{ex.Message}\nStackTrace: {ex.StackTrace}"