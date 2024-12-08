module Vec3.Transpiler.CodeGenerator

open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

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
    ]
    |> Map.ofList

let tryGetBuiltInFunction lexeme =
    if Map.containsKey lexeme builtInFunctionMap then
        Some(builtInFunctionMap.[lexeme])
    else
        None

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
    try
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
                    
                let funcName = generateUniqueName "lambda"
                
                let funcImpl = $"""
                Vec3Value* %s{funcName}(Vec3Value** args) {{
                    Vec3Env* func_env = vec3_new_environment(NULL);
                    
                    // Bind parameters
                    %s{
                        paramNames 
                        |> List.mapi (fun i name -> 
                            $"vec3_env_define(func_env, \"{name}\", args[{i}]);")
                        |> String.concat "\n    "
                    }
                    
                    // Execute body
                    Vec3Value* result = %s{generateExpr body};
                    
                    vec3_incref(result);
                    vec3_destroy_environment(func_env);
                    return result;
                }}
                """
                
                functionDefinitions := { Name = funcName; Implementation = funcImpl } :: !functionDefinitions
                
                $"vec3_new_function(\"%s{funcName}\", %d{List.length parameters}, %s{funcName}, env)"
    
        
        | EIdentifier(id, _) ->
            match id.Lexeme with
            | Identifier name -> $"vec3_env_get(env, \"{name}\")"
        
        | EList(exprs, _) ->
            let args = exprs |> List.map generateExpr |> String.concat ", "
            $"vec3_new_list(%d{List.length exprs}, %s{args})"
        
        | ETuple(exprs, _) ->
            let args = exprs |> List.map generateExpr |> String.concat ", "
            $"vec3_new_list(%d{List.length exprs}, %s{args})"
            
        | EIndex(list, index, _) ->
            let listCode = generateExpr list
            let indexCode = generateExpr index
            $"vec3_list_get(%s{listCode}, %s{indexCode})"

        | EGrouping(e, _) -> 
            $"(%s{generateExpr e})"
        
        | ECall(callee, args, _) ->
            let argExprs = args |> List.map generateExpr |> String.concat ", "
            match callee with
            | EIdentifier(id, _) ->
                match id.Lexeme with
                | Identifier name -> 
                    $"vec3_call_function(vec3_env_get(env, \"{name}\"), (Vec3Value*[]){{{argExprs}}}, {List.length args})"
                | Operator(op, _) ->
                    let funcName = 
                        match op with
                        | Plus -> "add"
                        | Minus -> "sub"
                        | Star -> "mul"
                        | Slash -> "div"
                        | Percent -> "mod"
                        | StarStar -> "pow"
                        | Caret -> "pow"
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
                        | _ -> failwithf "Unknown operator: %A" op
                    $"vec3_call_function(vec3_env_get(env, \"{funcName}\"), (Vec3Value*[]){{{argExprs}}}, {List.length args})"
                | _ -> failwithf "Unknown identifier type: %A" id.Lexeme
            | _ -> 
            let funcExpr = generateExpr callee
            $"vec3_call_function(%s{funcExpr}, (Vec3Value*[]){{{argExprs}}}, {List.length args})"        
        | EBlock(stmts, _) ->
            let stmtCodes = stmts |> List.map generateStmt |> String.concat "\n    "
            $"{{\n    Vec3Env* block_env = vec3_new_environment(env);\n    %s{stmtCodes}\n    vec3_destroy_environment(block_env);\n}}"
            
        | EIf(cond, thenExpr, elseExpr, _) ->
            let condCode = generateExpr cond
            let thenCode = generateExpr thenExpr
            let elseCode = generateExpr elseExpr
            $"vec3_is_truthy(%s{condCode}) ? %s{thenCode} : %s{elseCode}"

        | ETail(expr, _) -> generateExpr expr
            
        | ERange(start, end_, _) ->
            let startCode = generateExpr start
            let endCode = generateExpr end_
            $"vec3_range(%s{startCode}, %s{endCode})"

        | EIndexRange(list, start, end_, _) ->
            let listCode = generateExpr list
            let startCode = generateExpr start
            let endCode = generateExpr end_
            $"vec3_list_slice(%s{listCode}, %s{startCode}, %s{endCode})"

        | EMatch(expr, cases, _) ->
            failwithf "Match expressions not implemented for: %A" expr 

        | ERecordEmpty _ -> 
            "vec3_new_list(0)"
        
        | ERecordExtend((name, value, _), record, _) ->
            let recordCode = generateExpr record
            let valueCode = generateExpr value
            match name.Lexeme with
            | Identifier fieldName -> 
                $"vec3_record_extend(%s{recordCode}, \"%s{fieldName}\", %s{valueCode})"
            | _ -> failwith "Expected identifier in record field"
        
        | ERecordSelect(record, field, _) ->
            let recordCode = generateExpr record
            match field.Lexeme with
            | Identifier fieldName ->
                $"vec3_record_select(%s{recordCode}, \"%s{fieldName}\")"
            | _ -> failwith "Expected identifier in record field"
        
        | ERecordRestrict(record, field, _) ->
            let recordCode = generateExpr record
            match field.Lexeme with
            | Identifier fieldName ->
                $"vec3_record_restrict(%s{recordCode}, \"%s{fieldName}\")"
            | _ -> failwith "Expected identifier in record field"

    with ex ->
        printf $"%s{ex.Message}"
        failwithf "Error generating code for expression %A:\n%s\nStackTrace: %s" expr ex.Message ex.StackTrace

and generateStmt (stmt: Stmt) : string =
    try
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

        | STypeDeclaration _ -> ""  
        | SImport _ -> ""  
        | SRecFunc(name, parameters, body, _) -> 
            failwithf "Recursive functions not implemented for: %A" name
        | SAsync(name, parameters, body, _) -> 
            failwithf "Async functions not implemented for: %A" name

    with ex ->
        printf $"%s{ex.Message}"
        failwithf "Error generating code for statement %A:\n%s\nStackTrace: %s" stmt ex.Message ex.StackTrace

let generateCCode (program: Program) : string =
    try
        functionDefinitions := []
        functionNames := Set.empty
        
        let stmts = program 
                    |> List.map generateStmt
                    |> String.concat "\n    "
        
        let forwardDecls = 
            !functionDefinitions
            |> List.map (fun f -> $"Vec3Value* %s{f.Name}(Vec3Value** args);")
            |> String.concat "\n"
            
        let funcImplementations = 
            !functionDefinitions
            |> List.rev
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
    with ex ->
        failwithf "Error generating program code:\n%s\nStackTrace: %s" ex.Message ex.StackTrace