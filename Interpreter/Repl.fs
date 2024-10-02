module Vec3.Interpreter.Repl

open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open System
open Vec3.Interpreter.TypeChecker

let rec repl (env: Env) (typeEnv: TypeEnv) : Env * TypeEnv =
    Console.Write ">> "
    let input = Console.ReadLine()
    let parsed = parseStmt input
    // printfn $"{parsed}"
    let typeCheck = checkStmt typeEnv parsed
    match typeCheck with
    | typeEnv, Errors errors ->
        printfn $"Type Errors: {errors}"
        repl env typeEnv
    | typeEnv, Ok _ ->
        let value, env = evalStmt env parsed
        printfn $"{value}"
        repl env typeEnv


