module Vec3.Interpreter.Repl

open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open System
open Vec3.Interpreter.TypeChecker

let rec repl (env: Env) (typeEnv: TypeEnv) : Env * TypeEnv =
    Console.Write ">> "
    let input = Console.ReadLine()
    let parsed = parseStmt input
    let typeEnv = checkStmt typeEnv parsed
    let value, env = evalStatement env parsed
    printfn $"{value}"
    repl env typeEnv


