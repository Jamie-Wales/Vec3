module Vec3.Interpreter.Repl

open Vec3.Interpreter.Parser
open Vec3.Interpreter.Eval
open System

let rec repl (env: Env) =
    Console.Write ">> "
    // check for a gnu readline lib
    let input = Console.ReadLine()
    let parsed = parseStmt input
    printfn $"parsed: {parsed}"
    let value, env' = evalStmt env parsed
    printfn $"value: {value}"
    repl env'


