module Vec3.Interpreter.Repl

open System
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Typing.Checker
open Vec3.Interpreter.Typing.Inference
open Vec3.Interpreter.Eval


let evalRepl =
    let rec repl' (env: Env) (typeEnv: TypeEnv) =
        Console.Write ">> "
        let input = Console.ReadLine()
        let parsed = parseStmt input
        let typeCheck = inferStmt typeEnv parsed
        match typeCheck with
        | Ok (typeEnv, _) ->
            let value, env = evalStmt env parsed
            printfn $"{value}"
            repl' env typeEnv
        | Error errors ->
            printfn $"{formatTypeErrors errors}"
            repl' env typeEnv
            
        // let typeCheck = checkStmt typeEnv parsed
        // match typeCheck with
        // | typeEnv, Error errors ->
        //     printfn $"{formatTypeErrors errors}"
        //     repl' env typeEnv
        // | typeEnv, Ok _ ->
        //     let value, env = evalStmt env parsed
        //     printfn $"{value}"
        //     repl' env typeEnv
    
    repl' Map.empty defaultTypeEnv
    ()

let rec repl =
    let rec repl' () =
        Console.Write ">> "
        let input = Console.ReadLine()
        
        if input.ToLower() = "exit" then
            printfn "Exiting REPL..."
        else
            try
                let parsed = parse input
                match compileProgram parsed with
                | Ok (chunk, _) ->
                    interpret chunk |> ignore
                | Error (msg, _) ->
                    printfn $"Compilation error: {msg}"
            
            with
            | :? System.ArgumentException as e ->
                printfn $"Parsing error: {e.Message}"
            | e ->
                printfn $"An error occurred: {e.Message}"
            
            repl' ()
    
    repl' ()

let startRepl () =
    printfn "Welcome to the Vec3 REPL!"
    printfn "Type your code and press Enter to execute."
    printfn "Type 'exit' to quit the REPL."
    repl