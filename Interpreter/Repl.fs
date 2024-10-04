module Vec3.Interpreter.Repl

open System
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Typing.Checker
open Vec3.Interpreter.Typing.Inference
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Preprocessor


let evalRepl =
    let rec repl' (env: Env) (typeEnv: TypeEnv) =
        Console.Write ">> "
        let input = Console.ReadLine()
        let input = preprocessContent input
        match parse input with
        | Ok (program, _) ->
            let typeCheck = inferProgram typeEnv program
            match typeCheck with
            | Ok (typeEnv, _) ->
                let value, env = evalProgram env program
                printfn $"{value}"
                repl' env typeEnv
            | Error errors ->
                printfn $"{formatTypeErrors errors}"
                repl' env typeEnv
        | Error (e, s) -> 
            printfn $"{formatParserError e s}"
            repl' env typeEnv
                
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
                match parsed with
                | Ok(program, _) ->
                    match compileProgram program with
                    | Ok (chunk, _) ->
                        interpret chunk |> ignore
                    | Error (msg, _) ->
                        printfn $"Compilation error: {msg}"
                | Error (msg, _) ->
                    printfn $"Parsing error: {msg}"
            
            with
            | :? ArgumentException as e ->
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