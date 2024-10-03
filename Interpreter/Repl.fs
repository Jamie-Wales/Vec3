module Vec3.Interpreter.Repl

open System
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM


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
            
            // Continue the REPL
            repl' ()
    
    repl' ()

let startRepl () =
    printfn "Welcome to the Vec3 REPL!"
    printfn "Type your code and press Enter to execute."
    printfn "Type 'exit' to quit the REPL."
    repl