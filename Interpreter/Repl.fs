module Vec3.Interpreter.Repl

open System
open Vec3.Interpreter.Backend
open Vec3.Interpreter.Backend.Value
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Typing.Checker
open Vec3.Interpreter.Typing.Inference
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Preprocessor
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token

let rec exprToString  = function
    | ELiteral (lit, _) -> litToString lit
    | EList (exprs, _) -> $"""[{String.concat ", " (List.map exprToString exprs)}]"""
    | _ -> "()"

and litToString = function
    | LNumber n -> numberToString n
    | LString s -> $"\"{s}\""
    | LBool b -> $"{b}"
    | LUnit -> "()"

and numberToString = function
    | LInteger x -> $"{x}"
    | LFloat x -> $"{x}"
    | LRational (n, d) -> $"{n}/{d}"
    | LComplex (r, i) -> $"{r} + {i}i"


let evalRepl =
    let rec repl' (env: Env) (typeEnv: TypeEnv) =
        Console.Write ">> "
        let input = Console.ReadLine()
        let input = preprocessContent input
        match parse input with
        | Ok (program, _) ->
            let typeCheck = inferProgram typeEnv program
            match typeCheck with
            | Ok (typeEnv, _, program) ->
                let value, env = evalProgram env program
                printfn $"{exprToString value}"
                repl' env typeEnv
            | Error errors ->
                printfn $"{formatTypeErrors errors}"
                repl' env typeEnv
        | Error (e, s) -> 
            printfn $"{formatParserError e s}"
            repl' env typeEnv
                
    repl' Map.empty defaultTypeEnv
    ()

type ReplState = {
    VM: VM option
}


let createInitialState () = { VM = None }

let executeInRepl (state: ReplState) (input: string) : ReplState =
    try
        let parsed = parse input
        match parsed with
        | Ok(program, _) ->
            match compileProgram program with
            | Ok (chunk, _) ->
                let vm = 
                    match state.VM with
                    | Some existingVM -> 
                        { existingVM with 
                            Chunk = chunk
                            IP = 0
                            Stack = ResizeArray<Value>(256) }
                    | None -> createVM chunk
                let updatedVM = run vm
                { VM = Some updatedVM }
            | Error (msg, _) ->
                printfn $"Compilation error: {msg}"
                state
        | Error (msg, _) ->
            printfn $"Parsing error: {msg}"
            state
    with
    | :? ArgumentException as e ->
        printfn $"Parsing error: {e.Message}"
        state
    | e ->
        printfn $"An error occurred: {e.Message}"
        state

let rec repl (state: ReplState) =
    Console.Write ">> "
    let input = Console.ReadLine()
    
    if input.ToLower() = "exit" then
        printfn "Exiting REPL..."
    else
        let newState = executeInRepl state input
        repl newState

let startRepl () =
    printfn "Welcome to the Vec3 REPL!"
    printfn "Type your code and press Enter to execute."
    printfn "Type 'exit' to quit the REPL."
    let initialState = createInitialState()
    repl initialState
let parseAndCompile (code: string) =
        match parse code with
        | Ok (program, _) ->
            match compileProgram program with
            | Ok (chunk, _) -> Some chunk
            | Error (msg, _) ->
                printfn $"Compilation error: {msg}"
                None
        | Error (msg, _) ->
            printfn $"Parsing error: {msg}"
            None 
