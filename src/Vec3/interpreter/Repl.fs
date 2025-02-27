/// <summary>
/// REPL for the Vec3 language.
/// </summary>
module Vec3.Interpreter.Repl

open System
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Typing.Inference
open Vec3.Interpreter.Typing.Types
open Vec3.Interpreter.Typing.Exceptions
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Token
open Vec3.Interpreter.ConstantFolding
open Vec3.Interpreter.DCE

let litToString =
    function
    | LNumber n -> numberToString n
    | LString s -> $"\"{s}\""
    | LBool b -> $"{b}"
    | LUnit -> "()"

let rec exprToString =
    function
    | ELiteral(lit, _) -> litToString lit
    | EList(exprs, _) -> $"""[{String.concat ", " (List.map exprToString exprs)}]"""
    | ETuple(exprs, _) -> $"""({String.concat ", " (List.map exprToString exprs)})"""
    | _ -> "()"


let executeInRepl (input: string) (vm: VM) : VM =
    try
        let parsed = parse input true
        match parsed with
        | Ok(_, program) ->
            match compileProgram program with
            | Ok(func, _) -> loadFunction vm func
            | Error(msg, _) ->
                printfn $"Compilation error: {msg}"
                vm
        | Error(msg, _) ->
            printfn $"Parsing error: {msg}"
            vm
    with
    | :? ArgumentException as e ->
        printfn $"Parsing error: {e.Message}"
        vm
    | e ->
        printfn $"An error occurred: {e.Message}"
        vm

let rec repl (state: VM) =
    Console.Write ">> "
    let input = Console.ReadLine()

    if input.ToLower() = "exit" then
        printfn "Exiting REPL..."
    else
        let newState = executeInRepl input state
        repl newState

let startRepl () =
    printfn "Welcome to the Vec3 REPL!"
    printfn "Type your code and press Enter to execute."
    printfn "Type 'exit' to quit the REPL."
    repl (createNewVM (initFunction "Main"))

let noTcParseAndCompile (code: string) (vm: VM) t =
    match parse code t with
    | Ok(_, program) ->
        // let program = eliminate program
        let program = foldConstants program

        match compileProgram program with
        | Ok(func, _) -> Some (loadFunction vm func)
        | Error(msg, _) ->
            printfn $"Compilation error: {msg}"
            None
    | Error(e, s) ->
        printfn $"Parsing error: {formatParserError e s}"
        None

let parseAndCompile (code: string) (vm: VM) =
    match parse code true with
    | Ok(_, program) ->
        match inferProgram Map.empty defaultTypeEnv program with
        | Ok(_, _, _, program) ->

            let program = eliminate program
            let program = foldConstants program

            match compileProgram program with
            | Ok(func, _) -> Some(loadFunction vm func)
            | Error(msg, _) ->
                printfn $"Compilation error: {msg}"
                None
        | Error errors ->
            printfn $"Type error: {formatTypeErrors errors}"
            None
    | Error(e, s) ->
        printfn $"Parsing error: {formatParserError e s}"
        None

let parseAndCompileWithTE (code: string) (vm: VM) (env: TypeEnv) =
    match parse code true with
    | Ok(_, program) ->
        match inferProgram Map.empty env program with
        | Ok(env, _, _, program) ->
            let program = eliminate program
            let program = foldConstants program

            match compileProgram program with
            | Ok(func, _) -> Some(loadFunction vm func, env)
            | Error(msg, _) ->
                printfn $"Compilation error: {msg}"
                None
        | Error errors ->
            printfn $"Type error: {formatTypeErrors errors}"
            None
    | Error(e, s) ->
        printfn $"Parsing error: {formatParserError e s}"
        None
