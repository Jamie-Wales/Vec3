module Vec3.Interpreter.Repl

open System
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Backend.Types
open Vec3.Interpreter.Typing.Inference
open Vec3.Interpreter.Typing.Types
open Vec3.Interpreter.Typing.Exceptions
open Vec3.Interpreter.Eval
open Vec3.Interpreter.Preprocessor
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.ConstantFolding
open Vec3.Interpreter.Token

let numberToString (n: Number): string =
    match n with
    | LFloat f -> $"Float({f})"
    | LInteger i -> $"Integer({i})"
    | LRational (n, d) -> $"Rational({n}/{d})"
    | LComplex (r, i) -> $"Complex({r}i{i})"
    
let litToString = function
    | LNumber n -> numberToString n
    | LString s -> $"\"{s}\""
    | LBool b -> $"{b}"
    | LUnit -> "()"
    
let rec exprToString  = function
    | ELiteral (lit, _) -> litToString lit
    | EList (exprs, _) -> $"""[{String.concat ", " (List.map exprToString exprs)}]"""
    | ETuple (exprs, _) -> $"""({String.concat ", " (List.map exprToString exprs)})"""
    | _ -> "()"

let evalRepl =
    let rec repl' (aliases: AliasMap) (env: Env) (typeEnv: TypeEnv) =
        Console.Write ">> "
        let input = Console.ReadLine()
        let input = preprocessContent input
        match parse input with
        | Ok (_, program) ->
            let typeCheck = inferProgram aliases typeEnv program
            match typeCheck with
            | Ok (typeEnv, aliases, _, program) ->
                let program = foldConstants program
                let value, env = evalProgram env program
                printfn $"{exprToString value}"
                repl' aliases env typeEnv
            | Error errors -> 
                printfn $"{formatTypeErrors errors}"
                repl' aliases env typeEnv
        | Error (e, s) ->  
            printfn $"{formatParserError e s}"
            repl' aliases env typeEnv
                
    repl' Map.empty Map.empty defaultTypeEnv
    ()

let executeInRepl (input: string) (vm: VM) : VM =
    try
        let parsed = parse input
        match parsed with
        | Ok(_, program) ->
            match compileProgram program with
            | Ok (func, _) ->
                loadFunction vm func
            | Error (msg, _) ->
                printfn $"Compilation error: {msg}"
                vm
        | Error (msg, _) ->
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
    let func = initFunction "Main" None
    
    repl (createNewVM { Function = func; UpValues = [||] })
    
(* Note for Jake and Bake
 Repl input doest get type checked, because if variables
 are defined in the code editor they cant be found
 maybe worth being able to turn of variable resolution
 OR pass vm to parser to check env
*)
let noTcParseAndCompile (code: string) (vm:VM) =
    let code = Prelude.prelude + code
    let code = preprocessContent code
    match parse code with
    | Ok (_, program) ->
            match compileProgram program with
            | Ok (func, _) -> Some(loadFunction vm func)
            | Error (msg, _) ->
                printfn $"Compilation error: {msg}"
                None
    | Error (e, s) ->
        printfn $"Parsing error: {formatParserError e s}"
        None
        
let parseAndCompile (code: string) (vm:VM) =
    let code = Prelude.prelude + code
    let code = preprocessContent code
    match parse code with
    | Ok (_, program) ->
        match inferProgram Map.empty defaultTypeEnv program with
        | Ok (_, _, _, program) ->
            match compileProgram program with
            | Ok (func, _) -> Some(loadFunction vm func)
            | Error (msg, _) ->
                printfn $"Compilation error: {msg}"
                None
        | Error errors ->
            printfn $"Type error: {formatTypeErrors errors}"
            None
    | Error (e, s) ->
        printfn $"Parsing error: {formatParserError e s}"
        None
