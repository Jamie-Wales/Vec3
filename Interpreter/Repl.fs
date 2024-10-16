module Vec3.Interpreter.Repl

open System
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Backend.Compiler
open Vec3.Interpreter.Backend.VM
open Vec3.Interpreter.Typing.Inference
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
    | ERecord (fields, _) -> $"""{{ {String.concat ", " (List.map (fun (k, v, typ) -> $"{k.Lexeme}: {typ} = {exprToString v}") fields)} }}"""
    | _ -> "()"

let evalRepl =
    let rec repl' (env: Env) (typeEnv: TypeEnv) =
        Console.Write ">> "
        let input = Console.ReadLine()
        let input = preprocessContent input
        match parse input with
        | Ok (_, program) ->
            let typeCheck = inferProgram typeEnv program
            match typeCheck with
            | Ok (typeEnv, _, program) ->
                let program = foldConstants program
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

let createInitialState () = {
    VM = None
}

let executeInRepl (state: ReplState) (input: string) : ReplState =
    try
        let parsed = parse input
        match parsed with
        | Ok(_, program) ->
            match compileProgram program with
            | Ok (func, _) ->
                let vm = 
                    match state.VM with
                    | Some existingVM ->
                        let newFrame = {
                            Function = func
                            IP = 0
                            StackBase = existingVM.Stack.Count
                            Locals = [||] 
                        }
                        existingVM.Frames.Add(newFrame)
                        existingVM
                    | None -> createVM func
                let updatedVM, _ = interpretWithMode func (Some vm) true
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
    let code = preprocessContent code
    match parse code with
    | Ok (_, program) ->
        match inferProgram defaultTypeEnv program with
        | Ok (_, _, program) ->
            match compileProgram program with
            | Ok (func, _) -> Some func
            | Error (msg, _) ->
                printfn $"Compilation error: {msg}"
                None
        | Error errors ->
            printfn $"Type error: {formatTypeErrors errors}"
            None
    | Error (e, s) ->
        printfn $"Parsing error: {formatParserError e s}"
        None
