module Vec3.Transpiler.Transpiler

open System
open System.IO
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Typing

/// <summary>
/// Represents possible transpiler errors with detailed messages
/// </summary>
type TranspilerError = 
    | IOError of string
    | ParseError of string * ParserState
    | TypeError of string
    | CodeGenError of string
    | CompilationError of string

/// <summary>
/// Configuration for the transpiler including paths and compiler settings
/// </summary>
type TranspilerConfig = {
    OutputDir: string
    RuntimeDir: string  
    IncludeDir: string 
    CompilerPath: string 
}

/// <summary>
/// Finds the project root directory by looking for Executable folder
/// </summary>
let findProjectRoot () =
    let rec findUp dir =
        if Directory.Exists(Path.Combine(dir, "Executable")) then
            dir
        else
            let parent = Directory.GetParent(dir)
            if parent = null then
                failwith "Could not find project root (Executable directory not found in parent directories)"
            else
                findUp parent.FullName
    
    findUp (Directory.GetCurrentDirectory())

/// <summary>
/// Creates a default configuration with correct project paths
/// </summary>
let defaultConfig = 
    let isWindows = Environment.OSVersion.Platform = PlatformID.Win32NT
    let projectRoot = findProjectRoot()
    {
        OutputDir = Path.Combine(projectRoot, "build")
        RuntimeDir = Path.Combine(projectRoot, "Executable", "src")
        IncludeDir = Path.Combine(projectRoot, "Executable", "include")
        CompilerPath = if isWindows then "gcc.exe" else "gcc"
    }

/// <summary>
/// Gets the appropriate file extension for executables based on platform
/// </summary>
let getExecutableExtension() =
    if Environment.OSVersion.Platform = PlatformID.Win32NT then
        ".exe"
    else
        ""

/// <summary>
/// Ensures all necessary directories exist and returns proper paths
/// </summary>
let ensureDirectories (config: TranspilerConfig) =
    // Get project root and create absolute paths
    let projectRoot = findProjectRoot()
    let absOutputDir = 
        if Path.IsPathRooted(config.OutputDir) then
            config.OutputDir
        else
            Path.GetFullPath(Path.Combine(projectRoot, config.OutputDir))
            
    // Create build directory structure
    Directory.CreateDirectory(absOutputDir) |> ignore
    Directory.CreateDirectory(Path.Combine(absOutputDir, "src")) |> ignore
    Directory.CreateDirectory(Path.Combine(absOutputDir, "include")) |> ignore
    
    // Return config with absolute paths
    {
        OutputDir = absOutputDir
        IncludeDir = Path.GetFullPath(Path.Combine(projectRoot, "Executable", "include"))
        RuntimeDir = Path.GetFullPath(Path.Combine(projectRoot, "Executable", "src"))
        CompilerPath = config.CompilerPath
    }

/// <summary>
/// Copies runtime files to the build directory
/// </summary>
let copyRuntimeFiles (config: TranspilerConfig) =
    try
        printfn "Copying runtime files..."
        // Copy header files
        let headerFiles = Directory.GetFiles(config.IncludeDir, "*.h")
        for file in headerFiles do
            let destFile = Path.Combine(config.OutputDir, "include", Path.GetFileName(file))
            printfn $"Copying header: %s{file} -> %s{destFile}"
            File.Copy(file, destFile, true)

        // Copy source files
        let sourceFiles = Directory.GetFiles(config.RuntimeDir, "*.c")
        for file in sourceFiles do
            let destFile = Path.Combine(config.OutputDir, "src", Path.GetFileName(file))
            printfn $"Copying source: %s{file} -> %s{destFile}"
            File.Copy(file, destFile, true)
        Ok ()
    with ex ->
        Error(IOError $"Failed to copy runtime files: {ex.Message}")

/// <summary>
/// Writes the generated C code to a file
/// </summary>
let writeGeneratedCode (code: string) (outputPath: string) =
    try
        printfn $"Writing generated code to: %s{outputPath}"
        File.WriteAllText(outputPath, code)
        Ok ()
    with ex ->
        Error(IOError $"Failed to write generated code: {ex.Message}")

/// <summary>
/// Compiles the C code using gcc with proper include path handling
/// </summary>
let compileCode (config: TranspilerConfig) (mainFile: string) =
    try
        let outputExe = Path.Combine(config.OutputDir, "program" + getExecutableExtension())
        
        // Create proper include path with quotes to handle spaces
        let includePath = Path.Combine(config.OutputDir, "include")
        let includeFlag = $"-I\"%s{includePath}\""
        
        // Get all source files (excluding main.c) and add our generated main.c
        let sourceFiles = 
            Directory.GetFiles(Path.Combine(config.OutputDir, "src"), "*.c")
            |> Array.filter (fun f -> Path.GetFileName(f) <> "main.c") // Exclude the runtime main.c
            |> Array.append [| mainFile |]  // Add our generated main.c
            |> Array.map (fun path -> $"\"%s{path}\"")
            |> String.concat " "
            
        let compileCommand = $"{config.CompilerPath} {includeFlag} {sourceFiles} -o \"{outputExe}\""
        printfn $"Executing compile command: %s{compileCommand}"
        
        use proc = new System.Diagnostics.Process()
        proc.StartInfo.FileName <- 
            if Environment.OSVersion.Platform = PlatformID.Win32NT then
                "cmd.exe"
            else 
                "/bin/bash"
                
        proc.StartInfo.Arguments <- 
            if Environment.OSVersion.Platform = PlatformID.Win32NT then
                $"/c {compileCommand}"
            else
                $"-c \"{compileCommand}\""
                
        proc.StartInfo.UseShellExecute <- false
        proc.StartInfo.RedirectStandardOutput <- true
        proc.StartInfo.RedirectStandardError <- true
        proc.StartInfo.CreateNoWindow <- true
        
        if proc.Start() then
            let output = proc.StandardOutput.ReadToEnd()
            let error = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            
            if proc.ExitCode = 0 then 
                printfn "Compilation successful"
                Ok outputExe
            else
                printfn $"Compiler output: %s{output}"
                printfn $"Compiler error: %s{error}"
                Error(CompilationError $"Compilation failed:\nOutput: {output}\nError: {error}")
        else
            Error(CompilationError "Failed to start compiler process")
    with ex ->
        Error(CompilationError $"Compilation error: {ex.Message}")
/// <summary>
/// Main transpilation function that coordinates the entire process
/// </summary>
let transpile (inputPath: string) (config: TranspilerConfig) : Result<string, TranspilerError> =
    result {
        // Ensure directories exist and get absolute paths
        let config = ensureDirectories config
        
        printfn $"Using output directory: %s{config.OutputDir}"
        printfn $"Using include directory: %s{config.IncludeDir}"
        printfn $"Using runtime directory: %s{config.RuntimeDir}"
        
        // Read and process input file
        let! sourceCode = 
            try 
                let absInputPath = Path.GetFullPath(inputPath)
                printfn $"Reading source file: %s{absInputPath}"
                Ok(File.ReadAllText(absInputPath))
            with ex -> 
                Error(IOError $"Failed to read input file: {ex.Message}")
            
        // Parse the code
        let! _, program = 
            match parse sourceCode with
            | Ok result -> Ok result
            | Error (err, state) -> Error(ParseError(formatParserError err state, state))
            
        // Type check the program
        let! _, _, _, typedProgram =
            match Inference.inferProgram Map.empty Inference.defaultTypeEnv program with
            | Ok result -> Ok result
            | Error errors -> Error(TypeError $"Type error: %A{errors}")
            
        // Generate C code
        let! cCode =
            try 
                printfn "Generating C code..."
                Ok(CodeGenerator.generateCCode typedProgram)
            with ex -> 
                Error(CodeGenError $"Code generation failed: {ex.Message}")
            
        // Copy runtime files
        do! copyRuntimeFiles config
        
        // Write generated code
        let mainFile = Path.Combine(config.OutputDir, "src", "main.c")
        do! writeGeneratedCode cCode mainFile
        
        // Compile the code
        printfn "Starting compilation..."
        let! exePath = compileCode config mainFile
        
        return exePath
    }

/// <summary>
/// Helper to format error messages
/// </summary>
let formatError = function
    | IOError msg -> $"IO Error: {msg}"
    | ParseError (msg, _) -> $"Parse Error: {msg}"
    | TypeError msg -> $"Type Error: {msg}"
    | CodeGenError msg -> $"Code Generation Error: {msg}"
    | CompilationError msg -> $"Compilation Error: {msg}"

/// <summary>
/// Helper to determine if a path is absolute
/// </summary>
let isPathAbsolute (path: string) =
    if Environment.OSVersion.Platform = PlatformID.Win32NT then
        Path.IsPathRooted(path)
    else
        path.StartsWith("/")

/// <summary>
/// Helper to convert a relative path to absolute path
/// </summary>
let toAbsolutePath (basePath: string) (path: string) =
    if isPathAbsolute path then
        path
    else
        Path.GetFullPath(Path.Combine(basePath, path))

/// <summary>
/// Creates a TranspilerConfig with the specified output directory
/// </summary>
let createConfig (outputDir: string option) =
    match outputDir with
    | Some dir -> { defaultConfig with OutputDir = dir }
    | None -> defaultConfig