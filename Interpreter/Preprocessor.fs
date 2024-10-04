module Vec3.Interpreter.Preprocessor

open System
open System.IO
open System.Text.RegularExpressions

// preprocessing -> open "filename.vec3" -> read file -> preprocess -> return content

let rec preprocess (filename: string): string =
    let content = File.ReadAllText filename
    let lines = content.Split [| '\n' |] |> Array.toList
    
    let rec preprocess' (lines: string list) (acc: string) =
        match lines with
        | [] -> acc
        | line :: rest ->
            let line = line.Trim()
            if line.StartsWith "open" then
                let includeFile = line.Substring(5).Trim()
                let includeContent = preprocess includeFile
                preprocess' rest (acc + includeContent)
            else
                preprocess' rest (acc + line + "\n")
    
    preprocess' lines ""

let rec preprocessLine (line: string): string =
    if line.StartsWith "open" then
        let includeFile = line.Substring(5).Trim()
        preprocess includeFile
    else
        line

let rec preprocessContent (content: string): string =
    let lines = content.Split [| '\n' |] |> Array.toList
    
    let rec preprocess' (lines: string list) (acc: string) =
        match lines with
        | [] -> acc
        | line :: rest ->
            if line.StartsWith "open" then
                let includeFile = line.Substring(5).Trim()
                let includeContent = preprocess includeFile
                preprocess' rest (acc + includeContent)
            else
                preprocess' rest (acc + line + "\n")
    
    preprocess' lines ""