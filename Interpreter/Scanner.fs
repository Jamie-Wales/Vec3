module Vec3.Interpreter.Scanner

open Token
open System
open System.Text.RegularExpressions

let tokenPatterns = [
    ("Float", @"^\d+\.\d+");
    ("Integer", @"^\d+");
    ("Plus", @"^\+");
    ("Minus", @"^-");
    ("Star", @"^\*");
    ("Slash", @"^/");
    ("Equal", @"^==");
    ("BangEqual", @"^!=");
    ("Less", @"^<");
    ("LessEqual", @"^<=");
    ("Greater", @"^>");
    ("GreaterEqual", @"^>=");
    ("LeftParen", @"^\(");
    ("RightParen", @"^\)");
    ("Bang", @"^!");
    ("String", @"^"".*?""");
    ("Keyword", @"^(true|false|nil)\b");
    ("Identifier", @"^[a-zA-Z_][a-zA-Z0-9_]*");
]
let tokenize (input: string) =
    let rec tokenize_str (currentInput: string) (line: int) (tokens: Token list) =
        if String.IsNullOrEmpty(currentInput) then
            List.rev tokens 
        else
            let (currentInput, line) =
                if currentInput.StartsWith("\n") then
                    (currentInput.Substring(1), line + 1)
                else
                    (currentInput, line)
            let whitespaceMatch = Regex.Match(currentInput, @"^\s+")
            let currentInput =
                if whitespaceMatch.Success then
                    currentInput.Substring(whitespaceMatch.Length)
                else
                    currentInput
            if String.IsNullOrEmpty(currentInput) then
                List.rev tokens
            else
                let matched =
                    tokenPatterns
                    |> List.tryPick (fun (tokenType, pattern) ->
                        let m = Regex.Match(currentInput, pattern)
                        if m.Success then
                            let value = m.Value
                            let remainingInput = currentInput.Substring(m.Length)
                            let lexeme =
                                match tokenType with
                                | "Float" -> Lexeme.Number (Number.Float (float value))
                                | "Integer" -> Lexeme.Number (Number.Integer (int value))
                                | "Plus" -> Lexeme.Operator Operator.Plus
                                | "Minus" -> Lexeme.Operator Operator.Minus
                                | "Star" -> Lexeme.Operator Operator.Star
                                | "Slash" -> Lexeme.Operator Operator.Slash
                                | "Equal" -> Lexeme.Operator Operator.Equal
                                | "BangEqual" -> Lexeme.Operator Operator.BangEqual
                                | "Less" -> Lexeme.Operator Operator.Less
                                | "LessEqual" -> Lexeme.Operator Operator.LessEqual
                                | "Greater" -> Lexeme.Operator Operator.Greater
                                | "GreaterEqual" -> Lexeme.Operator Operator.GreaterEqual
                                | "LeftParen" -> Lexeme.Operator Operator.LeftParen
                                | "RightParen" -> Lexeme.Operator Operator.RightParen
                                | "Bang" -> Lexeme.Operator Operator.Bang
                                | "String" -> Lexeme.String (value.Substring(1, value.Length - 2))
                                | "Keyword" -> Lexeme.Keyword value
                                | "Identifier" -> Lexeme.Identifier value
                                | _ -> failwithf $"Unknown token type: %s{tokenType}"
                            let token = { lexeme = lexeme; line = line }
                            Some (token, remainingInput)
                        else
                            None)
                match matched with
                | Some (token, remainingInput) ->
                    tokenize_str remainingInput line (token :: tokens)
                | None ->
                    printfn $"Unrecognized token at line %d{line}: '%c{currentInput.[0]}'"
                    tokenize_str (currentInput.Substring(1)) line tokens
    tokenize_str input 1 []