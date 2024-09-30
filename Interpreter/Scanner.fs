module Vec3.Interpreter.Scanner

open Token
open System
open System.Text.RegularExpressions

type TokenPattern =
    | Float
    | Integer
    | Plus
    | Minus
    | Star
    | Slash
    | EqualEqual
    | BangEqual
    | Less
    | LessEqual
    | Greater
    | GreaterEqual
    | Equal
    | LeftParen
    | RightParen
    | Bang
    | String
    | Keyword
    | Identifier

let tokenPatterns : (TokenPattern * Regex) list = [
    (Float, Regex(@"^\d+\.\d+", RegexOptions.Compiled));
    (Integer, Regex(@"^\d+", RegexOptions.Compiled));
    (Plus, Regex(@"^\+", RegexOptions.Compiled));
    (Minus, Regex(@"^-", RegexOptions.Compiled));
    (Star, Regex(@"^\*", RegexOptions.Compiled));
    (Slash, Regex(@"^/", RegexOptions.Compiled));
    (EqualEqual, Regex(@"^==", RegexOptions.Compiled));
    (BangEqual, Regex(@"^!=", RegexOptions.Compiled));
    (Less, Regex(@"^<", RegexOptions.Compiled));
    (LessEqual, Regex(@"^<=", RegexOptions.Compiled));
    (Greater, Regex(@"^>", RegexOptions.Compiled));
    (GreaterEqual, Regex(@"^>=", RegexOptions.Compiled));
    (Equal, Regex(@"^=", RegexOptions.Compiled));
    (LeftParen, Regex(@"^\(", RegexOptions.Compiled));
    (RightParen, Regex(@"^\)", RegexOptions.Compiled));
    (Bang, Regex(@"^!", RegexOptions.Compiled));
    (String, Regex(@"^"".*?""", RegexOptions.Compiled));
    (Keyword, Regex(@"^(true|false|nil|let)\b", RegexOptions.Compiled));
    (Identifier, Regex(@"^[a-zA-Z_][a-zA-Z0-9_]*", RegexOptions.Compiled))
]

let whitespace = Regex(@"^\s+", RegexOptions.Compiled)

let lexemeFromPattern (pattern: TokenPattern) (value: string) =
    match pattern with
    | Float -> Lexeme.Number (Number.Float (float value))
    | Integer -> Lexeme.Number (Number.Integer (int value))
    | Plus -> Lexeme.Operator Operator.Plus
    | Minus -> Lexeme.Operator Operator.Minus
    | Star -> Lexeme.Operator Operator.Star
    | Slash -> Lexeme.Operator Operator.Slash
    | EqualEqual -> Lexeme.Operator Operator.EqualEqual
    | BangEqual -> Lexeme.Operator Operator.BangEqual
    | Less -> Lexeme.Operator Operator.Less
    | LessEqual -> Lexeme.Operator Operator.LessEqual
    | Greater -> Lexeme.Operator Operator.Greater
    | GreaterEqual -> Lexeme.Operator Operator.GreaterEqual
    | Equal -> Lexeme.Operator Operator.Equal
    | LeftParen -> Lexeme.Operator Operator.LeftParen
    | RightParen -> Lexeme.Operator Operator.RightParen
    | Bang -> Lexeme.Operator Operator.Bang
    | String -> Lexeme.String (value.Substring(1, value.Length - 2))
    | Keyword -> Lexeme.Keyword value
    | Identifier -> Lexeme.Identifier value

let tokenize (input: string) =
    let rec tokenize' (input: string) (line: int) (tokens: Token list) =
        if String.IsNullOrEmpty(input) then
            List.rev tokens 
        else
            let input, line =
                if input.StartsWith("\n") then
                    (input.Substring(1), line + 1)
                else
                    (input, line)
                    
            let input =
                match whitespace.Match(input) with
                | m when m.Success ->
                    input.Substring(m.Length)
                | _ -> input
                    
            if String.IsNullOrEmpty(input) then
                List.rev tokens
            else
                let matched =
                    tokenPatterns
                    |> List.tryPick (fun (tokenType, pattern) ->
                        let m = pattern.Match(input)
                        
                        if m.Success then
                            let value = m.Value
                            let remainingInput = input.Substring(m.Length)
                            let lexeme = lexemeFromPattern tokenType value
                            let token = { lexeme = lexeme; line = line }
                            Some (token, remainingInput)
                        else None)
                    
                match matched with
                | Some (token, remainingInput) ->
                    tokenize' remainingInput line (token :: tokens)
                | None ->
                    printfn $"Unrecognized token at line %d{line}: '%c{input[0]}'"
                    tokenize' (input.Substring(1)) line tokens
    tokenize' input 1 []