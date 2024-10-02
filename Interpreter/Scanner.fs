module Vec3.Interpreter.Scanner

open Token
open System
open System.Text.RegularExpressions

type TokenPattern =
    | Float
    | Integer
    | Rational
    | Complex
    | Plus
    | Minus
    | Star
    | Colon
    | Slash
    | StarStar
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
    | Comma
    | Semicolon
    | Arrow
    | Dot
    | LeftBrace
    | RightBrace
    | LeftBracket
    | RightBracket

let tokenPatterns : (TokenPattern * Regex) list = [
    (Complex, Regex(@"^([+-]?\d*\.?\d*)i\s*([+-]\s*\d*\.?\d+)?$", RegexOptions.Compiled));
    (Rational, Regex(@"^\d+/\d+", RegexOptions.Compiled));
    (Float, Regex(@"^\d+\.\d+", RegexOptions.Compiled));
    (Integer, Regex(@"^\d+", RegexOptions.Compiled));
    (Arrow, Regex(@"^->", RegexOptions.Compiled))
    (Plus, Regex(@"^\+", RegexOptions.Compiled));
    (Minus, Regex(@"^-", RegexOptions.Compiled))
    (StarStar, Regex(@"^\*\*", RegexOptions.Compiled))
    (Colon, Regex(@"^:", RegexOptions.Compiled))
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
    (String, Regex(@"^"".*?""", RegexOptions.Compiled))
    (Keyword, Regex(@"^(true|false|let|if|then|else|for)\b", RegexOptions.Compiled));
    (Identifier, Regex(@"^[a-zA-Z_][a-zA-Z0-9_]*", RegexOptions.Compiled))
    (Comma, Regex(@"^,", RegexOptions.Compiled))
    (Semicolon, Regex(@"^;", RegexOptions.Compiled))
    (Dot, Regex(@"^\.", RegexOptions.Compiled))
    (LeftBrace, Regex(@"^{", RegexOptions.Compiled))
    (RightBrace, Regex(@"^}", RegexOptions.Compiled))
    (LeftBracket, Regex(@"^\[", RegexOptions.Compiled))
    (RightBracket, Regex(@"^\]", RegexOptions.Compiled))
]

let whitespace = Regex(@"^\s+", RegexOptions.Compiled)

let parseComplex (value: string) =
    let parseImaginary (part: string) =
        if part = "i" then 1M
        elif part = "-i" then -1M
        else
            decimal (part.Replace("i", ""))

    let parseReal (part: string) =
        decimal part

    let parts = value.Replace(" ", "").Split([|'i'|], StringSplitOptions.RemoveEmptyEntries)

    match parts with
    | [||] ->
        Lexeme.Number (Number.Complex (0M, 0M))
    | [|""|] ->
        Lexeme.Number (Number.Complex (0M, parseImaginary "i"))

    | [|rPart|] when value.EndsWith("i") ->
        let i = parseImaginary rPart
        Lexeme.Number (Number.Complex (0M, i))

    | [|rPart|] ->
        let r = parseReal rPart
        Lexeme.Number (Number.Complex (r, 0M))

    | [|rPart; iPart|] ->
        let r = parseReal rPart
        let i = parseImaginary iPart
        Lexeme.Number (Number.Complex (r, i))

    | _ -> failwith "Invalid complex number format"

let lexemeFromPattern (pattern: TokenPattern) (value: string) =
    match pattern with
    | Complex -> parseComplex value
    | Rational -> let parts = value.Split('/')
                  let n = bigint.Parse parts[0]
                  let d = bigint.Parse parts[1]
                  Lexeme.Number (Number.Rational (n, d))
    | Float -> Lexeme.Number (Number.Float (decimal value))
    | Integer -> Lexeme.Number (Number.Integer (bigint.Parse value))
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
    | Keyword -> match value with
                    | "true" -> Lexeme.Keyword Keyword.True
                    | "false" -> Lexeme.Keyword Keyword.False
                    | "let" -> Lexeme.Keyword Keyword.Let
                    | "if" -> Lexeme.Keyword Keyword.If
                    | "then" -> Lexeme.Keyword Keyword.Then
                    | "else" -> Lexeme.Keyword Keyword.Else
                    | "for" -> Lexeme.Keyword Keyword.For
                    | "nil" -> Lexeme.Keyword Keyword.Nil
                    | _ -> failwith $"Unrecognized keyword: '{value}'"
    | Identifier -> Lexeme.Identifier value
    | Comma -> Lexeme.Comma
    | Semicolon -> Lexeme.Semicolon
    | Arrow -> Lexeme.Operator Operator.Arrow
    | StarStar -> Lexeme.Operator Operator.StarStar
    | Colon -> Lexeme.Colon
    | Dot -> Lexeme.Operator Operator.Dot
    | LeftBrace -> Lexeme.Operator Operator.LeftBrace
    | RightBrace -> Lexeme.Operator Operator.RightBrace
    | LeftBracket -> Lexeme.Operator Operator.LeftBracket
    | RightBracket -> Lexeme.Operator Operator.RightBracket

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