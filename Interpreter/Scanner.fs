module Vec3.Interpreter.Scanner

open Token
open System
open System.Text.RegularExpressions

type TokenPattern =
    | PComplex
    | PRational
    | PFloat
    | PInteger
    | PString
    
    | PArrow
    
    | PPlus
    | PMinus
    | PStar
    | PStarStar
    | PSlash
    | PPercent
    
    | PEqualEqual
    | PBangEqual
    | PLess
    | PLessEqual
    | PGreater
    | PGreaterEqual
    | PEqual
    
    | PBang
    
    | PLeftParen
    | PRightParen
    
    | PLeftBrace
    | PRightBrace
    
    | PLeftBracket
    | PRightBracket
    
    | PColon
    | PComma
    | PSemicolon
    | PDot
    
    | PIdentifier
    

let tokenPatterns : (TokenPattern * Regex) list = [
    (PComplex, Regex(@"^([+-]?\d*\.?\d*)i\s*([+-]\s*\d*\.?\d+)?$", RegexOptions.Compiled));
    (PRational, Regex(@"^\d+/\d+", RegexOptions.Compiled));
    (PFloat, Regex(@"^\d+\.\d+", RegexOptions.Compiled));
    (PInteger, Regex(@"^\d+", RegexOptions.Compiled))
    (PString, Regex(@"^"".*?""", RegexOptions.Compiled))
    
    (PArrow, Regex(@"^->", RegexOptions.Compiled))
    
    (PPlus, Regex(@"^\+", RegexOptions.Compiled));
    (PMinus, Regex(@"^-", RegexOptions.Compiled))
    (PStarStar, Regex(@"^\*\*", RegexOptions.Compiled))
    (PStar, Regex(@"^\*", RegexOptions.Compiled));
    (PSlash, Regex(@"^/", RegexOptions.Compiled))
    (PPercent, Regex(@"^%", RegexOptions.Compiled))
    
    (PEqualEqual, Regex(@"^==", RegexOptions.Compiled));
    (PBangEqual, Regex(@"^!=", RegexOptions.Compiled));
    (PLess, Regex(@"^<", RegexOptions.Compiled));
    (PLessEqual, Regex(@"^<=", RegexOptions.Compiled));
    (PGreater, Regex(@"^>", RegexOptions.Compiled));
    (PGreaterEqual, Regex(@"^>=", RegexOptions.Compiled));
    (PEqual, Regex(@"^=", RegexOptions.Compiled))
    
    (PBang, Regex(@"^!", RegexOptions.Compiled));
    
    (PLeftParen, Regex(@"^\(", RegexOptions.Compiled));
    (PRightParen, Regex(@"^\)", RegexOptions.Compiled))
    
    (PLeftBrace, Regex(@"^{", RegexOptions.Compiled))
    (PRightBrace, Regex(@"^}", RegexOptions.Compiled))
    
    (PLeftBracket, Regex(@"^\[", RegexOptions.Compiled))
    (PRightBracket, Regex(@"^\]", RegexOptions.Compiled))
    
    (PColon, Regex(@"^:", RegexOptions.Compiled))
    (PComma, Regex(@"^,", RegexOptions.Compiled))
    (PSemicolon, Regex(@"^;", RegexOptions.Compiled))
    (PDot, Regex(@"^\.", RegexOptions.Compiled))
    
    (PIdentifier, Regex(@"^[a-zA-Z_][a-zA-Z0-9_]*", RegexOptions.Compiled))
]

let keywordMap = 
    [ "let", Keyword.Let
      "if", Keyword.If
      "then", Keyword.Then
      "else", Keyword.Else
      "for", Keyword.For
      "true", Keyword.True
      "false", Keyword.False
      "nil", Keyword.Nil
      "print", Keyword.Print
       ]
    |> Map.ofList

let isKeyword (value: string) =
    keywordMap |> Map.containsKey value

let whitespace = Regex(@"^\s+", RegexOptions.Compiled)

let parseComplex (value: string) =
    let parseImaginary (part: string) =
        if part = "i" then 1
        elif part = "-i" then -1
        else
            int (part.Replace("i", ""))

    let parseReal (part: string) =
        float part

    let parts = value.Replace(" ", "").Split([|'i'|], StringSplitOptions.RemoveEmptyEntries)

    match parts with
    | [||] ->
        Lexeme.Number (Number.Complex (0, 0))
    | [|""|] ->
        Lexeme.Number (Number.Complex (0, parseImaginary "i"))

    | [|rPart|] when value.EndsWith("i") ->
        let i = parseImaginary rPart
        Lexeme.Number (Number.Complex (0, i))

    | [|rPart|] ->
        let r = parseReal rPart
        Lexeme.Number (Number.Complex (r, 0))

    | [|rPart; iPart|] ->
        let r = parseReal rPart
        let i = parseImaginary iPart
        Lexeme.Number (Number.Complex (r, i))

    | _ -> failwith "Invalid complex number format"
    
let lexemeFromIndent (value: string) =
    if isKeyword value then
        Lexeme.Keyword keywordMap[value]
    else
        Lexeme.Identifier value

let lexemeFromPattern (pattern: TokenPattern) (value: string) =
    match pattern with
    | PComplex -> parseComplex value
    | PRational -> let parts = value.Split('/')
                   let n = bigint.Parse parts[0]
                   let d = bigint.Parse parts[1]
                   Lexeme.Number (Number.Rational (int n, int d))
    | PFloat -> Lexeme.Number (Number.Float (float value))
    | PInteger -> Lexeme.Number (Number.Integer (int value))
    | PString -> Lexeme.String (value.Substring(1, value.Length - 2))
    
    | PPlus -> Lexeme.Operator Operator.Plus
    | PMinus -> Lexeme.Operator Operator.Minus
    | PStar -> Lexeme.Operator Operator.Star
    | PSlash -> Lexeme.Operator Operator.Slash
    | PStarStar -> Lexeme.Operator Operator.StarStar
    | PPercent -> Lexeme.Operator Operator.Percent
    
    | PEqualEqual -> Lexeme.Operator Operator.EqualEqual
    | PBangEqual -> Lexeme.Operator Operator.BangEqual
    | PLess -> Lexeme.Operator Operator.Less
    | PLessEqual -> Lexeme.Operator Operator.LessEqual
    | PGreater -> Lexeme.Operator Operator.Greater
    | PGreaterEqual -> Lexeme.Operator Operator.GreaterEqual
    | PEqual -> Lexeme.Operator Operator.Equal
    
    | PBang -> Lexeme.Operator Operator.Bang
    
    | PLeftParen -> Lexeme.Operator Operator.LeftParen
    | PRightParen -> Lexeme.Operator Operator.RightParen
    
    | PLeftBrace -> Lexeme.Operator Operator.LeftBrace
    | PRightBrace -> Lexeme.Operator Operator.RightBrace
    
    | PLeftBracket -> Lexeme.Operator Operator.LeftBracket
    | PRightBracket -> Lexeme.Operator Operator.RightBracket
    
    | PComma -> Lexeme.Comma
    | PSemicolon -> Lexeme.Semicolon
    | PArrow -> Lexeme.Operator Operator.Arrow
    | PColon -> Lexeme.Colon
    | PDot -> Lexeme.Operator Operator.Dot
    
    | PIdentifier -> lexemeFromIndent value

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