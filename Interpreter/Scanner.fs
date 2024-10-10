module Vec3.Interpreter.Scanner

open Token
open System
open System.Text.RegularExpressions

let isWhitespace = Char.IsWhiteSpace

let isDigit = Char.IsDigit

let isLetter = Char.IsLetter

let intVal (c: char): int = int c - int '0'

let str2lst (s: string) = [for c in s -> c]

let isKeyword (value: string) =
    keywordMap |> Map.containsKey value

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
    | PDotDot
    | PDot
    
    | PIdentifier
    
    | PAmpersandAmpersand
    | PPipePipe

let rec scInt (iStr: char list) (iVal: int): char list * int =
    match iStr with
    | c :: tail when isDigit c -> scInt tail (10 * iVal + intVal c)
    | _ -> (iStr, iVal)

let rec scFloat (fStr : char list) (fVal: float) (div: float): char list * float * float =
    match fStr with
    | c :: tail when isDigit c -> scFloat tail (fVal + (float (intVal c)) / div) (div * 10.0)
    | _ -> (fStr, fVal, div)

let rec scComplex (cStr: char list) (rVal: float) (iVal: float) (div: float): char list * float * float =
    match cStr with
    | c :: tail when isDigit c -> scComplex tail (rVal + (float (intVal c)) / div) iVal (div * 10.0)
    | '+' :: tail -> scComplex tail rVal iVal 1.0
    | '-' :: tail -> scComplex tail rVal iVal -1.0
    | 'i' :: tail -> (tail, rVal, iVal)
    | _ -> (cStr, rVal, iVal)

let rec scRational (rStr: char list) (nVal: int) (dVal: int): char list * int * int =
    match rStr with
    | c :: tail when isDigit c -> scRational tail (10 * nVal + intVal c) dVal
    | '/' :: tail -> scRational tail nVal 0
    | c :: tail when isDigit c -> scRational tail nVal (10 * dVal + intVal c)
    | _ -> (rStr, nVal, dVal)

// this is wrong
let rec scNumber (nStr: char list) (nVal: Number): char list * Number =
    match nStr with
    | c :: tail when isDigit c -> 
        let iStr, iVal = scInt tail (intVal c)
        (iStr, Number.Integer iVal)
    | '.' :: tail -> 
        let fStr, fVal, div = scFloat tail 0.0 10.0
        (fStr, Number.Float fVal)
    | 'i' :: tail -> (tail, Number.Complex (0.0, 1.0))
    | c :: tail when isDigit c -> 
        let cStr, rVal, iVal = scComplex nStr 0.0 0.0 10.0
        (cStr, Number.Complex (rVal, iVal))
    | '/' :: tail -> 
        let rStr, nVal, dVal = scRational tail 0 0
        (rStr, Number.Rational (nVal, dVal))
    | _ -> (nStr, nVal)
    
let rec scString (sStr: char list) (sVal: string): char list * string =
    match sStr with
    | '"' :: tail -> (tail, sVal)
    | c :: tail -> scString tail (sVal + string c)
    | _ -> (sStr, sVal)
   
let rec scIdentifier (iStr: char list) (iVal: string): char list * string =
    match iStr with
    | c :: tail when isLetter c -> scIdentifier tail (iVal + string c)
    | _ -> (iStr, iVal)

let lexer (input: string) : Token list =
    let rec scan (input: char list) (position: Position) : Token list =
        match input with
        | [] -> []
        | '-' :: '>' :: tail ->
           {Lexeme = Operator Arrow; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '*' :: '*' :: tail ->
           {Lexeme = Operator StarStar; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '+' :: tail ->
              {Lexeme = Operator Plus; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '-' :: tail ->
                {Lexeme = Operator Minus; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '*' :: tail ->
                {Lexeme = Operator Star; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '/' :: tail ->
                {Lexeme = Operator Slash; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '%' :: tail ->
                {Lexeme = Operator Percent; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '^' :: tail ->
                {Lexeme = Operator Caret; Position = position} :: scan tail { position with Column = position.Column + 1 }
        
        | '(' :: tail ->
                {Lexeme = Operator LeftParen; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | ')' :: tail ->
                {Lexeme = Operator RightParen; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '[' :: tail ->
                {Lexeme = Operator LeftBracket; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | ']' :: tail ->
                {Lexeme = Operator RightBracket; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '{' :: tail ->
                {Lexeme = Operator LeftBrace; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '}' :: tail ->
                {Lexeme = Operator RightBrace; Position = position} :: scan tail { position with Column = position.Column + 1 }
                
        | ':' :: tail ->
                {Lexeme = Colon; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | ',' :: tail ->
                {Lexeme = Comma; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | ';' :: tail ->
                {Lexeme = Semicolon; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '.' :: '.' :: tail ->
                {Lexeme = Operator DotDot; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '.' :: tail ->
                {Lexeme = Operator Dot; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '=' :: '=' :: tail ->
                {Lexeme = Operator EqualEqual; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '!' :: '=' :: tail ->
                {Lexeme = Operator BangEqual; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '<' :: '=' :: tail ->
                {Lexeme = Operator LessEqual; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '<' :: tail ->
                {Lexeme = Operator Less; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '>' :: '=' :: tail ->
                {Lexeme = Operator GreaterEqual; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '>' :: tail ->
                {Lexeme = Operator Greater; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '=' :: tail ->
                {Lexeme = Operator Equal; Position = position} :: scan tail { position with Column = position.Column + 1 }
        | '!' :: tail ->
                {Lexeme = Operator Bang; Position = position} :: scan tail { position with Column = position.Column + 1 }
        
        | '&' :: '&' :: tail ->
                {Lexeme = Operator AmpersandAmpersand; Position = position} :: scan tail { position with Column = position.Column + 2 }
        | '|' :: '|' :: tail ->
                {Lexeme = Operator PipePipe; Position = position} :: scan tail { position with Column = position.Column + 2 }
                
        | c :: tail when isWhitespace c ->
            match c with
            | '\n' -> scan tail { Line = position.Line + 1; Column = 0 }
            | _ -> scan tail { position with Column = position.Column + 1 }
        
        // FIX THIS !!!!
        | c :: tail when isDigit c ->
            let nStr, nVal = scNumber tail (Number.Integer (intVal c))
            { Lexeme = Number nVal; Position = position } :: scan nStr { position with Column = position.Column + 1 }
        
        | '"' :: tail ->
            let sStr, sVal = scString tail ""
            { Lexeme = Lexeme.String sVal; Position = position } :: scan sStr { position with Column = position.Column + 1 }
        
        | c :: tail when isLetter c ->
            let iStr, iVal = scIdentifier tail (string c)
            if isKeyword iVal then
                { Lexeme = Keyword keywordMap[iVal]; Position = position } :: scan iStr { position with Column = position.Column + iVal.Length }
            else
                { Lexeme = Identifier iVal; Position = position } :: scan iStr { position with Column = position.Column + iVal.Length }
                
        | _ -> failwith "todo"
        
    scan (str2lst input) { Line = 1; Column = 0 }
        


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
    (PDotDot, Regex(@"^\.\.", RegexOptions.Compiled))
    (PDot, Regex(@"^\.", RegexOptions.Compiled))
    
    (PAmpersandAmpersand, Regex(@"^&&", RegexOptions.Compiled))
    (PPipePipe, Regex(@"^\|\|", RegexOptions.Compiled))
    
    (PIdentifier, Regex(@"^[a-zA-Z_][a-zA-Z0-9_]*", RegexOptions.Compiled))
]




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

let lexemeFromPattern (pattern: TokenPattern) (value: string): Lexeme =
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
    | PDotDot -> Lexeme.Operator Operator.DotDot
    
    | PPipePipe -> Lexeme.Operator Operator.PipePipe
    | PAmpersandAmpersand -> Lexeme.Operator Operator.AmpersandAmpersand
    
    | PIdentifier -> lexemeFromIndent value

let tokenize (input: string) =
    let rec loop (input: string) (lineNum: int) (tokens: Token list) =
        if String.IsNullOrEmpty(input) then
            List.rev tokens 
        else
            let input, lineNum =
                if input.StartsWith("\n") then
                    (input.Substring(1), lineNum + 1)
                else
                    (input, lineNum)
                    
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
                            let token = { Lexeme = lexeme; Position = { Line = lineNum; Column = 0 } } // todo
                            Some (token, remainingInput)
                        else None)
                    
                match matched with
                | Some (token, remainingInput) ->
                    loop remainingInput lineNum (token :: tokens)
                | None ->
                    printfn $"Unrecognized token at line %d{lineNum}: '%c{input[0]}'"
                    loop (input.Substring(1)) lineNum tokens
    loop input 1 []