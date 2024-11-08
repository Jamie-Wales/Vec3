module Vec3.Interpreter.Scanner

open Token
open System

type LexerError =
    | UnterminatedString of Position
    | UnterminatedComment of Position
    | UnterminatedBlockComment of Position
    | UnknownCharacter of char * Position
    | UnexpectedCharacter of char * Position
    | UnexpectedEndOfFile of Position
    | InvalidNumber of string * Position
    | InvalidIdentifier of string * Position
    | Other of string * Position
    | Errors of LexerError list

exception LexerException of LexerError

type LexerResult<'a> = Result<'a, LexerError>


let isWhitespace = Char.IsWhiteSpace

let isDigit = Char.IsDigit

let isLetter = Char.IsLetter

let intVal (c: char) : int = int c - int '0'

let str2lst (s: string) = [ for c in s -> c ]

let scString (sStr: char list) : (char list * string) option =
    let rec inner (sStr: char list) (sVal: string) : (char list * string) option =
        match sStr with
        | '"' :: tail -> Some(tail, sVal)
        | c :: tail -> inner tail (sVal + string c)
        | _ -> None

    inner sStr ""

let rec scIdentifier (iStr: char list) (iVal: string) : (char list * string) option =
    match iStr with
    | c :: tail when isLetter c || isDigit c || c = '_' -> scIdentifier tail (iVal + string c)
    | _ -> Some(iStr, iVal)

// float: 1.0, 1.0e-3, 1.0e3
// int: 1, 100, 1000
// rational: 1/2, 1/3, 1/4
// complex: 1+2i, 1-2i, 1+2i, 1-2i, i, -i, 2i, etc TODO
// int return is length of the number string
let scNumber (nStr: char list) : (char list * Number * int) option =
    let rec scInt (iStr: char list) (iVal: int) (len: int) : char list * int * int =
        match iStr with
        | c :: tail when isDigit c -> scInt tail (10 * iVal + intVal c) (len + 1)
        | '-' :: tail -> let iTail, iInt, iLen = scInt tail 0 len in (iTail, -iInt, iLen + 1)
        | '+' :: tail -> scInt tail iVal (len + 1)
        | _ -> (iStr, iVal, len)

    let rec scFraction (fStr: char list) (acc: float) (div: float) (len: int) : char list * float * int =
        match fStr with
        | c :: tail when isDigit c ->
            let newAcc = acc + (float (intVal c)) / div
            scFraction tail newAcc (div * 10.0) (len + 1)
        | _ -> (fStr, acc, len)

    match nStr with
    | c :: tail when isDigit c ->
        let iStr, iVal, iLen = scInt tail (intVal c) 1

        match iStr with
        | '/' :: ratTail ->
            let fStr, fVal, fLen = scInt ratTail 0 0
            
            if fVal = 0 then
                Some(iStr, LInteger(iVal), iLen)
            else
                Some(fStr, LRational(iVal, int fVal), iLen + fLen + 1)
        | '.' :: d :: fracTail when isDigit d ->
            let fStr, fVal, fLen = scFraction (d :: fracTail) 0.0 10.0 0

            match fStr with
            | 'e' :: expTail
            | 'E' :: expTail ->
                let eStr, expVal, eLen = scInt expTail 0 0
                Some(eStr, LFloat((float iVal + fVal) * (10.0 ** float expVal)), iLen + fLen + eLen + 2)
            | _ -> Some(fStr, LFloat(float iVal + fVal), iLen + fLen + 1)
        | 'e' :: expTail ->
            let eStr, expVal, eLen = scInt expTail 0 0
            Some(eStr, LFloat(float iVal * (10.0 ** float expVal)), iLen + eLen + 1)
        | _ -> Some(iStr, LInteger iVal, iLen)
    | _ -> None


let lexer (input: string) : LexerResult<Token list> =
    let rec scan (input: char list) (position: Position) : LexerResult<Token> list =
        match input with
        | [] -> []
        | '\n' :: tail ->
            Ok
                { Lexeme = Punctuation Newline
                  Position = position }
            :: scan
                tail
                { position with
                    Column = 0; Line = position.Line + 1 }
        | '/' :: '/' :: tail ->
            let rec discardComment (input: char list) (column: int) : char list * int =
                match input with
                | '\n' :: tail -> tail, column
                | _ :: tail -> discardComment tail (column + 1)
                | [] -> [], column
            
            let tail, len = discardComment tail position.Column
            
            scan
                tail
                { position with
                    Column = len }
        
        | '/' :: '*' :: tail ->
            let rec discardBlockComment (input: char list) (column: int) (line: int) : char list * int * int =
                match input with
                | '*' :: '/' :: tail -> tail, column + 2, line
                | '\n' :: tail -> discardBlockComment tail 0 (line + 1)
                | _ :: tail -> discardBlockComment tail (column + 1) line
                | [] -> [], column, line
                
            let tail, column, line = discardBlockComment tail position.Column position.Line
            
            if List.isEmpty tail then
                [ Error(UnterminatedBlockComment position) ]
            else
                scan
                    tail
                    { position with
                        Column = column
                        Line = line }
        | '-' :: '>' :: tail ->
            Ok
                { Lexeme = Operator (Arrow, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
        | '*' :: '*' :: tail ->
            Ok
                { Lexeme = Operator (StarStar, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
        
        // | '+' :: c :: tail when isDigit c ->
        //     let nRes = scNumber (c :: tail)
        //     
        //     match nRes with
        //     | None ->
        //         Ok { Lexeme = Operator (Plus, None); Position = position }
        //         :: scan
        //             tail
        //             { position with
        //                 Column = position.Column + 1 }
        //     | Some(nStr, nVal, nLen) ->
        //         Ok { Lexeme = Number nVal; Position = position }
        //         :: scan
        //             nStr
        //             { position with
        //                 Column = position.Column + nLen }
        
        // | '-' :: c :: tail when isDigit c ->
        //     let nRes = scNumber (c :: tail)
        //     
        //     match nRes with
        //     | None ->
        //         Ok { Lexeme = Operator (Minus, None); Position = position }
        //         :: scan
        //             tail
        //             { position with
        //                 Column = position.Column + 1 }
        //     | Some(nStr, nVal, nLen) ->
        //         Ok { Lexeme = Number nVal; Position = position }
        //         :: scan
        //             nStr
        //             { position with
        //                 Column = position.Column + nLen }
        
        | '+' :: tail ->
            Ok
                { Lexeme = Operator (Plus, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '-' :: tail ->
            Ok
                { Lexeme = Operator (Minus, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '*' :: tail ->
            Ok
                { Lexeme = Operator (Star, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | 'X' :: tail ->
            Ok
                { Lexeme = Operator (Cross, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '$' :: tail ->
            Ok
                { Lexeme = Punctuation Dollar; Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '/' :: tail ->
            Ok
                { Lexeme = Operator (Slash, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '%' :: tail ->
            Ok
                { Lexeme = Operator (Percent, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '^' :: tail ->
            Ok
                { Lexeme = Operator (Caret, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }

        | '(' :: tail ->
            Ok
                { Lexeme = Punctuation LeftParen
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | ')' :: tail ->
            Ok
                { Lexeme = Punctuation RightParen
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '[' :: tail ->
            Ok
                { Lexeme = Punctuation LeftBracket
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | ']' :: tail ->
            Ok
                { Lexeme = Punctuation RightBracket 
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '{' :: tail ->
            Ok
                { Lexeme = Punctuation LeftBrace
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '}' :: tail ->
            Ok
                { Lexeme = Punctuation RightBrace
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        
        | ':' :: ':' :: tail ->
            Ok
                { Lexeme = Operator (ColonColon, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }

        | ':' :: tail ->
            Ok { Lexeme = Punctuation Colon; Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | ',' :: tail ->
            Ok { Lexeme = Punctuation Comma; Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | ';' :: tail ->
            Ok
                { Lexeme = Punctuation Semicolon
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }

        | '#' :: tail ->
            Ok
                { Lexeme = Operator (Hash, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        
        | '.' :: '*' :: tail ->
            Ok
                { Lexeme = Operator (DotStar, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
                
        | '.' :: '.' :: tail ->
            Ok
                { Lexeme = Operator (DotDot, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }

        | '.' :: c :: tail when isDigit c ->
            let nRes = scNumber ('0' :: '.' :: c :: tail)

            match nRes with
            | None ->
                Error(InvalidNumber(string c, position))
                :: scan
                    tail
                    { position with
                        Column = position.Column + 1 }
            | Some(nStr, nVal, nLen) ->
                Ok
                    { Lexeme = Number nVal
                      Position = position }

                :: scan
                    nStr
                    { position with
                        Column = position.Column + nLen }

        | '.' :: tail ->
            Ok
                { Lexeme = Operator (Dot, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '=' :: '=' :: tail ->
            Ok
                { Lexeme = Operator (EqualEqual, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
        | '!' :: '=' :: tail ->
            Ok
                { Lexeme = Operator (BangEqual, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
        | '<' :: '=' :: tail ->
            Ok
                { Lexeme = Operator (LessEqual, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
        | '<' :: tail ->
            Ok
                { Lexeme = Operator (Less, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '>' :: '=' :: tail ->
            Ok
                { Lexeme = Operator (GreaterEqual, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
        | '>' :: tail ->
            Ok
                { Lexeme = Operator (Greater, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '=' :: tail ->
            Ok
                { Lexeme = Operator (Equal, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '!' :: tail ->
            Ok
                { Lexeme = Operator (Bang, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        
        | '&' :: '&' :: tail ->
            Ok
                { Lexeme = Operator (AmpersandAmpersand, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 2 }
        | '|' :: '|' :: tail ->
            Ok
                { Lexeme = Operator (PipePipe, None)
                  Position = position }
            :: scan
                tail
                { position with
                     Column = position.Column + 2 }
                
        | '&' :: tail ->
            Ok
                { Lexeme = Operator (Ampersand, None)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
                
        | '|' :: tail ->
            Ok { Lexeme = Operator (Pipe, None); Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }

        | c :: tail when isWhitespace c ->
            match c with
            | '\n' -> scan tail { Line = position.Line + 1; Column = 0 }
            | _ ->
                scan
                    tail
                    { position with
                        Column = position.Column + 1 }

        | c :: tail when isDigit c ->
            let nRes = scNumber (c :: tail)

            match nRes with
            | None ->
                Error(InvalidNumber(string c, position))
                :: scan
                    tail
                    { position with
                        Column = position.Column + 1 }
            | Some(nStr, nVal, nLen) ->
                Ok
                    { Lexeme = Number nVal
                      Position = position }

                :: scan
                    nStr
                    { position with
                        Column = position.Column + nLen }

        | '"' :: tail ->
            match scString tail with
            | None ->
                Error(UnterminatedString position)
                :: scan
                    tail
                    { position with
                        Column = position.Column + 1 }
            | Some(sStr, sVal) ->
                Ok
                    { Lexeme = Lexeme.String sVal
                      Position = position }
                :: scan
                    sStr
                    { position with
                        Column = position.Column + sVal.Length + 2 }


        | c :: tail when isLetter c ->
            match scIdentifier tail (string c) with
            | None ->
                Error(InvalidIdentifier(string c, position))
                :: scan
                    tail
                    { position with
                        Column = position.Column + 1 }
            | Some(iStr, iVal) ->
                if isKeyword iVal then
                    Ok
                        { Lexeme = Keyword keywordMap[iVal]
                          Position = position }
                    :: scan
                        iStr
                        { position with
                            Column = position.Column + iVal.Length }
                else
                    Ok
                        { Lexeme = Identifier iVal
                          Position = position }
                    :: scan
                        iStr
                        { position with
                            Column = position.Column + iVal.Length }
        | _ :: tail ->
            Error(UnknownCharacter(List.head input, position))
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }

    let tokens = scan (str2lst input) { Line = 1; Column = 0 }

    if
        List.exists
            (fun x ->
                match x with
                | Error _ -> true
                | _ -> false)
            tokens
    then
        let errors =
            List.filter
                (fun x ->
                    match x with
                    | Error _ -> true
                    | _ -> false)
                tokens

        let errors =
            List.map
                (fun x ->
                    match x with
                    | Error e -> e
                    | _ -> failwith "Impossible")
                errors

        Error(Errors errors)
    else
        let tokens =
            List.map
                (fun x ->
                    match x with
                    | Ok t -> t
                    | _ -> failwith "Impossible")
                tokens

        Ok tokens

let tokenize = lexer
