/// <summary>
/// Provides a lexer for the Vec3 language.
/// </summary>

module Vec3.Interpreter.Scanner

open Token
open System

/// <summary>
/// Represents a possible error that can occur during lexing.
/// Each error contains a position in the input string where the error occurred, as well as a message describing the error.
/// </summary>
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

/// <summary>
/// Allows the creation of a LexerException from a LexerError.
/// </summary>
exception LexerException of LexerError

/// <summary>
/// Wrapper for the result of the lexer.
/// </summary>
type LexerResult<'a> = Result<'a, LexerError>


/// <summary>
/// Wrapper to check if a character is whitespace.
/// </summary>
let isWhitespace = Char.IsWhiteSpace

/// <summary>
/// Wrapper to check if a character is a digit.
/// </summary>
let isDigit = Char.IsDigit

/// <summary>
/// Wrapper to check if a character is a letter.
/// </summary>
let isLetter = Char.IsLetter

/// <summary>
/// Wrapper to check if a character is a symbol.
/// </summary>
let isSymbol =
    function
    | '+'
    | '-'
    | '*'
    | '/'
    | '%'
    | '^'
    | ':'
    | ','
    | '#'
    | '.'
    | '!'
    | '&'
    | '|'
    | '='
    | '<'
    | '>'
    | '€'
    | '$'
    | '@'
    | '?'
    | '~'
    | 'X' -> true
    | _ -> false

/// <summary>
/// Converts a character to an integer.
/// </summary>
/// <param name="c">The character to convert.</param>
/// <returns>The integer value of the character.</returns>
let intVal (c: char) : int = int c - int '0'

/// <summary>
/// Converts a string to a list of characters.
/// </summary>
/// <param name="s">The string to convert.</param>
/// <returns>The list of characters.</returns>
let str2lst (s: string) = [ for c in s -> c ]

/// <summary>
/// Scans in a string literal.
/// </summary>
/// <param name="sStr">The string to scan.</param>
/// <returns><c>Some</c>string literal and the rest of the input (<c>None</c> if the string is unterminated).</returns>
let scString (sStr: char list) : (char list * string) option =
    let rec inner (sStr: char list) (sVal: string) : (char list * string) option =
        match sStr with
        | '"' :: tail -> Some(tail, sVal)
        | c :: tail -> inner tail (sVal + string c)
        | _ -> None

    inner sStr ""

/// <summary>
/// Scans in an identifier (a keyword or variable name).
/// </summary>
/// <param name="iStr">The identifier to scan.</param>
/// <param name="iVal">The value of the identifier.</param>
/// <returns><c>Some</c>the identifier and the rest of the input (<c>None</c> if the identifier is invalid).</returns>
let rec scIdentifier (iStr: char list) (iVal: string) : (char list * string) option =
    match iStr with
    | c :: tail when isLetter c || isDigit c || c = '_' -> scIdentifier tail (iVal + string c)
    | _ -> Some(iStr, iVal)

let rec scOperator (iStr: char list) (iVal: string) : (char list * Operator * int) option =
    match iStr with
    | c :: tail when isSymbol c -> scOperator tail (iVal + string c)
    | _ ->
        match iVal with
        | "->" -> Some(iStr, Arrow, 2)
        | "**" -> Some(iStr, StarStar, 2)
        | "+" -> Some(iStr, Plus, 1)
        | "++" -> Some(iStr, PlusPlus, 2)
        | "-" -> Some(iStr, Minus, 1)
        | "*" -> Some(iStr, Star, 1)
        | "X" -> Some(iStr, Cross, 1)
        | "/" -> Some(iStr, Slash, 1)
        | "%" -> Some(iStr, Percent, 1)
        | "^" -> Some(iStr, Caret, 1)
        | ":" -> Some(iStr, Colon, 1)
        | "," -> Some(iStr, Comma, 1)
        | "$" -> Some(iStr, Dollar, 1)
        | "::" -> Some(iStr, ColonColon, 2)
        | "#" -> Some(iStr, Hash, 1)
        | ".*" -> Some(iStr, DotStar, 2)
        | ".." -> Some(iStr, DotDot, 2)
        | "." -> Some(iStr, Dot, 1)
        | "==" -> Some(iStr, EqualEqual, 2)
        | "!=" -> Some(iStr, BangEqual, 2)
        | "<=" -> Some(iStr, LessEqual, 2)
        | "<" -> Some(iStr, Less, 1)
        | ">=" -> Some(iStr, GreaterEqual, 2)
        | ">" -> Some(iStr, Greater, 1)
        | "=" -> Some(iStr, Equal, 1)
        | "!" -> Some(iStr, Bang, 1)
        | "&&" -> Some(iStr, AmpersandAmpersand, 2)
        | "||" -> Some(iStr, PipePipe, 2)
        | "&" -> Some(iStr, Ampersand, 1)
        | "|" -> Some(iStr, Pipe, 1)
        | _ -> Some(iStr, Custom iVal, iVal.Length)


// TODO: rationals dont work !!
/// <summary>
/// Scan in a number.
/// </summary>
/// <param name="nStr">The lexer input to scan.</param>
/// <returns><c>Some</c>the rest of the input, the scanned number and the length of the number literal (<c>None</c> if the number is // invalid).</returns>
let scNumber (nStr: char list) : (char list * Number * int) option =
    /// <summary>
    /// Scans in an integer.
    /// </summary>
    /// <param name="iStr">The input to scan.</param>
    /// <param name="iVal">The current value of the integer.</param>
    /// <param name="len">The length of the integer literal.</param>
    /// <returns>The rest of the input, the scanned integer and the length of the integer literal.</returns>
    let rec scInt (iStr: char list) (iVal: int) (len: int) : char list * int * int =
        match iStr with
        | c :: tail when isDigit c -> scInt tail (10 * iVal + intVal c) (len + 1)
        // | '-' :: tail -> let iTail, iInt, iLen = scInt tail 0 len in (iTail, -iInt, iLen + 1)
        // | '+' :: tail -> scInt tail iVal (len + 1)
        | _ -> (iStr, iVal, len)

    /// <summary>
    /// Scans in a fraction (the part after the decimal point).
    /// </summary>
    /// <param name="fStr">The input to scan.</param>
    /// <param name="acc">The current value of the fraction.</param>
    /// <param name="div">The current divisor.</param>
    /// <param name="len">The length of the fraction literal.</param>
    /// <returns>The rest of the input, the scanned fraction and the length of the fraction literal.</returns>
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
        | 'i' :: comTail
        | 'I' :: comTail ->
            failwith "todo"
        
        | '/' :: ratTail ->
            printfn "rat"
            let fStr, fVal, fLen = scInt ratTail 0 0
            Some(fStr, LRational(iVal, int fVal), iLen + fLen + 1)
        | '.' :: d :: fracTail when isDigit d ->
            let fStr, fVal, fLen = scFraction (d :: fracTail) 0.0 10.0 0

            match fStr with
            | 'e' :: expTail
            | 'E' :: expTail ->
                match expTail with
                | '+' :: expTail ->
                    let eStr, expVal, eLen = scInt expTail 0 0
                    Some(eStr, LFloat((float iVal + fVal) * (10.0 ** float expVal)), iLen + fLen + eLen + 2)
                | '-' :: expTail ->
                    let eStr, expVal, eLen = scInt expTail 0 0
                    Some(eStr, LFloat((float iVal + fVal) * (10.0 ** float -expVal)), iLen + fLen + eLen + 2)
                | _ ->
                    let eStr, expVal, eLen = scInt expTail 0 0
                    Some(eStr, LFloat((float iVal + fVal) * (10.0 ** float expVal)), iLen + fLen + eLen + 2)
            | _ -> Some(fStr, LFloat(float iVal + fVal), iLen + fLen + 1)
        | 'e' :: expTail ->
            match expTail with
            | '+' :: expTail ->
                let eStr, expVal, eLen = scInt expTail 0 0
                Some(eStr, LFloat((float iVal) * (10.0 ** float expVal)), iLen + eLen + 2)
            | '-' :: expTail ->
                let eStr, expVal, eLen = scInt expTail 0 0
                Some(eStr, LFloat((float iVal) * (10.0 ** float -expVal)), iLen + +eLen + 2)
            | _ ->
                let eStr, expVal, eLen = scInt expTail 0 0
                Some(eStr, LFloat((float iVal) * (10.0 ** float expVal)), iLen + eLen + 2)
        | _ -> Some(iStr, LInteger iVal, iLen)
    | _ -> None


/// <summary>
/// The lexer for the Vec3 language.
/// </summary>
/// <param name="input">The input string to lex.</param>
/// <returns>The list of tokens result.</returns>
let lexer (input: string) : LexerResult<Token list> =
    /// <summary>
    /// The inner scanner function. Works by recursively moving through the input string and scanning tokens.
    /// </summary>
    /// <param name="input">The input to scan.</param>
    /// <param name="position">The current position in the input.</param>
    /// <returns>The list of result tokens.</returns>
    let rec scan (input: char list) (position: Position) : LexerResult<Token> list =
        match input with
        | [] -> []
        | '/' :: '/' :: tail ->
            /// <summary>
            /// Discards a line comment.
            /// </summary>
            /// <param name="input">The input to scan.</param>
            /// <param name="column">The current column in the input.</param>
            /// <returns>The rest of the input and the new column number of scanning.</returns>
            let rec discardComment (input: char list) (column: int) : char list * int =
                match input with
                | '\n' :: tail -> tail, column
                | _ :: tail -> discardComment tail (column + 1)
                | [] -> [], column

            let tail, len = discardComment tail position.Column

            scan tail { position with Column = len }

        | '/' :: '*' :: tail ->
            /// <summary>
            /// Discards a block comment.
            /// </summary>
            /// <param name="input">The input to scan.</param>
            /// <param name="column">The current column in the input.</param>
            /// <param name="line">The current line in the input.</param>
            /// <returns>The rest of the input, the new column number and the new line number of scanning.</returns>
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
        | 'X' :: tail ->
            Ok
                { Lexeme = Operator(Cross, None)
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
        | ';' :: tail ->
            Ok
                { Lexeme = Punctuation Semicolon
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 1 }
        | '.' :: '*' :: tail ->
            Ok
                { Lexeme = Operator(DotStar, None)
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

        | c :: tail when isSymbol c ->
            let oRes = scOperator tail (string c)

            match oRes with
            | None ->
                Error(UnknownCharacter(c, position))
                :: scan
                    tail
                    { position with
                        Column = position.Column + 1 }
            | Some(oStr, oVal, oLen) ->
                Ok
                    { Lexeme = Operator(oVal, None)
                      Position = position }
                :: scan
                    oStr
                    { position with
                        Column = position.Column + oLen }

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

        | ''' :: ch :: ''' :: tail ->
            Ok
                { Lexeme = Number(LChar ch)
                  Position = position }
            :: scan
                tail
                { position with
                    Column = position.Column + 3 }

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
