/// <summary>
/// Parser for the Vec3 language.
/// Uses a Pratt parsing to parse the language.
/// </summary>
module Vec3.Interpreter.Parser

open Microsoft.FSharp.Core
open Token
open Grammar
open Scanner
open Vec3.Interpreter.SyntaxAnalysis.TailAnalyser

let rec getDefault =
    function
    | TBool -> Some(ELiteral(LBool true, TBool))
    | TInteger -> Some(ELiteral(LNumber(LInteger 0), TInteger))
    | TFloat -> Some(ELiteral(LNumber(LFloat 0), TFloat))
    | TRational -> Some(ELiteral(LNumber(LRational(0, 0)), TRational))
    | TComplex -> Some(ELiteral(LNumber(LComplex(0, 0)), TComplex))
    | TString -> Some(ELiteral((LString ""), TString))
    | TTensor(typ1, _) ->
        let def = getDefault typ1

        if Option.isSome def then
            Some(EList([ Option.get def ], None))
        else
            None
    | _ -> None

// result for computation expressions
type ResultBuilder() =
    member this.Bind(x, f) = Result.bind f x
    member this.Return(x) = Ok x
    member this.ReturnFrom(x) = x
    member this.Zero() = Ok()

let result = ResultBuilder()

// options for computation expressions
type OptionBuilder() =
    member this.Bind(x, f) = Option.bind f x
    member this.Return(x) = Some x
    member this.ReturnFrom(x) = x
    member this.Zero() = None

let option = OptionBuilder()

/// <summary>
/// Represents the precedence of an operator.
/// Used in the Pratt parser to determine the order of operations.
/// </summary>
type Precedence =
    | None = 0
    | Assignment = 1
    | Or = 2
    | And = 3
    | Equality = 4
    | Comparison = 5
    | Term = 6
    | Factor = 7
    | Exponent = 8
    | Unary = 9
    | Index = 10
    | Call = 11
    | Atom = 12

/// <summary>
/// A label for the parser state (the current state of the parser).
/// </summary>
type ParserLabel = string

/// <summary>
/// The position of the parser in the input.
/// </summary>
type Position =
    { CurrentLine: string
      Line: int
      Column: int
      Token: int }

/// <summary>
/// Represents the state of the parser.
/// </summary>
type ParserState =
    { Tokens: Token list
      Position: Position
      Label: ParserLabel }

/// <summary>
/// Various errors that can occur during parsing.
/// </summary>
type ParserError =
    | UnexpectedEndOfInput
    | UnexpectedToken of Token
    | ExpectedToken of Lexeme * Lexeme
    | Expected of string
    | ExpectedExpression
    | ExpectedStatement
    | ExpectedType of string
    | Other of string
    | LexerError of LexerError

/// <summary>
/// Makes a parser exception.
/// </summary>
exception ParserException of ParserError * ParserState

/// <summary>
/// Represents a parse success or failure.
/// </summary>
type ParseResult<'a> = Result<ParserState * 'a, ParserError * ParserState>

/// <summary>
/// Creates a new parser state from a list of tokens.
/// </summary>
/// <param name="tokens">The list of tokens to create the parser state from.</param>
/// <returns>The parser state.</returns>
let createParserState (tokens: Token list) : ParserState =
    { Tokens = tokens
      Position =
        { CurrentLine = ""
          Line = 1
          Column = 1
          Token = 0 }
      Label = "Initial" }

/// <summary>
/// Sets the label of the parser state.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="label">The label to set.</param>
/// <returns>The parser state with the label set.</returns>
let setLabel (state: ParserState) (label: string) : ParserState = { state with Label = label }

/// <summary>
/// Gets the current token from the parser state.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The current token as an option (if eof).</returns>
let getCurrentToken (state: ParserState) : Token option =
    List.tryItem state.Position.Token state.Tokens

/// <summary>
/// Views the current token in the parser state.
/// </summary>
let peek = getCurrentToken

/// <summary>
/// Increments the parser state to the next token.
/// </summary>
/// <param name="state">The current state.</param>
/// <returns>The new state with the token advanced.</returns>
let advance (state: ParserState) : ParserState =
    { state with
        ParserState.Position.Token = state.Position.Token + 1 }

/// <summary>
/// Decrements the parser state to the previous token.
/// </summary>
/// <param name="state">The current state.</param>
/// <returns>The new state with the token rewound.</returns>
let rewind (state: ParserState) : ParserState =
    { state with
        ParserState.Position.Token = state.Position.Token - 1 }


/// <summary>
/// Returns the previous token in the parser state.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The previous token as an option (if no previous).</returns>
let previous (state: ParserState) : Token option =
    List.tryItem (state.Position.Token - 1) state.Tokens

/// <summary>
/// Increments the state and returns the next token.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the next token as an option.</returns>
let nextToken (state: ParserState) : (ParserState * Token) option =
    peek state |> Option.map (fun token -> (advance state, token))

/// <summary>
/// Represents the parse rules for a given token.
/// </summary>
type ParseRule =
    { Prefix: (ParserState -> ParseResult<Expr>) option
      Infix: (ParserState -> Expr -> ParseResult<Expr>) option
      Postfix: (ParserState -> Expr -> ParseResult<Expr>) option
      Precedence: Precedence }

/// <summary>
/// None rule for default.
/// </summary>
let defaultRule =
    { Prefix = None
      Infix = None
      Postfix = None
      Precedence = Precedence.None }

/// <summary>
/// Expects a token to be the next token in the parser state.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="expected">The expected token.</param>
/// <returns>The new state if the token is the expected token, otherwise an error.</returns>
let expect (state: ParserState) (expected: Lexeme) : Result<ParserState, ParserError * ParserState> =
    match nextToken state with
    | Some(state, token) when token.Lexeme = expected -> Ok(state)
    | Some(state, token) -> Error(ExpectedToken(expected, token.Lexeme), state)
    | None -> Error(UnexpectedEndOfInput, state)

let expectIdentifier (state: ParserState) : ParseResult<Token> =
    match nextToken state with
    | Some(state, token) ->
        match token.Lexeme with
        | Identifier _ -> Ok(state, token)
        | _ -> Error(Expected("Identifier"), state)
    | None -> Error(UnexpectedEndOfInput, state)

let expectOperator (state: ParserState) : ParseResult<Token> =
    match nextToken state with
    | Some(state, token) ->
        match token.Lexeme with
        | Operator _ -> Ok(state, token)
        | _ -> Error(Expected("Operator"), state)
    | None -> Error(UnexpectedEndOfInput, state)

/// <summary>
/// Parser for unit or nil.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
let nil (state: ParserState) : ParseResult<Expr> = Ok(state, ELiteral(LUnit, TUnit))

/// <summary>
/// Parser for boolean values.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
let boolean (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Boolean"

    match previous state with
    | Some { Lexeme = Lexeme.Keyword True } -> Ok(state, ELiteral(LBool true, TBool))
    | Some { Lexeme = Lexeme.Keyword False } -> Ok(state, ELiteral(LBool false, TBool))
    | _ -> Error(Expected("Boolean"), state)

/// <summary>
/// Parser for string values.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
let string (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "String"

    match previous state with
    | Some { Lexeme = Lexeme.String s } -> Ok(state, ELiteral(LString s, TString))
    | _ -> Error(Expected("String"), state)

/// <summary>
/// Parser for numbers.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
let number (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Number"

    match previous state with
    | Some { Lexeme = Lexeme.Number n } ->
        match n with
        | LInteger i -> Ok(state, ELiteral(LNumber(LInteger(i)), TInteger))
        | LFloat f -> Ok(state, ELiteral(LNumber(LFloat(f)), TFloat))
        | LRational(n, d) -> Ok(state, ELiteral(LNumber(LRational(n, d)), TRational))
        | LComplex(r, i) -> Ok(state, ELiteral(LNumber(LComplex(r, i)), TComplex))
        | LChar c -> Ok(state, ELiteral(LNumber(LChar c), TChar))
    | _ -> Error(Expected "Number", state)

/// <summary>
/// Parser for an identifier.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
let ident (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Ident"

    match previous state with
    | Some id ->
        let expr = EIdentifier(id, None)
        Ok(state, expr)
    | _ -> Error(Expected("Identifier"), state)


/// <summary>
/// Parser for a match case pattern.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
let rec pattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "Pattern"

    match nextToken state with
    | Some(state, { Lexeme = Punctuation LeftBracket }) -> listPattern state
    | Some(state, { Lexeme = Punctuation LeftParen }) -> tuplePattern state
    | Some(state, { Lexeme = Lexeme.Number _ }) -> numberPattern state
    | Some(state, { Lexeme = Lexeme.String _ }) -> stringPattern state
    | Some(state, { Lexeme = Lexeme.Keyword True }) -> booleanPattern state
    | Some(state, { Lexeme = Lexeme.Keyword False }) -> booleanPattern state
    | Some(state, { Lexeme = Lexeme.Keyword Nil }) -> nilPattern state
    | Some(state, { Lexeme = Lexeme.Identifier _ }) -> identifierPattern state
    | _ -> Error(Expected("Pattern"), state)
    |> Result.bind (fun (state, pattern) -> consPattern state pattern)

/// <summary>
/// Parser for a list of patterns (e.g. 1, 2, 3).
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern list.</returns>
and patternList (state: ParserState) : ParseResult<Pattern list> =
    let state = setLabel state "PatternList"

    let rec loop (state: ParserState) (patterns: Pattern list) : ParseResult<Pattern list> =
        result {
            let! state, pattern = pattern state

            return!
                match peek state with
                | Some { Lexeme = Operator(Comma, _) } ->
                    let state = advance state
                    loop state (pattern :: patterns)
                | _ -> Ok(state, List.rev (pattern :: patterns))
        }

    loop state []
//     pattern state
//     |> Result.bind (fun (state, pattern) -> Ok(state, pattern :: patterns))
//     |> Result.bind (fun (state, patterns) ->
//         match peek state with
//         | Some { Lexeme = Punctuation Comma } ->
//             let state = advance state
//             loop state patterns
//         | _ -> Ok(state, List.rev patterns))
//
// loop state [] |> Result.bind (fun (state, patterns) -> Ok(state, List.rev patterns))

/// <summary>
/// Parser for a list or range pattern, e.g. [1, 2, 3] or [1..3]. TODO range
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
and listPattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "ListPattern"

    match nextToken state with
    | Some(state, { Lexeme = Punctuation RightBracket }) -> Ok(state, PList([]))
    | _ ->
        result {
            let! state, patterns = patternList state
            let! state = expect state (Punctuation RightBracket)
            return (state, PList(patterns))
        }

/// <summary>
/// Parser for a tuple pattern, e.g. (1, 2, 3).
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
and tuplePattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "TuplePattern"

    match nextToken state with
    | Some(state, { Lexeme = Punctuation RightParen }) -> Ok(advance state, PTuple([]))
    | _ ->
        result {
            let! state, patterns = patternList state
            let! state = expect state (Punctuation RightParen)
            return state, PTuple(patterns)
        }


/// <summary>
/// Parser for a number pattern.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
and numberPattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "NumberPattern"

    match previous state with
    | Some { Lexeme = Lexeme.Number n } ->
        match n with
        | LInteger i -> Ok(state, PLiteral(LNumber(LInteger(i))))
        | LFloat f -> Ok(state, PLiteral(LNumber(LFloat(f))))
        | LRational(n, d) -> Ok(state, PLiteral(LNumber(LRational(n, d))))
        | LComplex(r, i) -> Ok(state, PLiteral(LNumber(LComplex(r, i))))
        | LChar c -> Ok(state, PLiteral(LNumber(LChar c)))
    | _ -> Error(Expected("Number"), state)

/// <summary>
/// Parser for a string pattern.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
and stringPattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "StringPattern"

    match previous state with
    | Some { Lexeme = Lexeme.String s } -> Ok(state, PLiteral(LString s))
    | _ -> Error(Expected("String"), state)

/// <summary>
/// Parser for a boolean pattern.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
and booleanPattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "BooleanPattern"

    match previous state with
    | Some { Lexeme = Lexeme.Keyword True } -> Ok(state, PLiteral(LBool true))
    | Some { Lexeme = Lexeme.Keyword False } -> Ok(state, PLiteral(LBool false))
    | _ -> Error(Expected("Boolean"), state)

/// <summary>
/// Parser for a nil pattern.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
and nilPattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "NilPattern"

    match previous state with
    | Some { Lexeme = Lexeme.Keyword Nil } -> Ok(state, PLiteral(LUnit))
    | _ -> Error(Expected("Nil"), state)

/// <summary>
/// Parser for a cons pattern, e.g. 1 :: [2, 3].
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="pat">The pattern to cons onto.</param>
/// <returns>The new state and the parsed pattern.</returns>
and consPattern (state: ParserState) (pat: Pattern) : ParseResult<Pattern> =
    let state = setLabel state "ConsPattern"

    match peek state with
    | Some { Lexeme = Operator(ColonColon, _) } ->
        let state = advance state

        pattern state
        |> Result.bind (fun (state, patterns) -> Ok(state, PCons(pat, patterns)))
    | _ -> Ok(state, pat)

/// <summary>
/// Parser for an identifier pattern, e.g. x.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed pattern.</returns>
and identifierPattern (state: ParserState) : ParseResult<Pattern> =
    let state = setLabel state "IdentifierPattern"

    match previous state with
    | Some({ Lexeme = Lexeme.Identifier _ } as id) -> Ok(state, PIdentifier(id))
    | _ -> Error(Expected("Identifier"), state)


/// <summary>
/// Get the precedence rule for a given lexeme.
/// </summary>
/// <param name="lexeme">The lexeme to get the rule for.</param>
/// <returns>The parse rule for the lexeme.</returns>
let rec getRule (lexeme: Lexeme) : ParseRule =

    /// <summary>
    /// Get the precedence rule for a given punctuation.
    /// </summary>
    /// <param name="punc">The punctuation to get the rule for.</param>
    /// <returns>The parse rule for the punctuation.</returns>
    let getPunctuationRule (punc: Punctuation) =
        match punc with
        | LeftParen ->
            { Prefix = Some leftParen
              Infix = Some call
              Postfix = None
              Precedence = Precedence.Call }
        | LeftBrace ->
            { Prefix = Some leftBrace
              Infix = None
              Postfix = None
              Precedence = Precedence.None }
        | LeftBracket ->
            { Prefix = Some listOrRange
              Infix = Some index
              Postfix = None
              Precedence = Precedence.Index }
        | _ -> defaultRule


    /// <summary>
    /// Get the precedence rule for a given operator.
    /// </summary>
    /// <param name="op">The operator to get the rule for.</param>
    /// <returns>The parse rule for the operator.</returns>
    let getOperatorRule (op: Operator) =
        match op with
        | Dot ->
            { Prefix = None
              Infix = Some recordSelect
              Postfix = None
              Precedence = Precedence.Index }
        | Plus
        | PlusPlus
        | Minus ->
            { Prefix = Some unary
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Term }
        | Slash
        | Percent
        | Cross
        | DotStar
        | Star ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Factor }


        | Caret
        | StarStar ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Exponent }
        | BangEqual
        | EqualEqual
        | Equal ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Equality }
        | Greater
        | GreaterEqual
        | Less
        | LessEqual ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Comparison }
        | Bang ->
            { Prefix = Some unary
              Infix = None
              Postfix = None
              Precedence = Precedence.Unary }
        | AmpersandAmpersand ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.And }
        | PipePipe ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Or }
        | ColonColon ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Assignment }
        | Custom _ ->
            { Prefix = Some unary
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.None }

        | Colon ->
            { Prefix = None
              Infix = Some cast
              Postfix = None
              Precedence = Precedence.Call }
        | Dollar ->
            { Prefix = Some codeBlock
              Infix = None
              Postfix = None
              Precedence = Precedence.Assignment }

        | _ -> defaultRule

    /// <summary>
    /// Get the precedence rule for a given keyword.
    /// </summary>
    /// <param name="kw">The keyword to get the rule for.</param>
    /// <returns>The parse rule for the keyword.</returns>
    let getKeywordRule (kw: Keyword) =
        match kw with
        | True
        | False ->
            { Prefix = Some boolean
              Infix = None
              Postfix = None
              Precedence = Precedence.None }
        | Nil ->
            { Prefix = Some nil
              Infix = None
              Postfix = None
              Precedence = Precedence.None }
        | If ->
            { Prefix = Some ifElse
              Infix = Some ternary
              Postfix = None
              Precedence = Precedence.None }
        | In ->
            { Prefix = None
              Infix = None // todo, infix for list checking
              Postfix = None
              Precedence = Precedence.None }
        | Match ->
            { Prefix = Some matchCase
              Infix = None
              Postfix = None
              Precedence = Precedence.None }
        | Keyword.And ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.And }
        | Keyword.Or ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Or }
        | _ -> defaultRule

    match lexeme with
    | Operator(op, _) -> getOperatorRule op
    | Punctuation punc -> getPunctuationRule punc
    | Lexeme.Keyword kw -> getKeywordRule kw

    | Lexeme.Number _ ->
        { Prefix = Some number
          Infix = None
          Postfix = None
          Precedence = Precedence.None }
    | Lexeme.String _ ->
        { Prefix = Some string
          Infix = None
          Postfix = None
          Precedence = Precedence.None }
    | Lexeme.Identifier _ ->
        { Prefix = Some ident
          Infix = None
          Postfix = None
          Precedence = Precedence.None }


/// <summary>
/// Parser for a match case.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> match x with { case 1 -> 2, case 3 -> 4 } </c>
/// </example>
and matchCase (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Match"

    let rec matchCaseBranches
        (state: ParserState)
        (branches: (Pattern * Expr) list)
        : ParseResult<(Pattern * Expr) list> =
        match nextToken state with
        | Some(state, { Lexeme = Punctuation RightBrace }) -> Ok(state, branches)
        | Some(state, { Lexeme = Lexeme.Keyword Keyword.Case }) ->
            result {
                let! state, pattern = pattern state
                let! state = expect state (Operator(Arrow, None))
                let! state, branch = expression state Precedence.None

                match peek state with
                | Some { Lexeme = Punctuation Newline }
                | Some { Lexeme = Operator(Comma, _) } ->
                    return! matchCaseBranches (advance state) ((pattern, branch) :: branches)
                | Some { Lexeme = Punctuation RightBrace } ->
                    return (advance state, List.rev ((pattern, branch) :: branches))
                | _ -> return! Error(Expected("',' or '}' after case branch."), state)
            }
        | _ -> Error(Expected "case branch", state)

    result {
        let! state, expr = expression state Precedence.None
        let! state = expect state (Lexeme.Keyword Keyword.With)
        let! state = expect state (Punctuation LeftBrace)
        let! state, branches = matchCaseBranches state []
        return (state, EMatch(expr, branches, None))
    }

/// <summary>
/// Parser for casting an expression to a type.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="left">The expression to cast.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> x : int </c>
/// </example>
and cast (state: ParserState) (left: Expr) : ParseResult<Expr> =
    typeHint state
    |> Result.bind (fun (state, typ) ->
        let defaultPos = { Line = 0; Column = 0 }

        /// <summary>
        /// Get the default value for a given type.
        /// </summary>
        let rec getDefault =
            function
            | TBool -> Some(ELiteral(LBool true, TBool))
            | TInteger -> Some(ELiteral(LNumber(LInteger 0), TInteger))
            | TFloat -> Some(ELiteral(LNumber(LFloat 0), TFloat))
            | TRational -> Some(ELiteral(LNumber(LRational(0, 0)), TRational))
            | TChar -> Some(ELiteral(LNumber(LChar 'a'), TChar))
            | TComplex -> Some(ELiteral(LNumber(LComplex(0, 0)), TComplex))
            | TString -> Some(ELiteral((LString ""), TString))
            | TTensor(typ1, _) ->
                let def = getDefault typ1

                if Option.isSome def then
                    Some(EList([ Option.get def ], None))
                else
                    None
            | _ -> None

        let id = getDefault typ

        match id with
        | Some id ->
            let expr =
                ECall(
                    EIdentifier(
                        { Lexeme = Identifier "cast"
                          Position = defaultPos },
                        None
                    ),
                    [ left; id ],
                    None
                )

            Ok(state, expr)
        | _ -> Ok(state, left)

    )

/// <summary>
/// Parse a code block.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> $ { let x = 1 } </c>
/// </example>
and codeBlock (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Code Block"

    result {
        let! state = expect state (Punctuation LeftBrace)
        let! state, expr = block state
        return (state, ECodeBlock expr)
    }

/// <summary>
/// Expression parser (the main parser).
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="precedence">The precedence of the current expression.</param>
/// <returns>The new state and the parsed expression.</returns>
and expression (state: ParserState) (precedence: Precedence) : ParseResult<Expr> =
    let state = setLabel state "Expression"

    result {
        let! state, expr = prefix state
        return! infix precedence state expr
    }

/// <summary>
/// Parse a prefix expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> -1 </c>
/// </example>
and prefix (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Prefix"

    match peek state with
    | Some token ->
        match (getRule token.Lexeme).Prefix with
        | Some fn -> fn (advance state)
        | None -> Error(ExpectedExpression, state)
    | None -> Error(ExpectedExpression, state)

/// <summary>
/// Parse an infix expression.
/// </summary>
/// <param name="precedence">The precedence of the current expression.</param>
/// <param name="state">The parser state.</param>
/// <param name="expr">The left expression.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> 1 + 2 </c>
/// </example>
and infix (precedence: Precedence) (state: ParserState) (expr: Expr) : ParseResult<Expr> =
    let state = setLabel state "Infix"

    match peek state with
    | Some token ->
        let rule = getRule token.Lexeme

        if int precedence <= int rule.Precedence then
            match rule.Infix with
            | Some fn ->
                result {
                    let! state, expr = fn (advance state) expr
                    return! infix precedence state expr
                }
            | None -> postfix state expr
        else
            postfix state expr
    | None -> postfix state expr

/// <summary>
/// Parse a postfix expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="expr">The left expression.</param>
/// <returns>The new state and the parsed expression.</returns>
and postfix (state: ParserState) (expr: Expr) : ParseResult<Expr> =
    let state = setLabel state "Postfix"

    match peek state with
    | Some token ->
        match (getRule token.Lexeme).Postfix with
        | Some fn ->
            result {
                let! state, expr = fn (advance state) expr
                return! postfix state expr
            }
        | None -> Ok(state, expr)
    | None -> Ok(state, expr)

/// <summary>
/// Parses record select syntax.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="left">The left expression.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> x.y </c>
/// </example>
and recordSelect (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "RecordSelect"

    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) -> Ok(state, ERecordSelect(left, name, None))
    | _ -> Error(Expected("field name"), state)

/// <summary>
/// Parses a binary expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="left">The left expression.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> 1 + 2 </c>
/// </example>
and binary (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Binary"

    match previous state with
    | Some op ->
        let rule = getRule op.Lexeme
        let nextPrecedence: Precedence = enum (int rule.Precedence + 1)

        let nextPrecedence =
            if nextPrecedence = Precedence.Call then
                Precedence.None
            else
                nextPrecedence

        let op =
            match op with
            | { Lexeme = Operator(op, _)
                Position = pos } ->
                { Lexeme = Operator(op, Some Infix)
                  Position = pos }
            | _ -> op

        expression state nextPrecedence
        |> Result.bind (fun (state, right) -> Ok(state, ECall(EIdentifier(op, None), [ left; right ], None)))
    | _ -> Error(Expected "operator", state)

/// <summary>
/// Parses a unary expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> -1 </c>
/// </example>
and unary (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Unary"

    match previous state with
    | Some op ->
        let op =
            match op with
            | { Lexeme = Operator(op, _)
                Position = pos } ->
                { Lexeme = Operator(op, Some Prefix) // Unary function, therefore fixity is Prefix
                  Position = pos }
            | _ -> op

        expression state Precedence.Unary
        |> Result.bind (fun (state, expr) -> Ok(state, ECall(EIdentifier(op, None), [ expr ], None))) // Parse as function call
    | _ -> Error(Expected "operator", state)

/// <summary>
/// Determines what parser to choose when encountering a left brace in prefix position.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
and leftBrace (state: ParserState) : ParseResult<Expr> =
    // either record, block
    let state = setLabel state "LeftBrace"

    match peek state with
    | Some { Lexeme = Punctuation RightBrace } -> Ok(advance state, ERecordEmpty(TRowEmpty))
    | Some { Lexeme = Identifier _ } ->
        match peek (advance state) with
        | Some { Lexeme = Operator(Colon, _) }
        | Some { Lexeme = Operator(Equal, _) } -> record state // Must be a record
        // | Some { Lexeme = Lexeme.Keyword Keyword.With } -> recordUpdate state // TODO, not with identifier, just arb epxpr
        | _ -> block state // Otherwise is a block
    | _ -> block state

/// <summary>
/// Parses the fields of a record.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="fields">The current fields of the record.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> x = 1, y = 2 </c>
/// </example>
and recordFields
    (state: ParserState)
    (fields: (Token * Expr * TType option) list)
    : ParseResult<(Token * Expr * TType option) list> =
    result {
        match peek state with
        | Some { Lexeme = Punctuation RightBrace } -> return (advance state, List.rev fields)
        | Some { Lexeme = Lexeme.Identifier _ } ->
            match nextToken state with
            | Some(state, name) ->
                match nextToken state with
                | Some(state, { Lexeme = Operator(Equal, _) }) ->
                    let! state, value = expression state Precedence.Assignment

                    match peek state with
                    | Some { Lexeme = Operator(Comma, _) } ->
                        return! recordFields (advance state) ((name, value, None) :: fields)
                    | Some { Lexeme = Punctuation RightBrace } ->
                        return (advance state, List.rev ((name, value, None) :: fields))
                    | _ -> return! Error(Expected "',' or '}' after record field.", state)
                | Some(state, { Lexeme = Operator(Colon, _) }) ->
                    let! state, fieldType = typeHint state
                    let! state = expect state (Operator(Equal, None))
                    let! state, value = expression state Precedence.Assignment

                    match peek state with
                    | Some { Lexeme = Operator(Comma, _) } ->
                        return! recordFields (advance state) ((name, value, Some fieldType) :: fields)
                    | Some { Lexeme = Punctuation RightBrace } ->
                        return (advance state, List.rev ((name, value, Some fieldType) :: fields))
                    | _ -> return! Error(Expected "',' or '}' after record field.", state)
                | _ -> return! Error(Expected "field value", state)
            | _ -> return! Error(Expected "field name", state)
        | _ -> return! Error(Expected "field name", state)
    }

/// <summary>
/// Parses a record.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> { x = 1, y = 2 } </c>
/// </example>
and record (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Record"

    let rowsToExpr (rows: (Token * Expr * TType option) list) : Expr =
        let rec loop (rows: (Token * Expr * TType option) list) (record: Expr) : Expr =
            match rows with
            | [] -> record
            | (name, value, typ) :: rest -> loop rest (ERecordExtend((name, value, typ), record, None))

        loop rows (ERecordEmpty(TRowEmpty))

    recordFields state []
    |> Result.bind (fun (state, fields) -> Ok(state, rowsToExpr fields))


/// <summary>
/// Parses a comma separated list for tuples or lists.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> 1, 2, 3 </c>
/// </example>
and commaSeparatedList (state: ParserState) : ParseResult<Expr list> =
    let state = setLabel state "CommaSeparatedList"

    let rec loop (state: ParserState) (exprs: Expr list) : ParseResult<Expr list> =
        result {
            let! state, expr = expression state Precedence.None

            return!
                match peek state with
                | Some { Lexeme = Operator(Comma, _) } ->
                    let state = advance state
                    loop state (expr :: exprs)
                | _ -> Ok(state, List.rev (expr :: exprs))
        }

    loop state []

/// <summary>
/// Determines what parser to choose when encountering a left bracket in prefix position.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
and listOrRange (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "ListOrRange"

    match peek state with
    | Some { Lexeme = Punctuation RightBracket } -> Ok(advance state, EList([], None))
    | _ ->
        result {
            let! state, start = expression state Precedence.None

            match nextToken state with
            | Some(state, { Lexeme = Operator(DotDot, _) }) ->
                let! state, end_ = expression state Precedence.None
                let! state = expect state (Punctuation RightBracket)
                return (state, ERange(start, end_, None))
            | Some(state, { Lexeme = Operator(Comma, _) }) ->
                let! state, exprs = commaSeparatedList state
                let! state = expect state (Punctuation RightBracket)
                return (state, EList(start :: exprs, None))
            | Some(state, { Lexeme = Punctuation RightBracket }) -> return (state, EList([ start ], None))
            | _ -> return! Error(Expected "',' or '..' after list element.", state)
        }

/// <summary>
/// Parses a grouping.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> (1 + 2) </c>
/// </example>
and grouping (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Grouping"

    result {
        let! state, expr = expression state Precedence.None
        let! state = expect state (Punctuation RightParen)
        return (state, EGrouping(expr, None))
    }

/// <summary>
/// Parses an index expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="left">The left expression.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> x[1] </c>
/// <c> x[1..3] </c>
/// <c> x[1..] </c>
/// <c> x[..3] </c>
/// </example>
and index (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Index"

    match peek state with
    | Some { Lexeme = Operator(DotDot, _) } ->
        let state = advance state

        result {
            let! state, end_ = expression state Precedence.None
            let! state = expect state (Punctuation RightBracket)
            return (state, EIndexRange(left, ELiteral(LNumber(LInteger 0), TInteger), end_, None))
        }
    | _ ->
        result {
            let! state, start = expression state Precedence.None

            match peek state with
            | Some { Lexeme = Operator(DotDot, _) } ->
                let state = advance state

                match peek state with
                | Some { Lexeme = Punctuation RightBracket } ->
                    let state = advance state
                    return (state, EIndexRange(left, start, ELiteral(LNumber(LInteger 0), TInteger), None))
                | _ ->
                    let! state, end_ = expression state Precedence.None
                    let! state = expect state (Punctuation RightBracket)
                    return (state, EIndexRange(left, start, end_, None))
            | _ ->
                let! state = expect state (Punctuation RightBracket)
                return (state, EIndex(left, start, None))
        }

/// <summary>
/// Parses an if expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> if x then 1 else 2 </c>
/// </example>
and ifElse (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "If"

    result {
        let! state, condition = expression state Precedence.None
        let! state = expect state (Lexeme.Keyword Keyword.Then)
        let! state, thenBranch = expression state Precedence.None

        match nextToken state with
        | Some(state, { Lexeme = Keyword Else }) ->
            let! state, elseBranch = expression state Precedence.None
            return (state, EIf(condition, thenBranch, elseBranch, None))
        | _ -> return (state, EIf(condition, thenBranch, ELiteral(LUnit, TUnit), Some TUnit))
    }

/// <summary>
/// Parses a ternary expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="trueBranch">The true branch of the ternary expression.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> 1 if x else 2 </c>
/// </example>
and ternary (state: ParserState) (trueBranch: Expr) : ParseResult<Expr> =
    result {
        let! state, condition = expression state Precedence.None
        let! state = expect state (Lexeme.Keyword Keyword.Else)
        let! state, falseBranch = expression state Precedence.None
        return (state, EIf(condition, trueBranch, falseBranch, None))
    }

/// <summary>
/// Parses a call expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="callee">The callee of the call expression.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> f(1, 2) </c>
/// </example>
and call (state: ParserState) (callee: Expr) : ParseResult<Expr> =
    let state = setLabel state "Call"

    let rec loop (state: ParserState) (args: Expr list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Punctuation RightParen } ->
            let state = advance state
            Ok(state, ECall(callee, args, None))
        | _ ->
            expression state Precedence.None
            |> Result.bind (fun (state, arg) ->
                match nextToken state with
                | Some(state, { Lexeme = Operator(Comma, _) }) -> loop state (arg :: args)
                | Some(state, { Lexeme = Punctuation RightParen }) ->
                    Ok(state, ECall(callee, List.rev (arg :: args), None))
                | _ -> Error(Expected "argument or ')' after call.", state))

    loop state []

/// <summary>
/// Determines what parser to choose when encountering a left parenthesis in prefix position.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
and leftParen (state: ParserState) : ParseResult<Expr> =
    match peek state with
    | Some { Lexeme = Lexeme.Identifier _ } ->
        match peek (advance state) with
        | Some { Lexeme = Operator(Comma, _) } -> lambdaOrTuple state
        | Some { Lexeme = Operator(Colon, _) } -> lambda state

        | Some { Lexeme = Punctuation RightParen } ->
            match peek (advance (advance state)) with
            | Some { Lexeme = Operator(Arrow, _) }
            | Some { Lexeme = Operator(Colon, _) }
            | Some { Lexeme = Punctuation LeftBrace } -> lambda state
            | _ -> grouping state
        | _ -> grouping state
    | Some { Lexeme = Punctuation RightParen } ->
        match peek (advance state) with
        | Some { Lexeme = Operator(Arrow, _) }
        | Some { Lexeme = Operator(Colon, _) }
        | Some { Lexeme = Punctuation LeftBrace } -> lambda state
        | _ -> Ok(advance state, ELiteral(LUnit, TUnit))
    | _ -> groupingOrTuple state

/// <summary>
/// Parses a tuple.
/// </summary>
/// <param name="state">The parser state.</param>
/// <param name="items">The items of the tuple.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> (1, true) </c>
/// </example>
and parseTuple (state: ParserState) (items: Expr list) : Expr ParseResult =
    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Operator(Comma, _) } -> parseTuple (advance state) (expr :: items)
        | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, ETuple(List.rev (expr :: items), None))
        | _ -> Error(Expected "',' or ')'.", state))

/// <summary>
/// Determines what parser to choose when encountering a left parenthesis in prefix position.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
and lambdaOrTuple (state: ParserState) : Expr ParseResult =
    match peek (advance state) with
    | Some { Lexeme = Punctuation RightParen } -> Ok(advance (advance state), ELiteral(LUnit, TUnit))
    | Some { Lexeme = Operator(Comma, _) } ->
        let tuple = parseTuple state []

        match tuple with
        | Ok(newState, expr) ->
            match peek newState with
            | Some { Lexeme = Operator(Arrow, _) } -> lambda state
            | Some { Lexeme = Operator(Colon, _) } -> lambda state
            | Some { Lexeme = Punctuation LeftBrace } -> lambda state
            | _ -> Ok(newState, expr)
        | _ -> lambda state
    | _ -> lambda state

/// <summary>
/// Determines what parser to choose when encountering a left parenthesis in prefix position.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
and groupingOrTuple (state: ParserState) : ParseResult<Expr> =
    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Operator(Comma, _) } -> parseTuple (advance state) [ expr ]
        | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, expr)
        | _ -> Error(Expected "',' or ')'.", state))

/// <summary>
/// Parses a lambda expression.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> x -> x + 1 </c>
/// </example>
and lambda (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Function"

    let rec parseParameters
        (state: ParserState)
        (params': (Token * TType option) list)
        : ParseResult<(Token * TType option) list> =
        match nextToken state with
        | Some(state, { Lexeme = Punctuation RightParen }) -> Ok(state, List.rev params')
        | Some(state, ({ Lexeme = Identifier _ } as token)) ->
            let paramType, state =
                match peek state with
                | Some { Lexeme = Operator(Colon, _) } ->
                    let state = advance state

                    match typeHint state with
                    | Ok(state, paramType) -> Some paramType, state
                    | Error _ -> None, state
                | _ -> None, state

            match peek state with
            | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, (List.rev ((token, paramType) :: params')))
            | Some { Lexeme = Operator(Comma, _) } -> parseParameters (advance state) ((token, paramType) :: params')
            | _ -> Error(Expected "',' or ')'.", state)
        | _ -> Error(Expected "parameter name.", state)

    let parseBody (state: ParserState) : ParseResult<Expr> =
        match nextToken state with
        | Some(state, { Lexeme = Punctuation LeftBrace }) -> block state
        | Some(state, { Lexeme = Operator(Arrow, _) }) -> expression state Precedence.Assignment
        | _ -> Error(Expected "function body.", state)

    result {
        let! state, parameters = parseParameters state []

        match nextToken state with
        | Some(state, { Lexeme = Operator(Colon, _) }) ->
            let! state, returnType = typeHint state
            let! state, body = parseBody state
            return (state, ELambda(parameters, body, Some returnType, false, None, false))
        | _ ->
            let! state, body = parseBody state
            return (state, ELambda(parameters, body, None, false, None, false))
    }

/// <summary>
/// Parses a function type hint.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed type.</returns>
/// <example>
/// <c> (int, float) -> int </c>
/// </example>
and funcType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "FunctionType"

    let rec parseParams (state: ParserState) (paramList: TType list) : ParseResult<TType list> =
        match peek state with
        | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, paramList)
        | _ ->
            typeHint state
            |> Result.bind (fun (state, param) ->
                match peek state with
                | Some { Lexeme = Operator(Comma, _) } -> parseParams (advance state) (param :: paramList)
                | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, (param :: paramList))
                | _ -> Error(Expected "',' or ')'.", state))

    result {
        let! state, paramList = parseParams state []
        let! state = expect state (Operator(Arrow, None))
        let! state, returnType = typeHint state
        return (state, TFunction(paramList, returnType, false, false))
    }

/// <summary>
/// Parses a function type hint or a tuple type hint.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed type.</returns>
and funcTypeOrTuple (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "FunctionOrTupleType"
    // tuple in form (int, float)
    // function in form (int, float) -> int
    let rec parseParams (state: ParserState) (paramList: TType list) : ParseResult<TType list> =
        match peek state with
        | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, paramList)
        | _ ->
            typeHint state
            |> Result.bind (fun (state, param) ->
                match peek state with
                | Some { Lexeme = Operator(Comma, _) } -> parseParams (advance state) (param :: paramList)
                | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, (param :: paramList))
                | _ -> Error(Expected "',' or ')'.", state))

    result {
        let! state, paramList = parseParams state []

        match peek state with
        | Some { Lexeme = Operator(Arrow, _) } ->
            let state = advance state
            let! state, returnType = typeHint state
            return (state, TFunction(paramList, returnType, false, false))
        | _ ->
            let tupleType = TTuple(paramList)
            return (state, tupleType)
    }




/// <summary>
/// Parses a tensor type hint.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed type.</returns>
/// <example>
/// <c> [int] </c>
/// </example>
and tensorType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "TensorType"

    result {
        let! state, innerType = typeHint state
        let! state = expect state (Punctuation RightBracket)
        return (state, TTensor(innerType, DAny))
    }


/// <summary>
/// Parses a record type hint.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed type.</returns>
/// <example>
/// <c> { a: int, b: float } </c>
/// </example>
and recordType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "RecordType"
    // let x: { a: int, b: float } = { a = 1, b = 2.0 }

    let rec parseFields (state: ParserState) (fields: (Token * TType) list) : (Token * TType) list ParseResult =
        match nextToken state with
        | Some(state, { Lexeme = Punctuation RightBrace }) -> Ok(state, fields)
        | Some(state, ({ Lexeme = Identifier _ } as fieldName)) ->
            result {
                let! state = expect state (Operator(Colon, None))
                let! state, fieldType = typeHint state

                match nextToken state with
                | Some(state, { Lexeme = Operator(Comma, _) }) ->
                    return! parseFields state ((fieldName, fieldType) :: fields)
                | Some(state, { Lexeme = Punctuation RightBrace }) -> return (state, ((fieldName, fieldType) :: fields))
                | _ -> return! Error(Expected "',' or '}' after record field.", state)
            }
        | _ -> Error(Expected "field name", state)

    let rec fieldsToType (fields: (Token * TType) list) : TType =
        match fields with
        | [] -> TRowEmpty
        | (name, fieldType) :: rest -> TRowExtend(name, fieldType, fieldsToType rest)

    parseFields state []
    |> Result.bind (fun (state, fields) -> Ok(state, TRecord(fieldsToType fields)))



/// <summary>
/// Parses a type hint.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed type.</returns>
/// <example>
/// <c> int </c>
/// </example>
and typeHint (state: ParserState) : ParseResult<TType> =
    match nextToken state with
    | Some(state, ({ Lexeme = Lexeme.Identifier typeName } as tok)) ->
        match typeName with
        | "int" -> Ok(state, TInteger)
        | "float" -> Ok(state, TFloat)
        | "rational" -> Ok(state, TRational)
        | "complex" -> Ok(state, TComplex)
        | "bool" -> Ok(state, TBool)
        | "string" -> Ok(state, TString)
        | "unit" -> Ok(state, TUnit)
        | "never" -> Ok(state, TNever)
        | "any" -> Ok(state, TAny)
        | "char" -> Ok(state, TChar)
        | _ -> Ok(state, TAlias(tok, None))
    | Some(state, { Lexeme = Punctuation LeftParen }) -> funcTypeOrTuple state
    | Some(state, { Lexeme = Punctuation LeftBracket }) -> tensorType state
    | Some(state, { Lexeme = Punctuation LeftBrace }) -> recordType state
    | _ -> Error(ExpectedType "type name", state)

/// <summary>
/// Parses a block.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed expression.</returns>
/// <example>
/// <c> { 1 } </c>
/// </example>
and block (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Block"

    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Punctuation RightBrace } -> Ok(advance state, EBlock(List.rev stmts, false, None))
        | None -> Error(UnexpectedEndOfInput, state)
        | _ -> statement state |> Result.bind (fun (state, stmt) -> loop state (stmt :: stmts))

    loop state []


/// <summary>
/// Parses a variable declaration.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed statement.</returns>
/// <example>
/// <c> let x = 1 </c>
/// </example>
and varDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Variable"

    result {
        let! state, name = expectIdentifier state

        let! state, varType =
            match peek state with
            | Some { Lexeme = Operator(Colon, _) } ->
                let state = advance state
                typeHint state |> Result.bind (fun (state, typ) -> Ok(state, Some typ))
            | _ -> Ok(state, None)

        let! state = expect state (Operator(Equal, None))

        match peek state with
        | Some { Lexeme = Keyword Import } ->
            let state = advance state

            match nextToken state with
            | Some(state, { Lexeme = Lexeme.String s }) -> return (state, SImport(Some name, s, false, varType))
            | Some(state, { Lexeme = Identifier l }) -> return (state, SImport(Some name, l, true, varType))
            | _ -> return! Error(Expected "string or identifier after import keyword.", state)

        | _ ->

            let! state, expr = expression state Precedence.Assignment

            match varType with
            | Some typ ->
                let defaultPos = { Line = 0; Column = 0 }
                let id = getDefault typ

                match id with
                | Some typ ->
                    let expr =
                        ECall(
                            EIdentifier(
                                { Lexeme = Identifier "cast"
                                  Position = defaultPos },
                                None
                            ),
                            [ expr; typ ],
                            None
                        )

                    return (state, SVariableDeclaration(name, expr, varType))
                | None -> return (state, SVariableDeclaration(name, expr, varType))
            | None -> return (state, SVariableDeclaration(name, expr, varType))
    }

/// <summary>
/// Parses an operator declaration.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed statement.</returns>
and operatorDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Operator"

    result {
        let! state = expect state (Punctuation LeftParen)
        let! state, op = expectOperator state
        let pos = op.Position

        let op =
            match op.Lexeme with
            | Operator(op, _) -> op
            | _ -> failwith "Expected operator."

        let! state = expect state (Punctuation RightParen)
        let! state = expect state (Operator(Equal, None))
        let! state = expect state (Punctuation LeftParen)
        let! state, func = lambda state

        match func with
        | ELambda(parameters, _, _, false, None, false) ->
            let fixity = Seq.length parameters

            match fixity with
            | 1 ->
                let op =
                    { Lexeme = Operator(op, Some Prefix)
                      Position = pos }

                return (state, SVariableDeclaration(op, func, None))
            | 2 ->
                let op =
                    { Lexeme = Operator(op, Some Infix)
                      Position = pos }

                return (state, SVariableDeclaration(op, func, None))
            | _ -> return! Error(Expected "unary or binary operator.", state)
        | _ -> return! Error(Expected "lambda expression.", state)
    }

/// <summary>
/// Parses an assert statement.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed statement.</returns>
/// <example>
/// <c> assert x, "message" </c>
/// </example>
and assertStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Assert"

    result {
        let! state, expr = expression state Precedence.Assignment

        match peek state with
        | Some { Lexeme = Operator(Comma, _) } ->
            let state = advance state
            let! state, message = expression state Precedence.Assignment
            return (state, SAssertStatement(expr, Some message, Some TUnit))
        | _ -> return (state, SAssertStatement(expr, None, Some TUnit))
    }

/// <summary>
/// Parses a type declaration.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed statement.</returns>
/// <example>
/// <c> type T = int </c>
/// </example>
and typeDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Type"

    result {
        let! state, name = expectIdentifier state
        let! state = expect state (Operator(Equal, None))
        let! state, typ = typeHint state
        return (state, STypeDeclaration(name, typ, None))
    }

/// <summary>
/// Parses a statement.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed statement.</returns>
and statement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Statement"

    match peek state with
    | Some token ->
        match token.Lexeme with
        | Keyword kw ->
            match kw with
            | Keyword.Let ->
                let state = advance state

                match peek state with
                | Some { Lexeme = Identifier _ } -> varDecl state
                | Some { Lexeme = Punctuation LeftParen } -> operatorDecl state
                | Some { Lexeme = Keyword Rec } ->
                    let state = advance state

                    result {
                        let! state, var = varDecl state

                        match var with
                        | SVariableDeclaration(name, expr, _) ->
                            match expr with
                            | ELambda(parameters, body, returnType, _, _, _) ->
                                let func = SRecFunc(name, parameters, body, returnType)
                                let func = analyseStmt func
                                return (state, func)
                            | _ -> return! Error(Expected "lambda expression.", state)
                        | _ -> return! Error(Expected "variable declaration.", state)
                    }
                | Some { Lexeme = Keyword Async } ->
                    let state = advance state

                    result {
                        let! state, var = varDecl state

                        match var with
                        | SVariableDeclaration(name, expr, _) ->
                            match expr with
                            | ELambda(parameters, body, returnType, _, _, _) ->
                                return (state, SAsync(name, parameters, body, returnType))
                            | _ -> return! Error(Expected "lambda expression.", state)
                        | _ -> return! Error(Expected "variable declaration.", state)
                    }

                | _ -> Error(Expected "variable name.", state)
            | Keyword.Assert -> assertStatement (advance state)
            | Keyword.Type -> typeDecl (advance state)
            | Keyword.Import ->
                result {
                    let state = advance state

                    match nextToken state with
                    | Some(state, { Lexeme = Lexeme.String s }) -> return (state, SImport(None, s, false, None))
                    | Some(state, { Lexeme = Identifier l }) -> return (state, SImport(None, l, true, None))
                    | _ -> return! Error(Expected "string or identifier after import keyword.", state)
                }
            | _ ->
                expression state Precedence.None
                |> Result.bind (fun (state, expr) -> Ok(state, SExpression(expr, None)))
        | _ ->
            expression state Precedence.None
            |> Result.bind (fun (state, expr) -> Ok(state, SExpression(expr, None)))
    | None -> Ok((state, SExpression(ELiteral(LUnit, TUnit), None)))

/// <summary>
/// Parses a statement, may raise exceptions.
/// </summary>
/// <param name="input">The input string.</param>
/// <returns>The parsed statement.</returns>
/// <exception cref="ParserException">Thrown when the parser encounters an error.</exception>
let statementUnsafe (input: string) : Stmt =
    let tokens = tokenize input

    match tokens with
    | Ok tokens ->
        let initialState = createParserState tokens

        match statement initialState with
        | Ok(_, stmt) -> stmt
        | Error f -> raise (ParserException(f))
    | Error f -> raise (LexerException(f))

/// <summary>
/// Parses a statement.
/// </summary>
/// <param name="input">The input string.</param>
/// <returns>The parsed statement result.</returns>
let parseStmt (input: string) : Result<Stmt, ParserError> =
    let tokens = tokenize input

    match tokens with
    | Ok tokens ->
        let initialState = createParserState tokens
        let stmt = statement initialState

        match stmt with
        | Ok(_, stmt) -> Ok(stmt)
        | Error(f, _) -> Error f
    | Error f -> Error(LexerError f)

/// <summary>
/// Parses a program.
/// </summary>
/// <param name="state">The parser state.</param>
/// <returns>The new state and the parsed program.</returns>
let parseProgram (state: ParserState) : ParseResult<Program> =
    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Program> =
        match peek state with
        | Some _ -> statement state |> Result.bind (fun (state, stmt) -> loop state (stmt :: stmts))
        | None -> Ok(state, List.rev stmts)

    loop state []

/// <summary>
/// Parses a program from a token list.
/// </summary>
/// <param name="tokens">The token list.</param>
/// <param name="prelude">Whether to import the prelude.</param>
/// <returns>The parsed program.</returns>
let parseTokens (tokens: Token list) (prelude: bool) : Program ParseResult =
    let initialState = createParserState tokens

    match parseProgram initialState with
    | Ok(state, stmts) ->
        if prelude then
            let importPrelude = SImport(None, "Prelude", true, None)
            let stmts = importPrelude :: stmts
            Ok(state, stmts)
        else
            Ok(state, stmts)
    | Error f -> Error f

/// <summary>
/// Parses a program from a string.
/// </summary>
/// <param name="input">The input string.</param>
/// <param name="prelude">Whether to import the prelude.</param>
/// <returns>The parsed program result.</returns>
let parse (input: string) (prelude: bool) : Program ParseResult =
    let tokens = tokenize input

    match tokens with
    | Ok tokens -> parseTokens tokens prelude
    | Error f -> Error(LexerError f, createParserState [])

/// <summary>
/// Parses a program from a file.
/// </summary>
/// <param name="file">The file path.</param>
/// <param name="prelude">Whether to import the prelude.</param>
/// <returns>The parsed program result.</returns>
let parseFile (file: string) (prelude: bool) : Program ParseResult =
    let input = System.IO.File.ReadAllText(file)
    parse input prelude

/// <summary>
/// Formats a parser error.
/// </summary>
/// <param name="error">The parser error.</param>
/// <param name="state">The parser state.</param>
/// <returns>The formatted error message.</returns>
let formatParserError (error: ParserError) (state: ParserState) =
    let token = getCurrentToken state

    match token with
    | Some { Lexeme = name
             Position = { Line = l } } -> $"Error: {error} at line {l}, token {name}"
    | _ -> $"Error: {error}"
