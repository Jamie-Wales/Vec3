module Vec3.Interpreter.Parser

open Microsoft.FSharp.Core
open Token
open Grammar
open Scanner
open Vec3.Interpreter.Token



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

type ParserLabel = string

type Position =
    { CurrentLine: string
      Line: int
      Column: int
      Token: int }

type ParserState =
    { Tokens: Token list
      Position: Position
      Label: ParserLabel }

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

exception ParserException of ParserError * ParserState

type ParseResult<'a> = Result<ParserState * 'a, ParserError * ParserState>

let createParserState (tokens: Token list) : ParserState =
    { Tokens = tokens
      Position =
        { CurrentLine = ""
          Line = 1
          Column = 1
          Token = 0 }
      Label = "Initial" }

let setLabel (state: ParserState) (label: string) : ParserState = { state with Label = label }

let getCurrentToken (state: ParserState) : Token option =
    List.tryItem state.Position.Token state.Tokens

let peek = getCurrentToken

let advance (state: ParserState) : ParserState =
    { state with
        Position =
            { state.Position with
                Token = state.Position.Token + 1 } }

let rewind (state: ParserState) : ParserState =
    { state with
        Position =
            { state.Position with
                Token = state.Position.Token - 1 } }


let previous (state: ParserState) : Token option =
    List.tryItem (state.Position.Token - 1) state.Tokens

let nextToken (state: ParserState) : (ParserState * Token) option =
    peek state |> Option.map (fun token -> (advance state, token))

type ParseRule =
    { Prefix: (ParserState -> ParseResult<Expr>) option
      Infix: (ParserState -> Expr -> ParseResult<Expr>) option
      Postfix: (ParserState -> Expr -> ParseResult<Expr>) option
      Precedence: Precedence }

let defaultRule =
    { Prefix = None
      Infix = None
      Postfix = None
      Precedence = Precedence.None }

let expect (state: ParserState) (expected: Lexeme) : Result<ParserState, ParserError * ParserState> =
    match nextToken state with
    | Some(state, token) when token.Lexeme = expected -> Ok(state)
    | Some(state, token) -> Error(ExpectedToken(expected, token.Lexeme), state)
    | None -> Error(UnexpectedEndOfInput, state)


let nil (state: ParserState) : ParseResult<Expr> = Ok(state, ELiteral(LUnit, TUnit))

let boolean (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Boolean"

    match previous state with
    | Some { Lexeme = Lexeme.Keyword True } -> Ok(state, ELiteral(LBool true, TBool))
    | Some { Lexeme = Lexeme.Keyword False } -> Ok(state, ELiteral(LBool false, TBool))
    | _ -> Error(Expected("Boolean"), state)

let string (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "String"

    match previous state with
    | Some { Lexeme = Lexeme.String s } -> Ok(state, ELiteral(LString s, TString))
    | _ -> Error(Expected("String"), state)

let number (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Number"

    match previous state with
    | Some { Lexeme = Lexeme.Number n } ->
        match n with
        | Integer i -> Ok(state, ELiteral(LNumber(LInteger(i)), TInteger))
        | Float f -> Ok(state, ELiteral(LNumber(LFloat(f)), TFloat))
        | Rational(n, d) -> Ok(state, ELiteral(LNumber(LRational(n, d)), TRational))
        | Complex(r, i) -> Ok(state, ELiteral(LNumber(LComplex(r, i)), TComplex))
    | _ -> Error(Expected "Number", state)


let rec getRule (lexeme: Lexeme) : ParseRule =
    let getPunctuationRule (punc: Punctuation) =
        match punc with
        | LeftParen ->
            { Prefix = Some leftParen
              Infix = None
              Postfix = Some call
              Precedence = Precedence.Call }
        | LeftBrace ->
            { Prefix = Some leftBrace
              Infix = None
              Postfix = None
              Precedence = Precedence.None }
        | LeftBracket ->
            { Prefix = Some listOrRange
              Infix = None
              Postfix = Some index
              Precedence = Precedence.Index }
        | Colon ->
            {
                Prefix = None
                Infix = Some cast
                Postfix = None
                Precedence = Precedence.Call 
            }
        | _ -> defaultRule


    let getOperatorRule (op: Operator) =
        match op with
        | Operator.Dot ->
            { Prefix = None
              Infix = Some recordSelect
              Postfix = None
              Precedence = Precedence.Index }
        | Operator.Plus
        | Operator.Minus ->
            { Prefix = Some unary
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Term }
        | Operator.Slash
        | Operator.Percent
        | Operator.Cross
        | Operator.DotStar
        | Operator.Star ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Factor }
        | Operator.Caret
        | Operator.StarStar ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Exponent }
        | Operator.BangEqual
        | Operator.EqualEqual
        | Operator.Equal ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Equality }
        | Operator.Greater
        | Operator.GreaterEqual
        | Operator.Less
        | Operator.LessEqual ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Comparison }
        | Operator.Bang ->
            { Prefix = Some unary
              Infix = None
              Postfix = None
              Precedence = Precedence.Unary }
        | Operator.AmpersandAmpersand ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.And }
        | Operator.PipePipe ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Or }
        | Operator.ColonColon ->
            { Prefix = None
              Infix = Some binary
              Postfix = None
              Precedence = Precedence.Assignment }

        | _ -> defaultRule

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

and cast (state: ParserState) (left: Expr) : ParseResult<Expr> =
    typeHint state
    |> Result.bind(fun (state, typ) ->
                let defaultPos = { Line = 0; Column = 0 }
                
                let rec getDefault = function
                                     | TBool -> Some (ELiteral(LBool true, TBool))
                                     | TInteger -> Some (ELiteral(LNumber(LInteger 0), TInteger))
                                     | TFloat -> Some (ELiteral(LNumber(LFloat 0), TFloat))
                                     | TRational -> Some (ELiteral(LNumber(LRational (0, 0)), TRational))
                                     | TComplex -> Some (ELiteral(LNumber(LComplex (0, 0)), TComplex))
                                     | TString -> Some (ELiteral((LString ""), TString))
                                     | TTensor(typ1, _) ->
                                         let def = getDefault typ1
                                         
                                         if Option.isSome def then
                                            Some(EList([Option.get def], None))
                                         else None
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
                    Ok (state, expr)
                | _ -> Ok(state, left)
        
        )

and ident (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Ident"

    match previous state with
    | Some id ->
        let expr = EIdentifier(id, None)
        Ok(state, expr)
    | _ -> Error(Expected("Identifier"), state)

and expression (state: ParserState) (precedence: Precedence) : ParseResult<Expr> =
    let state = setLabel state "Expression"
    
    prefix state |> Result.bind (fun (state, expr) -> infix precedence state expr)


and prefix (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Prefix"

    match peek state with
    | Some token ->
        match (getRule token.Lexeme).Prefix with
        | Some fn -> fn (advance state)
        | None -> Error(ExpectedExpression, state)
    | None -> Error(ExpectedExpression, state)

and infix (precedence: Precedence) (state: ParserState) (expr: Expr) : ParseResult<Expr> =
    let state = setLabel state "Infix"

    match peek state with
    | Some token ->
        let rule = getRule token.Lexeme

        if int precedence <= int rule.Precedence then
            match rule.Infix with
            | Some fn ->
                fn (advance state) expr
                |> Result.bind (fun (state, expr) -> infix precedence state expr)
            | None -> postfix state expr
        else
            postfix state expr
    | None -> postfix state expr

and postfix (state: ParserState) (expr: Expr) : ParseResult<Expr> =
    let state = setLabel state "Postfix"

    match peek state with
    | Some token ->
        match (getRule token.Lexeme).Postfix with
        | Some fn -> fn (advance state) expr |> Result.bind (fun (state, expr) -> postfix state expr)
        | None -> Ok(state, expr)
    | None -> Ok(state, expr)

and recordSelect (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "RecordSelect"

    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) -> Ok(state, ERecordSelect(left, name, None))
    | _ -> Error(Expected("field name"), state)

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

and unary (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Unary"

    match previous state with
    | Some op ->
        let op =
            match op with
            | { Lexeme = Operator(op, _)
                Position = pos } ->
                { Lexeme = Operator(op, Some Prefix)
                  Position = pos }
            | _ -> op

        expression state Precedence.Unary
        |> Result.bind (fun (state, expr) -> Ok(state, ECall(EIdentifier(op, None), [ expr ], None)))
    | _ -> Error(Expected "operator", state)

and leftBrace (state: ParserState) : ParseResult<Expr> =
    // either record, block
    let state = setLabel state "LeftBrace"

    match peek state with
    | Some { Lexeme = Punctuation RightBrace } -> Ok(advance state, ERecordEmpty(TRowEmpty))
    | Some { Lexeme = Identifier _ } ->
        match peek (advance state) with
        | Some { Lexeme = Punctuation Colon } -> record state
        | Some { Lexeme = Operator(Equal, _) } -> record state
        // | Some { Lexeme = Lexeme.Keyword Keyword.With } -> recordUpdate state // TODO, not with identifier, just arb epxpr
        | _ -> block state
    | _ -> block state

and recordFields
    (state: ParserState)
    (fields: (Token * Expr * TType option) list)
    : ParseResult<(Token * Expr * TType option) list> =
    match peek state with
    | Some { Lexeme = Punctuation RightBrace } -> Ok(advance state, List.rev fields)
    | Some { Lexeme = Lexeme.Identifier _ } ->
        match nextToken state with
        | Some(state, name) ->
            match nextToken state with
            | Some(state, { Lexeme = Operator(Equal, _) }) ->
                expression state Precedence.Assignment
                |> Result.bind (fun (state, value) ->
                    match peek state with
                    | Some { Lexeme = Punctuation Comma } ->
                        recordFields (advance state) ((name, value, None) :: fields)
                    | Some { Lexeme = Punctuation RightBrace } ->
                        Ok(advance state, List.rev ((name, value, None) :: fields))
                    | _ -> Error(Expected "',' or '}' after record field.", state))
            | Some(state, { Lexeme = Punctuation Colon }) ->
                typeHint state
                |> Result.bind (fun (state, fieldType) ->
                    match nextToken state with
                    | Some(state, { Lexeme = Operator(Equal, _) }) ->
                        expression state Precedence.Assignment
                        |> Result.bind (fun (state, value) ->
                            match peek state with
                            | Some { Lexeme = Punctuation Comma } ->
                                recordFields (advance state) ((name, value, Some fieldType) :: fields)
                            | Some { Lexeme = Punctuation RightBrace } ->
                                Ok(advance state, List.rev ((name, value, Some fieldType) :: fields))
                            | _ -> Error(Expected "',' or '}' after record field.", state))
                    | _ -> Error(Expected "':' after record field.", state))
            | _ -> Error(Expected "field value", state)
        | _ -> Error(Expected "field name", state)
    | _ -> Error(Expected "field name", state)


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


and commaSeparatedList (state: ParserState) : ParseResult<Expr list> =
    let state = setLabel state "CommaSeparatedList"

    let rec loop (state: ParserState) (exprs: Expr list) : ParseResult<Expr list> =
        expression state Precedence.None
        |> Result.bind (fun (state, expr) -> Ok(state, expr :: exprs))
        |> Result.bind (fun (state, exprs) ->
            match peek state with
            | Some { Lexeme = Punctuation Comma } ->
                let state = advance state
                loop state exprs
            | _ -> Ok(state, List.rev exprs))

    loop state [] |> Result.bind (fun (state, exprs) -> Ok(state, List.rev exprs))

and listOrRange (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "ListOrRange"

    match peek state with
    | Some { Lexeme = Punctuation RightBracket } -> Ok(advance state, EList([], None))
    | _ ->
        expression state Precedence.None
        |> Result.bind (fun (state, start) ->
            match nextToken state with
            | Some(state, { Lexeme = Operator(DotDot, _) }) ->
                expression state Precedence.None
                |> Result.bind (fun (state, end_) ->
                    expect state (Punctuation RightBracket)
                    |> Result.bind (fun state -> Ok(state, ERange(start, end_, None))))
            | Some(state, { Lexeme = Punctuation Comma }) ->
                commaSeparatedList state
                |> Result.bind (fun (state, exprs) ->
                    expect state (Punctuation RightBracket)
                    |> Result.bind (fun state -> Ok(state, EList(start :: exprs, None))))
            | _ -> Error(Expected "',' or '..' after list element.", state))

and grouping (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Grouping"

    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        expect state (Punctuation RightParen)
        |> Result.bind (fun state -> Ok(state, EGrouping(expr, None))))

and index (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Index"

    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        expect state (Punctuation RightBracket)
        |> Result.bind (fun state -> Ok(state, EIndex(left, expr, None))))

and ifElse (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "If"

    expression state Precedence.None
    |> Result.bind (fun (state, condition) ->
        expect state (Lexeme.Keyword Keyword.Then)
        |> Result.bind (fun state ->
            expression state Precedence.None
            |> Result.bind (fun (state, thenBranch) ->
                match nextToken state with
                | Some(state, { Lexeme = Keyword Else }) ->
                    expression state Precedence.None
                    |> Result.bind (fun (state, elseBranch) ->
                        Ok(state, EIf(condition, thenBranch, elseBranch, None)))
                | _ -> Ok(state, EIf(condition, thenBranch, ELiteral(LUnit, TUnit), Some TUnit)))))

and ternary (state: ParserState) (trueBranch: Expr) : ParseResult<Expr> =
    expression state Precedence.None
    |> Result.bind (fun (state, condition) ->
        expect state (Lexeme.Keyword Keyword.Else)
        |> Result.bind (fun state ->
            expression state Precedence.None
            |> Result.bind (fun (state, falseBranch) -> Ok(state, EIf(condition, trueBranch, falseBranch, None)))))

and call (state: ParserState) (callee: Expr) : ParseResult<Expr> =
    let state = setLabel state "Call"

    let rec loop (state: ParserState) (args: Expr list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Punctuation RightParen } ->
            let state = advance state
            Ok(state, ECall(callee, args, None))
        | _ ->
            // replace with comma separated list
            expression state Precedence.None
            |> Result.bind (fun (state, arg) ->
                match nextToken state with
                | Some(state, { Lexeme = Punctuation Comma }) -> loop state (arg :: args)
                | Some(state, { Lexeme = Punctuation RightParen }) ->
                    Ok(state, ECall(callee, List.rev (arg :: args), None))
                | _ -> Error(Expected "argument or ')' after call.", state))

    loop state []

// should clean this
and leftParen (state: ParserState) : ParseResult<Expr> =
    match peek state with
    | Some { Lexeme = Lexeme.Identifier _ } ->
        match peek (advance state) with
        | Some { Lexeme = Punctuation Comma } -> lambdaOrTuple state
        | Some { Lexeme = Punctuation Colon } -> lambda state

        | Some { Lexeme = Punctuation RightParen } ->
            match peek (advance (advance state)) with
            | Some { Lexeme = Operator(Arrow, _) }
            | Some { Lexeme = Punctuation Colon }
            | Some { Lexeme = Punctuation LeftBrace } -> lambda state
            | _ -> grouping state
        | _ -> grouping state
    | Some { Lexeme = Punctuation RightParen } ->
        match peek (advance state) with
        | Some { Lexeme = Operator(Arrow, _) }
        | Some { Lexeme = Punctuation Colon }
        | Some { Lexeme = Punctuation LeftBrace } -> lambda state
        | _ -> Ok(advance state, ELiteral(LUnit, TUnit))
    | _ -> groupingOrTuple state

and parseTuple (state: ParserState) (items: Expr list) : Expr ParseResult =
    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Punctuation Comma } -> parseTuple (advance state) (expr :: items)
        | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, ETuple(List.rev (expr :: items), None))
        | _ -> Error(Expected "',' or ')'.", state))

and lambdaOrTuple (state: ParserState) : Expr ParseResult =
    match peek (advance state) with
    | Some { Lexeme = Punctuation RightParen } -> Ok(advance (advance state), ELiteral(LUnit, TUnit))
    | Some { Lexeme = Punctuation Comma } ->
        let tuple = parseTuple state []

        match tuple with
        | Ok(newState, expr) ->
            match peek newState with
            | Some { Lexeme = Operator(Arrow, _) } -> lambda state
            | Some { Lexeme = Punctuation Colon } -> lambda state
            | Some { Lexeme = Punctuation LeftBrace } -> lambda state
            | _ -> Ok(newState, expr)
        | _ -> lambda state
    | _ -> lambda state

and groupingOrTuple (state: ParserState) : ParseResult<Expr> =
    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Punctuation Comma } -> parseTuple (advance state) [ expr ]
        | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, expr)
        | _ -> Error(Expected "',' or ')'.", state))

// and this
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
                | Some { Lexeme = Punctuation Colon } ->
                    let state = advance state

                    match typeHint state with
                    | Ok(state, paramType) -> Some paramType, state
                    | Error _ -> None, state
                | _ -> None, state

            match peek state with
            | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, (List.rev ((token, paramType) :: params')))
            | Some { Lexeme = Punctuation Comma } -> parseParameters (advance state) ((token, paramType) :: params')
            | _ -> Error(Expected "',' or ')'.", state)
        | _ -> Error(Expected "parameter name.", state)

    let parseBody (state: ParserState) : ParseResult<Expr> =
        match nextToken state with
        | Some(state, { Lexeme = Punctuation LeftBrace }) -> block state
        | Some(state, { Lexeme = Operator(Arrow, _) }) -> expression state Precedence.Assignment
        | _ -> Error(Expected "function body.", state)

    parseParameters state []
    |> Result.bind (fun (state, params') ->
        match nextToken state with
        | Some(state, { Lexeme = Punctuation Colon }) ->
            typeHint state
            |> Result.bind (fun (state, returnType) ->
                parseBody state
                |> Result.bind (fun (state, body) ->
                    Ok(state, ELambda(params', body, Some returnType, false, None))))

        | _ ->
            parseBody state
            |> Result.bind (fun (state, body) ->
                Ok(state, ELambda(params', body, None, false, None))))

and funcType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "FunctionType"

    let rec parseParams (state: ParserState) (paramList: TType list) : ParseResult<TType list> =
        match peek state with
        | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, paramList)
        | _ ->
            typeHint state
            |> Result.bind (fun (state, param) ->
                match peek state with
                | Some { Lexeme = Punctuation Comma } -> parseParams (advance state) (param :: paramList)
                | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, (param :: paramList))
                | _ -> Error(Expected "',' or ')'.", state))

    parseParams state []
    |> Result.bind (fun (state, paramList) ->
        expect state (Punctuation Colon)
        |> Result.bind (fun state ->
            typeHint state
            |> Result.bind (fun (state, returnType) -> Ok(state, TFunction(paramList, returnType, false, false)))))


and tensorType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "TensorType"

    typeHint state
    |> Result.bind (fun (state, innerType) ->
        expect state (Punctuation RightBracket)
        |> Result.bind (fun state -> Ok(state, TTensor(innerType, DAny))))


and recordType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "RecordType"
    // let x: { a: int, b: float } = { a = 1, b = 2.0 }

    let rec parseFields (state: ParserState) (fields: (Token * TType) list) : (Token * TType) list ParseResult =
        match nextToken state with
        | Some(state, { Lexeme = Punctuation RightBrace }) -> Ok(state, fields)
        | Some(state, ({ Lexeme = Identifier _ } as fieldName)) ->
            expect state (Punctuation Colon)
            |> Result.bind (fun state ->
                typeHint state
                |> Result.bind (fun (state, fieldType) ->
                    match nextToken state with
                    | Some(state, { Lexeme = Punctuation Comma }) ->
                        parseFields state ((fieldName, fieldType) :: fields)
                    | Some(state, { Lexeme = Punctuation RightBrace }) ->
                        Ok(state, ((fieldName, fieldType) :: fields))
                    | _ -> Error(Expected "',' or '}' after record field.", state)))
        | _ -> Error(Expected "field name", state)

    let rec fieldsToType (fields: (Token * TType) list) : TType =
        match fields with
        | [] -> TRowEmpty
        | (name, fieldType) :: rest -> TRowExtend(name, fieldType, fieldsToType rest)

    parseFields state []
    |> Result.bind (fun (state, fields) -> Ok(state, TRecord(fieldsToType fields)))



// sumish types ? int | float etc with constrain type
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
        | _ -> Ok(state, TAlias(tok, None))
    | Some(state, { Lexeme = Punctuation LeftParen }) -> funcType state
    | Some(state, { Lexeme = Punctuation LeftBracket }) -> tensorType state
    | Some(state, { Lexeme = Punctuation LeftBrace }) -> recordType state
    | _ -> Error(ExpectedType "type name", state)



and block (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Block"

    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Punctuation RightBrace } -> Ok(advance state, EBlock(List.rev stmts, None))
        | _ -> statement state |> Result.bind (fun (state, stmt) -> loop state (stmt :: stmts))

    loop state []

and varDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Variable"

    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) ->
        let typeResult =
            match peek state with
            | Some { Lexeme = Punctuation Colon } ->
                let state = advance state
                typeHint state |> Result.bind (fun (state, typ) -> Ok(state, Some typ))
            | _ -> Ok(state, None)

        typeResult
        |> Result.bind (fun (state, varType) ->
            expect state (Operator(Equal, None))
            |> Result.bind (fun state ->
                expression state Precedence.Assignment
                |> Result.bind (fun (state, expr) ->
                    if Option.isSome varType then
                        let rec getDefault = function
                                             | TBool -> Some (ELiteral(LBool true, TBool))
                                             | TInteger -> Some (ELiteral(LNumber(LInteger 0), TInteger))
                                             | TFloat -> Some (ELiteral(LNumber(LFloat 0), TFloat))
                                             | TRational -> Some (ELiteral(LNumber(LRational (0, 0)), TRational))
                                             | TComplex -> Some (ELiteral(LNumber(LComplex (0, 0)), TComplex))
                                             | TString -> Some (ELiteral((LString ""), TString))
                                             | TTensor(typ1, _) ->
                                                 let def = getDefault typ1
                                                 
                                                 if Option.isSome def then
                                                    Some(EList([Option.get def], None))
                                                 else None
                                             | _ -> None
                        let defaultPos = { Line = 0; Column = 0 }
                        // clean this up too much repititon
                        let id = getDefault <| Option.get varType
                        
                        match id with
                        | Some id ->
                            let expr =
                                ECall(
                                    EIdentifier(
                                        { Lexeme = Identifier "cast"
                                          Position = defaultPos },
                                        None
                                    ),
                                    [ expr; id ],
                                    None
                                )

                            Ok(state, SVariableDeclaration(name, expr, varType))
                        | None ->
                            Ok(state, SVariableDeclaration(name, expr, varType))
                    else
                        Ok(state, SVariableDeclaration(name, expr, varType)))))
    | _ -> Error(Expected "variable name", state)


and assertStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Assert"

    expression state Precedence.Assignment
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Punctuation Comma } ->
            expression (advance state) Precedence.Assignment
            |> Result.bind (fun (state, message) -> Ok(state, SAssertStatement(expr, Some message, Some TUnit)))
        | _ -> Ok(state, SAssertStatement(expr, None, Some TUnit)))

and typeDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Type"

    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) ->
        expect state (Operator(Equal, None))
        |> Result.bind (fun state ->
            typeHint state
            |> Result.bind (fun (state, typ) -> Ok(state, STypeDeclaration(name, typ, None))))
    | _ -> Error(Expected "type name", state)

and recFunc (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Rec Func"
    
    let rec parseParameters
        (state: ParserState)
        (params': (Token * TType option) list)
        : ParseResult<(Token * TType option) list> =
        match nextToken state with
        | Some(state, { Lexeme = Punctuation RightParen }) -> Ok(state, List.rev params')
        | Some(state, ({ Lexeme = Identifier _ } as token)) ->
            let paramType, state =
                match peek state with
                | Some { Lexeme = Punctuation Colon } ->
                    let state = advance state

                    match typeHint state with
                    | Ok(state, paramType) -> Some paramType, state
                    | Error _ -> None, state
                | _ -> None, state

            match peek state with
            | Some { Lexeme = Punctuation RightParen } -> Ok(advance state, (List.rev ((token, paramType) :: params')))
            | Some { Lexeme = Punctuation Comma } -> parseParameters (advance state) ((token, paramType) :: params')
            | _ -> Error(Expected "',' or ')'.", state)
        | _ -> Error(Expected "parameter name.", state)
    
    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) ->
        expect state (Punctuation LeftParen)
        |> Result.bind(fun state ->
            parseParameters state []
            |> Result.bind (fun (state, parameters) ->
                expect state (Operator (Equal, None))
                |> Result.bind(fun state ->
                    expression state Precedence.Assignment
                    |> Result.bind(fun (state, expr) ->
                        let f = SRecFunc(name, parameters, expr, None)
                        Ok(state, f)
                    )
                )
            ))
    | _ -> Error(Expected "Identifier", state)

and statement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Statement"

    match peek state with
    | Some token ->
        match token.Lexeme with
        | Keyword kw ->
            match kw with
            | Keyword.Let -> varDecl (advance state)
            | Keyword.Assert -> assertStatement (advance state)
            | Keyword.Type -> typeDecl (advance state)
            | Keyword.Rec -> recFunc (advance state)
            | _ ->
                expression state Precedence.None
                |> Result.bind (fun (state, expr) -> Ok(state, SExpression(expr, None)))
        | _ ->
            expression state Precedence.None
            |> Result.bind (fun (state, expr) -> Ok(state, SExpression(expr, None)))
    | None -> Ok((state, SExpression(ELiteral(LUnit, TUnit), None)))

let statementUnsafe (input: string) : Stmt =
    let tokens = tokenize input

    match tokens with
    | Ok tokens ->
        let initialState = createParserState tokens

        match statement initialState with
        | Ok(_, stmt) -> stmt
        | Error f -> raise (ParserException(f))
    | Error f -> raise (LexerException(f))

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

let parseProgram (state: ParserState) : ParseResult<Program> =
    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Program> =
        match peek state with
        | Some _ -> statement state |> Result.bind (fun (state, stmt) -> loop state (stmt :: stmts))
        | None -> Ok(state, List.rev stmts)

    loop state []

let parseTokens (tokens: Token list) : Program ParseResult =
    let initialState = createParserState tokens
    parseProgram initialState

let parse (input: string) =
    let tokens = tokenize input

    match tokens with
    | Ok tokens ->
        parseTokens tokens
    | Error f -> Error(LexerError f, createParserState [])

let parseFile (file: string) =
    let input = System.IO.File.ReadAllText(file)
    parse input

let formatParserError (error: ParserError) (state: ParserState) =
    let token = getCurrentToken state

    match token with
    | Some { Lexeme = name
             Position = { Line = l } } -> $"Error: {error} at line {l}, token {name}"
    | _ -> $"Error: {error}"
