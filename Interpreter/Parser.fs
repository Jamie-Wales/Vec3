module Vec3.Interpreter.Parser

open Microsoft.FSharp.Core
open Token
open Grammar
open Scanner



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

type ParserPosition =
    { CurrentLine: string
      Line: int
      Column: int }

type ParserState =
    { Tokens: Token list
      Position: int
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
      Position = 0
      Label = "Initial" }

let setLabel (state: ParserState) (label: string) : ParserState = { state with Label = label }

let getCurrentToken (state: ParserState) : Token option =
    List.tryItem state.Position state.Tokens

let peek = getCurrentToken

let advance (state: ParserState) : ParserState =
    { state with
        Position = state.Position + 1 }

let rewind (state: ParserState) : ParserState =
    { state with
        Position = state.Position - 1 }

let previous (state: ParserState) : Token option =
    List.tryItem (state.Position - 1) state.Tokens

let nextToken (state: ParserState) : (ParserState * Token) option =
    peek state |> Option.map (fun token -> (advance state, token))

type ParseRule =
    { Prefix: (ParserState -> ParseResult<Expr>) option
      Infix: (ParserState -> Expr -> ParseResult<Expr>) option
      Precedence: Precedence }

let expect (state: ParserState) (expected: Lexeme) : Result<ParserState, ParserError * ParserState> =
    match nextToken state with
    | Some(state, token) when token.Lexeme = expected -> Ok(state)
    | Some(state, token) -> Error(ExpectedToken(expected, token.Lexeme), state)
    | None -> Error(UnexpectedEndOfInput, state)

// sort of like combinators, maybe move to monadic approach to avoid nesting
// lots of nested maps -> make the result a functor ? or at least extract out common patterns

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

let ident (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Ident"

    match previous state with
    | Some id -> Ok(state, EIdentifier(id, TInfer))
    | _ -> Error(Expected("Identifier"), state)

let rec getRule (lexeme: Lexeme) : ParseRule =
    let getOperatorRule (op: Operator) =
        match op with
        | Operator.LeftParen ->
            { Prefix = Some leftParen
              Infix = Some call
              Precedence = Precedence.Call }
        | Operator.LeftBrace ->
            { Prefix = Some leftBrace
              Infix = None
              Precedence = Precedence.None }
        | Operator.Dot ->
            { Prefix = None
              Infix = Some recordSelect
              Precedence = Precedence.Index }
        | Operator.LeftBracket ->
            { Prefix = Some listOrRange
              Infix = Some index
              Precedence = Precedence.Index }
        | Operator.Plus
        | Operator.Minus ->
            { Prefix = Some unary
              Infix = Some binary
              Precedence = Precedence.Term }
        | Operator.Slash
        | Operator.Percent
        | Operator.Cross
        | Operator.DotStar
        | Operator.Star ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Factor }
        | Operator.Caret
        | Operator.StarStar ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Exponent }
        | Operator.BangEqual
        | Operator.EqualEqual
        | Operator.Equal ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Equality }
        | Operator.Greater
        | Operator.GreaterEqual
        | Operator.Less
        | Operator.LessEqual ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Comparison }
        | Operator.Bang ->
            { Prefix = Some unary
              Infix = None
              Precedence = Precedence.None }
        | Operator.AmpersandAmpersand ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.And }
        | Operator.PipePipe ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Or }
        | Operator.ColonColon ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Assignment }

        | _ ->
            { Prefix = None
              Infix = None
              Precedence = Precedence.None }

    let getKeywordRule (kw: Keyword) =
        match kw with
        | True
        | False ->
            { Prefix = Some boolean
              Infix = None
              Precedence = Precedence.None }
        | Nil ->
            { Prefix = Some nil
              Infix = None
              Precedence = Precedence.None }
        | If ->
            { Prefix = Some ifElse
              Infix = Some ternary
              Precedence = Precedence.None }
        | In ->
            { Prefix = None
              Infix = None // todo, infix for list checking
              Precedence = Precedence.None }
        | And ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.And }
        | Or ->
            { Prefix = None
              Infix = Some binary
              Precedence = Precedence.Or }
        | _ ->
            { Prefix = None
              Infix = None
              Precedence = Precedence.None }

    match lexeme with
    | Operator op -> getOperatorRule op
    | Lexeme.Keyword kw -> getKeywordRule kw

    | Lexeme.Number _ ->
        { Prefix = Some number
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.String _ ->
        { Prefix = Some string
          Infix = None
          Precedence = Precedence.None }
    | Lexeme.Identifier _ ->
        { Prefix = Some ident
          Infix = None
          Precedence = Precedence.None }


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

and infix (precedence: Precedence) (state: ParserState) (leftExpr: Expr) : ParseResult<Expr> =
    let state = setLabel state "Infix"

    let rec loop (state: ParserState) (expr: Expr) : ParseResult<Expr> =
        match peek state with
        | Some token ->
            let rule = getRule token.Lexeme

            if int precedence <= int rule.Precedence then
                match rule.Infix with
                | Some fn -> fn (advance state) expr |> Result.bind (fun (state, expr) -> loop state expr)
                | None -> Ok(state, expr)
            else
                Ok(state, expr)
        | None -> Ok(state, expr)

    loop state leftExpr

and recordSelect (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "RecordSelect"

    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) -> Ok(state, ERecordSelect(left, name, TInfer))
    | _ -> Error(Expected("field name"), state)

and binary (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Binary"

    match previous state with
    | Some op ->
        let rule = getRule op.Lexeme
        let nextPrecedence: Precedence = enum (int rule.Precedence + 1)

        expression state nextPrecedence
        |> Result.bind (fun (state, right) -> Ok(state, EBinary(left, op, right, TInfer)))
    | _ -> Error(Expected "operator", state)

and unary (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Unary"

    match previous state with
    | Some op ->
        expression state Precedence.Unary
        |> Result.bind (fun (state, expr) -> Ok(state, EUnary(op, expr, TInfer)))
    | _ -> Error(Expected "operator", state)

and leftBrace (state: ParserState) : ParseResult<Expr> =
    // either record, block
    let state = setLabel state "LeftBrace"

    match peek state with
    | Some { Lexeme = Lexeme.Operator Operator.RightBrace } -> Ok(advance state, ERecordEmpty(TRowEmpty))
    | Some { Lexeme = Identifier _ } ->
        match peek (advance state) with
        | Some { Lexeme = Operator Colon } -> record state
        | Some { Lexeme = Lexeme.Operator Operator.Equal } -> record state
        // | Some { Lexeme = Lexeme.Keyword Keyword.With } -> recordUpdate state // TODO, not with identifier, just arb epxpr
        | _ -> block state
    | _ -> block state

// and recordUpdate (state: ParserState) : Expr ParseResult =
//     expression state Precedence.None
//     |> Result.bind (fun (state, expr) ->
//         expect state (Keyword With)
//         |> Result.bind (fun state ->
//             expect state (Operator LeftBrace)
//             |> Result.bind (fun state ->
//                 recordFields state []
//                 |> Result.bind (fun (state, fields) ->
//                     expect state (Lexeme.Operator Operator.RightBrace)
//                     |> Result.bind (fun state -> Ok(state, ERecordUpdate(expr, fields, TInfer)))))))

and recordFields (state: ParserState) (fields: (Token * Expr * TType) list) : ParseResult<(Token * Expr * TType) list> =
    match peek state with
    | Some { Lexeme = Lexeme.Operator Operator.RightBrace } -> Ok(advance state, List.rev fields)
    | Some { Lexeme = Lexeme.Identifier _ } ->
        match nextToken state with
        | Some(state, name) ->
            match nextToken state with
            | Some(state, { Lexeme = Operator Equal }) ->
                expression state Precedence.Assignment
                |> Result.bind (fun (state, value) ->
                    match peek state with
                    | Some { Lexeme = Operator Comma } ->
                        recordFields (advance state) ((name, value, TInfer) :: fields)
                    | Some { Lexeme = Lexeme.Operator Operator.RightBrace } ->
                        Ok(advance state, List.rev ((name, value, TInfer) :: fields))
                    | _ -> Error(Expected "',' or '}' after record field.", state))
            | Some(state, { Lexeme = Operator Colon }) ->
                typeHint state
                |> Result.bind (fun (state, fieldType) ->
                    match nextToken state with
                    | Some(state, { Lexeme = Operator Equal }) ->
                        expression state Precedence.Assignment
                        |> Result.bind (fun (state, value) ->
                            match peek state with
                            | Some { Lexeme = Operator Comma } ->
                                recordFields (advance state) ((name, value, fieldType) :: fields)
                            | Some { Lexeme = Lexeme.Operator Operator.RightBrace } ->
                                Ok(advance state, List.rev ((name, value, fieldType) :: fields))
                            | _ -> Error(Expected "',' or '}' after record field.", state))
                    | _ -> Error(Expected "':' after record field.", state))
            | _ -> Error(Expected "field value", state)
        | _ -> Error(Expected "field name", state)
    | _ -> Error(Expected "field name", state)


and record (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Record"

    let rowsToExpr (rows: (Token * Expr * TType) list) : Expr =
        let rec loop (rows: (Token * Expr * TType) list) (record: Expr) : Expr =
            match rows with
            | [] -> record
            | (name, value, typ) :: rest -> loop rest (ERecordExtend((name, value, typ), record, TInfer))

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
            | Some { Lexeme = Operator Comma } ->
                let state = advance state
                loop state exprs
            | _ -> Ok(state, List.rev exprs))

    loop state [] |> Result.bind (fun (state, exprs) -> Ok(state, List.rev exprs))

and listOrRange (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "ListOrRange"

    match peek state with
    | Some { Lexeme = Lexeme.Operator Operator.RightBracket } -> Ok(advance state, EList([], TInfer))
    | _ ->
        expression state Precedence.None
        |> Result.bind (fun (state, start) ->
            match nextToken state with
            | Some (state, { Lexeme = Operator DotDot }) ->
                expression state Precedence.None
                |> Result.bind (fun (state, end_) ->
                    expect state (Lexeme.Operator Operator.RightBracket)
                    |> Result.bind (fun state -> Ok(state, ERange(start, end_, TInfer)))
                )
            | Some (state, { Lexeme = Operator Comma }) ->
                commaSeparatedList state
                |> Result.bind (fun (state, exprs) ->
                    expect state (Lexeme.Operator Operator.RightBracket)
                    |> Result.bind (fun state -> Ok(state, EList(start :: exprs, TInfer)))
                )
            | _ -> Error(Expected "',' or '..' after list element.", state))

and grouping (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Grouping"

    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        expect state (Lexeme.Operator Operator.RightParen)
        |> Result.bind (fun state -> Ok(state, EGrouping(expr, TInfer))))

and index (state: ParserState) (left: Expr) : ParseResult<Expr> =
    let state = setLabel state "Index"

    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        expect state (Lexeme.Operator Operator.RightBracket)
        |> Result.bind (fun state -> Ok(state, EIndex(left, expr, TInfer))))

and ifElse (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "If"

    expression state Precedence.None
    |> Result.bind (fun (state, condition) ->
        expect state (Lexeme.Keyword Keyword.Then)
        |> Result.bind (fun state ->
            expression state Precedence.None
            |> Result.bind (fun (state, thenBranch) ->
                match nextToken state with
                | Some(state, { Lexeme = Lexeme.Keyword Keyword.Else }) ->
                    expression state Precedence.None
                    |> Result.bind (fun (state, elseBranch) ->
                        Ok(state, EIf(condition, thenBranch, elseBranch, TInfer)))
                | _ -> Ok(state, EIf(condition, thenBranch, ELiteral(LUnit, TUnit), TUnit)))))

and ternary (state: ParserState) (trueBranch: Expr) : ParseResult<Expr> =
    expression state Precedence.None
    |> Result.bind (fun (state, condition) ->
        expect state (Lexeme.Keyword Keyword.Else)
        |> Result.bind (fun state ->
            expression state Precedence.None
            |> Result.bind (fun (state, falseBranch) -> Ok(state, EIf(condition, trueBranch, falseBranch, TInfer)))))

and call (state: ParserState) (callee: Expr) : ParseResult<Expr> =
    let state = setLabel state "Call"

    let rec loop (state: ParserState) (args: Expr list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
            let state = advance state
            Ok(state, ECall(callee, args, TInfer))
        | _ ->
            // replace with comma separated list
            expression state Precedence.None
            |> Result.bind (fun (state, arg) ->
                match nextToken state with
                | Some(state, { Lexeme = Operator Comma }) -> loop state (arg :: args)
                | Some(state, { Lexeme = Operator RightParen }) ->
                    Ok(state, ECall(callee, List.rev (arg :: args), TInfer))
                | _ -> Error(Expected "argument or ')' after call.", state))

    loop state []

// should clean this
and leftParen (state: ParserState) : ParseResult<Expr> =
    match peek state with
    | Some { Lexeme = Lexeme.Identifier _ } ->
        match peek (advance state) with
        | Some { Lexeme = Operator Comma } -> lambdaOrTuple state
        | Some { Lexeme = Operator Colon } -> lambda state

        | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
            match peek (advance (advance state)) with
            | Some { Lexeme = Operator Arrow }
            | Some { Lexeme = Operator Colon }
            | Some { Lexeme = Operator LeftBrace } -> lambda state
            | _ -> grouping state
        | _ -> grouping state
    | Some { Lexeme = Lexeme.Operator Operator.RightParen } ->
        match peek (advance state) with
        | Some { Lexeme = Operator Arrow }
        | Some { Lexeme = Operator Colon }
        | Some { Lexeme = Operator LeftBrace } -> lambda state
        | _ -> Ok(advance state, ELiteral(LUnit, TUnit))
    | _ -> groupingOrTuple state

and parseTuple (state: ParserState) (items: Expr list) : Expr ParseResult =
    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Operator Comma } -> parseTuple (advance state) (expr :: items)
        | Some { Lexeme = Operator RightParen } -> Ok(advance state, ETuple(List.rev (expr :: items), TInfer))
        | _ -> Error(Expected "',' or ')'.", state))

and lambdaOrTuple (state: ParserState) : Expr ParseResult =
    match peek (advance state) with
    | Some { Lexeme = Operator RightParen } -> Ok(advance (advance state), ELiteral(LUnit, TUnit))
    | Some { Lexeme = Operator Comma } ->
        let tuple = parseTuple state []

        match tuple with
        | Ok(newState, expr) ->
            match peek newState with
            | Some { Lexeme = Operator Arrow } -> lambda state
            | Some { Lexeme = Operator Colon } -> lambda state
            | Some { Lexeme = Operator LeftBrace } -> lambda state
            | _ -> Ok(newState, expr)
        | _ -> lambda state
    | _ -> lambda state

and groupingOrTuple (state: ParserState) : ParseResult<Expr> =
    expression state Precedence.None
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Operator Comma } -> parseTuple (advance state) [ expr ]
        | Some { Lexeme = Operator RightParen } -> Ok(advance state, expr)
        | _ -> Error(Expected "',' or ')'.", state))

// and this
and lambda (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Function"

    let rec parseParameters (state: ParserState) (params': (Token * TType) list) : ParseResult<(Token * TType) list> =
        match nextToken state with
        | Some(state, { Lexeme = Operator RightParen }) -> Ok(state, List.rev params')
        | Some(state, ({ Lexeme = Identifier _ } as token)) ->
            let paramType, state =
                match peek state with
                | Some { Lexeme = Operator Colon } ->
                    let state = advance state

                    match typeHint state with
                    | Ok(paramType, state) -> state, paramType
                    | Error _ -> TInfer, state
                | _ -> TInfer, state

            match peek state with
            | Some { Lexeme = Operator RightParen } -> Ok(advance state, (List.rev ((token, paramType) :: params')))
            | Some { Lexeme = Operator Comma } -> parseParameters (advance state) ((token, paramType) :: params')
            | _ -> Error(Expected "',' or ')'.", state)
        | _ -> Error(Expected "parameter name.", state)

    let parseBody (state: ParserState) : ParseResult<Expr> =
        match nextToken state with
        | Some(state, { Lexeme = Operator LeftBrace }) -> block state
        | Some(state, { Lexeme = Operator Arrow }) -> expression state Precedence.Assignment
        | _ -> Error(Expected "function body.", state)

    parseParameters state []
    |> Result.bind (fun (state, params') ->
        match nextToken state with
        | Some(state, { Lexeme = Operator Colon }) ->
            typeHint state
            |> Result.bind (fun (state, returnType) ->
                parseBody state
                |> Result.bind (fun (state, body) ->
                    let paramTypes = List.map snd params'
                    let paramNames = List.map fst params'
                    Ok(state, ELambda(paramNames, body, TFunction(paramTypes, returnType)))))

        | _ ->
            parseBody state
            |> Result.bind (fun (state, body) ->
                let paramTypes = List.map snd params'
                let paramNames = List.map fst params'
                Ok(state, ELambda(paramNames, body, TFunction(paramTypes, TInfer)))))

and funcType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "FunctionType"

    let rec parseParams (state: ParserState) (paramList: TType list) : ParseResult<TType list> =
        match peek state with
        | Some { Lexeme = Operator RightParen } -> Ok(advance state, paramList)
        | _ ->
            typeHint state
            |> Result.bind (fun (state, param) ->
                match peek state with
                | Some { Lexeme = Operator Comma } -> parseParams (advance state) (param :: paramList)
                | Some { Lexeme = Operator RightParen } -> Ok(advance state, (param :: paramList))
                | _ -> Error(Expected "',' or ')'.", state))

    parseParams state []
    |> Result.bind (fun (state, paramList) ->
        expect state (Operator Colon)
        |> Result.bind (fun state ->
            typeHint state
            |> Result.bind (fun (state, returnType) -> Ok(state, TFunction(paramList, returnType)))))


and tensorType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "TensorType"

    typeHint state
    |> Result.bind (fun (state, innerType) ->
        expect state (Lexeme.Operator Operator.RightBracket)
        |> Result.bind (fun state -> Ok(state, TTensor(innerType, DAny))))
    
    
and recordType (state: ParserState) : ParseResult<TType> =
    let state = setLabel state "RecordType"
    // let x: { a: int, b: float } = { a = 1, b = 2.0 }

    let rec parseFields (state: ParserState) (fields: (Token * TType) list) : (Token * TType) list ParseResult =
        match nextToken state with
        | Some(state, { Lexeme = Lexeme.Operator Operator.RightBrace }) -> Ok(state, fields)
        | Some(state, ({ Lexeme = Lexeme.Identifier _ } as fieldName)) ->
            expect state (Operator Colon)
            |> Result.bind (fun state ->
                typeHint state
                |> Result.bind (fun (state, fieldType) ->
                    match nextToken state with
                    | Some(state, { Lexeme = Operator Comma }) -> parseFields state ((fieldName, fieldType) :: fields)
                    | Some(state, { Lexeme = Operator RightBrace }) -> Ok(state, ((fieldName, fieldType) :: fields))
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
        | _ -> Ok(state, TAlias(tok, TInfer))
    | Some(state, { Lexeme = Lexeme.Operator Operator.LeftParen }) -> funcType state
    | Some(state, { Lexeme = Lexeme.Operator Operator.LeftBracket }) -> tensorType state
    | Some(state, { Lexeme = Lexeme.Operator Operator.LeftBrace }) -> recordType state
    | _ -> Error(ExpectedType "type name", state)



and block (state: ParserState) : ParseResult<Expr> =
    let state = setLabel state "Block"

    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Expr> =
        match peek state with
        | Some { Lexeme = Lexeme.Operator Operator.RightBrace } -> Ok(advance state, EBlock(List.rev stmts, TInfer))
        | _ -> statement state |> Result.bind (fun (state, stmt) -> loop state (stmt :: stmts))

    loop state []

and varDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Variable"

    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) ->
        let typeResult =
            match peek state with
            | Some { Lexeme = Operator Colon } ->
                let state = advance state
                typeHint state
            | _ -> Ok(state, TInfer)

        typeResult
        |> Result.bind (fun (state, varType) ->
            expect state (Operator Equal)
            |> Result.bind (fun state ->
                expression state Precedence.Assignment
                |> Result.bind (fun (state, expr) -> Ok(state, SVariableDeclaration(name, expr, varType)))))
    | _ -> Error(Expected "variable name", state)

and printStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Print"

    expression state Precedence.Assignment
    |> Result.bind (fun (state, expr) -> Ok(state, SPrintStatement(expr, TUnit)))

and assertStatement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Assert"

    expression state Precedence.Assignment
    |> Result.bind (fun (state, expr) ->
        match peek state with
        | Some { Lexeme = Operator Comma } ->
            expression (advance state) Precedence.Assignment
            |> Result.bind (fun (state, message) -> Ok(state, SAssertStatement(expr, Some message, TUnit)))
        | _ -> Ok(state, SAssertStatement(expr, None, TUnit)))

and typeDecl (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Type"

    match nextToken state with
    | Some(state, ({ Lexeme = Identifier _ } as name)) ->
        expect state (Operator Equal)
        |> Result.bind (fun state ->
            typeHint state
            |> Result.bind (fun (state, typ) -> Ok(state, STypeDeclaration(name, typ, TInfer)))
        )
    | _ -> Error(Expected "type name", state)

and statement (state: ParserState) : ParseResult<Stmt> =
    let state = setLabel state "Statement"

    match peek state with
    | Some token ->
        match token.Lexeme with
        | Keyword kw ->
            match kw with
            | Keyword.Let -> varDecl (advance state)
            | Keyword.Print -> printStatement (advance state)
            | Keyword.Assert -> assertStatement (advance state)
            | Keyword.Type -> typeDecl (advance state)
            | _ ->
                expression state Precedence.None
                |> Result.bind (fun (state, expr) -> Ok(state, SExpression(expr, TInfer)))
        | _ ->
            expression state Precedence.None
            |> Result.bind (fun (state, expr) -> Ok(state, SExpression(expr, TInfer)))
    | None -> Ok((state, SExpression(ELiteral(LUnit, TUnit), TInfer)))

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
    | Error f -> raise (LexerException(f))

let parseProgram (state: ParserState) : ParseResult<Program> =
    let rec loop (state: ParserState) (stmts: Stmt list) : ParseResult<Program> =
        match peek state with
        | Some _ -> statement state |> Result.bind (fun (state, stmt) -> loop state (stmt :: stmts))
        | None -> Ok(state, List.rev stmts)

    loop state []

let parseTokens (tokens: Token list) =
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
