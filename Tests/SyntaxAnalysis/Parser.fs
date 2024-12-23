module Tests.Parser

open NUnit.Framework
open Vec3.Interpreter.Grammar
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Token

[<TestFixture>]
type ParserTests() =

    [<Test>]
    member _.Should_ParseEmptyInput() =
        let result = parse ""

        match result with
        | Ok(_, statements) -> Assert.That(statements, Is.Empty, "Expected no statements for empty input.")
        | Error _ -> Assert.Fail "Expected no errors for empty input."

    [<Test>]
    member _.Should_ParseSingleNumber() =
        let result = parse "42"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for single number.")

            match statements.Head with
            | SExpression(ELiteral(LNumber(LInteger 42), _), _) -> Assert.Pass()
            | _ -> Assert.Fail "Expected LInteger statement."
        | Error _ -> Assert.Fail "Expected no errors for valid number."

    [<Test>]
    member _.Should_ParseSimpleExpression() =
        let result = parse "3 + 4"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for simple expression.")

            match statements.Head with
            | SExpression(ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                [ ELiteral(LNumber(LInteger 3), _); ELiteral(LNumber(LInteger 4), _) ],
                                _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail "Expected Plus statement."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_HandleUnterminatedString() =
        let result = parse "\"Unfinished string"

        match result with
        | Ok _ -> Assert.Fail "Expected an error for unterminated string."
        | Error _ -> Assert.Pass()

    [<Test>]
    member _.Should_HandlePrecendence() =
        let result = parse "3 + 4 * 5"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for precedence expression.")

            match statements.Head with
            | SExpression(ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                [ ELiteral(LNumber(LInteger 3), _)
                                  ECall(EIdentifier({ Lexeme = Operator(Star, _) }, _),
                                        [ ELiteral(LNumber(LInteger 4), _); ELiteral(LNumber(LInteger 5), _) ],
                                        _) ],
                                _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail "Expected precedence statement."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_HandleGrouping() =
        let result = parse "(3 + 4) * 5"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for grouping expression.")

            match statements.Head with
            | SExpression(ECall(EIdentifier({ Lexeme = Operator(Star, _) }, _),
                                [ ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                        [ ELiteral(LNumber(LInteger 3), _); ELiteral(LNumber(LInteger 4), _) ],
                                        _)
                                  ELiteral(LNumber(LInteger 5), _) ],
                                _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected grouping statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_LeftAssociate() =
        let result = parse "3 + 4 + 5"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for left associativity expression.")

            match statements.Head with
            | SExpression(ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                [ ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                        [ ELiteral(LNumber(LInteger 3), _); ELiteral(LNumber(LInteger 4), _) ],
                                        _)
                                  ELiteral(LNumber(LInteger 5), _) ],
                                _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected left associativity statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseFunctionCall() =
        let result = parse "foo(3, 4)"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for function call.")

            match statements.Head with
            | SExpression(ECall(EIdentifier({ Lexeme = Identifier "foo" }, _),
                                [ ELiteral(LNumber(LInteger 3), _); ELiteral(LNumber(LInteger 4), _) ],
                                _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected function call statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseVariableDeclaration() =
        let result = parse "let foo = 42"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for variable declaration.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" }, ELiteral(LNumber(LInteger 42), _), _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected variable declaration statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseLambda() =
        let result = parse "let foo = (x) -> x + 1"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for lambda.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ELambda([ ({ Lexeme = Identifier "x" }, _) ],
                                           ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                                 [ EIdentifier({ Lexeme = Identifier "x" }, _)
                                                   ELiteral(LNumber(LInteger 1), _) ],
                                                 _),
                                           _,
                                           _,
                                           _,
                                           _),
                                   _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected lambda statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseIf() =
        let result = parse "if true then 1 else 0"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for if.")

            match statements.Head with
            | SExpression(EIf(ELiteral(LBool true, _),
                              ELiteral(LNumber(LInteger 1), _),
                              ELiteral(LNumber(LInteger 0), _),
                              _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected if statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseIfElseIf() =
        let result = parse "if true then 1 else if false then 2 else 0"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for if else if.")

            match statements.Head with
            | SExpression(EIf(ELiteral(LBool true, _),
                              ELiteral(LNumber(LInteger 1), _),
                              EIf(ELiteral(LBool false, _),
                                  ELiteral(LNumber(LInteger 2), _),
                                  ELiteral(LNumber(LInteger 0), _),
                                  _),
                              _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected if else if statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseIfNoElse() =
        let result = parse "if true then 1"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for if no else.")

            match statements.Head with
            | SExpression(EIf(ELiteral(LBool true, _), ELiteral(LNumber(LInteger 1), _), ELiteral(LUnit, _), _), _) ->
                Assert.Pass()
            | _ -> Assert.Fail $"Expected if no else statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseUnit() =
        let result = parse "()"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for unit.")

            match statements.Head with
            | SExpression(ELiteral(LUnit, _), _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected unit statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseTuple() =
        let result = parse "(1, 2)"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for tuple.")

            match statements.Head with
            | SExpression(ETuple([ ELiteral(LNumber(LInteger 1), _); ELiteral(LNumber(LInteger 2), _) ], _), _) ->
                Assert.Pass()
            | _ -> Assert.Fail $"Expected tuple statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseTupleWithUnit() =
        let result = parse "(1, ())"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for tuple with unit.")

            match statements.Head with
            | SExpression(ETuple([ ELiteral(LNumber(LInteger 1), _); ELiteral(LUnit, _) ], _), _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected tuple with unit statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseList() =
        let result = parse "[1, 2]"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for list.")

            match statements.Head with
            | SExpression(EList([ ELiteral(LNumber(LInteger 1), _); ELiteral(LNumber(LInteger 2), _) ], _), _) ->
                Assert.Pass()
            | _ -> Assert.Fail $"Expected list statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseRange() =
        let result = parse "[1..2]"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for range.")

            match statements.Head with
            | SExpression(ERange(ELiteral(LNumber(LInteger 1), _), ELiteral(LNumber(LInteger 2), _), _), _) ->
                Assert.Pass()
            | _ -> Assert.Fail $"Expected range statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseIndex() =
        let result = parse "foo[1]"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for index.")

            match statements.Head with
            | SExpression(EIndex(EIdentifier({ Lexeme = Identifier "foo" }, _), ELiteral(LNumber(LInteger 1), _), _), _) ->
                Assert.Pass()
            | _ -> Assert.Fail $"Expected index statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseIndexRange() =
        let result = parse "foo[1..2]"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for index range.")

            match statements.Head with
            | SExpression(EIndexRange(EIdentifier({ Lexeme = Identifier "foo" }, _),
                                      ELiteral(LNumber(LInteger 1), _),
                                      ELiteral(LNumber(LInteger 2), _),
                                      _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected index range statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseIndexRangeNoStart() =
        let result = parse "foo[..2]"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for index range no start.")

            match statements.Head with
            | SExpression(EIndexRange(EIdentifier({ Lexeme = Identifier "foo" }, _),
                                      ELiteral(LNumber(LInteger 0), _),
                                      ELiteral(LNumber(LInteger 2), _),
                                      _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected index range no start statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseIndexRangeNoEnd() =
        let result = parse "foo[1..]"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for index range no end.")

            match statements.Head with
            | SExpression(EIndexRange(EIdentifier({ Lexeme = Identifier "foo" }, _),
                                      ELiteral(LNumber(LInteger 1), _),
                                      ELiteral(LNumber(LInteger 0), _),
                                      _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected index range no end statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseOperatorDefinitions() =
        let result = parse "let (?) = (x, y) -> x + y"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for operator definition.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Operator(Custom "?", _) },
                                   ELambda([ ({ Lexeme = Identifier "x" }, _); ({ Lexeme = Identifier "y" }, _) ],
                                           ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                                 [ EIdentifier({ Lexeme = Identifier "x" }, _)
                                                   EIdentifier({ Lexeme = Identifier "y" }, _) ],
                                                 _),
                                           _,
                                           _,
                                           _,
                                           _),
                                   _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected operator definition statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseInfixOperator() =
        let result = parse "3 ? 4"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for infix operator.")

            match statements.Head with
            | SExpression(ECall(EIdentifier({ Lexeme = Operator(Custom "?", Some Infix) }, _),
                                [ ELiteral(LNumber(LInteger 3), _); ELiteral(LNumber(LInteger 4), _) ],
                                _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected infix operator statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParsePrefixOperator() =
        let result = parse "?3"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for prefix operator.")

            match statements.Head with
            | SExpression(ECall(EIdentifier({ Lexeme = Operator(Custom "?", Some Prefix) }, _),
                                [ ELiteral(LNumber(LInteger 3), _) ],
                                _),
                          _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected prefix operator statement, got {statements.Head}."

        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseVariableType() =
        let result = parse "let foo: int = 42"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for variable type.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ECall(EIdentifier({ Lexeme = Identifier "cast" }, _),
                                         [ ELiteral(LNumber(LInteger 42), _); ELiteral(LNumber(LInteger 0), _) ],
                                         _),
                                   Some TInteger) -> Assert.Pass()


            | _ -> Assert.Fail $"Expected variable type statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseFunctionType() =
        let result = parse "let foo: (int) -> int = (x) -> x"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for function type.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ELambda([ ({ Lexeme = Identifier "x" }, _) ],
                                           EIdentifier({ Lexeme = Identifier "x" }, _),
                                           _,
                                           _,
                                           _,
                                           _),
                                   Some(TFunction([ TInteger ], TInteger, _, _))) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected function type statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseListType() =
        let result = parse "let foo: [int] = [1, 2]"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for list type.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ECall(EIdentifier({ Lexeme = Identifier "cast" }, _),
                                         [ EList([ ELiteral(LNumber(LInteger 1), _); ELiteral(LNumber(LInteger 2), _) ],
                                                 _)
                                           EList([ ELiteral(LNumber(LInteger 0), _) ], _) ],
                                         _),
                                   Some(TTensor(TInteger, _))) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected list type statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseTupleType() =
        let result = parse "let foo: (int, int) = (1, 2)"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for tuple type.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ETuple([ ELiteral(LNumber(LInteger 1), _); ELiteral(LNumber(LInteger 2), _) ], _),
                                   Some(TTuple([ TInteger; TInteger ]))) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected tuple type statement, got {statements.Head}."
        | Error e -> Assert.Fail $"Expected no errors for valid expression. Got {e}."

    [<Test>]
    member _.Should_ParseTypedLambda() =
        let result = parse "let foo = (x: int) -> x"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for typed lambda.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ELambda([ ({ Lexeme = Identifier "x" }, Some TInteger) ],
                                           EIdentifier({ Lexeme = Identifier "x" }, _),
                                           _,
                                           _,
                                           _,
                                           _),
                                   _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected typed lambda statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseTypedRetLambda() =
        let result = parse "let foo = (x): int -> x"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for typed return lambda.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ELambda([ ({ Lexeme = Identifier "x" }, _) ],
                                           EIdentifier({ Lexeme = Identifier "x" }, _),
                                           Some TInteger,
                                           _,
                                           _,
                                           _),
                                   _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected typed return lambda statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseTypedLambdaWithReturnType() =
        let result = parse "let foo = (x: int): int -> x"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for typed lambda with return type.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ELambda([ ({ Lexeme = Identifier "x" }, Some TInteger) ],
                                           EIdentifier({ Lexeme = Identifier "x" }, _),
                                           Some TInteger,
                                           _,
                                           _,
                                           _),
                                   _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected typed lambda with return type statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseMultiParameterLambda() =
        let result = parse "let foo = (x, y) -> x + y"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for multi parameter lambda.")

            match statements.Head with
            | SVariableDeclaration({ Lexeme = Identifier "foo" },
                                   ELambda([ ({ Lexeme = Identifier "x" }, _); ({ Lexeme = Identifier "y" }, _) ],
                                           ECall(EIdentifier({ Lexeme = Operator(Plus, _) }, _),
                                                 [ EIdentifier({ Lexeme = Identifier "x" }, _)
                                                   EIdentifier({ Lexeme = Identifier "y" }, _) ],
                                                 _),
                                           _,
                                           _,
                                           _,
                                           _),
                                   _) -> Assert.Pass()
            | _ -> Assert.Fail $"Expected multi parameter lambda statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_ParseBlock() =
        let result = parse "{ 1 }"

        match result with
        | Ok(_, statements) ->
            Assert.That(statements, Has.Length.EqualTo(1), "Expected one statement for block.")

            match statements.Head with
            | SExpression(EBlock(stmts, _), _) ->
                Assert.That(stmts, Has.Length.EqualTo(1), "Expected one statement in block.")

                match stmts.Head with
                | SExpression(ELiteral(LNumber(LInteger 1), _), _) -> Assert.Pass()
                | _ -> Assert.Fail $"Expected block statement, got {stmts.Head}."
            | _ -> Assert.Fail $"Expected block statement, got {statements.Head}."
        | Error _ -> Assert.Fail "Expected no errors for valid expression."
