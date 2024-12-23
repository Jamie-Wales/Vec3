module Tests.SyntaxAnalysis.Scanner

open NUnit.Framework
open Vec3.Interpreter.Scanner
open Vec3.Interpreter.Token

[<TestFixture>]
type LexerTests() =

    [<Test>]
    member _.Should_TokenizeEmptyInput() =
        let result = lexer ""

        match result with
        | Ok tokens -> Assert.That(tokens, Is.Empty, "Expected no tokens for empty input.")
        | Error _ -> Assert.Fail "Expected no errors for empty input."

    [<Test>]
    member _.Should_TokenizeSingleNumber() =
        let result = lexer "42"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for single number.")

            match tokens.Head.Lexeme with
            | Number(LInteger 42) -> Assert.Pass()
            | _ -> Assert.Fail "Expected LInteger token."
        | Error _ -> Assert.Fail "Expected no errors for valid number."

    [<Test>]
    member _.Should_TokenizeSimpleExpression() =
        let result = lexer "3 + 4"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(3), "Expected three tokens for simple expression.")
            Assert.That(tokens[0].Lexeme, Is.EqualTo(Number(LInteger 3)), "Expected first token to be 3.")
            Assert.That(tokens[1].Lexeme, Is.EqualTo(Operator(Plus, None)), "Expected second token to be +.")
            Assert.That(tokens[2].Lexeme, Is.EqualTo(Number(LInteger 4)), "Expected third token to be 4.")
        | Error _ -> Assert.Fail "Expected no errors for valid expression."

    [<Test>]
    member _.Should_HandleUnterminatedString() =
        let result = lexer "\"Unfinished string"

        match result with
        | Ok _ -> Assert.Fail "Expected an error for unterminated string."
        | Error(Errors _) -> Assert.Pass() // todo
        | Error _ -> Assert.Fail "Expected unterminated string error."

    [<Test>]
    member _.Should_HandleBlockComments() =
        let result = lexer "/* Comment */ 42"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for number after block comment.")
            Assert.That(tokens[0].Lexeme, Is.EqualTo(Number(LInteger 42)), "Expected number token after block comment.")
        | Error _ -> Assert.Fail "Expected no errors with block comment."

    [<Test>]
    member _.Should_HandleUnterminatedBlockComment() =
        let result = lexer "/* Unterminated comment"

        match result with
        | Ok _ -> Assert.Fail "Expected an error for unterminated block comment."
        | Error(Errors _) -> Assert.Pass() // todo
        | Error _ -> Assert.Fail "Expected unterminated block comment error."

    [<Test>]
    member _.Should_TokenizeIdentifiers() =
        let result = lexer "varName"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for identifier.")

            match tokens.Head.Lexeme with
            | Identifier "varName" -> Assert.Pass()
            | _ -> Assert.Fail "Expected Identifier token."
        | Error _ -> Assert.Fail "Expected no errors for valid identifier."

    [<Test>]
    member _.Should_HandleCustomOperators() =
        let result = lexer "@"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for custom operator.")

            match tokens.Head.Lexeme with
            | Operator(Custom "@", None) -> Assert.Pass()
            | _ -> Assert.Fail "Expected CustomOperator token."
        | Error(Errors _) -> Assert.Pass() // todo

    [<Test>]
    member _.Should_TokenizeComplexNumber() =
        let result = lexer "3.14 + 2i"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(3), "Expected three tokens for complex number.")
            Assert.That(tokens[0].Lexeme, Is.EqualTo(Number(LFloat 3.14)), "Expected first token to be 3.14.")
            Assert.That(tokens[1].Lexeme, Is.EqualTo(Operator(Plus, None)), "Expected second token to be +.")
            Assert.That(tokens[2].Lexeme, Is.EqualTo(Number(LComplex(0.0, 2))), "Expected third token to be 2i.")
        | Error _ -> Assert.Fail "Expected no errors for valid complex number."

    [<Test>]
    member _.Should_TokenizeStringLiteral() =
        let result = lexer "\"Hello, World!\""

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for string literal.")

            match tokens.Head.Lexeme with
            | Lexeme.String "Hello, World!" -> Assert.Pass()
            | _ -> Assert.Fail "Expected String token."
        | Error _ -> Assert.Fail "Expected no errors for valid string literal."

    [<Test>]
    member _.Should_TokeniseIntegerWithUnderscores() =
        let result = lexer "1_000_000"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for integer with underscores.")

            match tokens.Head.Lexeme with
            | Number(LInteger 1_000_000) -> Assert.Pass()
            | _ -> Assert.Fail "Expected LInteger token."
        | Error _ -> Assert.Fail "Expected no errors for valid integer with underscores."

    [<Test>]
    member _.Should_TokeniseRationalNumber() =
        let result = lexer "1/2"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for rational number.")

            match tokens.Head.Lexeme with
            | Number(LRational(1, 2)) -> Assert.Pass()
            | _ -> Assert.Fail "Expected LRational token."
        | Error _ -> Assert.Fail "Expected no errors for valid rational number."

    [<Test>]
    member _.Should_TokeniseFloat() =
        let result = lexer "3.14"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(1), "Expected one token for float.")

            match tokens.Head.Lexeme with
            | Number(LFloat 3.14) -> Assert.Pass()
            | _ -> Assert.Fail "Expected LFloat token."
        | Error _ -> Assert.Fail "Expected no errors for valid float."

    [<Test>]
    member _.Should_TokeniseComplexNumberWithFloat() =
        let result = lexer "3.14 + 2i"

        match result with
        | Ok tokens ->
            Assert.That(tokens, Has.Length.EqualTo(3), "Expected three tokens for complex number with float.")
            Assert.That(tokens[0].Lexeme, Is.EqualTo(Number(LFloat 3.14)), "Expected first token to be 3.14.")
            Assert.That(tokens[1].Lexeme, Is.EqualTo(Operator(Plus, None)), "Expected second token to be +.")
            Assert.That(tokens[2].Lexeme, Is.EqualTo(Number(LComplex(0.0, 2.0))), "Expected third token to be 2.0i.")
        | Error _ -> Assert.Fail "Expected no errors for valid complex number with float."
