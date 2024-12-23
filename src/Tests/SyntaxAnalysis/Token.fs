module Tests.Token

open NUnit.Framework
open Vec3.Interpreter.Token

[<TestFixture>]
type TokenTests() =

    [<Test>]
    member _.Should_haveCorrectToStringForInteger() =
        let token = (LInteger 42)
        Assert.That(numberToString token, Is.EqualTo("Integer(42)"))

    [<Test>]
    member _.Should_haveCorrectToStringForFloat() =
        let token = (LFloat 42.0)
        Assert.That(numberToString token, Is.EqualTo("Float(42.0)"))

    [<Test>]
    member _.Should_haveCorrectToStringForString() =
        let token = String "Hello, World!"
        Assert.That(lexemeToString token, Is.EqualTo("""String("Hello, World!")"""))

    [<Test>]
    member _.Should_haveCorrectToStringForOperator() =
        let token = Operator(Plus, None)
        Assert.That(lexemeToString token, Is.EqualTo("Operator(+)"))

    [<Test>]
    member _.Should_haveCorrectToStringForIdentifier() =
        let token = Identifier "foo"
        Assert.That(lexemeToString token, Is.EqualTo("Identifier(foo)"))

    [<Test>]
    member _.Should_haveCorrectToStringForKeyword() =
        let token = Keyword Let
        Assert.That(lexemeToString token, Is.EqualTo("Keyword(let)"))

    [<Test>]
    member _.Should_haveCorrectToStringForPunctuation() =
        let token = Punctuation LeftBrace
        Assert.That(lexemeToString token, Is.EqualTo("Punctuation({)"))
