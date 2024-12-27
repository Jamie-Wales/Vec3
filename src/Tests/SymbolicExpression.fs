module Tests.SymbolicExpression

open NUnit.Framework
open Vec3.Interpreter.SymbolicExpression

[<TestFixture>]
type SymbolicExpressionTests() =

    [<Test>]
    member _.Should_ConvertToString() =
        let expr = Addition (Multiplication (X, Const 2), Const 3)
        Assert.That("((x * 2) + 3)", Is.EqualTo(toString expr))
    
    [<Test>]
    member _.Should_Evaluate() =
        let expr = Addition (Multiplication (X, Const 2), Const 3)
        Assert.That(7.0, Is.EqualTo( evaluate 2.0 expr))
    
    [<Test>]
    member _.Should_Differentiate() =
        let expr = Addition (Multiplication (X, Const 2), Const 3)
        Assert.That(Const 2, Is.EqualTo(differentiate expr))
    
    [<Test>]
    member _.Should_DifferentiateTwice() =
        let expr = Addition (Multiplication (X, Const 2), Const 3)
        Assert.That(Const 0, Is.EqualTo(differentiate (differentiate expr)))
    
    
    [<Test>]
    member _.Should_Integrate() =
        let expr = Addition (X, Const 3)
        Assert.That(Addition(Multiplication (Const 0.5, Multiplication (X, X)), Multiplication (Const 3.0, X)), Is.EqualTo(integrate expr))
    
    