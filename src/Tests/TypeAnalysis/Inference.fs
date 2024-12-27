module Tests.Typing.Inference

open NUnit.Framework
open Vec3.Interpreter.Token
open Vec3.Interpreter.Typing.Inference
open Vec3.Interpreter.Parser
open Vec3.Interpreter.Grammar

[<TestFixture>]
type InferenceTests() =

    [<Test>]
    member _.Should_InferType() =
        let expr = "let x = 2"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TInteger, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType2() =
        let expr = "let x = 2 + 2"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TInteger, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType3() =
        let expr = "let x = 2.0"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TFloat, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType4() =
        let expr = "let x = 2.0 + 2.0"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TFloat, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType5() =
        let expr = "let x = 2 + 2.0"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TInteger, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType6() =
        let expr = "let x = (2 + 2.0) * 3"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TInteger, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
    
    [<Test>]
    member _.Should_InferType7() =
        let expr = "let x = 2 + 2.0 * 3"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TInteger, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType8() =
        let expr = "let x = (() -> 2)()"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TInteger, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType9() =
        let expr = "let x = (() -> 2.0)()"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TFloat, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    [<Test>]
    member _.Should_InferType10() =
        let expr = "let x = (() -> 2 + 2.0)()"
        match parse expr false with
        | Ok (_, program) ->
            match inferProgram1 program with
            | Ok (env, _, _, _) ->
                Assert.That(TInteger, Is.EqualTo(Map.find (Identifier "x") env))
            | Error _ -> Assert.Fail()
        | Error _ -> Assert.Fail()
        
    
            
