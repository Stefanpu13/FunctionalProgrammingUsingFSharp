module Exercises22Tests
open NUnit.Framework
open FsUnit
open FsUnitTyped
open Exercises22.E

// multiply polynomial by number tests
[<TestFixture>]
type ``multiply polynomial by number tests``() = 
    [<Test>]
    member test.``Number is 2, polynomial is [], result is []"``() = 
        2 &* (Polynomial [])  |> shouldEqual (Polynomial [])

    [<Test>]
    member test.``Number is 2, polynomial is [2; 1], result is [4;2]"``() = 
        2 &* (Polynomial [2;1])  |> shouldEqual (Polynomial [4;2])

    [<Test>]
    member test.``Number is -2, polynomial is [2; 1], result is [-4;-2]"``() = 
        -2 &* (Polynomial [2;1])  |> shouldEqual (Polynomial [-4;-2])

// multiply polynomial by x tests
[<TestFixture>]
type ``multiply polynomial by x tests``() = 
    [<Test>]
    member test.``Polynomial is [], result is [0]"``() = 
        byX (Polynomial [])  |> shouldEqual (Polynomial [0])

    [<Test>]
    member test.``Polynomial is [2; 1], result is [0;2;1]"``() = 
        byX (Polynomial [2;1])  |> shouldEqual (Polynomial [0;2;1])

// add two polynmials tests
[<TestFixture>]
type ``add two polynmials tests``() = 
    [<Test>]
    member test.``Polynomials are [] and [], result is []"``() = 
        (Polynomial []) &+ (Polynomial []) |> shouldEqual (Polynomial [])

    [<Test>]
    member test.``Polynomial are [2; 1] and [0;1;1], result is [2;2;1]"``() =
        (Polynomial [2;1]) &+ (Polynomial [0;1;1])  |> shouldEqual (Polynomial [2;2;1])

