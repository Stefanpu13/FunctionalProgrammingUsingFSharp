module Exercises22Tests
open NUnit.Framework
open FsUnit
open FsUnitTyped
open Exercises22.E

// multiply polynomial by number tests
[<TestFixture>]
type ``multiply polynomial by number tests``() = 
    [<Test>]
    member test.``Number is 2, polynomial is [], result should equal []``() = 
        2 &* (Polynomial [])  |> shouldEqual (Polynomial [])

    [<Test>]
    member test.``Number is 2, polynomial is [2; 1], result should equal [4;2]``() = 
        2 &* (Polynomial [2;1])  |> shouldEqual (Polynomial [4;2])

    [<Test>]
    member test.``Number is -2, polynomial is [2; 1], result should equal [-4;-2]``() = 
        -2 &* (Polynomial [2;1])  |> shouldEqual (Polynomial [-4;-2])

// multiply polynomial by x tests
[<TestFixture>]
type ``multiply polynomial by x tests``() = 
    [<Test>]
    member test.``Polynomial is [], result should equal [0]``() = 
        byX (Polynomial [])  |> shouldEqual (Polynomial [0])

    [<Test>]
    member test.``Polynomial is [2; 1], result should equal [0;2;1]``() = 
        byX (Polynomial [2;1])  |> shouldEqual (Polynomial [0;2;1])

// add two polynmials tests
[<TestFixture>]
type ``add two polynmials tests``() = 
    [<Test>]
    member test.``Polynomials are [] and [], result should equal []``() = 
        (Polynomial []) &+ (Polynomial []) |> shouldEqual (Polynomial [])

    [<Test>]
    member test.``Polynomials are [2; 1] and [0;1;1], result should equal [2;2;1]``() =
        (Polynomial [2;1]) &+ (Polynomial [0;1;1])  |> shouldEqual (Polynomial [2;2;1])


// multiply two polynmials tests
[<TestFixture>]
type ``multiply two polynmials tests``() = 
    [<Test>]
    member test.``Polynomials are [] and [], result should equal []``() = 
       multiply (Polynomial []) (Polynomial []) |> shouldEqual (Polynomial [])

    [<Test>]
    member test.``Polynomials are [] and [1;1], result should equal [0]``() =
        multiply (Polynomial []) (Polynomial [1;1])  |> shouldEqual (Polynomial [0])

    [<Test>]
    member test.``Polynomials are [0] and [1;1], result should equal [0]``() =
        multiply (Polynomial [0]) (Polynomial [1;1])  |> shouldEqual (Polynomial [0])

    
    [<Test>]
    member test.``Polynomials are [1;1] and [1;1], result should equal [1;2;1]``() =
        multiply (Polynomial [1;1]) (Polynomial [1;1])  |> shouldEqual (Polynomial [1;2;1])
    
    [<Test>]
    member test.``Polynomials are [1;2] and [3;1], result should equal [3;7;2]``() =
        multiply (Polynomial [1;2]) (Polynomial [3;1])  |> shouldEqual (Polynomial [3;7;2])


    // polynomial toString tests
[<TestFixture>]
type ``polynomial toString tests``() = 
    [<Test>]
    member test.``Polynomial is [], result should equal ""``() = 
       toString (Polynomial []) |> shouldEqual ""

    [<Test>]
    member test.``Polynomial is [0], result should equal ""``() = 
       toString (Polynomial [0]) |> shouldEqual ""

    [<Test>]
    member test.``Polynomial is [1;1;2], result should equal "1 + x + 2x^2"``() = 
       toString (Polynomial [1;1;2]) |> shouldEqual "1 + x + 2x^2"

    [<Test>]
    member test.``Polynomial is [0;1], result should equal "x"``() = 
       toString (Polynomial [0;1]) |> shouldEqual "x"

    [<Test>]
    member test.``Polynomial is [1;0], result should equal "1"``() = 
       toString (Polynomial [1;0]) |> shouldEqual "1"

    [<Test>]
    member test.``Polynomial is [0;1;0;0;2], result should equal "x + 2x^4"``() = 
       toString (Polynomial [0;1;0;0;2]) |> shouldEqual "x + 2x^4"

    