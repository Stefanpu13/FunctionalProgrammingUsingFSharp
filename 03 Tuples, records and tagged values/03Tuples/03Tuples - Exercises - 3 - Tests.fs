module Exercises3Tests
open NUnit.Framework
open FsUnitTyped
open Exercises3.E

// Addition tests
[<TestFixture>]
type ``Addition Tests``() = 

    [<Test>]
    member test.``When p1 = (1.2, 2.3), p2 = (2.4, 3.0), output should be (3.6, 5.3)``() = 
        let c3 = Complex(1.2, 2.3) 
        let c4 = Complex(2.4, 3.0) 

        c3 &+ c4 |> shouldEqual (Complex (1.2 + 2.4, 2.3 + 3.0)) 

    [<Test>]
    member test.``When p1 = (-1.2, -2.3), p2 = (2.4, 3.0), output should be (1.2, 0.7)``() = 
        let c3 = Complex(-1.2, -2.3) 
        let c4 = Complex(2.4, 3.0) 

        c3 &+ c4 |> shouldEqual (Complex (-1.2 + 2.4, -2.3 + 3.0)) 

// Multiplication tests
[<TestFixture>]
type ``Multiplication Tests``() = 

    [<Test>]
    member test.``When p1 = (1.0, 2.0), p2 = (2.0, 3.0), output should be (-4.0, 7.0)``() = 
        let c3 = Complex(1.0, 2.0) 
        let c4 = Complex(2.0, 3.0) 

        c3 &* c4 |> shouldEqual (Complex (-4.0, 7.0)) 

    [<Test>]
    member test.``When p1 = (-1.0, 2.0), p2 = (2.0, 3.0), output should be (-8.0, 1.0)``() = 
        let c3 = Complex(-1.0, 2.0) 
        let c4 = Complex(2.0, 3.0) 

        c3 &* c4 |> shouldEqual (Complex (-8.0, 1.0))  

// Substraction tests
[<TestFixture>]
type ``Substraction Tests``() = 

    [<Test>]
    member test.``When p1 = (1.0, 2.0), p2 = (2.0, 3.0), output should be (-1.0, -1.0)``() = 
        let c3 = Complex(1.0, 2.0) 
        let c4 = Complex(2.0, 3.0) 

        c3 &- c4 |> shouldEqual (Complex (-1.0, -1.0)) 

    [<Test>]
    member test.``When p1 = (-1.0, 2.0), p2 = (2.0, 3.0), output should be (-3.0, -1.0)``() = 
        let c3 = Complex(-1.0, 2.0) 
        let c4 = Complex(2.0, 3.0) 

        c3 &- c4 |> shouldEqual (Complex (-3.0, -1.0))  

// Division tests
[<TestFixture>]
type ``Division Tests``() = 

    [<Test>]
    member test.``When p1 = (1.0, 2.0), p2 = (2.0, 3.0), output should be (-1.0, -1.0)``() = 
        let c3 = Complex(1.0, 2.0) 
        let c4 = Complex(2.0, 3.0) 

        let (Complex (c, d))  = Complex (2.0/(4.0+9.0), -3.0/(4.0+9.0))
        c3 &/ c4 |> shouldEqual (Complex (1.0 * c - 2.0 * d, 2.0*c+1.0*d)) 