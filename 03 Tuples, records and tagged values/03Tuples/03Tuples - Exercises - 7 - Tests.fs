module Exercises7Tests
open NUnit.Framework
open FsUnitTyped
open Exercises7.E

// isShape tests
[<TestFixture>]
type ``isShape tests``() = 
    [<Test>]
    member test.``When size is negative, output should be false``() = 
        let circle = Circle -6.0
        let rectangle = Square -1.0
        let triangle1 = Triangle (-1.0, 1.0, 1.0)
        let triangle2 = Triangle (1.0, -1.0, 1.0)
        let triangle3 = Triangle (1.0, 1.0, -1.0)

        isShape circle |> shouldEqual false
        isShape rectangle |> shouldEqual false
        isShape triangle1 |> shouldEqual false
        isShape triangle2 |> shouldEqual false
        isShape triangle3 |> shouldEqual false

    [<Test>]
    member test.``When shape is trinagle and (a >= b + c) || (b >= a + c ) || (c >= a+ b), output should be false``() = 
        let triangleWithBigASide = Triangle (6.0, 3.0, 3.0)
        let triangleWithBigBSide = Triangle (6.0, 9.0, 3.0)
        let triangleWithBigCSide = Triangle (6.0, 9.0, 15.0)

        isShape triangleWithBigASide |> shouldEqual false
        isShape triangleWithBigBSide |> shouldEqual false
        isShape triangleWithBigCSide |> shouldEqual false     

    [<Test>]
    member test.``When shape is trinagle and (a < b + c) && (b < a + c ) && (c < a+ b), output should be true``() = 
        let triangleWithBigASide = Triangle (4.0, 3.0, 3.0)
        let triangleWithBigBSide = Triangle (6.0, 6.0, 3.0)
        let triangleWithBigCSide = Triangle (6.0, 9.0, 12.0)

        isShape triangleWithBigASide |> shouldEqual true
        isShape triangleWithBigBSide |> shouldEqual true
        isShape triangleWithBigCSide |> shouldEqual true

    [<Test>]
    member test.``When shape is trinagle or square and size is positive,  output should be true``() = 
        let circle = Circle 4.0
        let square = Square 6.0        

        isShape circle |> shouldEqual true
        isShape square |> shouldEqual true   


// area tests
[<TestFixture>]
type ``area tests``() = 
    [<Test>]
    member test.``When shape is circle and size is 4.0, output should be 12.56``() = 
        let circle = Circle 4.0
        area circle |> shouldEqual (Area (System.Math.PI * 4.0 * 4.0))

    [<Test>]
    member test.``When shape is square and size is 4.0, output should be 16.00``() = 
        let square = Square 4.0
        area square |> shouldEqual (Area (4.0 * 4.0))

    [<Test>]
    member test.``When shape is triangle and size is (4.0, 3.0, 3.0), output should be valid``() = 
        let triangle = Triangle (4.0, 3.0, 3.0)
        area triangle |> shouldEqual (Area (sqrt(5.0*(5.0-4.0)*(5.0-3.0)*(5.0-3.0))))

    