module Exercises4To5Tests
open NUnit.Framework
open FsUnitTyped
open Exercises4To5.E

// Straight line tests
[<TestFixture>]
type ``Straight line Tests``() = 

    [<Test>]
    member test.``When Straight line is (2.6, 8) result is "y = 2.6 * x + 8.0"``() = 
        straightLineToString (StraightLine (2.6, 8.0)) |> shouldEqual "y = 2.6x + 8.0"

    [<Test>]
    member test.``When Straight line is (0.0, 8) result is "y = 8.0"``() = 
        straightLineToString (StraightLine (0.0, 8.0)) |> shouldEqual "y = 8.0"

    [<Test>]
    member test.``When Straight line is (5.0, 0.0) result is "y = 5.0x"``() = 
        straightLineToString (StraightLine (5.0, 0.0)) |> shouldEqual "y = 5.0x"

// Quadratic equation tests
[<TestFixture>]
type ``Quadratic equation Tests``() = 

    [<Test>]
    member test.``When discriminant is 0, equation has one solution``() = 
        solve (Equation(1.0, 2.0, 1.0)) |> shouldEqual (OneSolution -1.0)

    [<Test>]
    member test.``When a coef is 0, equation has one solution``() = 
        solve (Equation(0.0, 5.0, 6.0)) |> shouldEqual (OneSolution -1.2)
    
    [<Test>]
    member test.``When discriminant is negative, equation has no solution``() = 
        solve (Equation(1.0, 2.0, 2.0)) |> shouldEqual NoSolutions

    [<Test>]
    member test.``When discriminant is positive and not square, equation should has two solutions``() = 
        solve (Equation(1.0, 5.0, 6.0)) |> shouldEqual (TwoSolutions(-2.0, -3.0))
    