module Exercises3Tests
open NUnit.Framework
open FsUnitTyped
open Exercises3.E
open Exercises1Common.Types
open Exercises1Common.Utils

// // Test enhanced toString function
// [<TestFixture>]
// type ``Test enhanced toString function``() =

//     [<Test>]
//     member t.``If expression is (x + 7), result should be "x + 7"`` () = 
//         toString (Add(X, Const 7.0)) |> shouldEqual "x + 7"

//     [<Test>]
//     member t.``If expression is (x/2 + 7), result should be "x / 2 + 7"`` () = 
//         toString (Add(Div(X, Const 2.0), Const 7.0)) |> shouldEqual "x / 2 + 7"

//     [<Test>]
//     member t.``If expression is (x + 7) * 2, result should be "(x + 7) * 2"`` () = 
//         toString (Mul(Add(X, Const 7.0), Const 2.0)) |> shouldEqual "(x + 7) * 2"

// Test enhanced toStringSubtrahend function
[<TestFixture>]
type ``Test enhanced toStringSubtrahend function``() =

    [<Test>]
    member t.``If expression is (x - 7), result should be "x - 7"`` () = 
        toString (Sub(X, Const 7.0)) |> shouldEqual "x - 7"

    [<Test>]
    member t.``If expression is (x - 7/x), result should be "x - 7 / x"`` () = 
        toString (Sub(X, Div(Const 7.0, X))) |> shouldEqual "x - 7 / x"

    [<Test>]
    member t.``If expression is (x - 7 * x), result should be "x + 7 * x"`` () = 
        toString (Sub(X, Mul(Const 7.0, X))) |> shouldEqual "x - 7 * x"

    [<Test>]
    member t.``If expression is x - (7 + x), result should be "x + (7 + x)"`` () = 
        toString (Sub(X, Add(Const 7.0, X))) |> shouldEqual "x - (7 + x)"


    [<Test>]
    member t.``If expression is x - (7 - x), result should be "x + (7 - x)"`` () = 
        toString (Sub(X, Sub(Const 7.0, X))) |> shouldEqual "x - (7 - x)"

    [<Test>]
    member t.``If expression is (x + 4) - (7 - x), result should be "x + 4 - (7 - x)"`` () = 
        toString (Sub(Add(X, Const 4.0), Sub(Const 7.0, X))) |> shouldEqual "x + 4 - (7 - x)"

// Test enhanced toStringFactor function
[<TestFixture>]
type ``Test enhanced toStringFactor function``() =

    [<Test>]
    member t.``If expression is (x * 7), result should be "x * 7"`` () = 
        toString (Mul(X, Const 7.0)) |> shouldEqual "x * 7"

    [<Test>]
    member t.``If expression is (x * 7/x), result should be "x * 7 / x"`` () = 
        toString (Mul(X, Div(Const 7.0, X))) |> shouldEqual "x * 7 / x"

    [<Test>]
    member t.``If expression is (x * 7 * x), result should be "x * 7 * x"`` () = 
        toString (Mul(X, Mul(Const 7.0, X))) |> shouldEqual "x * 7 * x"


    [<Test>]
    member t.``If expression is (x - 2) * (7 + x), result should be "(x - 2) * (7 + x)"`` () = 
        toString (Mul(Sub(X, Const 2.0), Add(Const 7.0, X))) |> shouldEqual "(x - 2) * (7 + x)"

    [<Test>]
    member t.``If expression is (x + 2) * (7 - x), result should be "(x + 2) * (7 - x)"`` () = 
        toString (Mul(Add(X, Const 2.0), Sub(Const 7.0, X))) |> shouldEqual "(x + 2) * (7 - x)"

// Test enhanced toStringDividend function
[<TestFixture>]
type ``Test enhanced toStringDividend function``() =

    [<Test>]
    member t.``If expression is (x / 7), result should be "x / 7"`` () = 
        toString (Div(X, Const 7.0)) |> shouldEqual "x / 7"

    [<Test>]
    member t.``If expression is (x/x) / 7, result should be "x / x / 7"`` () = 
        toString (Div(Div(X, X), Const 7.0)) |> shouldEqual "x / x / 7"

    [<Test>]
    member t.``If expression is (x + 7)/x, result should be "(x + 7) / x"`` () = 
        toString (Div(Add(X, Const 7.0), X)) |> shouldEqual "(x + 7) / x"

    [<Test>]
    member t.``If expression is (x - 7) / x, result should be "(x - 7) / x"`` () = 
        toString (Div(Sub(X, Const 7.0), X)) |> shouldEqual "(x - 7) / x"

// Test enhanced toStringDivisor function
[<TestFixture>]
type ``Test enhanced toStringDivisor function``() =    

    [<Test>]
    member t.``If expression is (x / (x + 7)), result should be "x / (x + 7)"`` () = 
        toString (Div(X, Add(X, Const 7.0))) |> shouldEqual "x / (x + 7)"

    [<Test>]
    member t.``If expression is (x / (x - 7)), result should be "x / (x - 7)"`` () = 
        toString (Div(X, Sub(X, Const 7.0))) |> shouldEqual "x / (x - 7)"

    [<Test>]
    member t.``If expression is (x / (x * 7)), result should be "x / (x * 7)"`` () = 
        toString (Div(X, Mul(X, Const 7.0))) |> shouldEqual "x / (x * 7)"
        
    [<Test>]
    member t.``If expression is (x / (x / 7)), result should be "x / (x / 7)"`` () = 
        toString (Div(X, Div(X, Const 7.0))) |> shouldEqual "x / (x / 7)"
        
// Test enhanced toStringArgument function
[<TestFixture>]
type ``Test enhanced toStringArgument function``() =    

    [<Test>]
    member t.``If argument is x, result should be "f x"`` () = 
        toString (Sin X) |> shouldEqual "sin x"
        toString (Cos X) |> shouldEqual "cos x"
        toString (Log X) |> shouldEqual "log x"
        toString (Exp X) |> shouldEqual "exp x"

    [<Test>]
    member t.``If argument is Const 3.0, result should be "f 3"`` () = 
        toString (Sin (Const 3.0)) |> shouldEqual "sin 3"
        toString (Cos (Const 3.0)) |> shouldEqual "cos 3"
        toString (Log (Const 3.0)) |> shouldEqual "log 3"
        toString (Exp (Const 3.0)) |> shouldEqual "exp 3"

    [<Test>]
    member t.``If argument is x + 3, result should be "f (x + 3)"`` () = 
        toString (Sin (Add(X, Const 3.0))) |> shouldEqual "sin (x + 3)"
        toString (Cos (Add(X, Const 3.0))) |> shouldEqual "cos (x + 3)"
        toString (Log (Add(X, Const 3.0))) |> shouldEqual "log (x + 3)"
        toString (Exp (Add(X, Const 3.0))) |> shouldEqual "exp (x + 3)"

    [<Test>]
    member t.``If argument is x - 3, result should be "f (x - 3)"`` () = 
        toString (Sin (Sub(X, Const 3.0))) |> shouldEqual "sin (x - 3)"
        toString (Cos (Sub(X, Const 3.0))) |> shouldEqual "cos (x - 3)"
        toString (Log (Sub(X, Const 3.0))) |> shouldEqual "log (x - 3)"
        toString (Exp (Sub(X, Const 3.0))) |> shouldEqual "exp (x - 3)"

        
    [<Test>]
    member t.``If argument is x * 3, result should be "f (x * 3)"`` () = 
        toString (Sin (Mul(X, Const 3.0))) |> shouldEqual "sin (x * 3)"
        toString (Cos (Mul(X, Const 3.0))) |> shouldEqual "cos (x * 3)"
        toString (Log (Mul(X, Const 3.0))) |> shouldEqual "log (x * 3)"
        toString (Exp (Mul(X, Const 3.0))) |> shouldEqual "exp (x * 3)"

    [<Test>]
    member t.``If argument is x / 3, result should be "f (x / 3)"`` () = 
        toString (Sin (Div(X, Const 3.0))) |> shouldEqual "sin (x / 3)"
        toString (Cos (Div(X, Const 3.0))) |> shouldEqual "cos (x / 3)"
        toString (Log (Div(X, Const 3.0))) |> shouldEqual "log (x / 3)"
        toString (Exp (Div(X, Const 3.0))) |> shouldEqual "exp (x / 3)"

    [<Test>]
    member t.``If argument is sin x, result should be "f (sin x)"`` () = 
        toString (Sin (Sin(X))) |> shouldEqual "sin (sin x)"
        toString (Cos (Sin(X))) |> shouldEqual "cos (sin x)"
        toString (Log (Sin(X))) |> shouldEqual "log (sin x)"
        toString (Exp (Sin(X))) |> shouldEqual "exp (sin x)"

    [<Test>]
    member t.``If argument is cos x, result should be "f (cos x)"`` () = 
        toString (Sin (Cos(X))) |> shouldEqual "sin (cos x)"
        toString (Cos (Cos(X))) |> shouldEqual "cos (cos x)"
        toString (Log (Cos(X))) |> shouldEqual "log (cos x)"
        toString (Exp (Cos(X))) |> shouldEqual "exp (cos x)"

        
    [<Test>]
    member t.``If argument is log x, result should be "f (log x)"`` () = 
        toString (Sin (Log(X))) |> shouldEqual "sin (log x)"
        toString (Cos (Log(X))) |> shouldEqual "cos (log x)"
        toString (Log (Log(X))) |> shouldEqual "log (log x)"
        toString (Exp (Log(X))) |> shouldEqual "exp (log x)"

                
    [<Test>]
    member t.``If argument is exp x, result should be "f (exp x)"`` () = 
        toString (Sin (Exp(X))) |> shouldEqual "sin (exp x)"
        toString (Cos (Exp(X))) |> shouldEqual "cos (exp x)"
        toString (Log (Exp(X))) |> shouldEqual "log (exp x)"
        toString (Exp (Exp(X))) |> shouldEqual "exp (exp x)"
