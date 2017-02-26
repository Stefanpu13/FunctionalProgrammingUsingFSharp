module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E
open Exercises1Common.Types
open Exercises1Common.Utils

// proj setup test
[<TestFixture>]
type ``Test reduce function``() =
    [<Test>]
    member t.``If expression is D(x^2 * x), result should be "Add (Mul (Mul (Const 2.0,X),X),Mul (X,X))"`` () = 
        reduce (D(Mul(Mul(X, X), X))) |> shouldEqual (Add (Mul (Mul (Const 2.0,X),X),Mul (X,X)))

    [<Test>]
    member t.``If expression is D(3 * x), result should be "Const 3"`` () = 
        reduce (D(Mul(Const 3.0, X))) |> shouldEqual (Const 3.0)

    [<Test>]
    member t.``If expression is (X + X) * 0 or 0 * (X+X), result should be "Const 0.0"`` () = 
        reduce (Mul(Const 0.0, Add(X, X))) |> shouldEqual (Const 0.0)
        reduce (Mul(Add(X, X), Const 0.0)) |> shouldEqual (Const 0.0)

    [<Test>]
    member t.``If expression is 0 + X, or X + 0 , result should be "X"`` () = 
        reduce (Add(Const 0.0,  X)) |> shouldEqual X  
        reduce (Add(X, Const 0.0)) |> shouldEqual X  

    [<Test>]
    member t.``If expression is X * 1 or 1 * X, result should be "X"`` () = 
        reduce (Mul(Const 1.0,  X)) |> shouldEqual (X)
        reduce (Mul(X, Const 1.0)) |> shouldEqual (X)
    
    [<Test>]
    member t.``If expression is 2 + 4 , result should be "Const 6.0"`` () = 
        reduce (Add(Const 2.0, Const 4.0)) |> shouldEqual (Const 6.0)  

    [<Test>]
    member t.``If expression is 5 * 2, result should be "Const 10.0"`` () = 
        reduce (Mul(Const 5.0,  Const 2.0)) |> shouldEqual (Const 10.0)    
    
    [<Test>]
    member t.``If expression is X + X , result should be "Mul(Const 2.0, X)"`` () = 
        reduce (Add(X,  X)) |> shouldEqual (Mul(Const 2.0, X))   

    [<Test>]
    member t.``If expression is X^2 + X^2 , result should be "Mul(Const 2.0, Mul(X, X))"`` () = 
        reduce (Add(Mul(X, X),  Mul(X, X))) |> shouldEqual (Mul(Const 2.0, Mul(X, X))) 

    [<Test>]
    member t.``If expression is X + 2 , result should be "Add(X, Const 2.0)"`` () = 
        reduce (Add(X,  Const 2.0)) |> shouldEqual (Add(X, Const 2.0))   

    [<Test>]
    member t.``If expression is D(X/X) , result should be "Const 0.0"`` () = 
        reduce (D(Div(X, X))) |> shouldEqual (Const 0.0)
    
        

