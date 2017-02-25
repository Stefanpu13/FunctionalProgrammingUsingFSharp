module Exercises2Tests
open NUnit.Framework
open FsUnitTyped
open Exercises2.E

// proj setup test
[<TestFixture>]
type ``Test reduce function``() =
    [<Test>]
    member t.``If expression is (x + 7), result should be "x 7 +"`` () = 
        postFix (Add(X, Const 7.0)) |> shouldEqual "x 7 +"

