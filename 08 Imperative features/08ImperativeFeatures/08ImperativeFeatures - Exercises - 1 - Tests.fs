module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E

// test
[<TestFixture>]
type ``Test add function``() =

    [<Test>]
    member t.``test add function"`` () = 
        add 1 2  |> shouldEqual 3

