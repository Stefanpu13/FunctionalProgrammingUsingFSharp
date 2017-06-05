module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E

// Test add
[<TestFixture>]
type ``Test af``() =

    [<Test>]
    member t.``add test`` () = 
        add 1 2 |> shouldEqual <| 3
