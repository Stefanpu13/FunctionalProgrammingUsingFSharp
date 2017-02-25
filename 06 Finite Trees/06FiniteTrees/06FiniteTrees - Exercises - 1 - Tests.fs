module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E

// proj setup test
[<TestFixture>]
type `` proj setup test``() =
    [<Test>]
    member t.`` proj setup test`` () = 
        add 2 3 |> shouldEqual 5 

