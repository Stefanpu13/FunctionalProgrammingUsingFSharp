module Exercises6Tests
open NUnit.Framework
open FsUnitTyped
open Exercises6.E

// dom tests
[<TestFixture>]
type ``dom tests``() =
    [<Test>]
    member t.``colMap tests`` () = 
        dom  2 |> shouldEqual true

    