module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E

// initial test
[<TestFixture>]
type ``Initial test fixture``() =

    [<Test>]
    member t.``initial test`` () = 
        add 1 2 |> shouldEqual <| 3 

    
