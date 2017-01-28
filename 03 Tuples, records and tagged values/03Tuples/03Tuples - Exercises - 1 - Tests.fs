module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E

// "Comes before" is custom operator <.
[<TestFixture>]
type ``Comes before tests``() = 
    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,56,"AM"), output should be true``() = 
        let t1 = TimeOfDayTriple ( 11, 55, "AM")
        let t2 = TimeOfDayTriple (11, 54, "PM")

        t1 <. t2 |> shouldEqual true
        