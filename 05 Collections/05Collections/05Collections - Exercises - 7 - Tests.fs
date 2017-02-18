module Exercises7Tests
open NUnit.Framework
open FsUnitTyped
open Exercises7.E

//all subsets tests
[<TestFixture>]
type ``all subsets tests``() =
    [<Test>]
    member t.``If n=2 k=1, then result should be [[1];[2]]`` () = 
        allSubsets 2 1 |> shouldEqual [[1];[2]]

    [<Test>]
    member t.``If n=3 k=2, then result should be [[1;2];[1;3];[2;3]]`` () = 
        allSubsets 3 2 |> shouldEqual [[1;2];[1;3];[2;3]]

    [<Test>]
    member t.``If n=random k=random, then result should be [[1;2];[1;3];[2;3]]`` () = 
        let rand = System.Random()
        let x = rand.Next(1, 20)
        allSubsets x x |> shouldEqual [[1..x]]

    [<Test>]
    member t.``If n=random k=random+1, then result should be []`` () = 
        let rand = System.Random()
        let x = rand.Next(1, 20)
        allSubsets x (x + 1) |> shouldEqual []
  