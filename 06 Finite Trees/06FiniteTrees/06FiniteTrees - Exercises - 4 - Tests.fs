module Exercises4Tests
open NUnit.Framework
open FsUnitTyped
open Exercises4.E

// Test enhanced toStringSubtrahend function
[<TestFixture>]
type ``Test leafVals``() =
    let tr  = Node(Node(Leaf 1,"cd",Leaf 2),"ab",Node(Leaf 3, "l3", Node(Leaf 4, "hah", Leaf 5)))

    [<Test>]
    member t.``If tree is t1, result should be set [1;2;3;4;5]"`` () = 
        leafVals tr |> shouldEqual (set [1;2;3;4;5])

    
    [<Test>]
    member t.``If tree is Node(Leaf 1, "a", Leaf 1), result should be set [1]"`` () = 
        let tr1 = Node(Leaf 1, "a", Leaf 1)
        leafVals tr1 |> shouldEqual (set [1])
        


   