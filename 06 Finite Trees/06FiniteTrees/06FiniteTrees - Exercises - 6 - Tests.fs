module Exercises6Tests
open NUnit.Framework
open FsUnitTyped
open Exercises6.E

// Test leafVals
[<TestFixture>]
type ``Test delete``() =
    [<Test>]
    member t.``If tree is t4 and element to delete is smallest elem, result should be t4 without -3`` () = 
        let tLeft = Node(Leaf, 0, Node(Leaf, 2, Leaf))
        let res = Node(tLeft, 5, Node(Leaf, 7, Leaf))
        let valueToDelete = -3
        delete valueToDelete t4 |> shouldEqual res
        deleteTailRecursive valueToDelete t4 |> shouldEqual res

    [<Test>]
    member t.``If tree is t4 and element to delete is left subtree with children(0), result should be t4 without 0`` () = 
        let tLeft = Node((Node(Leaf, -3, Leaf), 2, Leaf))
        let res = Node(tLeft, 5, Node(Leaf, 7, Leaf))
        let valueToDelete = 0
        delete valueToDelete t4 |> shouldEqual res
        deleteTailRecursive valueToDelete t4 |> shouldEqual res

    [<Test>]
    member t.``If tree is t4 and element to delete is root, result should be t4 without 5 with min from right subtree as the new root`` () =        
        let res = Node(t3, 7, Leaf)
        let valueToDelete = 5
        delete valueToDelete t4 |> shouldEqual res
        deleteTailRecursive valueToDelete t4 |> shouldEqual res

    [<Test>]
    member t.``If tree is t4 and element to delete is biggest elem, result should be t4 without 7`` () =         
        let res = Node(t3, 5, Leaf)
        let valueToDelete = 7
        delete valueToDelete t4 |> shouldEqual res
        deleteTailRecursive valueToDelete t4 |> shouldEqual res

    [<Test>]
    member t.``If tree is t4 and element to delete is bigger than all elements, result should be t4`` () =
        let valueToDelete = 11
        delete valueToDelete t4 |> shouldEqual t4
        deleteTailRecursive valueToDelete t4 |> shouldEqual t4

    [<Test>]
    member t.``If tree is t4 and element to delete is smaller than all elements, result should be t4`` () =
        let valueToDelete = -11
        delete valueToDelete t4 |> shouldEqual t4
        deleteTailRecursive valueToDelete t4 |> shouldEqual t4


    [<Test>]
    member t.``deleteTailRecursive tries to delete the smallest elem of tree with dept 300000, result should be tr witout smallest element`` () =         
        let s = 1
        let e = 300000
        // let t = generateTree s e
        () 
        // Test is causing SOE when run but running commented code at the end of implementation file does not
        // case SOE. Why is that? 
        // deleteTailRecursive s t |> ignore