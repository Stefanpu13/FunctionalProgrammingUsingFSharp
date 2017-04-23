module Exercises9Tests
open NUnit.Framework
open FsUnitTyped
open Exercises9.E
open System
open System.Collections.Generic


let t7 = Node(7,[])
let t6 = Node(6,[])
let t5 = Node(5,[])     
let t3 = Node(3,[])    
let t2 = Node(2,[t5]) 
let t4 = Node(4,[t6; t7])
let t1 = Node(1,[t2; t3; t4])


// Test breadthFirstIter
[<TestFixture>]
type ``Test breadthFirstIter``() =

    [<Test>]
    member t.``If enumerating elements of tree t1, result should be [7; 6; 5; 4; 3; 2; 1]`` () =         
            breadthFirstIter (fun a x  -> x::a) [] t1 |> shouldEqual <| [7; 6; 5; 4; 3; 2; 1]

    [<Test>]
    member t.``If enumerating elements of tree t1, result should be same in breadthFirstFold and breadthFirstIter`` () =         
            breadthFirstIter (fun a x  -> x::a) [] t1 |> shouldEqual <| breadthFirstFold (fun a x  -> x::a) [] t1

    