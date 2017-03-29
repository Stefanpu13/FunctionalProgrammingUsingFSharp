module Exercises11Tests
open System
open NUnit.Framework
open FsUnitTyped
open Exercises11.E

let t7 = Node(7,[])
let t6 = Node(6,[])
let t5 = Node(5,[])     
let t3 = Node(3,[])    
let t2 = Node(2,[t5]) 
let t4 = Node(4,[t6; t7])
let t1 = Node(1,[t2; t3; t4])

// Test depthFirstFoldBack
[<TestFixture>]
type ``Test depthFirstFoldBack``() =

    [<Test>]
    member t.``If enumerating elements of tree t1, result should be [1; 2; 5; 3; 4; 6; 7]`` () =         
            depthFirstFoldBack (fun x a -> x::a) t1 [] |> shouldEqual <| [1; 2; 5; 3; 4; 6; 7]

// Test breathFirstFold
[<TestFixture>]
type ``Test breathFirstFold``() =

    [<Test>]
    member t.``If enumerating elements of tree t1, result should be [7; 6; 5; 4; 3; 2; 1]`` () =         
            breadthFirstFold (fun a x  -> x::a) [] t1 |> shouldEqual <| [7; 6; 5; 4; 3; 2; 1]

