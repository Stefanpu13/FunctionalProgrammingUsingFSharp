module Exercises7Tests
open NUnit.Framework
open FsUnitTyped
open Exercises7.E
open System
open System.Collections.Generic


let rand = Random()

let createRandomListAndHashSet () =
    let count = rand.Next(100); 
    let randomList = [0..count] |> List.fold (fun l _ -> rand.Next(100)::l) []
    let hashSetFromRandomList = HashSet()
    randomList |> List.iter (hashSetFromRandomList.Add >> ignore)|> ignore
    (randomList, hashSetFromRandomList)


// Test imperative hashSet fold function
[<TestFixture>]
type ``Test imperative hashSet fold function``() =

    [<Test>]
    member t.``folding empty hashSet should return initial value`` () = 
        let r = rand.Next(100); 
        hashSetFold (+) r (HashSet ()) |> shouldEqual <| r

    [<Test>]
    member t.``folding hashSet to find Max value should return same result as List.fold`` () = 
        let (randomList, hashSetFromRandomList) = createRandomListAndHashSet ()

        let initialMax = List.head randomList
        hashSetFold max initialMax hashSetFromRandomList 
            |> shouldEqual <| (List.distinct >> List.fold max initialMax) randomList
        
    [<Test>]
    member t.``folding hashSet to find sum value should return same result as List.fold`` () = 
        let (randomList, hashSetFromRandomList) = createRandomListAndHashSet ()
        
        hashSetFold (+) 0 hashSetFromRandomList 
            |> shouldEqual <| (List.distinct >> List.fold (+) 0) randomList
