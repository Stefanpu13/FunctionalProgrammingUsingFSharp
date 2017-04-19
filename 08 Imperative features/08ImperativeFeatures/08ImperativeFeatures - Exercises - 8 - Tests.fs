module Exercises8Tests
open NUnit.Framework
open FsUnitTyped
open Exercises8.E
open System
open System.Collections.Generic


let rand = Random()

let createRandomMapAndDictionary () =
    let count = rand.Next(100); 
    let randomList = 
        [0..count] 
            |> List.fold (fun l _ -> rand.Next(100)::l) []
            |> List.distinct
            |> List.map(fun el -> el, el)

    let map = Map.ofList randomList
    let dictionary = dict randomList
    
    (dictionary, map)

// Test dictionary fold function
[<TestFixture>]
type ``Test dictionary fold function``() =

    [<Test>]
    member t.``folding empty Dictionard should return initial value`` () = 
        let r = rand.Next(100); 
        dictionaryFold (fun key value state -> value + state) r (dict []) |> shouldEqual <| r

    [<Test>]
    member t.``folding dictionary to find Max value should return same result as Map.fold`` () = 
        let (randomDictionary, randomMap) = createRandomMapAndDictionary ()
        let maxVal state key value = max state value
        let initialMax = Int32.MinValue 
        dictionaryFold maxVal initialMax randomDictionary 
            |> shouldEqual <| Map.fold maxVal initialMax randomMap
        
    [<Test>]
    member t.``folding hashSet to find sum value should return same result as List.fold`` () = 
        let (randomDictionary, randomMap) = createRandomMapAndDictionary ()
        let sum state key value = state + value
        let initialMax = Int32.MinValue 
        dictionaryFold sum initialMax randomDictionary 
            |> shouldEqual <| Map.fold sum initialMax randomMap
