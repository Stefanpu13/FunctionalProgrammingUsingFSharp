module Exercises5Tests
open NUnit.Framework
open FsUnitTyped
open Exercises5.E

// isMember tests
[<TestFixture>]
type ``isMember tests``() =
    [<Test>]
    member t.``When input list is empty, should return false`` () = 
        isMember 1 [] |> shouldEqual false   
        isMember2 1 [] |> shouldEqual false

    [<Test>]
    member t.``When input list is [2; 3] and search term is 1, should return false`` () = 
        isMember 1 [2; 3] |> shouldEqual false   
        isMember2 1 [2; 3] |> shouldEqual false

    [<Test>]
    member t.``When input list is [2; 3] and search term is 2, should return true`` () = 
        isMember 2 [2; 3] |> shouldEqual true   
        isMember2 2 [2; 3] |> shouldEqual true

// canBeExtBy tests
[<TestFixture>]
type ``canBeExtBy tests``() =
    [<Test>]
    member t.``col is ["a"], map is [("a", "b"); ("c",  "d");], coutry is "b", should return false`` () = 
        let exMap = [("a", "b"); ("c",  "d");]
        let col =  ["a"]
        canBeExtBy exMap col "b" |> shouldEqual false
        canBeExtBy2 exMap col "b" |> shouldEqual false       

    [<Test>]
    member t.``col is ["a"], map is [("a", "b"); ("c",  "d");], coutry is "c", should return true`` () = 
        let exMap = [("a", "b"); ("c",  "d");]
        let col =  ["a"]
        canBeExtBy exMap col "c" |> shouldEqual true
        canBeExtBy2 exMap col "c" |> shouldEqual true  

// extColouring tests
[<TestFixture>]
type ``extColouring tests``() =
    [<Test>]
    member t.``extColouring. cols are [["a"];["b"]], map is [("a", "b"); ("c",  "d");], coutry is "c", should return  [["a"; "c"];["b"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a")]
        let cols =  [["a"];["b"]]
        extColouring exMap cols "c" |> shouldEqual [["c";"a"];["b"]]

    [<Test>]
    member t.``extColouring2. cols are [["a"];["b"]], map is [("a", "b"); ("c",  "d");], coutry is "c", should return  [["c";"a"];["b"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d");]
        let cols =  [["a"];["b"]]
        extColouring2 exMap cols "c" |> shouldEqual [["c";"a"];["b"]]

    [<Test>]
    member t.``extColouring. cols are [["a"];["b"]], map is [("a", "b"); ("c",  "d"); ("d","a")], coutry is "d", should return  [["a"];["d";"b"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a"); ]
        let cols =  [["a"];["b"]]
        extColouring exMap cols "d" |> shouldEqual [["a"];["d";"b"]]

    [<Test>]
    member t.``extColouring2. cols are [["a"];["b"]], map is [("a", "b"); ("c",  "d"); ("d","a")], coutry is "d", should return  [["a"];["d";"b"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a")]
        let cols =  [["a"];["b"]]
        extColouring2 exMap cols "d" |> shouldEqual [["a"];["d";"b"]]

    [<Test>]
    member t.``extColouring. cols are [], map is [("a", "b"); ("c",  "d"); ("d","a")], coutry is "a", should return  [["a"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a"); ]
        let cols =  []
        extColouring exMap cols "a" |> shouldEqual [["a"]]

    [<Test>]
    member t.``extColouring2. cols are [], map is [("a", "b"); ("c",  "d"); ("d","a")], coutry is "a", should return [["a"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a")]
        let cols =  []
        extColouring2 exMap cols "a" |> shouldEqual [["a"]]

    [<Test>]
    member t.``extColouring. cols are [["a"]], map is [("a", "b"); ("c",  "d"); ("d","a")], coutry is "b", should return  [["a"]; ["b"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a"); ]
        let cols =  [["a"]]
        extColouring exMap cols "b" |> shouldEqual [["a"]; ["b"]]

    [<Test>]
    member t.``extColouring2. cols are [["a"]], map is [("a", "b"); ("c",  "d"); ("d","a")], coutry is "b", should return [["a"]; ["b"]]`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a")]
        let cols =  [["a"]]
        extColouring2 exMap cols "b" |> shouldEqual [["a"]; ["b"]]


// countries tests
[<TestFixture>]
type ``countries tests``() =
    [<Test>]
    member t.``countries. map is [("a", "b"); ("c",  "d");], should return  ["a";"b";"c";"d"]`` () = 
        let exMap = [("a", "b"); ("c",  "d");]
        
        countries exMap |> shouldEqual ["a";"b";"c";"d"]


    [<Test>]
    member t.``countries2. map is [("a", "b"); ("c",  "d");], should return  ["a";"b";"c";"d"]`` () = 
        let exMap = [("a", "b"); ("c",  "d");]
        
        countries2 exMap |> shouldEqual ["a";"b";"c";"d"]

// colMap tests
[<TestFixture>]
type ``colMap tests``() =
    [<Test>]
    member t.``colMap tests`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a")]
        colMap exMap |> shouldEqual [["c"; "a"]; ["b"; "d"]]

    [<Test>]
    member t.``colMap2 tests`` () = 
        let exMap = [("a", "b"); ("c",  "d"); ("d","a")]       
        colMap2 exMap |> shouldEqual [["c"; "a"]; ["b"; "d"]]
  