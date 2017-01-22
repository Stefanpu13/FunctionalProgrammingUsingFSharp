module HelpersTests
open Helpers.L
open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``rev tests``() =
    
    [<Test>]
    member t.``When input list is empty should return empty list`` () = 
        rev [] |> should equal []    

    [<Test>]
    member t.``When input list is [1] should return [1]`` () = 
        rev [1] |> should equal [1]

    [<Test>]
    member t.``When input list is [1; 2; 3] should return [3; 2; 1]`` () =
        rev [1; 2; 3] |> should equal [3; 2; 1]

    [<Test>]
    member t.``When input list is string list like ["a"; "b"; "c"] should return ["c"; "b"; "a"]`` () =
        rev ["a"; "b"; "c"] |> should equal ["c"; "b"; "a"]

// filter tests
[<TestFixture>]
type ``filter tests``() =
    
    [<Test>]
    member t.``When input list is empty should return empty list`` () = 
        filter (fun i -> true) [] |> should equal []    

    [<Test>]
    member t.``When input list is [1...10] and filter checks for even number it should return [2; 4; 6; 8; 10]`` () = 
        filter (fun i -> i % 2 = 0 ) [1..10] |> should equal [2;4;6;8;10]

    [<Test>]
    member t.``When fitler is false for all values, it should return []`` () =
        filter (fun i -> i >= 3) [1; 2] |> should equal []

    [<Test>]
    member t.``When filter is true for all values, it should return same list`` () =
        filter (fun i -> i <= 3) [1; 2] |> should equal [1; 2]

// partition tests
[<TestFixture>]
type ``partition tests``() =
    
    [<Test>]
    member t.``When input list is empty should return two empty lists`` () = 
        partition (fun i -> true) [] |> should equal ([], [])    

    [<Test>]
    member t.``When input list is [1..4] and filter checks for even number it should return ([2;4],[1;3])`` () = 
        partition (fun i -> i % 2 = 0 ) [1..4] |> should equal ([2;4],[1;3])

// indexed tests
[<TestFixture>]
type ``indexed tests``() =
    
    [<Test>]
    member t.``When input list is empty, should return []`` () = 
        indexed [] |> should equal ([])    

    [<Test>]
    member t.``When input list is ["a"; "b"], should return [(0,"a");(1,"b")]`` () = 
        indexed ["a";"b"] |> should equal [(0,"a");(1,"b")]


// map tests
[<TestFixture>]
type ``map tests``() =
    
    [<Test>]
    member t.``When input list is empty, should return []`` () = 
        map (fun i -> i * 2) [] |> should equal ([])    

    [<Test>]
    member t.``When input list is [2; 5] and mapper multiplies by 2, should return [4;10]`` () = 
        map (fun i -> i * 2) [2;5] |> should equal [4;10]

    [<Test>]
    member t.``When input list is ["a"; "b"] and mapper adds "2", should return ["a2";"b2"]`` () = 
        map (fun i -> i + "2") ["a";"b"] |> should equal ["a2";"b2"]


// fold tests
[<TestFixture>]
type ``fold tests``() =
    
    [<Test>]
    member t.``When input list is empty, should return initial state`` () = 
        fold (*) 1 []  |> should equal 1    

    [<Test>]
    member t.``When input list is [2; 5], folder is (+) and state is 2, should return 9`` () = 
        fold (+) 2 [2;5] |> should equal 9

    [<Test>]
    member t.``When input list is ["s"; "t"; "e"], folder is (+) and state is "", should return "ste"`` () = 
        fold (+) "" ["s"; "t"; "e"] |> should equal "ste"

// TODO: foldBackNaive tests
[<TestFixture>]
type ``foldBackNaive tests``() =    
    [<Test>]
    member t.``When input list is empty, should return initial state`` () = 
        // Give full name qualifier to avoid name collision with "unique" in FsUnit
        foldBackNaive (+) [] 0 |> should equal 0  

    [<Test>]
    member t.``When input list is [2; 5], folder is (+) and state is 2, should return 9`` () = 
        foldBackNaive (+) [2;5] 2 |> should equal 9

    [<Test>]
    member t.``When list is [2;3;5], and folder copies list, should return [2;3;5]`` () = 
        foldBackNaive (fun el copy -> el::copy) [2; 3; 5] [] |> should equal [2; 3; 5]        
    

// contains tests 
[<TestFixture>]
type ``contains tests``() =
    
    [<Test>]
    member t.``When input list is empty, should be false`` () = 
        contains 3 [] |> should be False    

    [<Test>]
    member t.``When input list is [2; 5] searched elem is 2, should be true`` () = 
        contains 2 [2;5] |> should be True

    
    [<Test>]
    member t.``When input list is [2; 5] searched elem is 5, should be true`` () = 
        contains 5 [2;5] |> should be True

    [<Test>]
    member t.``When input list is [2; 5] searched elem is 11, should be false`` () = 
        contains 11 [2;5] |> should be False

    
// at tests
[<TestFixture>]
type ``at tests``() =    
    [<Test>]
    member t.``When input list is empty, should return None`` () = 
        at 1 []  |> should equal None    

    [<Test>]
    member t.``When index out of range, should return None`` () = 
        at 2 [2;5] |> should equal None
        at -1 [2;5] |> should equal None

    [<Test>]
    member t.``When index is 0, input list is [2;5], should return Some 2`` () = 
        at 0 [2; 5] |> should equal (Some 2)

    [<Test>]
    member t.``When index is 1, input list is [2;5], should return Some 5`` () = 
        at 1 [2; 5] |> should equal (Some 5)


// unique tests
[<TestFixture>]
type ``unique tests``() =    
    [<Test>]
    member t.``When input list is empty, should return []`` () = 
        // Give full name qualifier to avoid name collision with "unique" in FsUnit
        Helpers.L.unique [] |> should equal []  

    [<Test>]
    member t.``When list is [2;5], should return [2;5]`` () = 
        Helpers.L.unique [2;5] |> should equal [2;5]        

    [<Test>]
    member t.``When list is [2; 5; 2; 2; 5],  should return [2;5]`` () = 
        Helpers.L.unique [2; 5; 2; 2; 5] |> should equal [2;5]

    [<Test>]
    member t.``When list is ["a"; "a"], should return ["a"]`` () = 
        Helpers.L.unique ["a"; "a"] |> should equal ["a"]