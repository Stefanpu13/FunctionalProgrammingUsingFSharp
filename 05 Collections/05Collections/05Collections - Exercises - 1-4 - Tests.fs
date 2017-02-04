module Exercises1To4Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1To4.E

// filter using foldBack tests
[<TestFixture>]
type ``filter using foldBack tests``() =
    [<Test>]
    member t.``When input list is empty should return empty list`` () = 
        filter (fun i -> true) [] |> shouldEqual []    

    [<Test>]
    member t.``When input list is [1...10] and filter checks for even number it should return [2; 4; 6; 8; 10]`` () = 
        filter (fun i -> i % 2 = 0 ) [1..10] |> shouldEqual [2;4;6;8;10]

    [<Test>]
    member t.``When fitler is false for all values, it should return []`` () =
        filter (fun i -> i >= 3) [1; 2] |> shouldEqual []

    [<Test>]
    member t.``When filter is true for all values, it should return same list`` () =
        filter (fun i -> i <= 3) [1; 2] |> shouldEqual [1; 2]

// revrev tests
[<TestFixture>]
type ``revrev tests``() = 
    [<Test>]
    member test.``When list is [[];[]], should equal [[];[]]``() = 
        revrev [[];[]] |> shouldEqual [[];[]]       

    [<Test>]
    member test.``When list is [[];[2]], should equal [[2];[]]``() = 
        revrev [[];[2]] |> shouldEqual [[2];[]]
    [<Test>]
    member test.``When list is [[1;2];[3;4;5]], should equal [[5;4;3];[2;1]]``() = 
        revrev [[1;2];[3;4;5]] |> shouldEqual [[5;4;3];[2;1]]  

    [<Test>]
    member test.``When list is [[1;2];[3;4;5];[6;7;8;9]], should equal [[9;8;7;6];[5;4;3];[2;1]]``() = 
        revrev [[1;2];[3;4;5];[6;7;8;9]] |> shouldEqual [[9;8;7;6];[5;4;3];[2;1]]

    [<Test>]
    member test.``When list is  [['a'; 'c']; ['y'; 'x']], should equal  [['x'; 'y']; ['c'; 'a']]``() =         
          revrev [['a'; 'c']; ['y'; 'x']] |> shouldEqual [['x'; 'y']; ['c'; 'a']]

// sum tests
[<TestFixture>]
type ``sum tests``() = 
    [<Test>]
    member test.``When predicate is false for all members, should equal 0``() = 
        sum ((fun x -> x > 10), [1;2;3;4]) |> shouldEqual 0       

    [<Test>]
    member test.``When list is empty, should equal 0"``() = 
        sum ((fun x -> true), []) |> shouldEqual 0  

    [<Test>]
    member test.``When predicate is (x > 4), and list is [1;3;4; 5; 2; 4; 6;], should equal 11``() =         
        sum ((fun x -> x > 4), [1;3;4; 5; 2; 4; 6;]) |> shouldEqual 11

    [<Test>]
    member test.``When predicate is (x % 2 = 0), and list is [1;3;4; 5; 2; 4; 6;], should equal 16``() =         
        sum ((fun x -> x % 2 = 0), [1;3;4; 5; 2; 4; 6;]) |> shouldEqual 16
    

// downto1 tests
[<TestFixture>]
type ``downto1 tests``() = 

    [<Test>]
    member test.``downto1 when function is sum and n <= 0, e = 4, should equal e``() = 
        downto1 (fun (num, res) -> num + res) 0 4 |> shouldEqual 4
        downto1 (fun (num, res) -> num + res) -2 4 |> shouldEqual 4
    [<Test>]
    member test.``downto1 when function is sum and n = 3, e = 4, should equal 4+1+2+3 = 10``() = 
        downto1 (fun (num, res) -> num + res) 3 4 |> shouldEqual 10

    [<Test>]
    member test.``factorial when n = 4, should return 24"``() = 
        factorial 4 |> shouldEqual 24

    [<Test>]
    member test.``mapToN when function is ((*) 2) and n = 4, should return [2; 4; 6; 8]"``() = 
        mapToN ((*) 2) 4 |> shouldEqual [2; 4; 6; 8]
    