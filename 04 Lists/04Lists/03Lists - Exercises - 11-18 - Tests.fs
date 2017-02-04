module Exercises11To18Tests
open NUnit.Framework
open FsUnit
open Exercises11To18.E

// count tests
[<TestFixture>]
type ``count tests``() = 
    [<Test>]
    member test.``Simple check that module loading is working, should load module "Exercises1To10"``() = 
        count ([], 2) |> should equal 0
        count2 ([], 2) |> should equal 0

// insert tests
[<TestFixture>]
type ``insert tests``() = 
    [<Test>]
    member test.``When list is [], and inserted number is 2, should equal  [2]``() = 
        insert ([], 2) |> should equal [2]
        insert2 ([], 2) |> should equal [2]
        insert3 ([], 2) |> should equal [2]

    [<Test>]
    member test.``When list is [1;2;2;3;5], and inserted number is 4, should equal  [1;2;2;3;4;5]``() = 
        insert ([1;2;2;3;5], 4) |> should equal [1;2;2;3;4;5]
        insert2 ([1;2;2;3;5], 4) |> should equal [1;2;2;3;4;5]
        insert3 ([1;2;2;3;5], 4) |> should equal [1;2;2;3;4;5]

// intersect tests
[<TestFixture>]
type ``intersect tests``() = 
    [<Test>]
    member test.``When input is ([], []), should equal []``() = 
        intersect ([], []) |> should equal []        

    [<Test>]
    member test.``When input is ([1;1;1;2;2;2;3;4;4;], [1;1;2;2;2;2;4;]), should equal  [1;1;2;2;2;4]``() =         
        intersect ([1;1;1;2;2;2;3;4;4;], [1;1;2;2;2;2;4;]) |> should equal [1;1;2;2;2;4]

// plus tests
[<TestFixture>]
type ``plus tests``() = 
    [<Test>]
    member test.``When input is ([], []), should equal []``() = 
        plus ([], []) |> should equal []        

    [<Test>]
    member test.``When input is ([1;2;3;4;4;], [1;1;2;2;4;]), should equal  [1;1;1;2;2;2;3;4;4;4]``() =         
        plus ([1;2;3;4;4;], [1;1;2;2;4;]) |> should equal [1;1;1;2;2;2;3;4;4;4]

    [<Test>]
    member test.``When input is ([3;4;4;], [1;1;2;2;4]), should equal  [1;1;2;2;3;4;4;4]``() =         
        plus ([3;4;4;], [1;1;2;2;4]) |> should equal [1;1;2;2;3;4;4;4]

    [<Test>]
    member test.``When one input list is empty, should equal other list``() = 
        plus ([1;2], []) |> should equal [1;2] 
        plus ([], [3;4]) |> should equal [3;4]

// minus tests
[<TestFixture>]
type ``minus tests``() = 
    [<Test>]
    member test.``When input is ([], []), should equal []``() = 
        minus ([], []) |> should equal []        

    [<Test>]
    member test.``When input is ([1;2;3;4;4;], [1;1;2;2;4;]), should equal  [3;4]``() =         
        minus ([1;2;3;4;4;], [1;1;2;2;4;]) |> should equal [3;4]

    [<Test>]
    member test.``When input is ([3;4;4;], [1;1;2;2;4]), should equal  [1;1;2;2;3;4;4;4]``() =         
        minus ([1;1;1;3;4;4;], [1;1;2;2;4]) |> should equal [1;3;4]
    
    [<Test>]
    member test.``When input is ([], [1;2]), should equal []``() = 
        minus ([], [1;2]) |> should equal []

    [<Test>]
    member test.``When input is ([3;4], []), should equal [3;4]``() = 
        minus ([3;4], []) |> should equal [3;4]

// sum tests
[<TestFixture>]
type ``sum tests``() = 
    [<Test>]
    member test.``When predicate is false for all members, should equal 0``() = 
        sum ((fun x -> x > 10), [1;2;3;4]) |> should equal 0       

    [<Test>]
    member test.``When list is empty, should equal 0"``() = 
        sum ((fun x -> true), []) |> should equal 0  

    [<Test>]
    member test.``When predicate is (x > 4), and list is [1;3;4; 5; 2; 4; 6;], should equal 11``() =         
        sum ((fun x -> x > 4), [1;3;4; 5; 2; 4; 6;]) |> should equal 11

    [<Test>]
    member test.``When predicate is (x % 2 = 0), and list is [1;3;4; 5; 2; 4; 6;], should equal 16``() =         
        sum ((fun x -> x % 2 = 0), [1;3;4; 5; 2; 4; 6;]) |> should equal 16

// min tests
[<TestFixture>]
type ``min tests``() = 
    [<Test>]
    member test.``When list is empty, should equal None``() = 
        min [] |> should equal None       

    [<Test>]
    member test.``When list is [2], should equal Some 2``() = 
        min [2] |> should equal (Some 2)  

    [<Test>]
    member test.``When list is [2;3;4; 5; 1; 4; 6;], should equal 1``() =         
        min [2;3;4; 5; 1; 4; 6;] |> should equal (Some 1)

// delete tests
[<TestFixture>]
type ``delete tests``() = 
    [<Test>]
    member test.``When list is [], should equal []``() = 
        delete (5, []) |> should equal []       

    [<Test>]
    member test.``When number to delete is 2, and list is [3;4;5], should equal [3;4;5]``() = 
        delete (2, [3;4;5]) |> should equal [3;4;5] 

    [<Test>]
    member test.``When number to delete is 4, and list is [2;3;4; 5; 4; 6], should equal [2;3; 5; 4; 6]``() =         
        delete (4, [2;3;4; 5; 4; 6]) |> should equal [2;3; 5; 4; 6]


// sort tests
[<TestFixture>]
type ``sort tests``() = 
    [<Test>]
    member test.``When list is [], should equal []``() = 
        sort [] |> should equal []       

    [<Test>]
    member test.``When list is [2], should equal [2]``() = 
        sort [2] |> should equal [2]       

    [<Test>]
    member test.``When list is [5; 4], should equal [4; 5]``() = 
        sort[5; 4] |> should equal [4;5] 

    [<Test>]
    member test.``When list is [3; 4; 2; 5; 4; 3], should equal [2;3;3;4;4;5]``() =         
        sort [3; 4; 2; 5; 4; 3] |> should equal [2;3;3;4;4;5]

// revrev tests
[<TestFixture>]
type ``revrev tests``() = 
    [<Test>]
    member test.``When list is [[];[]], should equal [[];[]]``() = 
        revrev [[];[]] |> should equal [[];[]]       

    [<Test>]
    member test.``When list is [[];[2]], should equal [[2];[]]``() = 
        revrev [[];[2]] |> should equal [[2];[]]

    [<Test>]
    member test.``When list is [[1;2];[3;4;5]], should equal [[5;4;3];[2;1]]``() = 
        revrev [[1;2];[3;4;5]] |> should equal [[5;4;3];[2;1]]  

    [<Test>]
    member test.``When list is [[1;2];[3;4;5];[6;7;8;9]], should equal [[9;8;7;6];[5;4;3];[2;1]]``() = 
        revrev [[1;2];[3;4;5];[6;7;8;9]] |> should equal [[9;8;7;6];[5;4;3];[2;1]]

    [<Test>]
    member test.``When list is  [['a'; 'c']; ['y'; 'x']], should equal  [['x'; 'y']; ['c'; 'a']]``() =         
          revrev [['a'; 'c']; ['y'; 'x']] |> should equal [['x'; 'y']; ['c'; 'a']]

