module Exercises1To10Tests
open NUnit.Framework
open FsUnit
open Exercises1To10.E


[<TestFixture>]
type ``upto tests``() = 
    [<Test>]
    member test.``When input is <= 0, output list should have length 0``() = 
        upto 0 |> should haveLength 0
        upto -1 |> should haveLength 0 

    [<Test>]
    member test.``When input is 1, output list should be [1]``() = 
        upto 1 |> should equal [1] 

    [<Test>]
    member test.``When input is 7000, output list should have length 7000``() = 
        upto 7000 |> should haveLength 7000  

// downto1 tests
[<TestFixture>]
type ``downto1 tests``() = 
    [<Test>]
    member test.``When input is <= 0, output list should have length 0``() = 
          downto1 0 |> should haveLength 0
          downto1 -1 |> should haveLength 0     
    
    [<Test>]
    member test.``When input is 7000, output list should have length 7000``() = 
          downto1 7000 |> should haveLength 7000  

    [<Test>]
    member test.``When input is 1, output list should be [1]``() = 
          downto1 1 |> should equal [1] 

    [<Test>]
    member test.``When input is 3, output list should be [3;2;1]``() = 
          downto1 3 |> should equal [3;2;1]  

// evenN tests

[<TestFixture>]
type ``evenN tests``() = 
    [<Test>]
    member test.``When input is <= 0, output list should have length 0``() = 
          evenN 0 |> should haveLength 0
          evenN -1 |> should haveLength 0     
    
    [<Test>]
    member test.``When input is 7000, output list should have length 7000``() = 
          evenN 7000 |> should haveLength 7000  

    [<Test>]
    member test.``When input is 1, output list should be [2]``() = 
          evenN 1 |> should equal [2] 

    [<Test>]
    member test.``When input is 3, output list should be [2;4;6]``() = 
          evenN 3 |> should equal [2;4;6]  

//altsum tests
[<TestFixture>]
type ``altsum tests``() = 
    [<Test>]
    member test.``When input is [], output should be 0``() = 
          altsum [] |> should equal 0          
    
    [<Test>]
    member test.``When input is [2;5], output should be 2-5 =-3``() = 
          altsum [2;5] |> should equal -3  

    [<Test>]
    member test.``When input is [1;2;3;4;5], output should be 1``() = 
          altsum [1;2;3;4;5] |> should equal 3 

// rmodd tests
[<TestFixture>]
type ``rmodd tests``() = 
    [<Test>]
    member test.``When input is [], output should be []``() = 
          rmodd [] |> should equal []     
          rmodd2 [] |> should equal []    
          rmodd3 [] |> should equal []   
    
    [<Test>]
    member test.``When input is [2;5], output should be [2]``() = 
          rmodd [2;5] |> should equal [2]
          rmodd2 [2;5] |> should equal [2]
          rmodd3 [2;5] |> should equal [2]  

    [<Test>]
    member test.``When input is [1;2;3;3;5], output should be [1;3;5]``() = 
          rmodd [1;2;3;3;5] |> should equal [1;3;5] 
          rmodd2 [1;2;3;3;5] |> should equal [1;3;5] 
          rmodd3 [1;2;3;3;5] |> should equal [1;3;5] 

// removeEven tests
[<TestFixture>]
type ``removeEven tests``() = 
    [<Test>]
    member test.``When input is [], output should be []``() = 
          removeEven [] |> should equal [] 
    
    [<Test>]
    member test.``When input is [2;5], output should be [5]``() = 
          removeEven [2;5] |> should equal [5]  

    [<Test>]
    member test.``When input is [1;2;3;3;5], output should be [1;3;3;5]``() = 
          removeEven [1;2;3;3;5] |> should equal [1;3;3;5]   

// multiplicity tests
[<TestFixture>]
type ``multiplicity tests``() = 
    [<Test>]
    member test.``When input is -2 [], output should be 0``() = 
          multiplicity -2 [] |> should equal 0
          multiplicity2 -2 [] |> should equal 0 
    
    [<Test>]
    member test.``When input is 2 [2;5], output should be 1``() = 
          multiplicity 2 [2;5] |> should equal 1  
          multiplicity2 2 [2;5] |> should equal 1  

    [<Test>]
    member test.``When input is 3 [1;2;3;3;5], output should be 2``() = 
          multiplicity 3 [1;2;3;3;5] |> should equal 2
          multiplicity2 3 [1;2;3;3;5] |> should equal 2

    [<Test>]
    member test.``When input is 11 [1;2;3;3;5], output should be 0``() = 
          multiplicity 11 [1;2;3;3;5] |> should equal 0 
          multiplicity2 11 [1;2;3;3;5] |> should equal 0

// split tests
[<TestFixture>]
type ``split tests``() = 
    [<Test>]
    member test.``When input is [], output should be ([],[])``() = 
          split [] |> should equal ([],[])
          split2 [] |> should equal ([],[]) 
    
    [<Test>]
    member test.``When input is [2;5], output should be ([2],[5])``() = 
          split [2;5] |> should equal ([2],[5])  
          split2 [2;5] |> should equal ([2],[5])  

    [<Test>]
    member test.``When input is [1;2;3;3;5], output should be ([1;3;5],[2;3])``() = 
          split [1;2;3;3;5] |> should equal ([1;3;5],[2;3])
          split2 [1;2;3;3;5] |> should equal ([1;3;5],[2;3])
  

// zip tests
[<TestFixture>]
type ``zip tests``() = 
    [<Test>]
    member test.``When input is ([],[]), output should be []``() = 
          zip ([],[]) |> should equal []          
    
    [<Test>]
    member test.``When input is ([],[2;5]), output should throw exception``() = 
          (fun () -> zip ([],[2;5]) |> ignore) |> should throw typeof<System.Exception> 

    [<Test>]
    member test.``When input is 3 ([2;5], []), output should throw exception``() = 
          (fun () -> zip ([2;5], []) |> ignore ) |> should throw typeof<System.Exception> 
          
    [<Test>]
    member test.``When input is  ([1;2;3],[3;5;6]), output should be [(1,3);(2,5);(3,6)]``() = 
          zip  ([1;2;3],[3;5;6]) |> should equal [(1,3);(2,5);(3,6)] 
          
// prefix tests
[<TestFixture>]
type ``prefix tests``() = 
    [<Test>]
    member test.``When input is [] [], output should be true``() = 
        prefix [] [] |> should be True          
    
    [<Test>]    
    member test.``When input is [1;2] [1;2], output should be true``() = 
        prefix [1;2] [1;2] |> should be True

    [<Test>]
    member test.``When input is [1;2;3] [1;2], output should be false``() = 
          prefix [1;2;3] [1;2] |> should be False
          
    [<Test>]
    member test.``When input is [1;2;3] [1;2;3;5;6], output should be true``() = 
          prefix [1;2;3] [1;2;3;5;6] |> should be True
          