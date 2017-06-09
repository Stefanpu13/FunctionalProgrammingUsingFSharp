module Exercises1Tests
#if INTERACTIVE
#I @"bin\Debug"
#r "nunit.framework.dll"
#r  "FsUnit.NUnit.dll"
#endif

open NUnit.Framework
open FsUnitTyped
open Exercises1to6.E
open System

let rand = Random ()

// Test odd numbers
[<TestFixture>]
type ``Test odd numbers``() =       

    [<Test>]
    member t.``Random sequence element should be odd`` () = 
        (Seq.item (4) odd) % 2 |> shouldEqual <| 1
        
    [<Test>]
    member t.``List of 3th, 4th, 5th elem  should equal [5; 7; 9]`` () = 
        ([3..5] |> List.map( fun i -> Seq.item (i - 1) odd)) |> shouldEqual <| [5; 7; 9]

// Test factoriels
[<TestFixture>]
type ``Test factoriels``() =       

    [<Test>]
    member t.``List of 3th, 4th, 5th elem  should equal [2; 6; 24] `` () = 
        ([3..5] |> List.map( fun i -> Seq.item (i - 1) factoriels)) |> shouldEqual <| [2; 6; 24]

    [<Test>]
    member t.``For Random number between 1..10 "factoriels" and "cachedFactoriels" and "cachedFactoriels2" should be equal`` () =          
        let randomNum = rand.Next(1, 10)
        let initValue = Seq.item randomNum factoriels
        List.map (Seq.item randomNum) [cachedFactoriels;cachedFactoriels2] |> shouldEqual <| List.replicate 2 initValue

// Test sublist
[<TestFixture>]
type ``Test sublist``() =       

    [<Test>]
    member t.``If i < 0 list should be empty`` () = 
        sublist [1..5] -2  2 |> shouldBeEmpty
        
    [<Test>]
    member t.``If n <= 0 list should be empty`` () = 
        sublist [1..5] 1 -1 |> shouldBeEmpty    
        sublist [1..5] 1 0 |> shouldBeEmpty

    [<Test>]
    member t.``If input seq is empty,  list should be empty`` () = 
        sublist Seq.empty 1 2|> shouldBeEmpty    
        

    [<Test>]
    member t.``For list of 0 and natural numbers, i = 2, n = 5, should equal [2; 3; 4; 5; 6]`` () =          
        let zeroAndNat = Seq.initInfinite id       

        sublist zeroAndNat 2 5 |> shouldEqual <| seq [2; 3; 4; 5; 6]      

// Test iter
[<TestFixture>]
type ``Test iter``() =       

    [<Test>]
    member t.``iter zero times should return initial value`` () = 
        let iterFuncs =  
            [iter;iter2;iter3]
            |> List.map (fun f -> f ((+) 1) 1)    
        
        iterFuncs |> List.map (fun f -> f 0) |> shouldEqual <| [1;1;1]

    [<Test>]
    member t.``iter random times should return same value for iter;iter2;iter3`` () = 
        let initIter iterFunc = iterFunc ((+) 1) 1
        iter (fun iters-> 
            let randNum = rand.Next(3, 200)
            let itersResults = List.map (fun iterFunc -> initIter iterFunc randNum) iters

            itersResults |> shouldEqual <| List.replicate (List.length itersResults) (initIter iter randNum)

            iters 
        ) [iter;iter2;iter3] 10 |> ignore
    
// Test sRoot
[<TestFixture>]
type ``Test sRoot``() =           

    [<Test>]
    member t.``Square root of random num should return same value for sRoot; sRoot2`` () = 
        let initIter iterFunc = iterFunc ((+) 1) 1
        iter (fun sRootFuncs-> 
            let randNum = float (rand.Next(3, 200000))
            let sRootResults = List.map (fun sRootFunc -> sRootFunc randNum) sRootFuncs

            sRootResults |> shouldEqual <| List.replicate (List.length sRootResults) (sRoot randNum)

            sRootFuncs 
        ) [sRoot;sRoot2] 10 |> ignore
    