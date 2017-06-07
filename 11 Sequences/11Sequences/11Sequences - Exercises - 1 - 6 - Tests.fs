module Exercises1Tests
#if INTERACTIVE
#I @"bin\Debug"
#r "nunit.framework.dll"
#r  "FsUnit.NUnit.dll"
#endif

open NUnit.Framework
open FsUnitTyped
open Exercises1.E
open System
let rand = Random ()

// Test odd numbers
[<TestFixture>]
type ``Test odd numbers``() =   

    [<Test>]
    member t.``Random sequence element should be odd`` () = 
        (Seq.item (rand.Next(0, 100)) odd) % 2 |> shouldEqual <| 1
        
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
    member t.``For Random number between 1..10 "factoriels" and "cachedFactoriels" and "cachedFactoriels2" should equal`` () =          
        let randomNum = rand.Next(1, 10)
        let initValue = Seq.item randomNum factoriels
        List.map (Seq.item randomNum) [cachedFactoriels;cachedFactoriels2] |> shouldEqual <| List.replicate 2 initValue

// Test sublist
[<TestFixture>]
type ``Test sublist``() =   

    [<Test>]
    member t.``If i < 0 or i > n list should be empty`` () = 
        sublist [1..5] -2  2 |> shouldBeEmpty
        

    [<Test>]
    member t.``For Random number between 1..10 "factoriels" and "cachedFactoriels" and "cachedFactoriels2" should equal`` () =          
        let randomNum = rand.Next(1, 10)
        let initValue = Seq.item randomNum factoriels
        List.map (Seq.item randomNum) [cachedFactoriels;cachedFactoriels2] |> shouldEqual <| List.replicate 2 initValue
        
        
    