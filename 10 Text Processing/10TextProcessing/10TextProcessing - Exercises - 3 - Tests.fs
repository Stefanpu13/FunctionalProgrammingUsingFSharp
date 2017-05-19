module Exercises3Tests
open NUnit.Framework
open FsUnitTyped
open Exercises3.E
open System
open System.Collections.Generic

// Test addWordsWithoutHyphens
[<TestFixture>]
type ``Test addWordsWithoutHyphens``() =

    [<Test>]
    member t.``If no words with hyphens are passed, result should look line normal addWords`` () = 
        let line = "A few simple words without hyphens."        
        let dict =  Dictionary<string, int> ()       
        
        ["a", 1; "few", 1; "simple", 1; "words", 1; "without", 1; "hyphens", 1]             
            |> Seq.iter dict.Add

        addWordsWithoutHyphens (Dictionary<string, int>()) line |> shouldEqual <| dict
        