module Exercises3Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E
open Exercises3.E
open System
open System.IO
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

    [<Test>]
    member t.``If words with hyphens are passed, result should concat words with hyphens into whole words`` () = 
        let line = "A few simple words with two ad-hoc hyphen-words."        
        let dict =  Dictionary<string, int> ()       
        
        ["a", 1; "few", 1; "simple", 1; "words", 1; "with", 1;"two", 1; "adhoc", 1; "hyphenwords", 1]             
            |> Seq.iter dict.Add

        addWordsWithoutHyphens (Dictionary<string, int>()) line |> shouldEqual <| dict
        
// Test addSeparatedWordsWithoutHyphens
[<TestFixture>]
type ``Test addSeparatedWordsWithoutHyphens``() =


    [<Test>]
    member t.``If words with hyphens are passed, result should concat words with hyphens into whole words`` () = 
        let line = "A few simple words with two ad-hoc hyphen-words."        
        let dict =  Dictionary<string, int> ()       
        
        ["a", 1; "few", 1; "simple", 1; "words", 2; "with", 1;"two", 1; "ad", 1; "hoc", 1; "hyphen", 1]             
            |> Seq.iter dict.Add

        addSeparatedWordsWithoutHyphens (Dictionary<string, int>()) line |> shouldEqual <| dict

// Test addWordsFromLine2
[<TestFixture>]
type ``Test addWordsFromLine2``() =


    [<Test>]
    member t.``If words with hyphens are passed, result should concat words with hyphens into whole words`` () = 
        let lines = ["A few simple words with two ad-hoc hyphen-words."; "Also, see how longer words are hyphen-"; "nated."]
        // Console.WriteLine("Directory is.............................." + Files.inputDir @"10TextProcessing - Exercises - 3")
        
        File.WriteAllLines ((Files.inputDir @"\10TextProcessing - Exercises - 3"), lines)

        wordCount2 (Files.inputDir @"\10TextProcessing - Exercises - 3") (Files.outputDir @"\10TextProcessing - Exercises - 3")

        // Directory.GetCurrentDirectory() |> shouldEqual <| Files.baseDir


        // Files.inputDir @"10TextProcessing - Exercises - 3" |> shouldEqual <| Files.inputDir @"10TextProcessing - Exercises - 31"
