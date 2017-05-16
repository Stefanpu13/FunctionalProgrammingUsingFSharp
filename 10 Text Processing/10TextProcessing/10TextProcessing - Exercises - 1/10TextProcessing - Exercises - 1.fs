namespace Exercises1
open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
open TextProcessing.TextProcessing
module E = 
    (* 10.1
        The term “word” is used in this exercise to denote a string not containing blank characters. The
        blank characters of a string do hence divide the string into words. Make a program WordCount
        that is called with two parameters:
        WordCount inputFile outputFile
        The program should read the input file and produce an output file where each line contains a
        word found in the input file together with the number of occurrences, for example, “peter 3”.
        The words should appears in alphabetic order and the program should not distinguish between
        small and capital letters.
    *)
    // let wordRegex = Regex @"\G\W*(?:(\w+-*\w*)\W*)*$"

    module Words =         
        let private wordRegex = Regex @"\G[^А-я0-9]*(?:([А-я0-9]+(?:-*)[А-я0-9]*)[^А-я0-9]*)*$"

        //Provide the ability to pass regex for words
        // let wordRegex2 = @"\G\W*(?:(\w+-*\w*)\W*)*$"



        let private addWord (words:Dictionary<string, int>) word = 
            match  words.TryGetValue word with
            | true,  count -> 
                words.[word] <- count + 1                 
            | false, _ ->  
                words.Add(word, 1)
            words

        let private addWords (wordRegex:Regex) words (line: string) =
            let m = wordRegex.Match (line.ToLower())
            if m.Success 
            then           
                List.fold addWord words (captureList m 1)
            else 
                words            

        let private wordCount addWords inputFile outputFile = 
            if File.Exists inputFile
            then             
                let allWords = fileFold addWords (Dictionary<string, int> ()) inputFile           
                let output = 
                    allWords
                        |> Seq.sortBy(fun (KeyValue(k, v)) -> k)
                        |> Seq.map (fun (KeyValue(k, v)) -> k + " " + v.ToString())
                        |> String.concat Environment.NewLine
            
                File.WriteAllText (outputFile, output)
            else 
                failwith "file not found"       

        let create regexStr = 
            let neWordTegex = if String.IsNullOrEmpty regexStr then wordRegex else Regex regexStr
            wordCount (addWords neWordTegex) 

    let baseDir =  Directory.GetCurrentDirectory() + @"\10TextProcessing\10TextProcessing - Exercises - 1"
    let wordCount = Words.create null
    wordCount (baseDir + @"\files\input.txt") (baseDir + @"\files\output.txt")

    let wordCount2 = Words.create @"\G\W*(?:(\w+-*\w*)\W*)*$"
    wordCount2 (baseDir + @"\files\input.txt") (baseDir + @"\files\output2.txt")

