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
    let wordRegex = Regex @"\G[^А-я0-9]*(?:([А-я0-9]+(?:-*)[А-я0-9]*)[^А-я0-9]*)*$"

    let wordCount inputFile outputFile = 
        let addWord (words:Dictionary<string, int>) word = 
            match  words.TryGetValue word with
            | true,  count -> 
                words.[word] <- count + 1                 
            | false, _ ->  
                words.Add(word, 1)
            words

        let wordCount words (line: string) =
            let m = wordRegex.Match (line.ToLower())
            if m.Success 
            then           
                List.fold addWord words (captureList m 1)
            else 
                words
        
        if File.Exists inputFile
        then             
            let allWords = fileFold wordCount (Dictionary<string, int> ()) inputFile           
            let output = 
                allWords
                    |> Seq.sortBy(fun (KeyValue(k, v)) -> k)
                    |> Seq.map (fun (KeyValue(k, v)) -> k + " " + v.ToString())
                    |> String.concat Environment.NewLine
           
            File.WriteAllText (outputFile, output)
        else 
            failwith "file not found"    

    
    #time
    let baseDir =  Directory.GetCurrentDirectory() + @"\10TextProcessing\10TextProcessing - Exercises - 1"
    wordCount (baseDir + @"\files\input.txt") (baseDir + @"\files\output.txt")

    let writeAllText path contents = File.WriteAllText (path, contents)
    // get all files from given dir except input
    Directory.GetFiles(baseDir + @"\fileResources")
        |> Array.fold (fun allText filePath -> String.Concat [|(File.ReadAllText filePath); allText|]) "" 
        |> writeAllText (baseDir + @"\files\input.txt")
        
    
    // for each file write to input
