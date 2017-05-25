namespace Exercises1
open System
open System.IO
open System.Collections.Generic
open System.Text.RegularExpressions
#if INTERACTIVE
#load "../TextProcessing.fs"
#endif

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

    module Files =
        let baseDir = Directory.GetCurrentDirectory() + @"\10TextProcessing"       

        let inputDir parentDir = baseDir + parentDir + @"\input.txt"
        let outputDir parentDir = baseDir + parentDir + @"\output.txt"



    module Words =         
        let wordRegex =  @"\G[^А-я0-9]*(?:([А-я0-9]+(?:-*)[А-я0-9]*)[^А-я0-9]*)*$"

        let countWord (words:Dictionary<string, int>) word = 
            match  words.TryGetValue word with
            | true,  count -> 
                words.[word] <- count + 1                 
            | false, _ ->  
                words.Add(word, 1)
            words

        let countWords (wordRegex:Regex) words (line: string) =
            let m = wordRegex.Match (line.ToLower())
            if m.Success 
            then           
                List.fold countWord words (captureList m 1)
            else 
                words

        let private getWordRegex regexStr = 
            if String.IsNullOrEmpty regexStr then Regex wordRegex else Regex regexStr
        let createCountWordsFunc  = function
        | regexStr, None ->  countWords (getWordRegex regexStr)         
        | regexStr, Some countWordsFunc -> countWordsFunc (getWordRegex regexStr)  

        let createOutputFileContent allWords = 
            allWords
                |> Seq.sortBy(fun (KeyValue(k, v)) -> k)
                |> Seq.map (fun (KeyValue(k, v)) -> k + " " + v.ToString())
                |> String.concat Environment.NewLine

        let private wordCount getAllWords createOutputFileContent inputFile outputFile = 
            if File.Exists inputFile
            then             
                let allWords = getAllWords inputFile
                let output = createOutputFileContent allWords

                File.WriteAllText (outputFile, output)
            else 
              failwith "file not found"            

        let create (regex, (countWords, initialState) , writeToOutputFile) =                 
            let getAllWords = (fileFold (countWords (Regex regex)) initialState)
            wordCount getAllWords writeToOutputFile        

    #if INTERACTIVE 
    #time
    let baseDir =  Directory.GetCurrentDirectory() + @"\10TextProcessing\10TextProcessing - Exercises - 1"
    let countWordsDefaultPart = (Words.countWords, Dictionary<string, int>())
    let wordCount = Words.create (Words.wordRegex, countWordsDefaultPart, Words.createOutputFileContent)
    wordCount (baseDir + @"\files\input.txt") (baseDir + @"\files\output.txt")

    let wordCount2 = Words.create (@"\G\W*(?:(\w+-*\w*)\W*)*$", countWordsDefaultPart, Words.createOutputFileContent)
    wordCount2 (baseDir + @"\files\input.txt") (baseDir + @"\files\output.txt")    
    #endif
    