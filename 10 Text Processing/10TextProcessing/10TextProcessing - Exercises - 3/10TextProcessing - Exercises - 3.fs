namespace Exercises3
module E =
    open System.Text
    open System.Text.RegularExpressions
    open System.Collections.Generic
    open TextProcessing.TextProcessing
    open Exercises1.E
    
    (* 10.3
        This exercise is a continuation of Exercise 10.1.
        1. We do not consider the hyphen character “-” a proper character in a word. Make a function
        to capture the list of words in a string while removing any hyphen character.
    *)        

    let countWordsWithoutHyphens words (line:string) = 
        let lineWithoutHyphens = line.Replace("-", "")        
        let addWords = Words.createCountWordsFunc (@"\G\W*(?:(\w+-*\w*)\W*)*$", None)

        addWords words lineWithoutHyphens

    let countSeparatedWordsWithoutHyphens words (line:string) = 
        let lineWithoutHyphens = line.Replace("-", " ")        
        let countWords = Words.createCountWordsFunc (@"\G\W*(?:(\w+-*\w*)\W*)*$", None)

        countWords words lineWithoutHyphens 

    (*
      2.Make a function of type string -> (string list)*(string option) removing
        hyphen characters like the previous one, but treating the last word in the line in a special
        way: we get the result
        ([word0; . . . ;wordn−1],None)
        if the last word in the string does not end with a hyphen character, and
        ([word0; . . . ;wordn−2], Some wordn−1)
        if the last word terminates with a hyphen character.
        Make a new version of the WordCount program in Exercise 10.1 that in general ignores hyphen
        characters but handles words that are divided from a text line to the next by means of a
        hyphen character.   
    *)

    let getWordsInLine (regex:Regex) (line: string) =        
        let m = regex.Match (line.Replace("-","")) 
        if m.Success 
        then           
            let words = captureList m 1
            if line.[line.Length - 1] = '-'
            then 
                let reversed = List.rev words
                let (firstWords, lastWord) = (List.tail reversed, List.head reversed)
                List.rev firstWords, Some lastWord  
            else
                words, None        
        else 
            [], None  

    let countWordsFromLine (wordRegex:Regex) (words, previousLineWords) (currentLine:string) = 
        let mergeHyphenatedWord = function
        | (previousLineWords, Some word), (firstWord::remainingWords, lastWord) ->
            (word + firstWord)::remainingWords, lastWord
        | _ , (currentLine, lastWord) -> 
            (currentLine, lastWord) 

        let wordsInLine = getWordsInLine wordRegex currentLine
        let (newCurrentLine, lastWord) = mergeHyphenatedWord (previousLineWords, wordsInLine)
        (List.fold Words.countWord words newCurrentLine), wordsInLine

    let wordCount = 
        let regexStr = @"\G\W*(?:(\w+-*\w*)\W*)*$"
        
        let createOutputFileContent (words, previousLineWords) = Words.createOutputFileContent words
        let addWordsPart = countWordsFromLine, (Dictionary<string, int> (), ([], None))


        Words.create (regexStr, addWordsPart,  createOutputFileContent)    
