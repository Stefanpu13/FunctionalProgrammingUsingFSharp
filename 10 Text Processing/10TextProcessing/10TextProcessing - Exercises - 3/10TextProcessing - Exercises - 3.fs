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

    let addWordsWithoutHyphens words (line:string) = 
        let lineWithoutHyphens = line.Replace("-", "")        
        let addWords = Words.createAddWords (@"\G\W*(?:(\w+-*\w*)\W*)*$", None)

        addWords words lineWithoutHyphens

    let addSeparatedWordsWithoutHyphens words (line:string) = 
        let lineWithoutHyphens = line.Replace("-", " ")
        
        let addWords = Words.createAddWords (@"\G\W*(?:(\w+-*\w*)\W*)*$", None)

        addWords words lineWithoutHyphens 

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

    let getWordsInLine (regexStr:string) (line: string) =
        let regex = Regex regexStr 
        let m = regex.Match (line.Replace("-", "").ToLower()) 
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

    getWordsInLine @"\G\W*(?:(\w+-*\w*)\W*)*$" "some so-so basic words with hyphens-"
    (*
        For the whole file:
        1. read line
        2. addWordsFromLine 
            - getWordsInLine
            3. If revious line last word ends with hyphen
                - prevLine.lastWord + currentLine.firstWord::restWordsInCurrentLine
            4. For each word in line add to dictionary (List.fold addWord wordsDict wordsList)
    *)

    let private addWord (words:Dictionary<string, int>) word = 
        match  words.TryGetValue word with
        | true,  count -> 
            words.[word] <- count + 1                 
        | false, _ ->  
            words.Add(word, 1)
        words

    let addWordsFromLine (words, previousLineWords) currentLineWords = 
        let mergeHyphenatedWord  = function
        | (previousLine, Some word), (currentLine, lastWord) ->            
            match currentLine with
            | firstWord::remainingWords ->
                (word + firstWord)::remainingWords, lastWord
            | [] -> 
                (currentLine, lastWord)
        | (previousLine, None), (currentLine, lastWord) -> 
            (currentLine, lastWord) 

        let (newCurrentLine, lastWord) = mergeHyphenatedWord (previousLineWords, currentLineWords)
        (List.fold addWord words newCurrentLine), currentLineWords
