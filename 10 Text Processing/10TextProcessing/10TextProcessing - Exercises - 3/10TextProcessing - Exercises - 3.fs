namespace Exercises3
module E =
    open System.Text
    open System.Collections.Generic
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