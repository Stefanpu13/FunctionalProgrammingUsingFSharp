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
        let addWords = Words.createAddWords @"\G\W*(?:(\w+-*\w*)\W*)*$"

        addWords words lineWithoutHyphens

    let addSeparatedWordsWithoutHyphens words (line:string) = 
        let lineWithoutHyphens = line.Replace("-", " ")
        let addWords = Words.createAddWords @"\G\W*(?:(\w+-*\w*)\W*)*$"

        addWords words lineWithoutHyphens 