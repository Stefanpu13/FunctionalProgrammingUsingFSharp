namespace Exercises3
module E =
    open System.Text
    open System.Collections.Generic
    open Exercises1
    
    
    (* 10.3
        This exercise is a continuation of Exercise 10.1.
        1. We do not consider the hyphen character “-” a proper character in a word. Make a function
        to capture the list of words in a string while removing any hyphen character.
    *)        

    let addWordsWithoutHyphens words (line:string) = 
        let lineWithotuHyphens = line.Replace("-", "")
        let addWords = Exercises1.E.Words.createAddWords null

        addWords words lineWithotuHyphens