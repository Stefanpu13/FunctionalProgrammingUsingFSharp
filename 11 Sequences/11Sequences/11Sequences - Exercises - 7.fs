namespace Exercises7

open System.Collections.Generic

module E =     
    (* 11.7
        The exponential functions can be approximated using the Taylor’s series:
        ex =1/0! + x^1/1! + · · · + x^k/k! +  · · · (11.2)
    *)
    (*11.7.1
        Declare a function that for a given x can generate the sequence of summands in (11.2). Hint:
        Notice that the next summand can be generated from the previous one.
    *)

    let generateSummands x = 
        Seq.unfold (fun (up, down, i) -> 
            let el = up, down
            let state = up * x, down * (i), i+1 
            Some (el, state)
        ) (1, 1, 1)

    // generateSummands 2
