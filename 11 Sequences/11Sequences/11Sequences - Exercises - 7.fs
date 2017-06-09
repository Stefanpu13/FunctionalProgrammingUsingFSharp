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

    (* 11.7.2
        Declare a function that accumulates the elements of a sequence of floats. I.e. given a sequence
        seq [x0; x1; x2; . . .] it generates the sequence seq [x0; x0 + x1; x0 + x1 + x2; . . .].
    *)

    let accumulateSeq (seq:seq<float>) =
        let res, st = Seq.mapFold (fun sum el-> (sum + el, sum + el)) 0.0 seq
        res        

    accumulateSeq [1.0..4.0]
    accumulateSeq Seq.empty        