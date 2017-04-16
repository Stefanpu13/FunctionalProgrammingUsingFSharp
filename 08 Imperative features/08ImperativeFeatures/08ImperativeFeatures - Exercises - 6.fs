namespace Exercises6
module E = 
    (* 8.6
        Declare a function for computing Fibonacci numbers Fn (see Exercise 1.5) using a while
        loop. Hint: introduce variables to contain the two previously computed Fibonacci numbers.

        Exercise 1.5: 
        The sequence F0, F1, F2, . . . of Fibonacci numbers is defined by:
        F0 = 0
        F1 = 1
        Fn = Fn−1 +Fn−2
        Thus, the first members of the sequence are 0, 1, 1, 2, 3, 5, 8, 13, . . ..
        Declare an F# function to compute Fn. Use a declaration with three clauses, where the patterns
        correspond to the three cases of the above definition.
        Give an evaluations for F4.
    *)

    let fibonacci n =
        let rec fibonacci (``fn-1``, ``fn-2``) = function
        | zeroOrNeg when zeroOrNeg <= 0 -> None
        | 1 -> Some ``fn-2``
        | 2 -> Some ``fn-1``        
        | n -> fibonacci (``fn-1`` + ``fn-2``, ``fn-1``) (n - 1) 

        fibonacci (1, 0) n
    
    let fibonacciImperative = function
        | zeroOrNeg when zeroOrNeg <= 0 -> None
        | 1 -> Some 0
        | 2 -> Some 1
        | n ->
            let mutable counter = n
            let mutable ``fn-1`` = 1
            let mutable ``fn-2`` = 0
            while counter >= 3 do
                let fn = ``fn-1`` + ``fn-2``
                ``fn-2`` <- ``fn-1``
                ``fn-1`` <- fn
                counter <- counter - 1
            Some ``fn-1``
