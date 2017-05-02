namespace Exercises3
module E = 

    (* 9.3
        Declare an iterative solution to exercise 1.6. (
            1.6 Declare a recursive function sum: int * int -> int, where
            sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)        
    *)

    let sum (m, n)= 
        let rec sum state = function
        | (m, neg) when neg < 0 -> 0
        | (m, 0) -> state + m
        | (m, n) -> sum (state + m + n) (m, n-1)

        sum 0 (m, n)

    let rec sumIter (m, n) = 
        let rec sum state (m, n) = 
            if n < 0 then 0
            else if n = 0 then state + m
            else sum (state + m + n) (m, n-1)
        sum 0 (m, n)
