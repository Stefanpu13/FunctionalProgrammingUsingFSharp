(* 1.4
    Declare a recursive function f: int -> int, where
    f(n) = 1+2+· · · + (n − 1) + n
*)

let sumToN n = 
    let rec sum acc = function
        | negative when negative < 0 -> 0
        | 0 -> acc
        | n -> sum (acc+n) (n-1)
    
    sum 0 n

sumToN 4

(* 1.5
    The sequence F0, F1, F2, . . . of Fibonacci numbers is defined by:
    F0 = 0
    F1 = 1
    Fn = Fn−1 +Fn−2
    Thus, the first members of the sequence are 0, 1, 1, 2, 3, 5, 8, 13, . . ..
    Declare an F# function to compute Fn. Use a declaration with three clauses, where the patterns
    correspond to the three cases
*)
let fibonacci n = 
    let rec fib fibMinus2 fibMinus1 = function       
        | searched when searched = n -> fibMinus2 + fibMinus1
        | x -> fib fibMinus1 (fibMinus2 + fibMinus1) (x + 1)
  
    match n with 
    | 0 -> 0
    | 1 -> 1
    | x -> fib 0 1 2

{0..5} |> Seq.map fibonacci |> Seq.iter (printf "%i ")


(* 1.6
    Declare a recursive function sum: int * int -> int, where
    sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)
    for m ≥ 0 and n ≥ 0.
*)

let sum (m, n)= 
    let rec sumRec acc = function
        | (negM, negN ) when negM < 0 || negN < 0 -> 0
        | (m, 0) -> acc+m
        | (m, n) -> sumRec (acc+m+n) (m, n-1)
    sumRec 0 (m, n)

sum(2, 3)

