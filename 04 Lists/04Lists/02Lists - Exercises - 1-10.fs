namespace Exercises1To10
open Helpers.L

module E = 
    // You need to load to test module functions in isolation
    // #load "01Helpers.fs"
    

    (* 4.1
        Declare function upto: int -> int list such that upto n = [1; 2; . . . ; n].
    *)

    let upto n = 
        let rec upto acc = function
            | 0 -> acc
            | n -> upto (n::acc) (n - 1)

        if n <= 0 
        then [] 
        else upto [] n    

    (* 4.2 
        Declare function downto1: int -> int list such that the value of downto1 n is the list
        [n; n − 1; . . . ; 1].
    *)

    let downto1 n = 
        let rec downto1 acc = function
            | m when m = n+1 -> acc
            | n' -> downto1 (n'::acc) (n' + 1)

        if n <= 0 
        then []
        else downto1 [] 1

    (* 4.3
        Declare function evenN: int -> int list such that evenN n generates the list of the first
        n non-negative even numbers.
    *)
    let evenN n  = 
        let rec evenN evenNumbers = function
            | negOrZero when negOrZero <= 0 -> evenNumbers
            | n -> evenN ((n * 2)::evenNumbers) (n - 1)

        evenN [] n

    (* 4.4 
        Give a declaration for altsum (see Page 76) containing just two clauses.
        altsum [x0;x1;x2; . . . ;xn−1] = x0 - x1 + altsum [x2; . . . ;xn−1]
    *)
    let altsum l =     
        // "nested function" with accumulating sum to make function tail recursive
        let rec altsum sum (currentOp, nextOp) = function 
            | [] -> sum
            // pass addition and subtraction as tuple of functions and swap their places
            | x1::xs -> altsum (currentOp sum x1) (nextOp, currentOp) xs 
            
        // Why this works: see page 45; 2.7 Functions are first-class citizens
        altsum 0 ((+), (-)) l
    
    (* 4.5
        Declare an F# function rmodd removing the odd-numbered elements from a list:
        rmodd [x0;x1;x2;x3; . . . ] = [x0;x2; . . . ]
    *)

    let rmodd l = 
        let rec rmodd evenIndexedElems i = function
            | [] -> evenIndexedElems
            | x::xs -> 
                if i % 2 = 0
                then rmodd (x::evenIndexedElems) (i+1) xs
                else rmodd evenIndexedElems (i+1) xs

        rev (rmodd [] 0 l)

    // Of course, stack overflow will happen for larger lists
    let rmodd2 l = 
        let rec rmodd2 evenIndexedElems i= function
        | [] -> evenIndexedElems
        | x::xs -> 
            if i % 2 = 0
            then x::rmodd2 (evenIndexedElems) (i+1) xs
            else rmodd2 evenIndexedElems (i+1) xs

        rmodd2 [] 0 l

    // try with 1 or 2 zeroes added     

    let rmodd3 l = 
        l 
            |> indexed
            |> filter (fun (i, el) -> i % 2 = 0)
            |> map (fun (i, el) -> el)

    (* 4.6
        Declare an F# function to remove even numbers occurring in an integer list.
    *)

    let removeEven = filter (fun el -> el % 2 <> 0)

    (* 4.7
        Declare an F# function multiplicity x xs to find the number of times the value x occurs
        in the list xs.
    *)

    let rec multiplicity x xs= 
        let rec multiplicity count = function
        | [] -> count
        | y::ys ->
            if x = y
            then multiplicity (count+1) ys
            else multiplicity count ys
        
        multiplicity 0 xs

    let multiplicity2 x = fold (fun num el -> if x = el then num+1 else num) 0

    (* 4.8 
        Declare an F# function split such that:
        split [x0;x1;x2;x3; . . . ;xn−1] = ([x0;x2; . . . ], [x1;x3; . . . ])
    *)

    let split l = 
        let rec split (evenIndexedElems, oddIndexedElems) i = function
            | [] -> (rev evenIndexedElems, rev oddIndexedElems)
            | x::xs -> 
                if i % 2 = 0
                then split (x::evenIndexedElems, oddIndexedElems) (i+1) xs
                else split (evenIndexedElems, x::oddIndexedElems) (i+1) xs

        split ([], []) 0 l

    let split2 l =
        let split = foldBackNaive (fun (i, el) (evenIndexedElems, oddIndexedElems) -> 
                if i % 2 = 0
                then (el::evenIndexedElems, oddIndexedElems)
                else (evenIndexedElems, el::oddIndexedElems)
            )  

        split (l |> indexed) ([], [])

    (* 4.9
        Declare an F# function zip such that:
        zip([x0;x1; . . . ;xn−1],[y0;y1; . . . ;yn−1])
        = [(x0, y0);(x1, y1); . . . ;(xn−1, yn−1)]
        The function should raise an exception if the two lists are not of equal length.
    *)

    let zip (l1, l2) : ('a * 'a) list =
        let rec zip l = function 
            | [], [] -> (rev l)
            |(x::xs, y::ys) -> zip ((x, y)::l) (xs, ys)     
            | _ -> failwith "Two lists are not of equal length"
        // make length comparison only once and exit function early    
        match length l1 = length l2 with
        |true -> zip [] (l1, l2)
        |false -> failwith "Two lists are not of equal length"

    (* 4.10
        Declare an F# function prefix: ’a list -> ’a list -> bool when a : equality.
        The value of the expression prefix [x0;x1; . . . ;xm] [y0;y1; . . . ;yn] is true if m ≤ n
        and xi = yi for 0 ≤ i ≤ m, and false otherwise.
    *)

    let prefix l1 l2 = 
        let rec prefix isPrefix  = function
            | [], _ -> isPrefix
            | xs, [] -> isPrefix && (isEmpty xs) 
            |(x::xs, y::ys) -> prefix (x=y && isPrefix) (xs, ys)

        prefix true (l1, l2) 
