namespace Exercises7
module E = 
    (* 5.7
        Declare a function allSubsets such that allSubsets n k is the set of all subsets of
        {1,  2, . . . , n} containing exactly k elements. Hint: use ideas from Exercise 2.8. 
        For example, n k
        is the number of subsets of {1, 2, . . . , n} containing exactly k elements.
    *)

    // solution taken from:
    // https://rosettacode.org/wiki/Combinations#OCaml
    // More inportant than the algorithm is the fact the OCaml code often can be used as F# code. 
    let allSubsets n k =
        let rec comb m l =
            match m, l with
            | 0, _ -> [[]]
            | _, [] -> []
            | m, x::xs -> (List.map (fun y -> x::y) (comb (m - 1) xs)) @ comb m xs
        comb k [1..n]
    