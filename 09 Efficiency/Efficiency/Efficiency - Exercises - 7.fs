namespace Exercises7
module E = 

    (* 9.6
        Develop the following three versions of functions computing Fibonacci numbers Fn (see Exercise
        1.5):
        1. A version fibA: int -> int -> int -> int with two accumulating parameters n1 and
        n2, where fibA n n1 n2 = Fn, when n1 = Fn−1 and n2 = Fn−2. Hint: consider suitable
        definitions of F−1 and F−2.
        2. A continuation-based version fibC: int -> (int -> int) -> int that is based on the
        definition of Fn given in Exercise 1.5.
        Compare these two functions using the directive #time, and compare this with the while-loop
        based solution of Exercise 8.6.        
    *)

    (*
        1. A version fibA: int -> int -> int -> int with two accumulating parameters n1 and
        n2, where fibA n n1 n2 = Fn, when n1 = Fn−1 and n2 = Fn−2. Hint: consider suitable
        definitions of F−1 and F−2.
    *)
    let rec fibA n n1 n2 = 
        match n with
        | 0 -> n2
        | 1 -> n1
        | n -> fibA (n-1) (n1+n2) n1

    [0..5] |> List.map (fun i ->fibA i 1 0)

    (*
        2. A continuation-based version fibC: int -> (int -> int) -> int that is based on the
        definition of Fn given in Exercise 1.5.
    *)

    let rec fibExer1point5 = function
    | 0 -> 0
    | 1 -> 1
    | n -> fibExer1point5 (n-1) + fibExer1point5 (n-2)

    [0..5] |> List.map (fun i -> fibExer1point5 i)

    let rec fibC n cont = 
        match n with
        | 0 -> cont 0
        | 1 -> cont 1
        | n -> fibC (n - 2) (fun a -> fibC (n-1) (fun b -> cont (a + b)))

    [0..5] |> List.map (fun i -> fibC i id)