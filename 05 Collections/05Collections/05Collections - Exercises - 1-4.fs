namespace Exercises1To4
module E = 
    (* 5.1
        Give a declaration for List.filter using List.foldBack.
    *)
    let filter folder l = List.foldBack (fun el elems -> if folder el then el::elems else elems) l []

    (* 5.2
        Exercise 4.15: Declare an F# function revrev working on a list of lists, that maps a list to the reversed list of
        the reversed elements, for example:
        revrev [[1;2];[3;4;5]] = [[5;4;3];[2;1]]

        Solve Exercise 4.15 using List.fold or List.foldBack.
    *)       

    let revrev outerList =
        List.fold (
            fun reversedOuterlist innerList -> 
                (List.fold (fun reversedInnerList el -> el::reversedInnerList) [] innerList)::reversedOuterlist
        ) [] outerList     

    (* 5.3
        Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
        integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
        Test the function on different predicates (e.g., p(x) = x > 0).

        Solve Exercise 4.12 using List.fold or List.foldBack.
    *)

    let sum (p, xs) = List.fold (fun total el -> if p el then total + el else total) 0 xs

    (* 5.4
        Declare a function downto1 such that:
        downto1 f n e = f(1, f(2, . . . , f(n−1, f(n, e)) . . .)) for n > 0
        downto1 f n e = e for n ≤ 0
        Declare the factorial function by use of downto1.
        Use downto1 to declare a function that builds the list [g(1), g(2), . . . , g(n)] for a function g
        and an integer n.
    *)
    let downto1 f n e = List.foldBack (fun num res -> f(num, res)) [1..n] e
    let factorial n = downto1 (fun (n, res) -> res * n ) n 1
    let mapToN g n = downto1 (fun  (n', res) -> (g n')::res) n []