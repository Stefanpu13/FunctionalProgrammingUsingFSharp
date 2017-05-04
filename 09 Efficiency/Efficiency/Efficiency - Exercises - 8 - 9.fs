namespace Exercises8_9
module E = 

    (* 9.8
        Develop a version of the counting function for binary trees
        countA: int -> BinTree<’a> -> int
        that makes use of an accumulating parameter. Observe that this function is not tail recursive.
    *)
    type BinTree<'a> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

    let rec countA count = function
    | Leaf -> count
    | Node(l, a, r) -> countA 0 l + (count + 1) + countA 0 r

    (* 9.9
        Declare a tail-recursive functions with the type
        countAC : BinTree<’a> -> int -> (int -> ’b) -> ’b
        such that count t = countAC t 0 id. The intuition with countAC t a c is that a is the
        number of nodes being counted so far and c is the continuation.
    *)

    let rec countAC tr count cont = 
        match tr with
        | Leaf -> cont count
        | Node(l, a, r) -> 
            countAC l 0 (fun lCount -> countAC r 0 (fun rCount -> cont (lCount + rCount + 1)))



        


  