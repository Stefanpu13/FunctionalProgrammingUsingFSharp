namespace Exercises7
module E = 
    open System.Collections.Generic
    (* 8.7
        Use a HashSet traversal for loop to declare a function
        HashSetFold: (’b -> ’a -> ’b) -> ’b -> HashSet<’a> -> ’b
        such that
        f b set = f (. . . (f (f b a0) a1) . . .) an−1
        where a0, . . . , an−1 are the elements of the HashSet set.
    *)

    let hashSetFold folder state (hashSet: HashSet<'a>) = 
        let mutable res = state
        
        for el in hashSet do
            res <- folder res el
        res
