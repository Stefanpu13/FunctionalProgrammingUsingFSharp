namespace Exercises6
module E = 
    (* 5.6
        We define a relation from a set A to a set B as a subset of A × B. A relation r
        is said to be smaller than r, if r is a subset of r, that is, if r ⊆ r. 
        A relation r is called finite if it is a finite subset of A × B. 
        Assuming that the sets A and B are represented by F# types ’a and ’b
        allowing comparison we can represent a finite relation r by a value of type set<’a * ’b>.
    *)

    (* 5.6.1
        The domain dom r of a relation r is the set of elements a in A where there exists an element
        b in B such that (a, b) ∈ r. Write an F# declaration expressing the domain function.
    *)
    let domain relation setA setB = 
        setA |> 
            Set.filter (fun elA -> setB |> Set.exists (fun elB -> Set.contains (elA, elB) relation))