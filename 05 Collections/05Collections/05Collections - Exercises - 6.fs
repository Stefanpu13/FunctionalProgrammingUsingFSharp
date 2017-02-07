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

    (* 5.6.2
        The range rng r of a relation r is the set of elements b in B where there exists an element a
        in A such that (a, b) ∈ r. Write an F# declaration expressing the range function.
    *)

    let range relation setA setB = 
        setB |> 
            Set.filter (fun elB -> setA |> Set.exists (fun elA -> Set.contains (elA, elB) relation))

    (* 5.6.3
        If r is a finite relation from A to B and a is an element of A, then the application of r to a,
        apply r a, is the set of elements b in B such that (a, b) ∈ r. Write an F# declaration expressing
        the apply function.
    *)

    let apply relation elA setA setB = setB |> Set.filter(fun elB -> Set.contains (elA, elB) relation)

    (* 5.6.4
        A relation r from a set A to the same set is said to be symmetric if (a1, a2) ∈ r implies
        (a2, a1) ∈ r for any elements a1 and a2 in A. The symmetric closure of a relation r is the
        smallest symmetric relation containing r. Declare an F# function to compute the symmetric
        closure.
    *)
    let symmetricClosure relation setA = 
        relation |> Set.fold(
            fun symClosure (a1, a2) -> 
                match (Set.contains a1 setA, Set.contains a2 setA, symClosure) with
                | (true, true, Some symCl) -> 
                    if relation |> Set.contains (a2, a1) 
                    then Some symCl 
                    else Some (Set.add (a2, a1) symCl)
                | _ -> None
            ) (Some relation)         
        