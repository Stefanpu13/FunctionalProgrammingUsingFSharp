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

    (* 5.6.5
        The relation composition r ◦◦ s of a relation r from a set A to a set B and a relation s from
        B to a set C is a relation from A to C. It is defined as the set of pairs (a, c) where there exist
        an element b in B such that (a, b) ∈ r and (b, c) ∈ s. Declare an F# function to compute the
        relational composition.
    *)

    let relationalComposition r s =         
        // A = 1,2,3 ; B = 4,5,6 ; C = 7,8,9
        // r = 1,4; 2,4; 3,4; 2,5; 1,5 ; s = 4,7; 4,8; 5,8
        //r ** s = 1,7; 2,7; 3,7; 2,8; 1,8   
        r |> Set.fold(
            fun composition (a, b) ->
                let newCompositionValues = 
                    s 
                        |> Set.filter (fun (b', c) -> b=b')
                        |> Set.map (fun (b', c) -> (a, c))

                Set.union composition newCompositionValues
        ) Set.empty   

    (* 5.6.6
        A relation r from a set A to the same set A is said to be transitive if (a1, a2) ∈ r and
        (a2, a3) ∈ r implies (a1, a3) ∈ r for any elements a1, a2 and a3 in A. The transitive closure
        of a relation r is the smallest transitive relation containing r. If r contains n elements, then
        the transitive closure can be computed as the union of the following n relations:
        r ∪ (r ◦◦ r) ∪ (r ◦◦ r ◦◦ r) ∪ · · · ∪ (r ◦◦ r ◦◦ · · · ◦◦ r)
        Declare an F# function to compute the transitive closure.
    *)

    // See: http://math.stackexchange.com/questions/1255179/determining-whether-a-relation-is-transitive-or-not
    let transitiveClosure r = 
        let rec transitiveClosure result currentRelationComposition = function
        | 0 -> result
        | n -> 
            let newRelationComposition = relationalComposition currentRelationComposition r 
            transitiveClosure (Set.union result newRelationComposition) newRelationComposition (n - 1)
        
        transitiveClosure r r ((Set.count r))
            