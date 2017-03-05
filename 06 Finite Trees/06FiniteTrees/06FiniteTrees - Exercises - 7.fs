namespace Exercises7
module E = 
    
    (* 6.7.1
        Define a type to represent formulas in propositional logic. A proposition is either an atom
        given by its name which is a string, or a composite proposition built from atoms using the
        operators for negation (¬), conjunction (∧), and disjunction (∨).
    *)

    type Proposition = 
        | Atom of string          
        | Neg of Proposition
        | Conj of (Proposition * Proposition) // ^
        | Disj of (Proposition * Proposition) // v  
        | FlattenedDisj of Proposition list

    let x = Conj (Neg(Atom "string"), Atom "hi")

    (* 6.7.2
        A proposition is in negation normal form if the negation operator only appears as applied
        directly to atoms. Write an F# function transforming a proposition into an equivalent proposition
        in negation normal form, using the de Morgan laws:
            ¬(p ∧ q) ⇔ (¬p) ∨ (¬q)
            ¬(p ∨ q) ⇔ (¬p) ∧ (¬q)
        and the law: ¬(¬p) ⇔ p.
    *)

    let rec toNegationNormalForm = function
        | Neg(Conj(a, b)) -> Disj(toNegationNormalForm <| Neg(a), toNegationNormalForm <| Neg(b))
        | Neg(Disj(a, b)) -> Conj(toNegationNormalForm <| Neg(a), toNegationNormalForm <| Neg(b))
        | Neg(Neg(a)) -> toNegationNormalForm a
        | Neg(a) -> Neg(toNegationNormalForm a)
        | Conj(a, b)  -> Conj (toNegationNormalForm a, toNegationNormalForm b)
        | Disj(a, b)  -> Disj (toNegationNormalForm a, toNegationNormalForm b)
        | proposition -> proposition


    toNegationNormalForm <| (Atom "a" |> Neg |> Neg |> Neg |> Neg |> Neg)
    let proposition = Conj(Conj(Atom "a1", Neg(Neg(Atom "a2"))), Neg(Disj(Conj(Atom "b1", Atom "b2"), Atom "c")))
    toNegationNormalForm proposition

    toNegationNormalForm <| (Neg(Disj(Neg (Atom "a"), Neg(Atom "b"))))

    toNegationNormalForm <| (Neg(Conj(Neg (Atom "a"), Neg(Atom "b"))))

    (* 6.7.3
        A literal is an atom or its negation. A proposition is in conjunctive normal form if it is a
        conjunction of propositions, where each conjunct (i.e., proposition in the conjunction) is a
        disjunction of literals. Write an F# function that transforms a proposition into an equivalent
        proposition in conjunctive normal form, using the above result and the laws:
            p ∨ (q ∧ r) ⇔ (p ∨ q) ∧ (p ∨ r)
            (p ∧ q) ∨ r ⇔ (p ∨ r) ∧ (q ∨ r) - yes, for CNS only one of the distributive laws should be used
            
            (P⋁(Q⋀R))↔(P⋁Q)⋀(P⋁R)           
            (P⋀(Q⋁R))↔(P⋀Q)⋁(P⋀R)
    *)

    let (|Literal|_|) p =
        match p with
        | Atom a -> Some (Atom a)
        | Neg(Atom a) -> Some (Neg(Atom a))
        | _ -> None

    let flattenFlattenedDisj flatenedDisj =         
        let rec inOrder literals = function            
            | FlattenedDisj (x::xs) -> (inOrder literals x)  @ (List.fold inOrder [] xs)
            | x -> x::literals           

        FlattenedDisj (inOrder [] flatenedDisj)
    let fls = 
        FlattenedDisj[
            FlattenedDisj[
                FlattenedDisj [
                    FlattenedDisj[ Atom "a"; Atom "b"]; 
                Atom "c"];
                FlattenedDisj[
                    FlattenedDisj[Atom "d"; Atom "e"]
                ]; 
                FlattenedDisj [Atom "f"; Atom "g"];
            ] 
            FlattenedDisj [Atom "h"; Atom "i"]
        ]

    
    flattenFlattenedDisj fls    
    let flattenDisjunction prop = 
        let rec flattenDisjunction literals = function        
            | Disj(Disj(p), Literal q) -> flattenDisjunction (q::literals) (Disj(p))
            | Disj(Literal p, Disj(q)) -> flattenDisjunction (p::literals) (Disj(q))
            | Disj(Disj(p) as d1, (Disj(q)as d2)) ->  
                let (flattenedDisj1) = (flattenDisjunction literals d1)
                let (flattenedDisj2) = flattenDisjunction literals d2
                flattenFlattenedDisj (FlattenedDisj ([flattenedDisj1; flattenedDisj2]))
            | Disj(Literal p, (Literal q)) -> FlattenedDisj (p::q::literals) 
            | Conj (p, q) -> Conj (flattenDisjunction literals p, flattenDisjunction literals q)            
            | p -> p
        flattenDisjunction [] prop

    flattenDisjunction (Conj(Conj(Disj(Disj(Disj(Disj(Atom "a", Atom "b"), Atom "c"), Atom "d"), Atom "e"), Atom "f"), Atom "r"))
    let toConjuctiveNormalForm proposition =
        let rec toConjuctiveNormalForm = function             
            | Disj(p, Conj(q, r)) ->
                Conj(toConjuctiveNormalForm  <| Disj(p, q),toConjuctiveNormalForm <| Disj(p, r))
            | Disj(Conj(p, q), r) -> 
                Conj(toConjuctiveNormalForm <| Disj(p, r),toConjuctiveNormalForm <| Disj(q, r))      
            | Disj(p, q) as disj -> 
                 let d = Disj(toConjuctiveNormalForm p,toConjuctiveNormalForm q)                 
                 if d = disj then d else toConjuctiveNormalForm d
            | p -> p

        proposition 
            |> toNegationNormalForm 
            |> toConjuctiveNormalForm
            |> flattenDisjunction


    let pr1 = Disj(Conj(Atom "a", Atom "b"), Conj(Atom "q", Atom "r"))
    toConjuctiveNormalForm pr1
    let prop = 
        Disj(
            Disj(
                Disj(
                    Conj(Atom "a", Atom "b"), 
                    Conj(Atom "q", Atom "r")
                ), 
                Atom "z"
            ), 
            Disj(Conj(Atom "x", Atom "y"), Atom "z" )
        )
    toConjuctiveNormalForm prop
    let alotNegs = (Atom "a" |> Neg |> Neg |> Neg |> Neg |> Neg)
    let disjWithDoubleNegs = Disj(alotNegs, Conj(Neg(Neg(Atom "b")), Neg(Neg(Atom "c"))))
    toConjuctiveNormalForm <| Disj(alotNegs, Conj(Neg(Neg(Atom "b")), Neg(Neg(Atom "c"))))

    let secondDisj = Disj(Conj(Atom "e",Neg(Atom "d")), Conj(Atom "f", Neg(Neg(Atom "i"))))
    Conj(disjWithDoubleNegs, secondDisj) |> (toNegationNormalForm >> toConjuctiveNormalForm)