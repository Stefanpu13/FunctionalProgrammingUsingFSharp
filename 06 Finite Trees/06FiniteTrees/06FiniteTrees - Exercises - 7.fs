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
        | FlattenedConj of Proposition list

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

    (* 6.7.3
        A literal is an atom or its negation. A proposition is in conjunctive normal form if it is a
        conjunction of propositions, where each conjunct (i.e., proposition in the conjunction) is a
        disjunction of literals. Write an F# function that transforms a proposition into an equivalent
        proposition in conjunctive normal form, using the above result and the laws:
            p ∨ (q ∧ r) ⇔ (p ∨ q) ∧ (p ∨ r)
            (p ∧ q) ∨ r ⇔ (p ∨ r) ∧ (q ∨ r)
    *)

    let (|Literal|_|) p =
        match p with
        | Atom a -> Some (Atom a)
        | Neg(Atom a) -> Some (Neg(Atom a))
        | _ -> None

    let flattenDisjunction prop = 
        let rec flattenDisjunction literals = function
            | Disj(p, q) ->
                (flattenDisjunction literals p) @ (flattenDisjunction [] q)
            | p -> p::literals
            
        match prop with
        | FlattenedDisj l as fl -> fl 
        | p -> FlattenedDisj (flattenDisjunction [] p)        

    let flattenDisjunctionInFlattenedConjuction = function
            | FlattenedConj l -> FlattenedConj (List.map flattenDisjunction l)
            | Disj(p, q) as disj -> flattenDisjunction disj
            | p -> p 

    let flattenConjuction prop = 
        let rec flattenConjuction literals = function
            | Conj (p, q) -> (flattenConjuction literals p) @ (flattenConjuction [] q)
            | p -> p::literals

        match prop with
        | Conj(p, q) as conj -> FlattenedConj (flattenConjuction [] conj)  
        | p -> p

    // for tests see: http://math.stackexchange.com/questions/214338/how-to-convert-to-conjunctive-normal-form
    let toConjuctiveNormalForm proposition =
        let rec toConjuctiveNormalForm = function             
            | Disj(p, Conj(q, r)) ->
                Conj(Disj(p, q) |> toConjuctiveNormalForm ,Disj(p, r) |> toConjuctiveNormalForm)
            | Disj(Conj(p, q), r) -> 
                Conj(Disj(p, r) |> toConjuctiveNormalForm,Disj(q, r) |> toConjuctiveNormalForm)      
            | Disj(p, q) as disj -> 
                 let d = Disj(toConjuctiveNormalForm p,toConjuctiveNormalForm q)                 
                 if d = disj then d else toConjuctiveNormalForm d
            | Conj(p, q) as conj -> 
                 let d = Conj(toConjuctiveNormalForm p,toConjuctiveNormalForm q)                 
                 if d = conj then d else toConjuctiveNormalForm d
            | p -> p

        proposition 
            |> toNegationNormalForm 
            |> toConjuctiveNormalForm
            |> flattenConjuction
            |> flattenDisjunctionInFlattenedConjuction

    (* 6.7.4
        A proposition is a tautology if it has truth value true for any assignment of truth values to the
        atoms. A disjunction of literals is a tautology exactly when it contains the atom as well as
        the negated atom for some name occurring in the disjunction. A conjunction is a tautology
        precisely when each conjunct is a tautology. Write a tautology checker in F#, that is, an F#
        function which determines whether a proposition is a tautology or not.
    *)
    let isTautology prop = 
        let flatenedDisjIsTautology = function
            | FlattenedDisj l ->
                let rec containsNegation negationFound = function
                    | x::xs ->                          
                        (List.exists(fun p -> x = Neg(p) || Neg(x) = p) xs) || 
                        containsNegation false xs 
                    | [] -> negationFound

                containsNegation false l
            | p -> false

        match  (toConjuctiveNormalForm prop) with 
        | FlattenedConj l -> List.forall flatenedDisjIsTautology l
        | p -> flatenedDisjIsTautology p
    