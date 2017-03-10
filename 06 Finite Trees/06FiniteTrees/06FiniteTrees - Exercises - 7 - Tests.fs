module Exercises7Tests
open NUnit.Framework
open FsUnitTyped
open Exercises7.E

// Test toNegationNormalForm
[<TestFixture>]
type ``Test toNegationNormalForm``() =
    let allNegationsAreOnAtoms proposition = 
        let rec allNegationsAreOnAtoms examinedNegationsOnAtoms = function
            | Neg(Atom b) -> examinedNegationsOnAtoms && true
            | Neg(p) -> examinedNegationsOnAtoms && false
            | Conj(a, b) | Disj(a, b) -> 
                (allNegationsAreOnAtoms examinedNegationsOnAtoms a) && 
                (allNegationsAreOnAtoms examinedNegationsOnAtoms b)
            | p -> examinedNegationsOnAtoms
            
        allNegationsAreOnAtoms true proposition

    [<Test>]
    member t.``If propposition is odd number of time nested Neg (a), result should be Neg(a)`` () = 
        let proposition = Neg(Neg(Neg(Atom "a")))
        toNegationNormalForm proposition |> shouldEqual  (Neg(Atom "a"))

    [<Test>]
    member t.``If propposition is even number of time nested Neg (a), result should be a`` () = 
        let proposition = Neg(Neg(Neg(Neg(Atom "a"))))
        toNegationNormalForm proposition |> shouldEqual  (Atom "a")

    [<Test>]
    member t.``If propposition is Neg(Conj(a, b)), result should be Disj(Neg(a),Neg(b))`` () = 
        let proposition = Neg(Conj(Atom "a", Atom "b"))
        toNegationNormalForm proposition |> shouldEqual  (Disj(Neg(Atom "a"),Neg(Atom "b")))

    [<Test>]
    member t.``If propposition is Neg(Disj(a, b)), result should be Conj(Neg(a),Neg(b))`` () = 
        let proposition = Neg(Disj(Atom "a", Atom "b"))
        toNegationNormalForm proposition |> shouldEqual  (Conj(Neg(Atom "a"),Neg(Atom "b")))

    [<Test>]
    member t.``If propposition is Neg of nested conjuctions, result should be all Negations applied to Atoms`` () = 
        let proposition = Neg(Conj(Conj( Atom "a", Conj(Atom "b", Atom "c")), Conj(Atom "d", Atom "e")))
        proposition 
            |> toNegationNormalForm
            |> allNegationsAreOnAtoms
            |> shouldEqual  true

    [<Test>]
    member t.``If propposition is Neg of nested disjuctions, result should be all Negations applied to Atoms`` () = 
        let proposition = Neg(Disj(Disj( Atom "a", Disj(Atom "b", Atom "c")), Disj(Atom "d", Atom "e")))
        proposition 
            |> toNegationNormalForm
            |> allNegationsAreOnAtoms
            |> shouldEqual  true

    [<Test>]
    member t.``If propposition is nested disjuctions and conjuctions, result should be all Negations applied to Atoms`` () = 
        let proposition = 
            Conj(
                Disj(
                    Neg(Disj( 
                            Atom "a", 
                            Disj(Atom "b", Atom "c")
                    )), 
                    Disj(Atom "d", Atom "e")
                ), 
                Neg(
                    Conj(
                        Atom "g", 
                        Disj(Atom "h", Atom "i")
                    )
                )
            )
        proposition 
            |> toNegationNormalForm
            |> allNegationsAreOnAtoms
            |> shouldEqual  true



// http://math.stackexchange.com/questions/214338/how-to-convert-to-conjunctive-normal-form
// Test toConjuctionNormalForm
[<TestFixture>]
type ``Test toConjuctionNormalForm``() =
    // Each Conj consists either of conjs or of flattened disjs
    let isInConjuctiveNormalForm proposition = 
        let rec allPropositionsAreAtoms previousPropositionsAreAtoms = function
            | (Literal x)::xs -> allPropositionsAreAtoms previousPropositionsAreAtoms xs
            | x::xs -> false
            | [] -> previousPropositionsAreAtoms

        let flattenedDisjunctionPropositionsAreAtoms = function
        | FlattenedDisj l -> allPropositionsAreAtoms true l
        | p -> false

        let rec isInConjuctiveNormalForm isInForm = function
            | FlattenedConj l -> List.forall flattenedDisjunctionPropositionsAreAtoms l
            | Literal p -> true
            |  p -> flattenedDisjunctionPropositionsAreAtoms p
            

        isInConjuctiveNormalForm true proposition

    [<Test>]
    member t.``For any Literal prop, result should be Literal`` () = 
        let proposition = Neg(Neg(Neg(Atom "a")))
        proposition 
            |> toConjuctiveNormalForm            
            |> shouldEqual (Neg(Atom "a"))

    [<Test>]
    member t.``For any Disj of literals or Disjs, result should be FlattenedDisj of literals`` () = 
        let proposition = Disj(Disj(Atom "a", Neg(Atom "b")), Disj(Atom "b", Disj(Atom "a", Atom "c")))
        proposition 
            |> toConjuctiveNormalForm             
            |> shouldEqual (FlattenedDisj [Atom "a"; Neg(Atom "b");Atom "b"; Atom "a"; Atom "c" ])

    [<Test>]
    member t.``For any Disj, result should be isInConjuctiveNormalForm`` () = 
        let proposition1 = Disj(Conj(Atom "a", Neg(Atom "b")), Disj(Atom "b", Conj(Atom "a", Atom "c")))
        proposition1 
            |> toConjuctiveNormalForm             
            |> isInConjuctiveNormalForm
            |> shouldEqual true

        let proposition2 = Disj(Atom "a", Conj(Atom "b", Conj(Atom "a", Atom "c")))
        proposition2
            |> toConjuctiveNormalForm             
            |> isInConjuctiveNormalForm
            |> shouldEqual true

        Disj(Atom "a", Conj(Atom "b", Conj(Atom "c", Atom "d")))             
            |> toConjuctiveNormalForm             
            |> isInConjuctiveNormalForm
            |> shouldEqual true
        

// Test isTautology
[<TestFixture>]
type ``Test isTautology``() =
    [<Test>]
    member t.``If proposition is  any literal, it should not be tautology`` () = 
        let proposition1 = Neg(Neg(Neg(Atom "a")))
        isTautology proposition1 |> shouldEqual false

        let proposition2 = Atom("b")
        isTautology proposition2 |> shouldEqual false

    [<Test>]
    member t.``If proposition is FlattenedDisj that does not contain negation for any of its props , it should not be tautology`` () = 
        let proposition1 = FlattenedDisj [Atom ("a"); Atom("b"); Atom("c")]
        isTautology proposition1 |> shouldEqual false

        let proposition2 = FlattenedDisj [Atom ("a"); Neg (Atom("b")); Atom("a")]
        isTautology proposition2 |> shouldEqual false

    [<Test>]
    member t.``If proposition is FlattenedDisj that contains negation for any of its props , it should be tautology`` () = 
        let proposition1 = FlattenedDisj [Atom ("a"); Atom("b"); Atom("c"); Neg(Atom("b"))]
        isTautology proposition1 |> shouldEqual true

        let proposition2 = FlattenedDisj [Atom ("a"); Neg (Atom("b")); Atom("a"); Atom ("c"); Atom("b")]
        isTautology proposition2 |> shouldEqual true

    [<Test>]
    member t.``If proposition is FlattenedConj and not all FlattenedDisjs are tautology, it should not be tautology`` () = 
        let proposition1 =FlattenedConj [FlattenedDisj [Atom ("a"); Atom("b"); Atom("c");]]
        isTautology proposition1 |> shouldEqual false

        let proposition2 = 
            FlattenedConj[
                FlattenedDisj [Atom ("a"); Atom("b"); Atom("c"); Neg(Atom("a"))]
                FlattenedDisj [Atom ("a"); Neg (Atom("b")); Atom("a"); Atom ("c"); Atom("b")]
                FlattenedDisj [Atom ("a"); Neg (Atom("b")); Atom("a"); Atom ("c");]
                ]
        isTautology proposition2 |> shouldEqual false

    [<Test>]
    member t.``If proposition is FlattenedConj and all FlattenedDisjs are tautology, it should be tautology`` () = 
        let proposition1 =FlattenedConj [FlattenedDisj [Atom ("a"); Atom("b"); Atom("c"); Neg(Atom("a"))]]
        isTautology proposition1 |> shouldEqual true

        let proposition2 = 
            FlattenedConj[
                FlattenedDisj [Atom ("a"); Atom("b"); Atom("c"); Neg(Atom("a"))]
                FlattenedDisj [Atom ("a"); Neg (Atom("b")); Atom("a"); Atom ("c"); Atom("b")]
                FlattenedDisj [Neg (Atom("b")); Atom ("a"); Neg(Atom("a")); Atom("b")]
                ]
        isTautology proposition2 |> shouldEqual true
