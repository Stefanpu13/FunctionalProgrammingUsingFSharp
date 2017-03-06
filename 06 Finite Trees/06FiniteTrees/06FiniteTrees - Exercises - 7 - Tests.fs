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


// Test toConjuctionNormalForm
[<TestFixture>]
type ``Test toConjuctionNormalForm``() =
    [<Test>]
    member t.``If tree is t4 and element to delete is smallest elem, result should be t4 without -3`` () = 
        let proposition = Neg(Neg(Neg(Atom "a")))
        toNegationNormalForm proposition |> shouldEqual  (Neg(Atom "a"))