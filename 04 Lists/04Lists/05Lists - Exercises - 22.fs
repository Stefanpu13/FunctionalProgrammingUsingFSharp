
namespace Exercises22

//#load "01Helpers.fs"
open Helpers.L

module E = 
    (* 4.22
        We represent the polynomial a0 +a1 · x+...+an · xn with integer coefficients a0, a1, ..., an by
        the list [a0, a1, ..., an]. For instance, the polynomial x3 + 2 is represented by the list [2, 0, 0, 1].
        1. Declare an F# function for multiplying a polynomial by a constant.
        2. Declare an F# function for multiplying a polynomial Q(x) by x.
        3. Declare infix F# operators for addition and multiplication of polynomials in the chosen representation.
        The following recursion formula is useful when defining the multiplication:
        0 · Q(x) =0(a0 + a1 · x + ... + an · xn) · Q(x)
        = a0 · Q(x) + x · (a1 + a2 · x + ... + an · xn−1) · Q(x)
        
        4. Declare an F# function to give a textual representation for a polynomial.
    *)

    (* 4.22.1 
        Declare an F# function for multiplying a polynomial by a constant.
    *)

    type Polynomial = Polynomial of int list
    let (&*) c (Polynomial p) =
        match c with
        | 0 -> Polynomial []
        | c -> Polynomial (p |> map ((*) c))

    (* 4.22.2
        Declare an F# function for multiplying a polynomial Q(x) by x. 
    *)

    let byX (Polynomial p) = Polynomial (0::p)

    (* 4.22.3
        Declare infix F# operators for addition and multiplication of polynomials in the chosen representation.
        The following recursion formula is useful when defining the multiplication:
        0 · Q(x) =0
        (a0 + a1 · x + ... + an · xn) · Q(x) = a0 · Q(x) + x · (a1 + a2 · x + ... + an · xn−1) · Q(x)
    *)

    let (&+) (Polynomial p1) (Polynomial p2) = 
        let rec add result = function
            | ([], []) -> Polynomial (rev result)
            | ([], coefs) -> Polynomial ((rev result) @ coefs)
            | (coefs, []) -> Polynomial ((rev result) @ coefs)
            | (coef1::coefs1, coef2::coefs2) ->
                add ((coef1 + coef2)::result) (coefs1, coefs2)
        
        add [] (p1, p2)

    let multiply p1 p2 = 
        let rec multiply p1 = function
            | Polynomial [] -> Polynomial []
            | Polynomial [coef] ->(coef &* p1) 
            | Polynomial (coef::coefs) -> 
                (coef &* p1) &+ (byX (multiply (Polynomial coefs) p1))
        multiply p1 p2
    
    (* 4.22.4
        4. Declare an F# function to give a textual representation for a polynomial.
    *)

    let toString (Polynomial p) =    
        let attachPowerToCoefs = indexed
        let attachSignToCoefs = 
            map (fun (index, polCoef) -> 
                match polCoef with
                    | neg when neg < 0 -> (index, abs neg, "-") 
                    | zero when zero = 0 ->(index, zero, "")
                    | pos -> (index, pos, "+")
            )
        let removeZeroCoefs = filter (fun (_, polCoef, _) -> polCoef <> 0) 

        p
            |> attachPowerToCoefs    
            |> attachSignToCoefs
            |> removeZeroCoefs
            |> indexed
            |> fold (
                    fun polStr (position, (pow, polCoef, sign)) ->
                        match pow with
                        | 0 -> polCoef.ToString()
                        | _ ->
                            let power = if pow = 1 then "" else "^" + pow.ToString()
                            let coef = if polCoef = 1 then "" else polCoef.ToString()                            
                            let strBeforeTerm = if position = 0 then "" else " " + sign + " "
                            polStr + strBeforeTerm + coef + "x" + power 
            ) ""
