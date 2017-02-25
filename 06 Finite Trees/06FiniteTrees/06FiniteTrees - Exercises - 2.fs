namespace Exercises2
module E = 
    (* 6.1
        Postfix form is a particular representation of arithmetic expressions where each operator is
        preceded by its operand(s), for example:
        (x + 7.0) has postfix form x 7.0 +
        (x + 7.0) ∗ (x − 5.0) has postfix form x 7.0 + x 5.0 − ∗
        Declare an F# function with type Fexpr -> string computing the textual, postfix form of
        expression trees from Section 6.2.
    *)

    type Fexpr = 
        | Const of float
        | X
        | Add of Fexpr * Fexpr
        | Sub of Fexpr * Fexpr
        | Mul of Fexpr * Fexpr
        | Div of Fexpr * Fexpr
        | Sin of Fexpr
        | Cos of Fexpr
        | Log of Fexpr
        | Exp of Fexpr

    let rec D = function
        | Const _ -> Const 0.0
        | X -> Const 1.0
        | Add(fe,ge) -> Add(D fe, D ge)
        | Sub(fe,ge) -> Sub(D fe, D ge)
        | Mul(fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
        | Div(fe,ge) -> Div(Sub(Mul(D fe,ge), Mul(fe,D ge)),Mul(ge,ge))
        | Sin fe -> Mul(Cos fe, D fe)
        | Cos fe -> Mul(Const -1.0, Mul(Sin fe, D fe))
        | Log fe -> Div(D fe, fe)
        | Exp fe -> Mul(Exp fe, D fe)

    let postFix expr = 
        let rec postFix l = function
            | Const x -> x.ToString():: l
            | X -> "x" :: l
            | Add (x, y) -> (postFix l x) @ (postFix l y) @ ["+"]            
            | Sub (x, y) -> (postFix l x) @ (postFix l y) @ ["-"]            
            | Mul (x, y) -> (postFix l x) @ (postFix l y) @ ["*"]            
            | Div (x, y) -> (postFix l x) @ (postFix l y) @ ["/"]            
            | _ -> l

        let result = (postFix [] expr) |> List.fold(fun str term -> str + " " + term) ""
        
        result.Substring(1)
             
    

    postFix (Add(X, Const 7.0))
    postFix (Mul(Add(X, Const 7.0), Sub(X, Const 5.0)))
    