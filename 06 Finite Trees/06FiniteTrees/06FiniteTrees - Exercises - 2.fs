namespace Exercises2
module E = 
    open Exercises1Common.Types
    open Exercises1Common.Utils
    (* 6.1
        Postfix form is a particular representation of arithmetic expressions where each operator is
        preceded by its operand(s), for example:
        (x + 7.0) has postfix form x 7.0 +
        (x + 7.0) ∗ (x − 5.0) has postfix form x 7.0 + x 5.0 − ∗
        Declare an F# function with type Fexpr -> string computing the textual, postfix form of
        expression trees from Section 6.2.
    *)

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
    