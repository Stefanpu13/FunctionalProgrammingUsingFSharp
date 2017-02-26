namespace Exercises3
module E = 
    open Exercises1Common.Types.Expression
    open Exercises1Common.Utils

    (* 6.3
        Make a refined version of the toString function on Page 130 using the following conventions:
        A subtrahend, factor or dividend must be in brackets if it is an addition or subtraction.
        A divisor must be in brackets if it is an addition, subtraction, multiplication or division. The
        argument of a function must be in brackets unless it is a constant or the variable x. (Hint: use a
        set of mutually recursive declarations.)

        6 - 3 - "3" is subtrahend
        11 * 2 - "11" and "2" are factors
        12/3 -"12" is dividend
        12/3 - "3" is divisor
    *)

    let rec toString = function
        | Const x -> string x
        | X -> "x"
        | Add(fe1,fe2) -> (toString fe1) + " + " + (toString fe2)
        | Sub(fe1,fe2) -> (toString fe1) + " - " + (toStringSubrahend fe2)
        | Mul(fe1,fe2) -> (toStringFactor fe1) + " * " + (toStringFactor fe2)
        | Div(fe1,fe2) -> (toStringDividend fe1) + " / " + (toStringDivisor fe2)
        | Sin fe -> "sin " + (toStringArgument fe) 
        | Cos fe -> "cos " + (toStringArgument fe)
        | Log fe -> "log " + (toStringArgument fe)
        | Exp fe -> "exp " + (toStringArgument fe)
    and addOrSub = function
        | Add(fe1,fe2) -> "(" + (toString fe1) + " + " + (toString fe2) + ")"
        | Sub(fe1,fe2) -> "(" + (toString fe1) + " - " + (toStringSubrahend fe2) + ")"
        | expr -> toString expr 
    and toStringSubrahend = addOrSub
    and toStringFactor = addOrSub
    and toStringDividend = addOrSub
    and toStringDivisor = function        
        | Mul(fe1,fe2) -> "(" + (toStringFactor fe1) + " * " + (toStringFactor fe2) + ")"
        | Div(fe1,fe2) -> "(" + (toStringDividend fe1) + " / " + (toStringDivisor fe2) + ")"
        | expr -> addOrSub expr
    and toStringArgument = function
        | Const x -> string x
        | X -> "x"
        | expr -> "(" + (toString expr) + ")"

