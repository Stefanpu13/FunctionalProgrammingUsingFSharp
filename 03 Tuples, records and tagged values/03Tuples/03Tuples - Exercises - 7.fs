namespace Exercises7
module E =
   
    (* 3.7
        Give a declaration for the area function on Page 61 using guarded patterns rather than an
        if...then...else expression.
    *)    
    type Shape = 
        | Circle of float
        | Square of float
        | Triangle of float * float * float

    type Area  = 
    | Area of float
    | NotAShape of string
    
    let (|Positive|_|) = function
    | pos when pos > 0.0 -> Some pos
    | _ -> None

    let isShape = function
        | Circle (Positive r) -> true 
        | Square (Positive a) -> true
        | Triangle (Positive a, Positive b, Positive c) ->
             a < b + c && b < c + a && c < a + b
        | _ -> false

    let area  = function
        | y when not (isShape y) -> NotAShape "Not a legal shape"
        | x -> match x with
                | Circle r -> Area (System.Math.PI * r * r)
                | Square a -> Area (a * a)
                | Triangle(a,b,c) ->
                    let s = (a + b + c)/2.0
                    Area (sqrt(s*(s-a)*(s-b)*(s-c)))
