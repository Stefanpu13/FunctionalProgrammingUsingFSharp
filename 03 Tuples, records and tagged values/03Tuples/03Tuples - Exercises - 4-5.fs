namespace Exercises4To5
module E =
    (* 3.4
        A straight line y = ax + b in the plane can be represented by the pair (a, b) of real numbers.
        1. Declare a type StraightLine for straight lines.
        2. Declare functions to mirror straight lines around the x and y-axes.
        3. Declare a function to give a string representation for the equation of a straight line.
    *)
    // 3.4.1
    type StraightLine = StraightLine of float * float

    // 3.4.2
    let mirror (StraightLine (a, b)) = StraightLine(-a, -b)
    // 3.4.3
    let straightLineToString (StraightLine (a, b)) = 
        match (a, b) with
        | (0.0, b) -> sprintf "y = %.1f" b
        | (a, 0.0) -> sprintf "y = %.1fx" a
        | (a, b) -> sprintf "y = %.1fx + %.1f" a b 
    

    (* 3.5
        Make a type Solution capturing the three capabilities for roots in a quadratic equation: two
        roots, one root and no root (cf. Section 3.5). Declare a corresponding solve function.
    *)

    type Solution = 
    | NoSolutions 
    | OneSolution of float 
    | TwoSolutions of float * float

    type Equation = Equation of float * float * float
    let solve (Equation (a, b, c)) =
        let d = b*b - 4.0*a*c
        match (a, b, c) with   
        | (_, _, _) when d < 0.0 -> NoSolutions
        | (a, _, _) when a = 0.0 -> OneSolution (-c / b) // use c, b from match clause
        | (a, b, _) when d = 0.0 -> OneSolution (-b / (2.0 * a))
        | (a, b, c) -> 
            let x1 = (-b + sqrt d) / (2.0 * a)
            let x2 = (-b - sqrt d) / (2.0 * a)
            TwoSolutions (x1, x2)

    (* 3.7
        Give a declaration for the area function on Page 61 using guarded patterns rather than an
        if...then...else expression.
    *)

    type Shape = 
        | Circle of float
        | Square of float
        | Triangle of float*float*float

    type Area  = 
    | Area of float
    | NotAShape of string

    let isShape = function
        | Circle r -> r > 0.0
        | Square a -> a > 0.0
        | Triangle(a,b,c) ->
        a > 0.0 && b > 0.0 && c > 0.0
        && a < b + c && b < c + a && c < a + b

    let area  = function
        | y when not (isShape y) -> NotAShape "Not a legal shape"
        | x -> match x with
                | Circle r -> Area (System.Math.PI * r * r)
                | Square a ->Area (a * a)
                | Triangle(a,b,c) ->
                    let s = (a + b + c)/2.0
                    Area (sqrt(s*(s-a)*(s-b)*(s-c)))