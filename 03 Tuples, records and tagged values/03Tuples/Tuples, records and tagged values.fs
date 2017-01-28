// 3.3 Example: Geometric vectors (page 49 / 63 of 376)
(*
    We will consider the following operators on vectors:
    Vector addition: (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
    Vector reversal: −(x, y) = (−x,−y)
    Vector subtraction: (x1, y1) − (x2, y2) = (x1 − x2, y1 − y2)
    = (x1, y1) + −(x2, y2)
    Multiplication by a scalar: λ (x1, y1) = (λx1, λy1)
    Dot product: (x1, y1) · (x2, y2) = x1x2 + y1y2
    Norm (length): 
    (x1, y1)
    =sqrt(x1^2 + y1^2)
    We cannot use the operator symbols +,-,*, and so on, to denote the operations
*)

let (~-.) (x,y) = (-x,-y) : float * float
//val ( ˜-. ) : float * float -> float * float

let (+.) (x1, y1) (x2,y2) = (x1+x2,y1+y2): float*float

let (-.) v1 v2 = v1 +. -. v2

let ( *.) x (x1,y1) = (x*x1, x*y1): float*float

let (&.) (x1,y1) (x2,y2) = x1*x2 + y1*y2: float

let norm (x1,y1) = sqrt(x1*x1+y1*y1) : float

let a = (1.0, -2.0)
let b = (3.0, 4.0)

let c = 2.0 *. a -. b

let d = c &. a

// Quadratic equation
type Equation = Equation of float * float * float
type Solution =
    | NotQuadraticEquation
    | HasNoRealSolutions
    | Solution of float * float
let solve (Equation (a, b, c)) =    
    let d = b*b - 4.0*a*c
    match (a, b, c) with
    | (a, _, _) when a = 0.0 -> NotQuadraticEquation 
    | (a, b, c) when d < 0.0 -> HasNoRealSolutions
    | (a, b, c) -> 
        let x1 = (-b + sqrt d) / (2.0 * a)
        let x2 = (-b - sqrt d) / (2.0 * a)
        Solution (x1, x2)

let solveQuadratic eq = 
    match solve eq with
        | NotQuadraticEquation -> "Not A Quadratic Equation"
        | HasNoRealSolutions -> "Has No Real Solutions"
        | Solution (x1, x2) -> "x1: " + x1.ToString() + "; x2: " + x2.ToString()

solveQuadratic (Equation (2.0, 6.0, 4.0)) |> printfn "%s"

