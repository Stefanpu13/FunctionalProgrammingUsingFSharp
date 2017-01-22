// right assosiativity:
// same as 2.0 ** (2.0 ** 3.00)  
2.0 ** 2.0 ** 3.0

// left assosiativity:
// same as: (2 * 6) * 3
2 * 6 *3

// Strings 
@"c0 c1 . . . cn−1"
// It denotes the string of characters

// Funtions as first class citizens 
let add = (+)
let add1 = add 1

// Function Composition 
// (f ◦ g)(x) = f(g(x)).
// Note that (f ◦ g)(x) is written as (f << g) x in f#
((fun y -> y+3) << (fun x -> x*x)) 4

let f x= x+1 
let g x = 2*x

(f << g) 2 


// Closures 

let pi = System.Math.PI
let circleArea r = pi * r * r;;
circleArea 3.0


// Prefix and inflix Operators
let (.||.) p q = (p || q) && not(p && q);;
//val ( .||. ) : bool -> bool -> bool
(1 > 2) .||. (2 + 3 < 5)

// The bracket notation converts from infix or prefix operator to (prefix) function:
// Example: 
1 + 2 = (+) 1 2

// 2.10 Equality and ordering
let ordText x y =
    match compare x y with
    | t when t > 0 -> "greater"
    | 0 -> "equal"
    | _ -> "less" 

ordText "Me" "Me"

// 2.11 Function application operators |> and <|

2 |> (-) <| 3