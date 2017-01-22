// Times function
let times (m, n) = 
    let rec timesRec acc = function
        | (m, 0) -> acc
        | (m, n) -> timesRec (acc+m) (m, n-1)

    timesRec 0 (m,n)

times (13187,111006)

// Factorial

let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)

fact 3

let fact2 n = 
    let rec fact acc =
        function 
        | 0 -> acc
        | x -> fact (x * acc) (x - 1)
    fact 1 n

fact2 3

let fact3 n = 
    let rec fact =
        function 
        | (x, 0) -> x
        | (x, n) -> fact (x*n, n-1)
    fact (1, n)

fact3 3

// Euclidian Algoritm

let rec gcd (a:int) b = 
    if b = 0 
        then a
        else gcd b (a % b)

gcd 20 50

gcd 456464550 243745720

let rec gcd2 = function
    | (0,n) -> n
    | (m,n) -> gcd2(n % m,m)

gcd2 (116, 36)
gcd2 (36, 116)