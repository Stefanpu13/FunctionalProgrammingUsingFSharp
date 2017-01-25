namespace Values

module E = 

    (* 2. 1 
        Declare a function f: int -> bool such that f(n) = true exactly when n is divisible by 2
        or divisible by 3 but not divisible by 5. Write down the expected values of f(24), f(27), f(29)
        and f(30) and compare with the result.
    *)

    let f = function
    | tr when (tr % 2 = 0 || tr % 3 = 0) && tr % 5 <> 0 -> true
    | _ -> false

    (* 2.2
        Declare an F# function pow: string * int -> string, where:
        pow(s, n) = s · s ··· · · s    
        where we use · to denote string concatenation. (The F# representation is +.)
    *)

    let pow (s, n) =
        let rec p acc = function
            // | 0 -> acc
            | negOrZero when negOrZero <= 0 -> acc
            | n -> p (acc + s) (n - 1)
        p "" n

    (* 2.3
        Declare the F# function isIthChar: string * int * char -> bool where the value of
        isIthChar(str, i, ch) is true if and only if ch is the i’th character in the string str (numbering
        starting at zero).
    *)

    let isIthChar ((s:string), i, c) = 
        if i < 0 || i >=s.Length 
        then false
        else s.[i] = c

    (* 2.4
        Declare the F# function occFromIth: string * int * char -> int where
        occFromIth(str, i, ch) = the number of occurrences of character ch
        in positions j in the string str with j ≥ i.
    *)

    let occFromIth ((s:string), i, c) = 
        let rec occFromIthRec acc = function
            | l when l >= s.Length || l < 0 -> acc 
            | n -> 
                if s.[n] = c 
                then occFromIthRec (acc + 1) (n + 1)
                else occFromIthRec acc (n + 1)
        
        occFromIthRec 0 i

    let occFromIth2 ((s:string), i, c) =
        if i < 0 || i >= s.Length 
        then 0
        else 
            s.Substring i 
            |> Seq.filter ((=) c)
            |> Seq.length

    (* 2.5
        Declare the F# function occInString: string * char -> int where
        occInString(str, ch) = the number of occurrences of character ch
        in the string str.
    *)

    let occInString ((s:string), c) = occFromIth (s, 0, c)   

    (* 2.6
        Declare the F# function notDivisible: int * int -> bool where
        notDivisible(d, n) is true if and only if d is not a divisor of n.
        For example notDivisible(2,5) is true, and notDivisible(3,9) is false.
    *)
    let notDivisible (d, n) =
        if d = 0 
        then true
        else n % d <> 0
        
    (* 2.7
        1. Declare the F# function test: int * int * int -> bool. The value of test(a, b, c),
        for a ≤ b, is the truth value of:
        notDivisible(a, c)
        and notDivisible(a + 1, c)
        ...
        and notDivisible(b, c)

        2. Declare an F# function prime: int -> bool, where prime(n) = true, if and only if n
        is a prime number.
        3. Declare an F# function nextPrime: int -> int, where nextPrime(n) is the smallest
        prime number > n.
    *)

    // 1.
    let test (a, b, c) =    
        if a <= b
        then [a..b] |> List.fold (fun isNotDivisible x -> isNotDivisible && notDivisible (x, c) ) true
        else false

    test (3, 5, 69)

    // 2.
    let prime = function    
        |smThanTwo when smThanTwo < 2 -> false
        |2|3 -> true    
        |n ->
            let sqrtOfN = (n |> float |> sqrt |> int)  
            test (2, sqrtOfN, n)

    [1..100] |> List.filter prime |> List.iter (printf "%i ") 

    // 3. 
    let nextPrime n =
        let rec nextPrimeRec num = 
            match prime num with
            |true -> num
            |false -> nextPrimeRec (num + 1)
        nextPrimeRec (n + 1)

    nextPrime 2123123124

    (* 2.8
        The following figure gives the first part of Pascal’s triangle:
            1
        1 1
        1 2 1
        1 3 3 1
        1 4 6 4 1
        The entries of the triangle are called binomial coefficients.
        Declare an F# function bin: int * int -> int to compute binomial coefficients.
    *)

    // See https://en.wikipedia.org/wiki/Binomial_coefficient#Multiplicative_formula
    let  getCoeficient (n, k) = 
        {1..k} |> 
        Seq.fold (fun res i -> res * (n + 1 - i) / i) 1

    getCoeficient (5, 3)

    (* 2.9
        Consider the declaration:
        let rec f = function
        | (0,y) -> y
        | (x,y) -> f(x-1, x*y);;
        1. Determine the type of f.
        2. For which arguments does the evaluation of f terminate?
        3. Write the evaluation steps for f(2,3).
        4. What is the mathematical meaning of f(x, y)?

    *)

    // 2.9.1 -> type of f2: (int * int) -> int
    // 2.9.2 -> x = 0
    // 2.9.3 -> f(2, 3) -> f(1 , 2 * 3) -> f(0, 1*2*3) - >1*2*3
    // 2.9.4 -> y*x!

    let rec f2 = function
    | (0,y) -> y
    | (x,y) -> f2(x-1, x*y)

    f2(3, 5)

    (* 2.10
        Consider the following declaration:
        let test(c,e) = if c then e else 0;;
        1. What is the type of test?
        2. What is the result of evaluating test(false, fact(-1))?
        3. Compare this with the result of evaluating
        if false then fact -1 else 0
    *)

    // 2.10.1 -> type of f: (bool * int) -> int
    // 2.10.2 -> x = 0
    // 2.9.3 -> x = 0

    let rec fact = function
    | 0 -> 1
    | n -> n * fact(n-1)
    let test2(c,e) = if c then e else 0

    if false then fact -1 else 0

    // stack overflow
    test2(false, fact(-1))

    (* 2.11

        Declare a function VAT: int -> float -> float such that the value VAT n x is obtained
        by increasing x by n percent.
        Declare a function unVAT: int -> float -> float such that
        unVAT n (VAT n x) = x
    *)

    let VAT n x = x + (x *  (float n/100.0))

    VAT 12 52.0

    let unVAT n x = x / (1.0 + float n/100.0)

    unVAT 15 (VAT 15 52.0)
    unVAT 13 (VAT 13 68.0)

    (* 2.12
        Declare a function min of type (int -> int) -> int. The value of min(f) is the smallest
        natural number n where f(n) = 0 (if it exists).
    *)

    let min f = 
        let rec findMin = function
        | neg when neg < 0 -> neg
        | res when f res = 0 -> res
        | n -> findMin (n + 1)
        
        findMin 1

    min (fun x -> 2 * x - x)

    let pow2 x n = 
        let rec powRec res = function 
        | 1 -> res
        | n -> powRec (res * x) (n - 1)

        powRec x n

    pow2 2 31

    // pow 2 32  breaks the function, because of integer overflow
    min (pow2 2)    

    (* 2.13
        The functions curry and uncurry of types
        curry : (’a * ’b -> ’c) -> ’a -> ’b -> ’c
        uncurry : (’a -> ’b -> ’c) -> ’a * ’b -> ’c
        are defined in the following way:
        curry f is the function g where g x is the function h where h y = f(x, y).
        uncurry g is the function f where f(x, y) is the value h y for the function h = g x.
        Write declarations of curry and uncurry.
    *)
    let curry f = 
        let g x =
            let h y =
                f (x, y)
            h
        g
    let add3 = curry (fun (a, b) -> a + b + 1) 2

    add3 6

    let uncurry g = 
        let f (x, y) = 
            // let h = g x
            // h y
            g x y
        f

    let add2 = uncurry (fun a b-> a + b + 2)

    add2 (2, 3)

    //Better sollution for problem 2.13
    let curry2 f = fun a -> fun b -> f (a,b)
    let uncurry2 f = fun (a,b) -> f a b