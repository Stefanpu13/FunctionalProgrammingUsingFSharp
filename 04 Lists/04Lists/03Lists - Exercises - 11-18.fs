namespace Exercises11To18
open Helpers.L
open Exercises1To10

module E = 
    (* 4.11
        A list of integers [x0;x1; . . . ;xn−1] is weakly ascending if the elements satisfy:
        x0 ≤ x1 ≤ x2 ≤ . . . ≤ xn−2 ≤ xn−1
        or if the list is empty. The problem is now to declare functions on weakly ascending lists.
    *)

    (* 4.11.1
        Declare an F# function count: int list * int -> int, where count(xs, x) is the
        number of occurrences of the integer x in the weakly ascending list xs.
    *)

    let count (l, num) = Exercises1To10.E.multiplicity num l
    
    let count2 (l, num) = Exercises1To10.E.multiplicity2 num l

    (* 4.11.2
        Declare an F# function insert: int list * int -> int list, where the value of
        insert(xs, x) is a weakly ascending list obtained by inserting the number x into the weakly
        ascending list xs.
    *)

    // not tail recursive
    let insert (l, num) = 
        let rec insert = function 
            | [] -> [num]
            | x::xs ->
                if x < num
                then  x :: insert (xs)
                else num :: x :: xs

        insert l

    // tail recursive
    let insert2 (l, num) =         
        let rec insert2 acc = function 
            | [] -> rev (num::acc)
            | x::xs ->
                if x < num
                then insert2 (x::acc) xs
                else (rev (x::num::acc)) @ xs

        insert2 [] l

    let insert3 (l, num) =
        let smaller, larger = partition (fun el -> el < num) l
        smaller @ num::larger

    (* 4.11.3
        Declare an F# function intersect: int list * int list -> int list, where the
        value of intersect(xs, xs ) is a weakly ascending list containing the common elements
        of the weakly ascending lists xs and xs
        . For instance:
        intersect([1;1;1;2;2], [1;1;2;4]) = [1;1;2]
    *)

    let intersect (l1, l2) = 
        let rec intersect commonElems = function
            | (x::xs, y::ys) -> 
                match (x, y) with
                | (sm, lg) when sm < lg -> intersect commonElems (xs, y::ys)
                | (lg, sm) when lg > sm -> intersect commonElems (x::xs, ys)
                | (eq1, eq2) -> intersect (eq1::commonElems) (xs, ys)
            | _ -> rev commonElems

        intersect ([]) (l1, l2)

    (* 4.11.4
        Declare an F# function plus: int list * int list -> int list, where the value of
        plus(xs, xs) is a weakly ascending list, that is the union of the weakly ascending lists xs
        and xs. For instance: plus([1;1;2],[1;2;4]) = [1;1;1;2;2;4]
    *)

    let plus (l1, l2) : int list = 
        let rec plus union = function
            | [], [] -> rev union
            // Can merge "ls,[]" and "[], ls" in one match, but here I find this more readable
            | ls, [] -> (rev union) @ ls
            | [], ls -> (rev union) @ ls
            | x::xs, y::ys ->
                if x < y 
                then plus (x::union) (xs, y::ys)
                else plus (y::union) (x::xs, ys)
        
        plus [] (l1, l2)

    (* 4.11.5
        Declare an F# function minus: int list * int list -> int list, where the value
        of minus(xs, xs') is a weakly ascending list obtained from the weakly ascending list xs by
        removing those elements, that are also found in the weakly ascending list xs'
       . For instance: 
        minus([1;1;1;2;2],[1;1;2;3]) = [1;2]
        minus([1;1;2;3],[1;1;1;2;2]) = [3]
    *)

    let minus (l1,l2) : int list =
        let rec minus l1UniqueElems = function
            // Can merge "[],[]" and "[], ls" in one match, but here I find this more readable     
            | [], [] -> rev l1UniqueElems
            | ls, [] -> (rev l1UniqueElems) @ ls    
            | [], ls -> rev l1UniqueElems
            | x::xs, y::ys ->                    
                match (x, y) with
                | (sm, lg) when sm < lg -> minus (sm::l1UniqueElems) (xs, lg::ys)
                | (lg, sm) when lg > sm -> minus (l1UniqueElems) (lg::xs, ys)
                | (eq1, eq2) -> minus l1UniqueElems (xs, ys)  
        
        minus [] (l1, l2)

    (* 4.12
        Declare a function sum(p, xs) where p is a predicate of type int -> bool and xs is a list of
        integers. The value of sum(p, xs) is the sum of the elements in xs satisfying the predicate p.
        Test the function on different predicates (e.g., p(x) = x > 0).
    *)

    let sum (predicate, xs) = 
        let rec sum s = function
            | [] -> s
            | x::xs ->
                if predicate x 
                then sum (s+x) xs
                else sum s xs
        sum 0 xs

    (* 4.13
        Naive sort function:
        1. Declare an F# function finding the smallest element in a non-empty integer list.
        2. Declare an F# function delete: int * int list -> int list, where the value of
        delete(a, xs) is the list obtained by deleting one occurrence of a in xs (if there is one).
        3. Declare an F# function that sorts an integer list so that the elements are placed in weakly
        ascending order.
        Note that there is a much more efficient sort function List.sort in the library.
    *)

    (* 4.13.1
        1. Declare an F# function finding the smallest element in a non-empty integer list.
    *)

    let min l = 
        let rec min = function
            | (None, []) -> None       
            | (Some x, []) -> Some x     
            | (Some x, y::ys) when x < y -> min ((Some x), ys)
            | (_, y::ys) -> min ((Some y), ys)           
        
        min (None, l)

    (* 4.13.2
        Declare an F# function delete: int * int list -> int list, where the value of
        delete(a, xs) is the list obtained by deleting one occurrence of a in xs (if there is one).
    *)

    let delete (a, l) = 
        let rec delete acc = function
            | [] -> rev acc
            | x::xs ->
                if x = a 
                then (rev acc) @ xs
                else delete (x::acc) xs  

        delete [] l
    
    (* 4.13.3
        Declare an F# function that sorts an integer list so that the elements are placed in weakly
        ascending order.
        Note that there is a much more efficient sort function List.sort in the library.
    *)

    let sort l = 
        let rec sort sortedElems xs=
            match min xs with
            | Some m ->
                let xss = delete (m, xs)
                sort (m::sortedElems) xss
            | None -> rev sortedElems

        sort [] l

    (* 4.14
        Declare a function of type int list -> int option for finding the smallest element in an
        integer list.
    *)

    // same as 4.13.1
    let smallest = min  

    (* 4.15
        Declare an F# function revrev working on a list of lists, that maps a list to the reversed list of
        the reversed elements, for example: revrev [[1;2];[3;4;5]] = [[5;4;3];[2;1]]
    *)

    let revrev l = 
        let rec revrev reversedElems = function
            | [] -> reversedElems
            | l::ls -> revrev ((rev l)::reversedElems) ls
        
        revrev [] l 

    (* 4.16
        Consider the declarations:
        let rec f = function
        | (x, []) -> []
        | (x, y::ys) -> (x+y)::f(x-1, ys);;

        let rec g = function
        | [] -> []
        | (x,y)::s -> (x,y)::(y,x)::g s;;

        let rec h = function
        | [] -> []
        | x::xs -> x::(h xs)@[x];;

        Find the types for f, g and h and explain the value of the expressions:
        1. f(x, [y0,y1, . . . ,yn−1]), n ≥ 0
        2. g[(x0, y0),(x1, y1), . . . ,(xn−1, yn−1)], n ≥ 0
        3. h[x0,x1, . . . ,xn−1], n ≥ 0

    f: int * int list -> int list
    1. f(x, [y0,y1, . . . ,yn−1]) = [y0+x; y1+(x-1);....(yn−1)+(x-(n-1))]

    g:  ('a * 'a) list -> ('a * 'a) list  
    g[(x0, y0),(x1, y1), . . . ,(xn−1, yn−1)] = [(x0, y0);(y0, x0);(x1, y1);(y1, x1), . . . ,(xn−1, yn−1); (yn−1, xn−1)]

    h: 'a list -> 'a list
    h[x0,x1, . . . ,xn−1] = [x0; x1; ... xn-1; xn-1;xn-2;....x0]

    *)


    (* 4.17
        Consider the declaration:
    let rec p q = function
        | [] -> []
        | x::xs -> 
            let ys = p q xs
            if q x then x::ys else ys@[x]

        Find the type for p and explain the value of the expression:
        p q [x0; x1; x3; . . . ; xn−1]


        p: ('a -> bool) -> 'a list -> 'a list

        p q [x0; x1; x3; . . . ; xn−1]
        starting from last el of a list, if q true for el prepend else append to new list
    *)

    // p (fun x -> x%2 = 0) [1; 2; 3; 4;]


    (* 4.18
        Consider the declaration:
    let rec f g = function
        | [] -> []
        | x::xs -> g x :: f (fun y -> g(g y)) xs;;

        Find the type for f and explain the value of the expression:
        f g [x0; x1; x2; . . . ; xn−1]

        ('a -> 'a) -> 'a list -> 'a list
        f g [x0; x1; x2; . . . ; xn−1] = [g(x0); g(g(x1)); g(g(g(x2)));...]
        apply the function g m-times over element xm 
    *)

    // f (fun x -> x+2) [1; 2; 3; 4]

