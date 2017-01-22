(*
    We want to define a function succPairs such that:
    succPairs [] = []
    succPairs [x] = []
    succPairs [x0;x1; . . . ;xn−1] = [(x0,x1);( x1,x2); . . . ;(xn−2,xn−1)]
*)

let rec succPairs = function
    | x0 :: x1 :: xs -> (x0,x1) :: succPairs(x1::xs)
    | _ -> []

let rec succPairs2 = function
| x0::(x1::_ as xs) -> (x0,x1) :: succPairs xs
| _ -> []

let rec sumProd = function
    | [] -> (0,1)
    | x::rest ->
        let (rSum,rProd) = sumProd rest
        (x+rSum,x*rProd)


let sumProd2 l = 
    let rec sumProdRec (s, p) = function
        | [] -> (s, p)
        | x::rest -> sumProdRec (x+s, x*p) rest
    sumProdRec (0, 1) l


[1..30000] |> sumProd // stack overflow
[1..6] |> sumProd2 // tail recursive - no stack overflow

// Unzip

let rec unzip = function
    | [] -> ([],[])
    | (x,y)::rest ->
        let (xs,ys) = unzip rest
        (x::xs,y::ys) // not tail recursive

let rec unzip2 l = 
    let rec unzipRec (l1, l2)= function
        | [] -> (l1, l2)
        | (x,y)::rest ->  unzipRec (x::l1,y::l2) rest

    unzipRec ([], []) (List.rev l)

List.zip [1..30000] [1..30000] |> unzip // stack overflow     
List.zip [1..30000] [1..30000] |> unzip2 // no stack overflow     

// Mix

let rec mix = function
    | (x::xs,y::ys) -> x::y::(mix (xs,ys))
    | ([],[]) -> []
    | _ -> failwith "mix: parameter error"


// Append

let rec (@) xs ys =
    match xs with
    | [] -> ys
    | x::xs -> x::(xs @ ys)

// Reverse
let rec naiveRev xls =
    match xls with
    | [] -> []
    | x::xs -> naiveRev xs @ [x]

let revrev l = 
    let rec rev acc = function
        | [] -> acc
        | l::ls -> rev ((rev l)::acc) ls
    
    rev [] l