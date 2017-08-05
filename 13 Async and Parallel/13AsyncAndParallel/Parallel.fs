module Parallel

open System
let isPrime =
    let rec testDiv a b c =
        a>b || c%a <> 0 && testDiv (a+1) b c
    function
    | 0 | 1 -> false
    | n -> testDiv 2 (n-1) n


// [Int32.MaxValue - 5..Int32.MaxValue] |> List.filter (fun i-> isPrime (202221233 + i))

// isPrime 2022231233

let gen = 
    let generator = System.Random()
    generator.Next

// gen 10000


let arr = [|1..550|]

let lambda i = 
    let num = 202221233 + i
    printfn "num: %i; is prime: %b" num (isPrime num)

// #time
Array.Parallel.iter lambda arr
Array.iter lambda arr


// ------------------- Bin treee. Task parallelism

type BinTree<'a> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>


let rec exists p t =
    match t with
    | Leaf -> false
    | Node(_,v,_) when p v -> true
    | Node(tl,_,tr) -> exists p tl || exists p tr

let rec genTree n range =
    if n=0 then Leaf
    else 
        let tl = genTree (n-1) range
        let tr = genTree (n-1) range
        Node(tl, gen range, tr)

let t = genTree 25 3000

// exists (fun n -> isPrime n && n>10000) t

open System.Threading.Tasks
let rec parExists p t =
    match t with
    | Leaf -> false
    | Node(_,v,_) when p v -> true
    | Node(tl,_,tr) ->
        let b1 = Task.Factory.StartNew(fun () -> parExists p tl)
        let b2 = Task.Factory.StartNew(fun () -> parExists p tr)
        b1.Result||b2.Result

// parExists (fun n -> isPrime n && n>10000) t

let rec parExistsDepth p t n =
    if n=0 then exists p t
    else 
        match t with
        | Leaf -> false
        | Node(_,v,_) when p v -> true
        | Node(tl,_,tr) ->
            let b1 = Task.Factory.StartNew(fun () -> 
                parExistsDepth p tl (n-1))
            let b2 = Task.Factory.StartNew(fun () -> 
                parExistsDepth p tr (n-1))
            b1.Result||b2.Result

// parExistsDepth (fun n -> isPrime n && n>10000) t 4

// -------------------- Quick sort

let swap (a: 'a[]) i j =
    let v = a.[i]
    a.[i] <- a.[j]
    a.[j] <- v

let rec partition (a:'a[]) v k1 k2 =
    if k2=k1-1 then k2 //empty section
    else if a.[k2] >= v then 
        partition a v k1 (k2-1)
    else 
        swap a k1 k2
        partition a v (k1+1) k2

let rec qsort a i j =
    if j-i>1 then 
        let k = partition a a.[i] (i+1) (j-1)
        swap a i k
        qsort a i k
        qsort a (k+1) j    

let sort a = qsort a 0 (Array.length a)

let rec pqsort a i j depth =
    if j-i<= 1 then ()
    else if depth=0 then qsort a i j
    else 
        let k = partition a a.[i] (i+1) (j-1)
        swap a i k
        let s1 = Task.Factory.StartNew(fun () -> 
            pqsort a i k (depth-1))
        let s2 = Task.Factory.StartNew(fun () -> 
            pqsort a (k+1) j (depth-1))
        Task.WaitAll[|s1;s2|]

let parSort a d = pqsort a 0 (Array.length a) d

let a32 = Array.init 3200000 (fun _ -> gen 1000000000) 
let a32cp = Array.copy a32

sort a32
parSort a32cp 7
