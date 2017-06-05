namespace Exercises1

module E = 
    let add a b = a + b

    (* 11.1
        Make a declaration for the sequence of odd numbers.
    *)

    let odd = Seq.initInfinite (fun i -> 2 * i - 1) |> Seq.skip 1
    
    (* 11.2
        Make a declaration for the sequence of numbers 1, 1, 2, 6, . . . , n!, . . ..
    *)

    let factoriels = seq{
        yield 1
        yield! Seq.initInfinite (fun i -> [1..i] |> List.fold (*) 1) |> Seq.skip 1
    }
    
    // #time - run it to see 
    Seq.iter (fun it ->
        Seq.iter (fun i -> (Seq.item i factoriels) |> ignore) [1..13]) [1..100000]


    let cachedFactoriels = Seq.cache (seq{
        yield 1
        yield! (Seq.initInfinite (fun i -> [1..i] |> List.fold (*) 1) |> Seq.skip 1)
    })

    Seq.item 5 cachedFactoriels
    Seq.iter (fun it ->
        Seq.iter (fun i -> (Seq.item i cachedFactoriels) |> ignore) [1..13]) [1..100000]



    (* 11.3
        Make a declaration for the sequence of seq [1; 1; 2; 6; . . . ; n!; . . .], where the i+1’st element is
        generated from the i'th element by multiplication with i + 1.
    *)
    let cachedFactoriels2 = seq{
        yield 1        
        yield! Seq.unfold (fun (prevFact, currentNum) -> 
            Some (currentNum * prevFact, (currentNum * prevFact,   currentNum + 1 ))) (1, 1)      
    }    

    Seq.iter (fun it ->
        Seq.iter (fun i -> (Seq.item i cachedFactoriels2) |> ignore) [1..13]) [1..100000]

    (* 11.4
        Declare a function that, for given i and n, selects the sublist [ai; ai+1; . . . ; ai+n−1] of a sequence
        seq [a0; a1; . . .].
    *)

    let sublist seq i n = 
        if i>n || i < 0
        then Seq.empty
        else 
            [i..n] |> Seq.map (fun i -> Seq.item i seq) 

    sublist cachedFactoriels2 6 10


    (* 11.5
        The declaration of the function iterate f on Page 260 has the drawback that fn x is computed
        when the n'th element is demanded. Give an alternative declaration of this function using
        the property that the n + 1’st element of the sequence can be computed from the n’th element
        by an application of f.
    *)
    let rec iter f x = function
    | 0 -> x
    | n -> iter f (f x) (n-1)


    // let iterCached f x n = 
    //     let rec iterCached f x n = Seq.cache (seq {
    //         match n with 
    //         | 0 -> yield x
    //         | n -> yield! iterCached f (f x) (n-1)
    //     })

    //     Seq.head (iterCached f x n)


    let sss = Seq.initInfinite
    let rec iterCached f x n =  seq {
            match n with 
            | 0 -> yield x
            | n -> yield! iterCached f (f x) (n-1)
        }

    // let iterCached2 f x = function
    // | 0 -> x
    // | n -> Seq.



    iter ((+) 1) 1 10
    iterCached ((+) 1) 1 10

    #time
    Seq.iter (fun it ->
        Seq.iter (fun i -> iter ((+) 1) 1 i |> ignore) [1..1000]) [1..10000]

    Seq.iter (fun it ->
        Seq.iter (fun i -> Seq.head (iterCached ((+) 1) 1 i) |> ignore) [1..1000]) [1..10]    