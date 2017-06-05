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
    
    #time
    Seq.iter (fun it ->
        Seq.iter (fun i -> (Seq.item i factoriels) |> ignore) [1..13]) [1..100000]


    let cachedFactoriels = Seq.cache (seq{
        yield 1
        yield! (Seq.initInfinite (fun i -> [1..i] |> List.fold (*) 1) |> Seq.skip 1)
    })

    Seq.item 5 cachedFactoriels
    Seq.iter (fun it ->
        Seq.iter (fun i -> (Seq.item i cachedFactoriels) |> ignore) [1..13]) [1..100000]




    let cachedFactoriels2 = seq{
        yield 1        
        yield! Seq.unfold (fun (prevFact, currentNum) -> 
            Some (currentNum * prevFact, (currentNum * prevFact,   currentNum + 1 ))) (1, 1)      
    }

    
    // let cachedFactoriels2 = 
    //     Seq.append [1] (Seq.unfold (fun (prevFact, currentNum) -> 
    //         Some (currentNum * prevFact, (currentNum * prevFact,   currentNum + 1 ))) (1, 1))
    

    Seq.iter (fun it ->
        Seq.iter (fun i -> (Seq.item i cachedFactoriels2) |> ignore) [1..13]) [1..100000]

    