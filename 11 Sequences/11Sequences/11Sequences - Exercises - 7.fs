namespace Exercises7

#if INTERACTIVE
#load "11Sequences - Exercises - 1 - 6.fs"
#endif

open Exercises1to6.E

module E =     
    (* 11.7
        The exponential functions can be approximated using the Taylor’s series:
        ex =1/0! + x^1/1! + · · · + x^k/k! +  · · · (11.2)
    *)
    (*11.7.1
        Declare a function that for a given x can generate the sequence of summands in (11.2). Hint:
        Notice that the next summand can be generated from the previous one.
    *)
    
    let generateSummands (x:double) = 
        Seq.unfold (fun (divident, divisor, i) -> 
            let el = divident, divisor
            let state = divident * x, divisor * (i), i + 1.0 
            Some (el, state)
        ) (1.0, 1.0, 1.0) 
        |> Seq.map (fun (divident, divisor) -> divident/divisor)

    // generateSummands 5

    (* 11.7.2
        Declare a function that accumulates the elements of a sequence of floats. I.e. given a sequence
        seq [x0; x1; x2; . . .] it generates the sequence seq [x0; x0 + x1; x0 + x1 + x2; . . .].
    *)

    let accumulateSeq s : seq<double> = 
        Seq.mapi (fun i _ -> Seq.sum (sublist s 0 (i+1))) s

    // Much faster as sum is not recalculated constantly
    let accumulateSeq2 s  = 
        let mutable sum = double 0.0
        // Sequence needs to be cached to avoid accumulation of sum 
        // every time sequence is accessed. This is because a closure is formed that has access
        // to ever changing, but never reset, "sum" variable  
        Seq.cache (Seq.map (fun el -> sum <- el + sum; sum) s)

    (* "accumulateSeq2" is faster as sum is not recalculated constantly

    #time

    List.iter (fun i -> Seq.item i (accumulateSeq (seq{1.0..10000.0})); ()) [1..150]    
    List.iter (fun i -> Seq.item i (accumulateSeq2 (seq{1.0..10000.0})); ()) [1..150]    

    -------------------------------------
    !!!!!!
    Change "accumulateSeq2" declaration by removing "Seq.cache" and see how it breaks
    !!!!!

    let seq1 = accumulateSeq (Seq.initInfinite double) 
    let seq2 = accumulateSeq2 (Seq.initInfinite double)

    List.iter ( fun i ->
        printfn "gen1: %A; gen2: %A"( Seq.item i seq1) (Seq.item i seq2)
    ) [1..3]

    *)
    (* 11.7.3
        Declare a function to generate the sequence of approximations for the function e^x on the
        basis of (11.2).
    *)

    let generateSeqOfApproximations summards = accumulateSeq summards
    let generateSeqOfApproximations2 summards = accumulateSeq2 summards

    (* The second version is faster

    #time
    List.map (fun i -> 
        Seq.item i (generateSeqOfApproximations (generateSummands 2.0))
    ) [0..15] 
    List.map (fun i -> 
        Seq.item i (generateSeqOfApproximations2 (generateSummands 2.0))
    ) [0..15]

    *)
    (* 11.7.4
    
        Declare a function to approximate e^x within a given tolerance.
    *)

    // Note: type inference is based on usage. If ONLY THIS function is sent to the terminal
    // it will assume that default numeric type is int, but the IDE (VS Code in my case) will
    // infer to be other type if later in the code it is used with double. To avoid that,
    // either specify return type, or send function to terminal tegother with a line that uses it.

    let approximateExpFunction generateSeqOfApproximationsFunc x eps : double = 
        let expFuncApproximations = generateSeqOfApproximationsFunc (generateSummands x)
        let rec inTolerance i expValue =
            let newExpValue =  Seq.item i expFuncApproximations
            if abs (expValue - newExpValue) > eps
            then inTolerance (i+1) newExpValue
            else newExpValue 
            
        inTolerance 1 (Seq.item 0 expFuncApproximations)

    // #time
    // approximateExpFunction generateSeqOfApproximations 64.0 1E-6
    // approximateExpFunction generateSeqOfApproximations2  64.0 1E-6