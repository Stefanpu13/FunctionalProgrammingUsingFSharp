namespace Exercises12

#if INTERACTIVE
#load "11Sequences - Exercises - 1 - 6.fs"
#load "11Sequences - Exercises - 10.fs"
#endif

open Exercises1to6.E
open Exercises10.E

module E =     
    (* 11.12
        Solve Exercise 11.7 using sequence expressions..
    *)

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
    
    let generateSummands2 (x:double) = seq{
        let rec generateSummands divident divisor i = seq{
            yield divident / divisor
            yield! generateSummands (divident * x) (divisor * (i)) (i + 1.0) 
        }

        yield! generateSummands 1.0 1.0 1.0
    }

    (*
    #time
    
    let sum1 = generateSummands 2.0
    let sum2 = generateSummands2 2.0

    List.iter(fun i ->
        Seq.item i sum1; ()
    ) [1..10000]    

    
    List.iter(fun i ->
        Seq.item i sum2; ()
    ) [1..10000]    
    *)

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

    let accumulateSeq3 s = 
        let mutable sum = double 0.0
        Seq.cache ( seq {
            for el in s do  
                sum <- el + sum
                yield sum 
        })
    (* "accumulateSeq2" is faster as sum is not recalculated constantly

    #time

    List.iter (fun i -> Seq.item i (accumulateSeq (seq{1.0..10000.0})); ()) [1..150]    
    List.iter (fun i -> Seq.item i (accumulateSeq2 (seq{1.0..10000.0})); ()) [1..150]
    List.iter (fun i -> Seq.item i (accumulateSeq3 (seq{1.0..10000.0})); ()) [1..150]       

    -------------------------------------
    !!!!!!
    Change "accumulateSeq2" declaration by removing "Seq.cache" and see how it breaks
    !!!!!

    let seq1 = accumulateSeq (Seq.initInfinite double) 
    let seq2 = accumulateSeq2 (Seq.initInfinite double)
    let seq3 = accumulateSeq3 (Seq.initInfinite double)

    List.iter ( fun i ->
        printfn "gen1: %A; gen2: %A"( Seq.item i seq1) (Seq.item i seq2)
    ) [1..3]

    *)
    (* 11.7.3
        Declare a function to generate the sequence of approximations for the function e^x on the
        basis of (11.2).
    *)

    let generateSeqOfApproximations summards = accumulateSeq summards
    // let generateSeqOfApproximations2 summards = accumulateSeq2 summards
    // let generateSeqOfApproximations3 summards = accumulateSeq3 summards
    let generateSeqOfApproximationsFunc generateSummandsFunc accumulateSeqFunc = 
        generateSummandsFunc >> accumulateSeqFunc

    (* The second version is faster

    #time
    List.map (fun i -> 
        Seq.item i (generateSeqOfApproximations (generateSummands 2.0))
    ) [0..15] 
    List.map (fun i -> 
        Seq.item i (generateSeqOfApproximations2 (generateSummands 2.0))
    ) [0..15]

    List.map (fun i -> 
        Seq.item i (generateSeqOfApproximations3 (generateSummands 2.0))
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
        let expFuncApproximations = generateSeqOfApproximationsFunc x
        let rec inTolerance i expValue =
            let newExpValue =  Seq.item i expFuncApproximations
            if abs (expValue - newExpValue) > eps
            then inTolerance (i+1) newExpValue
            else newExpValue 
            
        inTolerance 1 (Seq.item 0 expFuncApproximations)

(*
    #time    

    let generateSummardsFuncs = [generateSummands ; generateSummands2]
    let accumulateSeqFuncs : (seq<double> -> seq<double>) list = 
        [accumulateSeq; accumulateSeq2; accumulateSeq3]

    let approximateExpFuncs = 
        (cartesian generateSummardsFuncs accumulateSeqFuncs)
            |> Seq.map (fun (generateSumardsFunc, accumulateSequenceFunc) ->
                generateSeqOfApproximationsFunc generateSumardsFunc accumulateSequenceFunc
            )

    approximateExpFuncs |> Seq.map(fun approximateExpFunc ->
        approximateExpFunction approximateExpFunc 64.0 1E-6
    )

    let generateSeqOfApproximations1 = generateSeqOfApproximationsFunc generateSummands accumulateSeq 
    let generateSeqOfApproximations2 = generateSeqOfApproximationsFunc generateSummands2 accumulateSeq2
    let generateSeqOfApproximations3 = generateSeqOfApproximationsFunc generateSummands2 accumulateSeq3
    
    approximateExpFunction generateSeqOfApproximations1 64.0 1E-6
    approximateExpFunction generateSeqOfApproximations2 68.0 1E-6
    approximateExpFunction generateSeqOfApproximations3 68.0 1E-6
*)