module Exercises12Tests

#if INTERACTIVE
#I @"bin\Debug"
#r "nunit.framework.dll"
#r  "FsUnit.NUnit.dll"
#endif

open NUnit.Framework
open FsUnitTyped
open Exercises12.E
open System

let rand = Random ()

// Test generateSummands
[<TestFixture>]
type ``Test generateSummands``() =       

    [<Test>]
    member t.``For random number in [1..10] "generateSummands" should equal "generateSummands2"`` () = 
        List.iter(fun _ ->
            let num = rand.Next(1, 10) |> double
            
            let summards = generateSummands num
            let summards2 = generateSummands2 num
            List.iter (fun i ->
                Seq.item i summards |> shouldEqual <| Seq.item i summards2
            ) [0..3]
        ) [1..10]
        
        
// Test accumulateSeq
[<TestFixture>]
type ``Test accumulateSeq``() =       

    [<Test>]
    member t.``For natural numbers "accumulateSeq", "accumulateSeq2" and accumulateSeq3" should be equal`` () = 
        let seq1 = accumulateSeq (Seq.initInfinite double) 
        let seq2 = accumulateSeq2 (Seq.initInfinite double) 
        let seq3 = accumulateSeq3 (Seq.initInfinite double) 
        
        List.iter (fun _ ->
            let i = rand.Next (1,100)
            Seq.item i seq1 |> shouldEqual <| Seq.item i seq2
            Seq.item i seq2 |> shouldEqual <| Seq.item i seq3
        ) [1..10]
        
        
// Test approximateExpFunction
[<TestFixture>]
type ``Test approximateExpFunction``() =       

    [<Test>]
    member t.``For eps = 1E-6 and random x in [1..10], abs (result - (exp x)) < eps`` () = 
        let eps = 1e-6
        let generateSeqOfApproximations1 = 
            generateSeqOfApproximationsFunc generateSummands accumulateSeq 
        let generateSeqOfApproximations2 = 
            generateSeqOfApproximationsFunc generateSummands2 accumulateSeq2
        let generateSeqOfApproximations3 = 
            generateSeqOfApproximationsFunc generateSummands2 accumulateSeq3
        
        List.iter (fun i ->            
            let num = double i
            let approxsExp1 = approximateExpFunction generateSeqOfApproximations1 num eps
            let approxsExp2 = approximateExpFunction generateSeqOfApproximations1 num eps
            let approxsExp3 = approximateExpFunction generateSeqOfApproximations1 num eps

            true |> shouldEqual <| (abs ((exp num) - approxsExp1) < eps)
            true |> shouldEqual <| (abs ((exp num) - approxsExp2) < eps)
            true |> shouldEqual <| (abs ((exp num) - approxsExp3) < eps) 
        ) [1..15]       

    [<Test>]
    member t.``For random x in [1..30], "generateSeqOfApproximations" functions should be equal`` () = 
        let eps = 1e-6
        let generateSeqOfApproximations1 = 
            generateSeqOfApproximationsFunc generateSummands accumulateSeq
        let generateSeqOfApproximations2 = 
            generateSeqOfApproximationsFunc generateSummands2 accumulateSeq2
        let generateSeqOfApproximations3 = 
            generateSeqOfApproximationsFunc generateSummands2 accumulateSeq3
        
        List.iter (fun i ->            
            let num = double i
            let approxsExp1 = approximateExpFunction generateSeqOfApproximations1 num eps
            let approxsExp2 = approximateExpFunction generateSeqOfApproximations2 num eps
            let approxsExp3 = approximateExpFunction generateSeqOfApproximations3 num eps

            approxsExp1 |> shouldEqual <| approxsExp2
            approxsExp2 |> shouldEqual <| approxsExp3
        ) [1..15]       
