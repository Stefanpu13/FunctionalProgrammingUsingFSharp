module Exercises7Tests

#if INTERACTIVE
#I @"bin\Debug"
#r "nunit.framework.dll"
#r  "FsUnit.NUnit.dll"
#endif

open NUnit.Framework
open FsUnitTyped
open Exercises7.E
open System

let rand = Random ()

// Test generateSummands
[<TestFixture>]
type ``Test generateSummands``() =       

    [<Test>]
    member t.``For random number in [1..10] "generateSummands" should generate Taylor series members`` () = 
        List.iter(fun _ ->
            let num = rand.Next(1, 10) |> double
            let taylor = [1.0; num / 1.0; (pown num 2) / 2.0;(pown num 3) / 6.0 ]
            let summards = generateSummands num
            List.iter (fun i ->
                Seq.item i summards |> shouldEqual <| Seq.item i taylor
            ) [0..3]
        ) [1..10]
        
        
// Test accumulateSeq
[<TestFixture>]
type ``Test accumulateSeq``() =       

    [<Test>]
    member t.``For natural numbers the first 5 members must be [1;3;6;10;15] `` () = 
        let seq1 = accumulateSeq (Seq.initInfinite double) 
        let expected = [0;1;3;6;10;15] |> Seq.map double
        List.iter (fun i ->
            Seq.item i seq1 |> shouldEqual <| Seq.item i expected
        ) [0..4]

    [<Test>]
    member t.``For natural numbers "accumulateSeq" and "accumulateSeq2" should be equal`` () = 
        let seq1 = accumulateSeq (Seq.initInfinite double) 
        let seq2 = accumulateSeq2 (Seq.initInfinite double) 
        
        List.iter (fun _ ->
            let i = rand.Next (1,100)
            Seq.item i seq1 |> shouldEqual <| Seq.item i seq2
        ) [1..10]
        
        
// Test approximateExpFunction
[<TestFixture>]
type ``Test approximateExpFunction``() =       

    [<Test>]
    member t.``For eps = 1E-6 and random x in [1..10], abs (result - (exp x)) < eps`` () = 
        let eps = 1e-6
        
        List.iter (fun i ->            
            let num = double i
            let approxsExp = approximateExpFunction generateSeqOfApproximations num eps           

            true |> shouldEqual <| (abs ((exp num) - approxsExp) < eps)            
        ) [1..15]       

    [<Test>]
    member t.``For random x in [1..30], "generateSeqOfApproximations" functions should be equal`` () = 
        let eps = 1e-6
        
        List.iter (fun i ->            
            let num = double i
            let approxsExp1 = approximateExpFunction generateSeqOfApproximations num eps           
            let approxsExp2 = approximateExpFunction generateSeqOfApproximations2 num eps           

            approxsExp1 |> shouldEqual <| approxsExp2
        ) [1..15]       
