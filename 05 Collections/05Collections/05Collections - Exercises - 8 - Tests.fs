module Exercises8Tests
open NUnit.Framework
open FsUnitTyped
open Exercises8.E

// Note: assume that original makeBill3 is correct. Do not test results contents. 
//all makeBill3 using fold tests
[<TestFixture>]
type ``all makeBill3 using fold tests``() =
    [<Test>]
    member t.``Same register of two products, then result should be equal for fold and foldBack versions`` () = 
        let register = 
            Map.ofList [
                ("a1", ("cheese", 25)); 
                ("a2", ("herring", 4));
                ("a3", ("soft drink", 5)); 
                ("a4", ("bread", 8))
            ]
        let purchase = Map.ofList [("a1", 3);("a3", 5)]

        let (billWithFoldBack, priceWithFoldBack) = makeBill3FoldBack register purchase
        let (billWithFold, priceWithFold) = makeBill3Fold register purchase 

        List.length billWithFold |> shouldEqual (List.length billWithFoldBack)
        List.iter (fun item -> (List.contains item billWithFoldBack) |> shouldEqual true) billWithFold
        priceWithFold |> shouldEqual priceWithFoldBack

    [<Test>]
    member t.``Same empty register, then result should be equal for fold and foldBack versions`` () = 
        let register = 
            Map.ofList [
                ("a1", ("cheese", 25)); 
                ("a2", ("herring", 4));
                ("a3", ("soft drink", 5)); 
                ("a4", ("bread", 8))
            ]
        let purchase = Map.ofList []

        let (billWithFoldBack, priceWithFoldBack) = makeBill3FoldBack register purchase
        let (billWithFold, priceWithFold) = makeBill3Fold register purchase 

        List.length billWithFold |> shouldEqual (List.length billWithFoldBack)
        List.iter (fun item -> (List.contains item billWithFoldBack) |> shouldEqual true) billWithFold
        priceWithFold |> shouldEqual priceWithFoldBack
