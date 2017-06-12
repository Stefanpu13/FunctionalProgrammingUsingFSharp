module Exercises8Tests

open NUnit.Framework
open FsUnitTyped
open Exercises8.E
open System



// Test piApprox
[<TestFixture>]
type ``Test piApprox``() =       

    [<Test>]
    member t.``For n in [100..1000..100000] "piApprox n" should be closer to pi than "piApprox (n-1)"`` () = 
        [100.0..1000.0..100000.0] 
            |> List.pairwise
            |> List.iter (fun (prev, curr) ->                
                abs (Math.PI - piApprox prev) > abs (Math.PI - piApprox curr) |> shouldEqual <| true 
            )

    [<Test>]
    member t.``For random n in [100..100000] "piApprox n" is equal to "piApprox n"`` () = 
        let rand = Random()
        List.iter(fun _ ->
            let n = rand.Next(100, 1000000) |> float
            piApprox n |> shouldEqual <| piApprox2 n
        ) [1..10]

 