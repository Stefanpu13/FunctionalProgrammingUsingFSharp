module Exercises6Tests
open NUnit.Framework
open FsUnitTyped
open Exercises6.E
open System


let rand = Random()
// Test imperative fibonacci function
[<TestFixture>]
type ``Test imperative fibonacci function``() =

    [<Test>]
    member t.``recursive and imperative fibonacci function should produce the same results"`` () = 
        let ``recursive and imperative fibonacci are same`` _ = 
            let m = rand.Next(100)
            let n = m + rand.Next(100)
            let l = [m..n]

            (l |> List.map fibonacci)  |> shouldEqual <| (l |> List.map fibonacciImperative)
            
        [1..100] |> List.iter ``recursive and imperative fibonacci are same``
    
    


