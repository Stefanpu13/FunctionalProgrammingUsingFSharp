module Exercises5Tests
open NUnit.Framework
open FsUnitTyped
open Exercises5.E
open System


let rand = Random()
// Test imperative gcd function
[<TestFixture>]
type ``Test imperative gcd function``() =

    [<Test>]
    member t.``recursive and imperative gcd function should produce the same results"`` () = 
        let ``recursive and imperative gcd are same`` i = 
            let m = rand.Next(100)
            let n = rand.Next(100)

            gcd (m, n) |> shouldEqual <| gcdRec (m, n)
            
        [1..100] |> List.iter ``recursive and imperative gcd are same``
    
    


