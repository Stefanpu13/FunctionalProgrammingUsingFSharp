module Exercises10Tests

open NUnit.Framework
open FsUnitTyped
open Exercises10.E
open System



// Test cartesian
[<TestFixture>]
type ``Test cartesian``() =       

    [<Test>]
    member t.``For seqx = [1;2] and seqy = [3;4;5] result should be [1,3; 1,4; 1,5; 2,3 ;2,4; 2,5]"`` () = 
        cartesian [1;2] [3;4;5] |> shouldEqual <| seq [1,3; 1,4; 1,5; 2,3 ;2,4; 2,5]

    [<Test>]
    member t.``For random seqx and seqy "cartesian seqx seqy" should equal "cartesian seqx seqy"`` () = 
        let rand = Random()
        List.iter(fun _ ->
            let startX = rand.Next(1, 20)
            let endX = rand.Next(startX, 30)
            let startY = rand.Next(1, 20)
            let endY = rand.Next(startX, 30)

            cartesian [startX..endX] [startY..endY] 
                |> shouldEqual <| 
                cartesian2 [startX..endX] [startY..endY]
        ) [1..10]

 