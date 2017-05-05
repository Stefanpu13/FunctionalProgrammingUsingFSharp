module Exercises11Tests
open NUnit.Framework
open FsUnitTyped
open Exercises8and9.E
open Exercises11.E
open System

// initial test
[<TestFixture>]
type ``Test countA``() =

    [<Test>]
    member t.``countA  test`` () = 
        countA 0 (leftTree 200(Leaf, 0)) |> shouldEqual <| 201
        

    
