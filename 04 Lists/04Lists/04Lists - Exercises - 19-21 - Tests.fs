module Exercises19To21Tests
open NUnit.Framework
open FsUnit
open FsUnitTyped
open Exercises19To21.E
open Exercises19To21.E.ColMap_4_21

// colMap tests
[<TestFixture>]
type ``colMap tests``() = 
    [<Test>]
    member test.``Only two contries that are neighbours, should have length 2"``() = 
        let exMap = 
            Map [
                Neighbours (Country "a",Country "b");            
            ]        
        
        let (Colouring colours) = colMap exMap
        colours |> shouldHaveLength 2        

    [<Test>]
    member test.``Two neighbours (a, b) and Island 'e', should equal [['b'; 'e']; ['a']]"``() = 
        let exMap = 
            Map [
                Neighbours (Country "a",Country "b");            
                Island (Country "e")
            ]
        
        let colouring = Colouring [Colour [Country "b"; Country "e"]; Colour [Country "a"]]        
        colMap exMap |> shouldEqual colouring

    [<Test>]
    member test.``A few neighbouring countries, should have two colours"``() = 
        let exMap = 
            Map [
                Neighbours (Country "a",Country "b");
                Neighbours (Country "c", Country "d");
                Neighbours (Country "d", Country "a");
                Island (Country "e")
            ]
        
        let colouring = 
            Colouring [
                Colour [Country "c"; Country "a"; Country "e"]; 
                Colour [Country "b"; Country"d"]
            ]        
        colMap exMap |> shouldEqual colouring
    
    [<Test>]
    member test.``More neighbouring countries, should have three colours"``() = 
        let exMap = 
            Map [
                Neighbours (Country "a", Country "b");
                Neighbours (Country "c", Country "d");
                Neighbours (Country "d", Country "a");
                Neighbours (Country "c", Country "b");
                Neighbours (Country "d", Country "b");
                Neighbours (Country "a", Country "i");
                Island (Country "n")
                Island (Country "k")
            ]
        
        let colouring = 
            Colouring [
                Colour [Country "b"; Country "i"; Country "n"; Country "k"];
                Colour [Country "c"; Country "a"];
                Colour [Country "d"]
            ]        
        colMap exMap |> shouldEqual colouring        
