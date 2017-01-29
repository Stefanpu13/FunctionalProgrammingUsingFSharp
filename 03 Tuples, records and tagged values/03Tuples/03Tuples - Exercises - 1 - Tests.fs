module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises1.E

// "Comes before" is custom operator - (<.) for triples and (<..) for records
[<TestFixture>]
type ``Comes before tests``() = 
    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,56,"AM"), output should be true``() = 
        let t1 = TimeOfDayTriple ( 11, 55, "AM")
        let t2 = TimeOfDayTriple (11, 56, "АM")

        t1 <. t2 |> shouldEqual true
        
        let t3 = {Hour= 11; Minute= 55; F = "AM"}
        let t4 = {Hour= 11; Minute= 56; F = "АM"} 

        t3 <.. t4 |> shouldEqual true

    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,54,"AM"), output should be false``() = 
        let t1 = TimeOfDayTriple ( 11, 55, "AM")
        let t2 = TimeOfDayTriple (11, 54, "АM")

        t1 <. t2 |> shouldEqual false
        
        let t3 = {Hour= 11; Minute= 55; F = "AM"}
        let t4 = {Hour= 11; Minute= 54; F = "АM"} 

        t3 <.. t4 |> shouldEqual false

    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,55,"AM"), output should be false``() = 
        let t1 = TimeOfDayTriple ( 11, 55, "AM")
        let t2 = TimeOfDayTriple ( 11, 55, "AM")

        t1 <. t2 |> shouldEqual false
        
        let t3 = {Hour= 11; Minute= 55; F = "AM"}
        let t4 = {Hour= 11; Minute= 55; F = "AM"} 

        t3 <.. t4 |> shouldEqual false

    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,54,"PM"), output should be true``() = 
        let t1 = TimeOfDayTriple ( 11, 55, "AM")
        let t2 = TimeOfDayTriple (11, 54, "PM")

        t1 <. t2 |> shouldEqual true
        
        let t3 = {Hour= 11; Minute= 55; F = "AM"}
        let t4 = {Hour= 11; Minute= 54; F = "PM"} 

        t3 <.. t4 |> shouldEqual true

    [<Test>]
    member test.``When t1=(11,55,"PM"), t2=(11,56,"AM"), output should be false``() = 
        let t1 = TimeOfDayTriple ( 11, 55, "PM")
        let t2 = TimeOfDayTriple (11, 56, "AM")

        t1 <. t2 |> shouldEqual false
        
        let t3 = {Hour= 11; Minute= 55; F = "PM"}
        let t4 = {Hour= 11; Minute= 56; F = "AM"} 

        t3 <.. t4 |> shouldEqual false

