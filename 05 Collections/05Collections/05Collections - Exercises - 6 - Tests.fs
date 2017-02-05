module Exercises6Tests
open NUnit.Framework
open FsUnitTyped
open Exercises6.E

// domain tests
[<TestFixture>]
type ``domain tests``() =
    [<Test>]
    member t.``If relation contains person first names "Peter" and Steve", the result should be set ["Peter";"Steve"] `` () = 
        let firstNames = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let lastNames = set ["Wonder"; "Milton"; "Ford"; "Garden"]
        let personNames = set [("Peter", "Milton"); ("Peter","Garden"); ("Steve","Wonder")]
        domain  personNames firstNames lastNames |> shouldEqual (set ["Peter";"Steve"])
    
    [<Test>]
    member t.``If relation is empty, the result should be set [] `` () = 
        let firstNames = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let lastNames = set ["Wonder"; "Milton"; "Ford"; "Garden"]
        let personNames = set []
        domain  personNames firstNames lastNames |> shouldEqual (set [])

    [<Test>]
    member t.``If relation contains "Peter" and "John", the result should be set ["Peter"; "John"] `` () = 
        let names = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let olympicGames = set [("Rio", 2016); ("Pekin", 2008); ("London", 2012); ("Athens", 2004)]
        let participations = set [("Peter", ("Pekin", 2008);); ("Peter", ("London", 2012);); ("John", ("London", 2012);)]
        domain  participations names olympicGames |> shouldEqual (set ["Peter"; "John"])
    