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
        domain personNames firstNames lastNames |> shouldEqual (set ["Peter";"Steve"])
    
    [<Test>]
    member t.``If relation is empty, the result should be set [] `` () = 
        let firstNames = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let lastNames = set ["Wonder"; "Milton"; "Ford"; "Garden"]
        let personNames = set []
        domain personNames firstNames lastNames |> shouldEqual (set [])

    [<Test>]
    member t.``If relation contains "Peter" and "John", the result should be set ["Peter"; "John"] `` () = 
        let names = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let olympicGames = set [("Rio", 2016); ("Pekin", 2008); ("London", 2012); ("Athens", 2004)]
        let participations = set [("Peter", ("Pekin", 2008);); ("Peter", ("London", 2012);); ("John", ("London", 2012);)]
        domain participations names olympicGames |> shouldEqual (set ["Peter"; "John"])

// range tests
[<TestFixture>]
type ``range tests``() =
    [<Test>]
    member t.``If relation contains person first names "Peter" and Steve", the result should be set ["Milton";"Garden";"Wonder"] `` () = 
        let firstNames = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let lastNames = set ["Wonder"; "Milton"; "Ford"; "Garden"]
        let personNames = set [("Peter", "Milton"); ("Peter","Garden"); ("Steve","Wonder")]
        range personNames firstNames lastNames |> shouldEqual (set ["Milton";"Garden";"Wonder"])
    
    [<Test>]
    member t.``If relation is empty, the result should be set [] `` () = 
        let firstNames = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let lastNames = set ["Wonder"; "Milton"; "Ford"; "Garden"]
        let personNames = set []
        range personNames firstNames lastNames |> shouldEqual (set [])

    [<Test>]
    member t.``If relation contains "Peter" and "John", the result should be set [("Pekin", 2008); ("London", 2012)] `` () = 
        let names = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let olympicGames = set [("Rio", 2016); ("Pekin", 2008); ("London", 2012); ("Athens", 2004)]
        let participations = set [("Peter", ("Pekin", 2008);); ("Peter", ("London", 2012);); ("John", ("London", 2012);)]
        range participations names olympicGames |> shouldEqual (set [("Pekin", 2008); ("London", 2012)])
    

// apply tests
[<TestFixture>]
type ``apply tests``() =
    [<Test>]
    member t.``If relation contains person first names "Peter" and Steve" and elA = "Peter", the result should be set ["Milton";"Garden"] `` () = 
        let firstNames = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let lastNames = set ["Wonder"; "Milton"; "Ford"; "Garden"]
        let personNames = set [("Peter", "Milton"); ("Peter","Garden"); ("Steve","Wonder")]
        apply personNames "Peter" firstNames lastNames |> shouldEqual (set ["Milton";"Garden"])
    
    [<Test>]
    member t.``If relation is empty, the result should be set [] `` () = 
        let firstNames = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let lastNames = set ["Wonder"; "Milton"; "Ford"; "Garden"]
        let personNames = set []
        apply personNames "Steve" firstNames lastNames |> shouldEqual (set [])

    [<Test>]
    member t.``If relation contains "Peter" and "John" and elA = John, the result should be set [("London", 2012)] `` () = 
        let names = set ["Steve"; "Peter"; "John"; "Michael";"Gregory"]
        let olympicGames = set [("Rio", 2016); ("Pekin", 2008); ("London", 2012); ("Athens", 2004)]
        let participations = set [("Peter", ("Pekin", 2008);); ("Peter", ("London", 2012);); ("John", ("London", 2012);)]
        apply participations "John" names olympicGames |> shouldEqual (set [("London", 2012)])

// symmetric closure tests
[<TestFixture>]
type ``symmetric closure tests``() =

    [<Test>]
    member t.``If set is [1; 2; 3] and relation contains [(1, 2); (1,4); (2,3)], the result should be None`` () = 
        let setA = set [1; 2; 3]
        let relation = set [(1, 2); (1,4); (2,3)]
        symmetricClosure relation setA |> shouldEqual None

    [<Test>]
    member t.``If set is [1; 2; 3] and relation contains [(1, 2); (1,4)], the result should be None`` () = 
        let setA = set [1; 2; 3]
        let relation = set [(1, 2); (1,4)]
        symmetricClosure relation setA |> shouldEqual None

    [<Test>]
    member t.``If set is [1; 2; 3] and relation contains [(1, 2); (1,3); (2,1)], the result should be Some (set [(1, 2); (1,3); (2,1); (3,1)])`` () = 
        let setA = set [1; 2; 3]
        let relation = set [(1, 2); (1,3)]
        symmetricClosure relation setA |> shouldEqual (Some (set [(1, 2); (1,3); (2,1); (3,1)]))

    [<Test>]
    member t.``If set is [1; 2; 3] and relation contains [(1, 2)], the result should be set [(1, 2); (2,1)] `` () = 
        let setA = set [1; 2; 3]
        let relation = set [(1, 2)]
        symmetricClosure relation setA |> shouldEqual (Some (set [(1, 2); (2,1)]))
    
    
    [<Test>]
    member t.``If set is [1; 2; 3] and relation contains [(1, 2); (1,3); (2,1); (3,1)], the result should be Some (set [(1, 2); (1,3); (2,1); (3,1)])`` () = 
        let setA = set [1; 2; 3]
        let relation = set [(1, 2); (1,3); (2,1); (3,1)]
        symmetricClosure relation setA |> shouldEqual (Some (set [(1, 2); (1,3); (2,1); (3,1)]))

// relation composition tests
[<TestFixture>]
type ``relation composition tests``() =
    [<Test>]
    member t.``If r = [(1,4); (2,5)], s = [(4, 7); (5,8)], then  r**s should be [(1, 7);(2, 8)]`` () = 
        let r = Set [(1,4); (2,5)]
        let s = Set [(4, 7); (5,8)]
        relationalComposition r s |> shouldEqual (Set [(1, 7);(2, 8)])

    [<Test>]
    member t.``If r = [(1,4);(2,4); (2,5)], s = [(4, 7); (5,8)], then  r**s should be [(1, 7);(2,7);(2, 8)]`` () = 
        let r = Set [(1,4);(2,4); (2,5)]
        let s = Set [(4, 7); (5,8)]
        relationalComposition r s |> shouldEqual (Set [(1, 7);(2,7);(2, 8)])

    [<Test>]
    member t.``If r = [(1,4);(1,5)], s = [(4,8);(5,8)], then r**s should be [(1,8)]`` () = 
        let r = Set [(1,4);(1,5)]
        let s = Set [(4,8); (5,8)]
        relationalComposition r s |> shouldEqual (Set [(1,8)])

    [<Test>]
    member t.``If r = [], s = [(4,8);(5,8)], then r**s should be []`` () = 
        let r = Set []
        let s = Set [(4,8); (5,8)]
        relationalComposition r s |> shouldEqual (Set [])

    [<Test>]
    member t.``If r = [(1,4);(1,5)], s = [], then r**s should be []`` () = 
        let r = Set [(1,4);(1,5)]
        let s = Set []
        relationalComposition r s |> shouldEqual (Set [])

    [<Test>]
    member t.``If r = [(1,4);(1,5)], s = [(6,8)], then r**s should be []`` () = 
        let r = Set [(1,4);(1,5)]
        let s = Set [(6,8)]
        relationalComposition r s |> shouldEqual (Set [])
 
//transitive relation closure tests
[<TestFixture>]
type ``transitive relation closure tests``() =
    [<Test>]
    member t.``If r = [(1,2); (2,1)], then transitive closure should be [(1,2);(2,1);(1,1);(2;2)]`` () = 
        let r = Set [(1,2); (2,1)]
        
        transitiveClosure r|> shouldEqual (Set [(1, 2);(2, 1);(1,1);(2,2)])

    [<Test>]
    member t.``If r = [(1,2); (1,1)], then transitive closure should be [(1,2);(1,1)]`` () = 
        let r = Set [(1,2); (1,1)]
        
        transitiveClosure r|> shouldEqual (Set [(1,2); (1,1)])

    [<Test>]
    member t.``If r = [(2,1); (2,2)], then transitive closure should be [(2,1);(2,2)]`` () =         
        let r = Set [(2,1); (2,2)]
        
        transitiveClosure r|> shouldEqual (Set [(2,1); (2,2)])

    [<Test>]
    member t.``If r = [(1,1); (2,2)], then transitive closure should be [(1,1);(2,2)]`` () = 
        let r = Set [(1,1); (2,2)]
        
        transitiveClosure r|> shouldEqual (Set [(1,1);(2,2)])    

    [<Test>]
    member t.``If r = [(2,1)], then transitive closure should be [(2, 1)]`` () = 
        // Is this test correct? Shouldn`t every relation have transitive closure? - Yes
        // Transitive closure: When (a,b) in R and (b,c) in R implies that (a, c) in R
        // But here r concsists only of (2, 1), so there is no (a, b) and (b, c).
        // Hense, it is transitive "by vacuity"??
        // See: http://math.stackexchange.com/questions/1255179/determining-whether-a-relation-is-transitive-or-not
        let r = Set [(2,1)]
        
        transitiveClosure r|> shouldEqual (Set [(2,1)])

//all subsets tests
[<TestFixture>]
type ``all subsets tests``() =
    [<Test>]
    member t.``If n=2 k=1, then result should be [[1];[2]]`` () = 
        allSubsets 2 1 |> shouldEqual [[1];[2]]

    [<Test>]
    member t.``If n=3 k=2, then result should be [[1;2];[1;3];[2;3]]`` () = 
        allSubsets 3 2 |> shouldEqual [[1;2];[1;3];[2;3]]

    [<Test>]
    member t.``If n=random k=random, then result should be [[1;2];[1;3];[2;3]]`` () = 
        let rand = System.Random()
        let x = rand.Next(1, 20)
        allSubsets x x |> shouldEqual [[1..x]]

    [<Test>]
    member t.``If n=random k=random+1, then result should be []`` () = 
        let rand = System.Random()
        let x = rand.Next(1, 20)
        allSubsets x (x + 1) |> shouldEqual []

    