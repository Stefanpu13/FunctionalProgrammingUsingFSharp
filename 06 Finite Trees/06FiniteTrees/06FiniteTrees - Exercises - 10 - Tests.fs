module Exercises10Tests
open System
open NUnit.Framework
open FsUnitTyped
open Exercises9.E
open Exercises1Common.Types.Expression

let depWithTwoSubDeps = 
    (Department (Name "base2", GrossIncome 2.0, 
        [   
            (Department (Name "base3", GrossIncome 3.0, []))
            (Department (Name "base4", GrossIncome 4.0, []))
        ]))
let deepDeps = 
    Department (Name "base1", GrossIncome 1.0, 
        [
            depWithTwoSubDeps
            (Department (Name "base5", GrossIncome 5.0, [(Department (Name "base6", GrossIncome 6.0, []))]))
            (Department (Name "base7", GrossIncome 7.0, []))
        ])

// Test allDepartments
[<TestFixture>]
type ``Test allDepartments``() =

    [<Test>]
    member t.``If dep. has no subdeps, list should consist of single pair`` () =         
        let deps = Department (Name "base1", GrossIncome 12.0, [])
        allDepartments deps |> shouldEqual <| [(Name "base1", GrossIncome 12.0)] 

    [<Test>]
    member t.``If dep. has one subdep, list should be two pairs, ending with the top dep`` () =         
        let deps = Department (Name "base1", GrossIncome 12.0, [(Department (Name "base2", GrossIncome 10.0, []))])
        allDepartments deps |> shouldEqual <| [(Name "base2", GrossIncome 10.0); (Name "base1", GrossIncome 12.0)] 

    [<Test>]
    member t.``If dep. has subdeps, that have subdeps, list should have included top deps first `` () =         

        allDepartments deepDeps |> shouldEqual <| [ (Name "base7", GrossIncome 7.0); (Name "base6", GrossIncome 6.0);
                                                    (Name "base5", GrossIncome 5.0); (Name "base4", GrossIncome 4.0);
                                                    (Name "base3", GrossIncome 3.0); (Name "base2", GrossIncome 2.0);
                                                    (Name "base1", GrossIncome 1.0)]

// Test totalIncome
[<TestFixture>]
type ``Test totalIncome``() =

    [<Test>]
    member t.``If dep. has no subdeps and Income 3, totalIncome should be 3`` () =         
        let deps = Department (Name "base1", GrossIncome 3.0, [])
        totalIncome deps |> shouldEqual <| GrossIncome 3.0
    
    [<Test>]
    member t.``If dep. has two subdeps and Income 2, and two subdeps have income 3 and 4, totalIncome should be 9`` () =
        totalIncome depWithTwoSubDeps |> shouldEqual <| GrossIncome 9.0

    [<Test>]
    member t.``If dep. is nested, totalIncome should be sum of all subdeps`` () =
        totalIncome deepDeps |> shouldEqual <| GrossIncome (List.sum [1.0..7.0])


// Test allDepartmentsTotalIncome
[<TestFixture>]
type ``Test allDepartmentsTotalIncome``() =

    [<Test>]
    member t.``If dep. has no subdeps and Income 3, list must be 1 pair`` () =         
        let deps = Department (Name "base1", GrossIncome 3.0, [])
        allDepartmentsTotalIncome deps |> shouldEqual <| [(Name "base1", GrossIncome 3.0)]
        
    [<Test>]
    member t.``If dep. has two subdeps and Income 2, and two subdeps have income 3 and 4, list must be 3 pairs`` () =                 
        allDepartmentsTotalIncome depWithTwoSubDeps |> shouldEqual <| [ (Name "base2", GrossIncome 9.0); 
                                                                        (Name "base4", GrossIncome 4.0);
                                                                        (Name "base3", GrossIncome 3.0)]

    [<Test>]
    member t.``If dep. is nested, list must contain all subdeps as pairs`` () =                 
        allDepartmentsTotalIncome deepDeps |> shouldEqual <| [  (Name "base1", GrossIncome 28.0);
                                                                (Name "base7", GrossIncome 7.0);
                                                                (Name "base5", GrossIncome 11.0); 
                                                                (Name "base6", GrossIncome 6.0);
                                                                (Name "base2", GrossIncome 9.0); 
                                                                (Name "base4", GrossIncome 4.0);
                                                                (Name "base3", GrossIncome 3.0)]

// Test format
[<TestFixture>]
type ``Test format``() =

    [<Test>]
    member t.``If dep. has no subdeps and name "base1", format must be "base1"`` () =         
        let deps = Department (Name "base1", GrossIncome 3.0, [])
        format deps |> shouldEqual <| 
        Environment.NewLine +
        "base1" +
        Environment.NewLine
        
    [<Test>]
    member t.``If dep. has name "base2" and two subdeps with names "base3"and "base4", subdeps must be intendent two spaces and each on new line`` () =                 
        format depWithTwoSubDeps |> shouldEqual <|  Environment.NewLine +
                                                    "base2" + Environment.NewLine +
                                                    "  base3" + Environment.NewLine +
                                                    "  base4" + Environment.NewLine 
        
    [<Test>]
    member t.``If dep. is nested, subdeps must be intendent two spaces and each on new line`` () =                 
        format deepDeps |> shouldEqual <|   Environment.NewLine +
                                            "base1" + Environment.NewLine +
                                            "  base2" + Environment.NewLine +
                                            "    base3" + Environment.NewLine +
                                            "    base4" + Environment.NewLine +
                                            "  base5" + Environment.NewLine + 
                                            "    base6" + Environment.NewLine + 
                                            "  base7" + Environment.NewLine
        

    