module Exercises6Tests
open NUnit.Framework
open FsUnitTyped
open Exercises5.E

// Test leafVals
[<TestFixture>]
type ``Test leafVals``() =
    let tr = 
        Info(
            Info(
                Info(
                    Info(
                        Info (Unspec, "Dad4", Unspec), 
                        "Dad3", 
                        Unspec
                    ), 
                    "DadsDad", 
                    Unspec
                ), 
                "Dad1", 
                Info(Unspec, "Dad1sMom", Unspec)
            ), 
            "Me",
            Info(
                Info(
                    Unspec, 
                    "MomsDad", 
                    Info(Unspec, "MomsDadMom", Unspec)
            ),
                "Mom",
                Info(
                    Info(
                        Unspec, "MomsMomsDad", Unspec
                    ), 
                    "MomsMom", 
                    Info(Unspec, "OneMoreMom", Unspec)
                )
            )
        )

    [<Test>]
    member t.``If tree is tr, result should be set ["Dad4"; "Dad3"; "DadsDad"; "Dad1"; "MomsDad"; "MomsMomsDad"]"`` () = 
        maleAnc tr |> shouldEqual ["Dad4"; "Dad3"; "DadsDad"; "Dad1"; "MomsDad"; "MomsMomsDad"]

    
    [<Test>]
    member t.``If tree tr, result should be ["Dad1sMom"; "MomsDadMom"; "OneMoreMom"; "MomsMom"; "Mom"]"`` () = 
         femaleAnc tr |> shouldEqual ["Dad1sMom"; "MomsDadMom"; "OneMoreMom"; "MomsMom"; "Mom"]
        