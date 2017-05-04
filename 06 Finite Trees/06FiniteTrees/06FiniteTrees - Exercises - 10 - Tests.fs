module Exercises10Tests
open System
open NUnit.Framework
open FsUnitTyped
open Exercises10.E


// Test IfThenElse
[<TestFixture>]
type ``Test IfThenElse``() =

    [<Test>]
    member t.``If "If" expression is only constants - If true then 5 else 6 - ,result should be 5`` () =         
        let expr = If (BooleanConst true, Const 5, Const 6)

        eval expr Map.empty |> shouldEqual <| 5

    [<Test>]
    member t.``If "If" expression is only constants - If false then 5 else 6 - , result should be 6`` () =         
        let expr = If (BooleanConst false, Const 5, Const 6)

        eval expr Map.empty |> shouldEqual <| 6

        
    [<Test>]
    member t.``If boolean expression is comparison of constants - If (5 < 6) then 5 else 6,result should be 5`` () =         
        let expr = If (LT(Const 5, Const 6), Const 5, Const 6)

        eval expr Map.empty |> shouldEqual <| 5

    [<Test>]
    member t.``If boolean expression is comparison of expressions - If (5 * a > 6 * a) then 5 else 6, and a > 0, result should be 6`` () =         
        let expr = If (GT(Prod (Const 5, Ident "a"), Prod(Const 6, Ident "a")), Const 5, Const 6)
        let env = Map.add "a" 2 Map.empty
        eval expr env |> shouldEqual <| 6

    [<Test>]
    member t.``If boolean expression is complex - If (5 * a > 6 * a && a < b) then 5 else 6, and a > 0, result should be 6`` () =         
        let expr = 
            If (
                And( 
                    GTE(Prod (Const 5, Ident "a"), Prod(Const 6, Ident "a")),
                    LT(Ident "a", Ident "b")), 
                Const 5, Const 6)
        let env = Map.add "a" 2 Map.empty
        let env1 = Map.add "b" 3 env

        eval expr env1 |> shouldEqual <| 6

    [<Test>]
    member t.``If "If" expression is complex, and bool expression is true - If (5 * a > 6 * a || a < b) then a-b else a+b, result should be a-b`` () =         
        let expr =
            If(
                Or ( 
                    GTE (Prod (Const 5, Ident "a"), Prod(Const 6, Ident "a")),
                    LT(Ident "a", Ident "b")), 
                Diff (Ident "a", Ident "b"),
                Sum (Ident "a", Ident "b"))

        let env = Map.add "a" 2 Map.empty
        let env1 = Map.add "b" 3 env

        eval expr env1 |> shouldEqual <| (eval (Diff(Ident "a", Ident "b")) env1)

    [<Test>]
    member t.``If "If" expression is complex, and part of complex expression, complex expression should be evaluated`` () =         
        let ifExpr =
            If(
                Or ( 
                    GTE (Prod (Const 5, Ident "a"), Prod(Const 6, Ident "a")),
                    LT(Ident "a", Ident "b")), 
                Diff (Ident "a", Ident "b"),
                Sum (Ident "a", Ident "b"))
        let expr = 
            Prod(
                If(
                    Not(LTE(Const 3, Ident "b")),
                    Ident "c", 
                    Sum (Ident "c", Const 4)),
                ifExpr)            

        let env = 
            Map.empty 
                |> Map.add "a" 2 
                |> Map.add "b" 3  
                |> Map.add "c" 4

        let realExpr = 
            (if not (3 <= 3) then 4 else 4 + 4) * (if 5 * 2 > 6 * 2 || 2 < 3 then 2 - 3 else 2 + 3)

        eval expr env |> shouldEqual <| realExpr