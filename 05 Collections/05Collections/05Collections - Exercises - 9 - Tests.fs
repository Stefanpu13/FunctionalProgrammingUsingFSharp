module Exercises9Tests
open NUnit.Framework
open FsUnitTyped
open Exercises9.E
open Types.T


[<TestFixture>]
type ``all makeBill3 using fold tests``() =
    let register = 
        Register (Map.ofList [
                    (ArticleCode "a1"), (ArticleName "chocolate", Price 12, None);
                    (ArticleCode "a2"), (ArticleName "eggs", Price 5, Discount.create 0.2);
                    (ArticleCode "a3"), (ArticleName "salmon", Price 22, None);
                    (ArticleCode "a4"), (ArticleName "rise", Price 3, None);
                    (ArticleCode "a5"), (ArticleName "flour", Price 5, None);
                    (ArticleCode "a6"), (ArticleName "oil", Price 4, Discount.create 0.3);
        ])    

    [<Test>]
    member t.``Items list has 1 chocolate, 2 eggs, 1chocolate, should equal purchase of 2 chocolates and 2 eggs`` () = 
        let purchase = 
            Purchase.create [
                        (ArticleCode "a1", NoPieces 1);
                        (ArticleCode "a2", NoPieces 2);
                        (ArticleCode "a1", NoPieces 1)
                    ]

        let purchaseMap = Purchase.value purchase

        purchaseMap |> shouldEqual 
            (Map.ofList [
                (ArticleCode "a1", NoPieces 2);
                (ArticleCode "a2", NoPieces 2)
            ])  

    [<Test>]
    member t.``Items list has 1 egg, 2 eggs, 1chocolate, should equal purchase of 1 chocolate and 3 eggs`` () = 
        let purchase = 
            Purchase.create [
                        (ArticleCode "a2", NoPieces 1);
                        (ArticleCode "a2", NoPieces 2);
                        (ArticleCode "a1", NoPieces 1)
                    ]

        let purchaseMap = Purchase.value purchase

        purchaseMap |> shouldEqual 
            (Map.ofList [
                (ArticleCode "a1", NoPieces 1);
                (ArticleCode "a2", NoPieces 3)
            ])  

