module Exercises10Tests
open NUnit.Framework
open FsUnitTyped
open Exercises9.E
open Exercises10.E
open Exercise9.Types.T

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
    member t.``1 chocolate and 2 eggs, then total should equal 20 and info should be [(1 chocolate, 12); (2, eggs, 8)]`` () = 
        let purchase = 
            Purchase.create [
                        (ArticleCode "a1", NoPieces 1);
                        (ArticleCode "a2", NoPieces 1);
                        (ArticleCode "a2", NoPieces 1)
                    ]

        let (Bill (Infoseq  info,Price total)) = makeBill3 register purchase

        total |> shouldEqual 20
        info |> shouldEqual [
                Info (NoPieces 1, ArticleName "chocolate", Price 12);
                Info (NoPieces 2, ArticleName "eggs", Price 8)
            ] 

    [<Test>]
    member t.``1 flour and 1 rise, then total should equal 8 and info should  be ordered and be [(1 rise, 3); (1, flour, 5)]`` () = 
        let purchase = 
            Purchase.create [
                        (ArticleCode "a5", NoPieces 1);
                        (ArticleCode "a4", NoPieces 1)
                    ]

        let (Bill (Infoseq  info,Price total)) = makeBill3 register purchase

        total |> shouldEqual 8
        info |> shouldEqual [
                Info (NoPieces 1, ArticleName "rise", Price 3);
                Info (NoPieces 1, ArticleName "flour", Price 5);                
            ] 

    [<Test>]
    member t.``empty, then total should equal 0 and info should be empty`` () = 
        let purchase = 
            Purchase.create [] 

        let (Bill (Infoseq  info,Price total)) = makeBill3 register purchase

        total |> shouldEqual 0
        info |> shouldEqual [] 


    