module Exercises1Tests
open NUnit.Framework
open FsUnitTyped
open Exercises4.E

let getLinksTree ()= 
    [
        {link=Unchecked.defaultof<T>; data=1};
        {link=Unchecked.defaultof<T>; data=2};
        {link=Unchecked.defaultof<T>; data=3};
        {link=Unchecked.defaultof<T>; data=4};
        {link=Unchecked.defaultof<T>; data=5};
    ]

// test insert function
[<TestFixture>]
type ``Test insert function``() =

    [<Test>]
    member t.``inserting the first link should create one link with data = 1 and link = null"`` () = 
        let chain = insert {link=defaultT; data=1} defaultT       
        chain.link |> shouldEqual defaultT
        chain.data |> shouldEqual 1
        
    [<Test>]
    member t.``inserting inserting link the first link should create two links"`` () = 
        let link1 = insert {link=defaultT; data=1} defaultT
        let link2 = insert {link=defaultT; data=2} link1
        link2.link |> shouldEqual link1
        link2.data |> shouldEqual 2

// test createChain function
[<TestFixture>]
type ``Test createChain function``() =

    [<Test>]
    member t.``creating chain of empty list should create chain=null"`` () = 
        createChain [] |> shouldEqual defaultT
    
    [<Test>]
    member t.``creating chain of 3 links should create chain with first link = first item, second link = second item, etc"`` () = 
        let ts = getLinksTree()
        let chain  = createChain ts.[..3]
        chain |> shouldEqual ts.[0]
        chain.link |> shouldEqual ts.[1]
        chain.link.link |> shouldEqual ts.[2]
        chain.link.link.link |> shouldEqual ts.[3]
        chain.link.link.link.link |> shouldEqual defaultT

// test createChain function
[<TestFixture>]
type ``Test createCircle function``() =

    [<Test>]
    member t.``creating circle of empty list should create cirle=null"`` () = 
        createCircle [] |> shouldEqual defaultT

    [<Test>]
    member t.``creating cirle of 1 links should create circle where link points to itself"`` () = 
        let ts = getLinksTree ()
        let circle  = createCircle ts.[..0]
        circle |> shouldEqual ts.[0]
        circle.link |> shouldEqual ts.[0]
        circle.link.link |> shouldEqual ts.[0]
        circle.link.link.link |> shouldEqual ts.[0]
    
    [<Test>]
    member t.``creating cirle of 2 links should create circle where first link is link of second link and vice versa"`` () = 
        let ts = getLinksTree()
        let circle  = createCircle ts.[..1]
        circle |> shouldEqual ts.[0]
        circle.link |> shouldEqual ts.[1]
        circle.link.link |> shouldEqual ts.[0]
        circle.link.link.link |> shouldEqual ts.[1]

    [<Test>]
    member t.``creating cirle of 3 links should create circle where links are connected like: link1 -> link2 -> link3 -> link1 -> ..."`` () = 
        let ts = getLinksTree()
        let circle  = createCircle ts.[..2]
        
        let iterCircle iterator steps link =         
            let rec iterCircle step link = 
                iterator step link
                if step < steps
                then iterCircle (step + 1) link.link                

            iterCircle 0 link

        iterCircle (fun step link -> link |> shouldEqual ts.[step % 3]) 10 circle |> ignore
        