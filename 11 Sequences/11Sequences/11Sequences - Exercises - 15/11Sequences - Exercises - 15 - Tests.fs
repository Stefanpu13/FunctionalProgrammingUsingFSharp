(*
    Before all tests
        1. Create db? (how will this work with type providers and intellisence)
        2. Create tables in db.
    Before each test    
        1. Fill in data in tables
    After each test    
        1. Remove data from tables

    After all tests - delete db
*)

module Exercises15Tests

open NUnit.Framework
open FsUnitTyped
open Exercises15
open System
open Models

let rand = Random ()

// Test adding articles to db
[<TestFixture>]
type ``Test adding articles to db``() =       

    // Before each test
    [<SetUp>]
    member t.Init () =         
        Startup.deleteAllRecords "Register" 
        Repository.reInit()   


    [<Test>]
    
    member t.``One added product should be found`` () = 
        let brd = ("brd", ("Bread", Price 10))
        Repository.addArticle brd
        Repository.findArticle "brd" |> shouldEqual <| (seq [brd])

    [<Test>]
    
    member t.``Adding product with same code more than once should not change db`` () = 
        let brd = ("brd", ("Bread", Price 10))
        Repository.addArticle brd
        Repository.findArticle "brd" |> shouldEqual <| (seq [brd])

        Repository.addArticle brd
        Repository.findArticle "brd" |> shouldEqual <| (seq [brd])

        let brd2 = ("brd", ("Bread2", Price 12))
        Repository.addArticle brd2
        Repository.findArticle "brd" |> shouldEqual <| (seq [brd])
