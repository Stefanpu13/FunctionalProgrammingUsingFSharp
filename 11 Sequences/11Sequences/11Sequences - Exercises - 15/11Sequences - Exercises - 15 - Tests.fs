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
open E
open System.Data.SqlClient

let private connString = @"Data Source=.;        
    Initial Catalog=Register;
    Integrated Security=True"

let private execNonQuery connString s =
    use conn  = new SqlConnection (connString)        
    conn.Open()
    let comm = new SqlCommand(s, conn, CommandTimeout = 10)
    comm.ExecuteNonQuery() |> ignore           

let deleteAllRecords tableName = 
    let deleteTableContents = "
        Truncate table Register.[dbo]." + tableName
    execNonQuery connString deleteTableContents


// Test adding articles to db
[<TestFixture>]
type ``Test adding articles to db``() =       

    // Before each test
    [<SetUp>]
    member t.Init () =         
        deleteAllRecords "Register" 
        Repository.reInit()   


    [<Test>]
    
    member t.``One added product should be found`` () = 
        let brd = ("brd", ("Bread", Price 10))
        Repository.addArticle brd
        Repository.findArticle "brd" |> shouldEqual <| Some brd

    [<Test>]    
    member t.``Adding product with same code more than once should not change db`` () = 
        let brd = ("brd", ("Bread", Price 10))
        Repository.addArticle brd
        Repository.findArticle "brd" |> shouldEqual <| Some brd

        Repository.addArticle brd
        Repository.findArticle "brd" |> shouldEqual <| Some brd

        let brd2 = ("brd", ("Bread2", Price 12))
        Repository.addArticle brd2
        Repository.findArticle "brd" |> shouldEqual <| Some brd


    [<Test>]    
    member t.``Non existent products in Bill should be ignored`` () = 
        let brd = ("brd", ("Bread", Price 10))
        let mlk = ("mlk", ("Milk", Price 14))

        Repository.addArticle brd
        Repository.addArticle mlk

        let purchase = 
            Purchase [
                Item (NoPieces 2, "brd")
                Item (NoPieces 1, "clr")
        ]

        let (Bill (_, Price p)) =  makeBill purchase 

        p |> shouldEqual <| 20
        

    [<Test>]    
    member t.``A purchase of 3 * 14 + 1 * 12 + 2 * 17, should equal 88 `` () = 
        let brd = ("brd", ("Bread", Price 12))
        let mlk = ("mlk", ("Milk", Price 14))
        let chkn = ("chkn", ("Chiken", Price 17))

        [brd;mlk;chkn] |> List.iter Repository.addArticle        

        let purchase = 
            Purchase [
                Item (NoPieces 1, "brd")
                Item (NoPieces 3, "mlk")
                Item (NoPieces 2, "chkn")
        ]

        let (Bill (_, Price p)) =  makeBill purchase 

        p |> shouldEqual <| 88