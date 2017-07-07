module Exercises16Tests

open NUnit.Framework
open FsUnitTyped
open Exercise16
open System
open Models
open DAL
open API
open System.Data.SqlClient


let private connString = @"Data Source=.;        
    Initial Catalog=DatingBureau;
    Integrated Security=True"

let private execNonQuery connString s =
    use conn  = new SqlConnection (connString)        
    conn.Open()
    let comm = new SqlCommand(s, conn, CommandTimeout = 10)
    comm.ExecuteNonQuery() |> ignore           

let deleteAllRecords tableName = 
    let deleteTableContents = "
        Delete FROM DatingBureau.[dbo]." + tableName
    execNonQuery connString deleteTableContents


let rand = Random()
// Test Client matching
[<TestFixture>]
type ``Test Client matching``() =          

    // Before each test
    [<SetUp>]
    member t.Init () =         
        ["ClientInterest";"Interest";"InterestCategory";"Client";] 
        |> List.iter deleteAllRecords

        DAL.reInit()

        [("Sport", ["Football"; "Golf"; "Tenis"]); ("Music", ["Rock"; "Pop"])] 
        |> List.iter (fun (cat, intr) ->             
            List.map ((fun interest -> 
                {Name=interest;Category=cat}) >> addInterest
            ) intr |> ignore 
        )
        
    [<Test>]    
    member t.``If all records are same sex as match client, match should be empty`` () = 
        (*
            1. Add few interests 
            2. Add few male of similar age clients
            3. Create male client to match against
            4. Match client and check if list of matches is empty
        *)
        // let x = 
        // [("Sport", ["Football"; "Golf"; "Tenis"]); ("Music", ["Rock"; "Pop"])] 
        // |> List.iter (fun (cat, intr) ->             
        //     List.map ((fun interest -> 
        //         {Name=interest;Category=cat}) >> addInterest
        //     ) intr |> ignore 
        // )

        let clients = [
            {
                ClientId=0;
                Name="Stefan";
                Sex = Sex.Male;
                YearOfBirth=1990;
                TelephoneNum="12313";
                ThemesOfInterest=[{Category="Sport";Name="Footbal"}]
            };
            {
                ClientId=0;
                Name="Goro";
                Sex = Sex.Male;
                YearOfBirth=1985;
                TelephoneNum="1231213";
                ThemesOfInterest=[{Category="Sport";Name="Tenis"}]
            };
            {
                ClientId=0;
                Name="Misho";
                Sex = Sex.Male;
                YearOfBirth=1992;
                TelephoneNum="11232313";
                ThemesOfInterest=[{Category="Music";Name="Rock"}]
            }
        ] 
        
        clients |> List.iter addClient

        let cl1 = (List.head clients)        
        let matchingCLients = API.getMatchingClients cl1

        0 |> shouldEqual <| Seq.length matchingCLients
        

    [<Test>]
    
    member t.``If all records of correct sex are 10+ years older or younger, match should be empty`` () = 
        (*
            1. Add few interests 
            2. Add few male of similar age clients
            3. Create male client to match against
            4. Match client and check if list of matches is empty
        *)

        let clients = [
            {
                ClientId=0;
                Name="Stefan_ClientToMatch";
                Sex = Sex.Male;
                YearOfBirth=1990;
                TelephoneNum="12313";
                ThemesOfInterest=[{Category="Sport";Name="Footbal"}; {Category="Music"; Name="Rock"}]
            };
            {
                ClientId=0;
                Name="Mimi";
                Sex = Sex.Female;
                YearOfBirth=1975;
                TelephoneNum="1231213";
                ThemesOfInterest=[{Category="Sport";Name="Football"}]
            };
            {
                ClientId=0;
                Name="Mishi";
                Sex = Sex.Other;
                YearOfBirth=2002;
                TelephoneNum="11232313";
                ThemesOfInterest=[{Category="Music";Name="Rock"}]
            }
        ] 
        
        clients |> List.iter addClient

        let cl1 = (List.head clients)        
        let matchingCLients = API.getMatchingClients cl1

        0 |> shouldEqual <| Seq.length matchingCLients
        
        
