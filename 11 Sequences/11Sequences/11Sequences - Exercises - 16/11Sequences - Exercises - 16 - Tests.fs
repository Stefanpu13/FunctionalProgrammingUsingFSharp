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



// Test Client matching
[<TestFixture>]
type ``Test Client matching``() =       

    // Before each test
    [<SetUp>]
    member t.Init () =         
        ["ClientInterest";"Interest";"InterestCategory";"Client";] 
        |> List.iter deleteAllRecords

        DAL.reInit()
        
    [<Test>]
    
    member t.``If db contains only males and match client is male, match should be empty`` () = 
        (*
            1. Add few interests 
            2. Add few male of similar age clients
            3. Create male client to match against
            4. Match client and check if list of matches is empty
        *)
        // let x = 
        [("Sport", ["Football"; "Golf"; "Tenis"]); ("Music", ["Rock"; "Pop"])] 
        |> List.iter (fun (cat, intr) ->             
            List.map ((fun interest -> 
                {Name=interest;Category=cat}) >> addInterest
            ) intr |> ignore 
        )

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

        let cl1 = {(List.head clients) with Sex=Sex.Female}
        // let cl1 = List.head clients

        API.getMatchingClients cl1
        

        
