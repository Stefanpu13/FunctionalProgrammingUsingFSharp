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

    let resetTableId = sprintf "
        DBCC CHECKIDENT ('[%s]', RESEED, 0);" tableName
    execNonQuery connString resetTableId

let rand = Random()
// Test Client matching
[<TestFixture>]
type ``Test Client matching``() =          

    let cl1 = {
        ClientId=0;
        Name="Stefan_ClientToMatch";
        Sex = Sex.Male;
        YearOfBirth=1990;
        TelephoneNum="12313";
        ThemesOfInterest=[{Category="Sport";Name="Football"}; {Category="Music";Name="Rock"}]
    }
    let cl2 = {
        cl1 with         
            Name="Goro";        
            YearOfBirth=1985;        
            ThemesOfInterest=[{Category="Sport";Name="Tenis"}]
    }
    let cl3 = {
        cl1 with         
            Name="Misho";        
            YearOfBirth=1992;        
            ThemesOfInterest=[{Category="Music";Name="Rock"}]
    }

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
        let clients = [
            cl1;
            cl2;
            cl3
        ] 
        
        clients |> List.iter addClient

        let cl1 = (List.head clients)        
        let matchingClients = API.getMatchingClients cl1

        0 |> shouldEqual <| Seq.length matchingClients
        

    [<Test>]
    
    member t.``If all records of opposite sex are 10+ years older or younger, match should be empty`` () = 
        (*
            1. Add few interests 
            2. Add few male of similar age clients
            3. Create male client to match against
            4. Match client and check if list of matches is empty
        *)

        let clients = [
            cl1
            {cl2 with Sex=Sex.Female; YearOfBirth=1975}
            {cl3 with Sex=Sex.Other; YearOfBirth=2002}
        ] 
        
        clients |> List.iter addClient

        let cl1 = (List.head clients)        
        let matchingClients = API.getMatchingClients cl1

        0 |> shouldEqual <| Seq.length matchingClients
        
        
    [<Test>]
    
    member t.``If all records of opposite sex and similar age have no matching interests, match should be empty`` () = 
        
        let clients = [
            cl1;
            {cl2 with 
                Sex = Sex.Female;  
                YearOfBirth=1985
                ThemesOfInterest=[{Category="Sport";Name="Basketball"}]
            };            
            { cl3 with
                Sex = Sex.Other;
                YearOfBirth=1992;                
                ThemesOfInterest=[{Category="Music";Name="Latino"}]
            }
        ] 
        
        clients |> List.iter addClient

        let cl1 = (List.head clients)        
        let matchingClients = API.getMatchingClients cl1

        0 |> shouldEqual <| Seq.length matchingClients
        

        
    [<Test>]
    
    member t.``All records of opposite sex, with similar age and matching interests, should be matched`` () = 
        
        let clients = [
            cl1;
            cl2;
            cl3;
            { cl3 with                
                Name="Mishi";
                Sex = Sex.Other;
                YearOfBirth=1992;                
                ThemesOfInterest=[{Category="Music";Name="Latino"}]
            };
            { cl2 with
                Name="Gigi";
                Sex = Sex.Other;
                YearOfBirth=1994;                
                ThemesOfInterest=[{Category="Music";Name="Rock"}; {Category="Sport";Name="Tenis"}]
            };
            {cl3 with                 
                Name="Didi";
                Sex = Sex.Other;
                YearOfBirth=1994;                
                ThemesOfInterest=[{Category="Sport";Name="Football"}]
            };
        ] 
        
        clients |> List.iter addClient

        let cl1 = (List.head clients)        
        let matchingClients = API.getMatchingClients cl1        
        let matchingClientsList = matchingClients |> List.ofSeq

        2 |> shouldEqual <| List.length matchingClientsList

        match matchingClientsList with
        | [{Name="Gigi"; YearOfBirth=1994}; {Name="Didi"; YearOfBirth=1994}] -> ()
        | l -> shouldFail id
