namespace Exercise16

open Models

#if INTERACTIVE
#r "FSharp.Data.TypeProviders"
#endif
open Microsoft.FSharp.Data.TypeProviders

open System
open Microsoft.FSharp.Linq
open Microsoft.FSharp.Reflection

module DAL = 
    type DbSchema = SqlDataConnection<"Data Source=.;
        Initial Catalog=DatingBureau;
        Integrated Security=True">
    type SchemaTypes = DbSchema.ServiceTypes

    let mutable private db = DbSchema.GetDataContext()
    let private rand = System.Random()

    let reInit () =
        db.DataContext.Dispose()
        db <- DbSchema.GetDataContext()  

    let private submit () = 
        try            
            db.DataContext.SubmitChanges()            
        with
        | exn -> 
            raise exn
    let getInterestCategories () =     
        query {
            for row in db.InterestCategory do
                select row
        } |> List.ofSeq

    let private findInterestCategory name = 
        try    
            let interest =  query {
                for row in db.InterestCategory do
                    find (row.Name=name) 
            } 

            Some interest
        with 
        | :? InvalidOperationException -> 
            None

    let private findClient clientId = 
        try    
            let client =  query {
                for row in db.Client do
                    find (row.ClientId=clientId) 
            } 

            Some client
        with 
        | :? InvalidOperationException -> 
            None

    let private findInterest name = 
        try    
            let interest =  query {
                for row in db.Interest do
                    find (row.Name=name) 
            } 

            Some interest
        with 
        | :? InvalidOperationException -> 
            None

    let addInterestCategory interestCategoryName  = 
        match findInterestCategory interestCategoryName with 
        | Some ic -> ic
        | None ->
            let newInterestCategory = 
                DbSchema.ServiceTypes.InterestCategory(
                    Name=interestCategoryName
                )
            db.InterestCategory.InsertOnSubmit(newInterestCategory)            
            newInterestCategory

    let rec addInterest interest = 
        let interestCategory = addInterestCategory(interest.Category)        
        let interest = 
            match findInterest interest.Name with 
            | Some i -> i
            | None ->
                let newInterest = 
                    DbSchema.ServiceTypes.Interest(
                        Name=interest.Name,
                        InterestCategory = interestCategory                        
                    )
                db.Interest.InsertOnSubmit(newInterest)
                newInterest
            
        submit()
        interest

    let private toClient (dbClient:DbSchema.ServiceTypes.Client) = 
        {
            ClientId = dbClient.ClientId;
            Name=dbClient.Name;
            YearOfBirth=dbClient.YearOfBirth;
            Sex = enum dbClient.SexId;
            TelephoneNum=dbClient.TelephoneNum
            ThemesOfInterest = 
                Seq.map (fun (clientInterest:DbSchema.ServiceTypes.ClientInterest) -> 
                    {
                        Name= clientInterest.Interest.Name; 
                        Category=clientInterest.Interest.InterestCategory.Name
                    }
                ) dbClient.ClientInterest
                |> List.ofSeq
        }    

    let getClientsWithDifferentSex cl =  
        query{
            for c in db.Client do
            where (c.SexId <> int cl.Sex)            
            select c
        } |> Seq.map toClient

    let addClient (client:Client) = 
        let newClient = 
            DbSchema.ServiceTypes.Client(
                Name=client.Name,
                TelephoneNum=client.TelephoneNum,
                SexId= int client.Sex,
                YearOfBirth=client.YearOfBirth
            )
        
        db.Client.InsertOnSubmit(newClient)

        let interests = 
            List.map (addInterest >> (fun interest ->
                    DbSchema.ServiceTypes.ClientInterest(
                        Interest=interest,
                        Client=newClient
                    ))) client.ThemesOfInterest        

        submit()