namespace Exercise16
open Models

#if INTERACTIVE
#r "FSharp.Data.TypeProviders"
#endif
open Microsoft.FSharp.Data.TypeProviders

open System
open Microsoft.FSharp.Linq

module Repository = 
    type DbSchema = SqlDataConnection<"Data Source=.;
        Initial Catalog=DatingBureau;
        Integrated Security=True">

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
    let getInterestTypes () =     
        query {
            for row in db.InterestType do
                select row
        }
    let addInterestType interestType = 
        let interestTypeName = getInterestTypeName interestType
        let newInterestType = 
            DbSchema.ServiceTypes.InterestType(
                Name = interestTypeName
            )

        db.InterestType.InsertOnSubmit(newInterestType)        
        submit()

    let addInterest interestType =         
        let interestName = 
            match interestType with
            | Sport s ->             
                match s with
                | Football -> "Football"
                | Tenis -> "Tenis"
                | Baseball -> "Baseball"
                | TableTenis -> "TableTenis"
                | Basketball -> "Basketball"                
            | Music m ->                 
                match m with
                |Jazz ->"Jazz"
                |Rock -> "Rock"
                |Pop -> "Pop"
                |Salsa -> "Salsa"
                |HipHop -> "HipHop"
                |``Classical Music`` -> "Classical Music"
            | Reading r ->             
                match r with
                |Poetry -> "Poetry"
                |SciFi -> "SciFi"
                |Magazines -> "Magazines"
                |Novels -> "Novels"
                |``Techincal Literature`` -> "Techincal Literature"       

        let interestTypes = getInterestTypes () |> List.ofSeq
        let interestDb = 
            DbSchema.ServiceTypes.Interest(
                Name=interestName,
                InterestType = 
                    List.find (fun intrType -> intrType.Name = getInterestTypeName interestType) interestTypes
            )        
        db.Interest.InsertOnSubmit(interestDb)

    addInterestType (Sport Football)
    addInterest (Sport Football)


    let private womenNames = [
        "MARY"; "PATRICIA"; "LINDA"; "BARBARA"; "ELIZABETH"; 
        "JENNIFER"; "MARIA"; "SUSAN"; "MARGARET"; "DOROTHY"; "LISA"; 
        "NANCY"; "KAREN"; "BETTY"; "HELEN"; "SANDRA"; "DONNA"; "CAROL";
        "RUTH"; "SHARON"; "MICHELLE"; "LAURA"; "SARAH"; "KIMBERLY"; "DEBORAH";
        "JESSICA"; "SHIRLEY"; "CYNTHIA"; "ANGELA"; "MELISSA";
        ]

    let private menNames = [
        "JAMES"; "JOHN"; "ROBERT"; "MICHAEL";
        "WILLIAM"; "DAVID"; "RICHARD"; "CHARLES"; "JOSEPH"; "THOMAS";
        "CHRISTOPHER"; "DANIEL"; "PAUL"; "MARK"; "DONALD"; "GEORGE";
        "KENNETH"; "STEVEN"; "EDWARD"; "BRIAN"; "RONALD"; "ANTHONY";
        "KEVIN"; "JASON"; "MATTHEW"; "GARY"; "TIMOTHY"; "JOSE"; "LARRY"; "JEFFREY";
        ] 

    let mutable private firstTimeCall = true
    let mutable private clients = [];
    let private find id =     
        match List.filter (fun c -> c.Id = id) clients with         
        | [c] -> Some c
        | _ -> None

    let private replace c = 
        let rec replace beforeClient = function
            | [] -> (List.rev beforeClient) @ [c]
            | x::xs ->
                if x.Id=c.Id 
                then (List.rev beforeClient) @ (c :: xs)
                else replace (x::beforeClient) xs
        
        replace [] clients

    let private update c =
        match find c.Id with
        | Some client -> 
            let updatedClient = { 
                client with 
                    Sex = c.Sex; 
                    Name = c.Name;
                    TelephoneNum = c.TelephoneNum;
                    YearOfBirth = c.YearOfBirth;
                    ThemesOfInterest = c.ThemesOfInterest;
                }

            clients <- replace updatedClient
            Some updatedClient            
        | None -> None

    let private updateClients ()= List.map update
    let resetState () = firstTimeCall <- true
    
    let getSex ()= 
        match rand.Next(2) with
        | 0 -> Male
        | 1 -> Female
        | _ -> Other

    // let getName names = 
    //     match at (rand.Next(length names)) names with 
    //     | None -> failwith "Error in 'getName' helper method"
    //     | Some name -> name 

    // let getPhoneNumber ()= 
    //   seq { 
    //       for n in 0..7 do             
    //           yield rand.Next(10).ToString()
    //   } |> List.ofSeq |> List.fold (+) ""

    // let getYearOfBirth () = rand.Next(1975, 1997)
    // let createRandomPerson () = 
    //     let sex = getSex()
    //     let name = 
    //         match sex with
    //         |Female -> getName womenNames
    //         |_ -> getName menNames
    //     let phoneNumber = getPhoneNumber()
    //     let yearOfBirth = getYearOfBirth()

    //     {
    //         Id = rand.Next(10000000)
    //         Name=name;
    //         Sex = sex;
    //         TelephoneNum =Some phoneNumber;
    //         ThemesOfInterest = None;
    //         YearOfBirth = yearOfBirth;
    //     } 
    //     |> generateRandomInterests

    // let all () = 
    //     match firstTimeCall with
    //     | false -> ()
                       
    //     | true ->
    //         clients <- seq {
    //             for i in 0..20 do
    //                 yield createRandomPerson ()
    //         } |> List.ofSeq 

    //         firstTimeCall <- false
            
    //     copy clients
