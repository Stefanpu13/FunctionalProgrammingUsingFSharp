namespace ClientsRepo
open Helpers.L
open Types.T
open ClientHelpers.H


module File =
    let rand = System.Random()
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

    let getSex ()= 
        match rand.Next(2) with
        | 0 -> Male
        | 1 -> Female
        | _ -> Other

    let getName names = 
        match at (rand.Next(length names)) names with 
        | None -> failwith "Error in 'getName' helper method"
        | Some name -> name 

    let getPhoneNumber ()= 
      seq { 
          for n in 0..7 do             
              yield rand.Next(10).ToString()
      } |> List.ofSeq |> fold (+) ""

    let getYearOfBirth () = rand.Next(1975, 1997)
    let createRandomPerson () = 
        let sex = getSex()
        let name = 
            match sex with
            |Female -> getName womenNames
            |_ -> getName menNames
        let phoneNumber = getPhoneNumber()
        let yearOfBirth = getYearOfBirth()

        {
            Id = rand.Next(10000000)
            Name=name;
            Sex = sex;
            TelephoneNum =Some phoneNumber;
            ThemesOfInterest = None;
            YearOfBirth = yearOfBirth;
        } 
        |> generateRandomInterests

    let mutable private firstTimeCall = true
    let mutable private clients = [];
    let private find id =     
        match filter (fun c -> c.Id = id) clients with         
        | [c] -> Some c
        | _ -> None

    let private replace c = 
        let rec replace beforeClient = function
            | [] -> (rev beforeClient) @ [c]
            | x::xs ->
                if x.Id=c.Id 
                then (rev beforeClient) @ (c :: xs)
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

    let private updateClients ()= map update
    let resetState () = firstTimeCall <- true

    let all () = 
        match firstTimeCall with
        | false -> ()
                       
        | true ->
            clients <- seq {
                for i in 0..20 do
                    yield createRandomPerson ()
            } |> List.ofSeq 

            firstTimeCall <- false
            
        copy clients
