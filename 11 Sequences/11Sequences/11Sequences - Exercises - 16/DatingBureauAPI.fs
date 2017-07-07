namespace Exercise16
open Models
open DAL

module API = 
    let private intersect (l1, l2) = 
        let rec intersect commonElems = function            
            | (x::xs, y::ys) -> 
                    match (x, y) with
                    | (sm, lg) when sm < lg -> intersect commonElems (xs, y::ys)
                    | (lg, sm) when lg > sm -> intersect commonElems (x::xs, ys)
                    | (eq1, eq2) -> intersect (eq1::commonElems) (xs, ys)
            | _ -> List.rev commonElems

        intersect [] (List.sort l1,List.sort l2)

    let private commonThemes subTheme1 subTheme2 =     
        match intersect (subTheme1,subTheme2) with
        | [] -> false
        | l -> true
    let private haveCommonInterests cl1 cl2 =
        let {ThemesOfInterest = themes1} = cl1
        let {ThemesOfInterest = themes2} = cl2

        commonThemes themes1 themes2

    let private areMatching client1 client2 = 
        let areOfDifferentSex = client1.Sex <> client2.Sex
        let ageDiffIsLessThan10Years = (abs (client1.YearOfBirth - client2.YearOfBirth)) < 10
        let haveCommonInterests = haveCommonInterests client1 client2 

        areOfDifferentSex && ageDiffIsLessThan10Years && haveCommonInterests
        
    let getMatchingClients cl1 = 
        DAL.getClientsWithDifferentSex cl1 
        |> Seq.filter (fun cl2 -> areMatching cl1 cl2)    

// module Client = 
//     let cl =
//         {
//             ClientId = 0;
//             Name="Stefan2";
//             YearOfBirth=1989;
//             Sex = enum 1;
//             TelephoneNum="213123"
//             ThemesOfInterest = 
//             [
//                 {
//                     Category="Sport";
//                     Name="Baseball"
//                 }
//             ] 
//         }

//     // API.getMatchingClients cl