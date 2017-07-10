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
        (intersect >> List.isEmpty >> not) (subTheme1,subTheme2)
        
    let private haveCommonInterests cl1 cl2 =
        let {ThemesOfInterest = themes1} = cl1
        let {ThemesOfInterest = themes2} = cl2

        commonThemes themes1 themes2

    let private areMatching client1 client2 =         
        let ageDiffIsLessThan10Years = (abs (client1.YearOfBirth - client2.YearOfBirth)) < 10
        let haveCommonInterests = haveCommonInterests client1 client2 

        ageDiffIsLessThan10Years && haveCommonInterests
        
    let getMatchingClients cl1 = 
        DAL.getClientsWithDifferentSex cl1 
        |> Seq.filter (fun cl2 -> areMatching cl1 cl2)    
