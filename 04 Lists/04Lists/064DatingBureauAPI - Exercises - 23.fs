namespace DatingBureau

open Types.T

module API = 
    // Same function as 4.11.3
    let private intersect (l1, l2) = 
        let rec intersect commonElems = function
            | (x::xs, y::ys) -> 
                match (x, y) with
                | (sm, lg) when sm < lg -> intersect commonElems (xs, y::ys)
                | (lg, sm) when lg > sm -> intersect commonElems (x::xs, ys)
                | (eq1, eq2) -> intersect (eq1::commonElems) (xs, ys)
            | _ -> rev commonElems

        intersect ([]) (l1, l2)
    let private commonSubthemes subTheme1 subTheme2 =     
        match intersect (subTheme1,subTheme2) with
        | [] -> false
        | l -> true

    let private commonThemes themes1 themes2 = 
        // Preserves static type safety if new themes are added or themes are removed
        let t1 = {Sports = themes1.Sports; Music = themes1.Music; Reading = themes1.Reading;}
        let t2 = {Sports = themes2.Sports; Music = themes2.Music; Reading = themes2.Reading;}
        
        (commonSubthemes t1.Sports t2.Sports) ||
        (commonSubthemes t1.Music t2.Music) ||
        (commonSubthemes t1.Reading t2.Reading)

    let private haheCommonInterests cl1 cl2 =
        let {ThemesOfInterest = themes1} = cl1
        let {ThemesOfInterest = themes2} = cl2

        match themes1, themes2 with    
        | Some t1, Some t2 -> commonThemes t1 t2
        | _ -> false

    let areMatching client1 client2 = 
        let areOfDifferentSex = client1.Sex <> client2.Sex
        let ageDiffIsLessThan10Years = (abs (client1.YearOfBirth - client2.YearOfBirth)) < 10
        let haveCommonInterests = haheCommonInterests client1 client2 

        areOfDifferentSex && ageDiffIsLessThan10Years && haveCommonInterests
