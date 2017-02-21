namespace DatingBureau

open Exercise11.Types.T
open Exercise11.Helpers.L
// open Exercises11To18.E

module API = 
    let private commonSubthemes subTheme1 subTheme2 =     
        match intersect (subTheme1,subTheme2) with
        | [] -> false
        | l -> true

    let private commonSubthemesSet subTheme1 subTheme2 =     
        not (Set.isEmpty (Set.intersect subTheme1 subTheme2))

    let private commonSubthemesMap (ReadingMap subthemes1) (ReadingMap subthemes2) = 
        Map.fold (fun foundCommonInterests reading v -> 
            foundCommonInterests || (Map.containsKey reading subthemes2)) false subthemes1 
        

    let private commonThemes themes1 themes2 = 
        // If directly using "themes1" and "themes2" and theme is added/removed this method will still compile
        // Creating new values preserves static type safety if new themes are added or themes are removed
        let t1 = {Sports = themes1.Sports; Music = themes1.Music; Reading = themes1.Reading;}
        let t2 = {Sports = themes2.Sports; Music = themes2.Music; Reading = themes2.Reading;}
        
        

        (commonSubthemesSet t1.Sports t2.Sports) ||
        (commonSubthemes t1.Music t2.Music) ||
        (commonSubthemesMap t1.Reading t2.Reading)

    let private haveCommonInterests cl1 cl2 =
        let {ThemesOfInterest = themes1} = cl1
        let {ThemesOfInterest = themes2} = cl2

        match themes1, themes2 with    
        | Some t1, Some t2 -> commonThemes t1 t2
        | _ -> false

    let areMatching client1 client2 = 
        let areOfDifferentSex = client1.Sex <> client2.Sex
        let ageDiffIsLessThan10Years = (abs (client1.YearOfBirth - client2.YearOfBirth)) < 10
        let haveCommonInterests = haveCommonInterests client1 client2 

        areOfDifferentSex && ageDiffIsLessThan10Years && haveCommonInterests
