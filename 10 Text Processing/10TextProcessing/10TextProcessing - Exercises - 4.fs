namespace Exercies4
open System
open System.Text.RegularExpressions
#if INTERACTIVE
#load "TextProcessing.fs"
#endif
open TextProcessing.TextProcessing
module E =     
    type Position = {degrees: float; minutes: float; seconds: float; sign: float}
    let signNum sign = 
        if sign = "E" || sign = "N" 
        then  1.0 
        elif sign = "W" || sign = "S" 
        then -1.0 
        else 0.0

    let getTotalSeconds {degrees=deg; minutes=min; seconds=sec; sign=sign} = 
        (sec + 60.0 * min + 60.0 * 60.0 * deg) * sign

    let toPosition  = function
    | degs::mins::secs::[sign] ->
        {degrees=float degs;minutes=float mins; seconds=float secs; sign=sign}
    | _ -> {degrees=0.0;minutes=0.0; seconds=0.0; sign=0.0}

    let toListOfPositionParts (posistion:string) = 
        Regex
            .Replace(posistion,"[^.\w]", " ")
            .Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

    let parsePositionParts positions = 
        List.map (fun pos -> 
            match Double.TryParse (pos) with
            | true, res -> res
            | false, _ -> signNum pos) positions
    let getPosition position = 
        let regex = @"(\s?(?:[0-9]*\.*[0-9]*[^\w]*){3} [E|W|S|N])*"
        let m = Regex.Match (position, regex)
        let captures = captureList m 1
        let positions =
            List.map (
                toListOfPositionParts 
                >> List.ofArray 
                >> parsePositionParts 
                >> toPosition
                >> getTotalSeconds) captures                 

        match positions with
        | lat::[lgt] -> lat , lgt
        | _ -> Double.MinValue, Double.MinValue

    getPosition """14'27"35.03"' W 55'13"47"' N"""