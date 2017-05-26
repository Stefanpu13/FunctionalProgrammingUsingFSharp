namespace Exercies4
open System
open System.Text.RegularExpressions
#if INTERACTIVE
#load "TextProcessing.fs"
#endif
open TextProcessing.TextProcessing
module E =         
    type ValidPosition = {degrees: float; minutes: float; seconds: float; sign: float}
    type Position =  Lat of ValidPosition  | Lgt of ValidPosition  | InvalidPosition
    type Coord = ValidCoord of float | InvalidCoord    
    type Coords =  ValidCoords of (float * float) | InvalidCoords

    let toListOfPositionParts (posistion:string) = 
        Regex
            .Replace(posistion,"[^.\w]", " ")
            .Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 

    let toPosition pos = function
    | ValidCoord degs::ValidCoord mins::ValidCoord secs::[ValidCoord sign] ->
         pos {degrees=float degs;minutes=float mins; seconds=float secs; sign=sign}
    | _ -> InvalidPosition

    let parsePositionPart  posistionPart= 
        match Double.TryParse (posistionPart) with
        | true, res -> ValidCoord res
        | false, _ -> InvalidCoord

    let parsePositionParts = function
    | degs::mins::secs::["N"] -> (List.map (parsePositionPart) [degs;mins;secs;"1.0"]) |> toPosition Lat
    | degs::mins::secs::["S"] -> (List.map (parsePositionPart) [degs;mins;secs;"-1.0"]) |> toPosition Lat 
    | degs::mins::secs::["E"] -> (List.map (parsePositionPart) [degs;mins;secs;"1.0"]) |> toPosition Lgt 
    | degs::mins::secs::["W"] ->  (List.map (parsePositionPart) [degs;mins;secs;"-1.0"]) |> toPosition Lgt 
    | _ -> InvalidPosition

    let parsePosition = (toListOfPositionParts >> parsePositionParts) 

    let getTotalSeconds {degrees=deg; minutes=min; seconds=sec; sign=sign} = 
        ((sec + 60.0 * min + 60.0 * 60.0 * deg) * sign)
    let parseCoordinates (latStr, lgtStr) = 
        match parsePosition latStr, parsePosition lgtStr with
        | Lat lat, Lgt lgt -> ValidCoords (getTotalSeconds lat, getTotalSeconds lgt)
        | _ -> InvalidCoords
        
    let getPosition position = 
        let regex = @"(\s?(?:[0-9]*\.*[0-9]*[^\w]*){3} [E|W|S|N])*"
        let m = Regex.Match (position, regex)
        let captures = captureList m 1
        match captures with
        | lat::[lgt] -> parseCoordinates (lat, lgt)
        | _ -> InvalidCoords

module Client = 
    match E.getPosition """14'27"35.03"' N 55'13"47"' E""" with
    | E.InvalidCoords -> printfn "Invalid"
    | E.ValidCoords (lat, lgt) -> printfn "%A %A" lat lgt
