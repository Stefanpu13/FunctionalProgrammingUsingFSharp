namespace Exercises4
open System
open System.Text.RegularExpressions
#if INTERACTIVE
#load "TextProcessing.fs"
#endif
open TextProcessing.TextProcessing
module E =    
    type Direction = E|W|S|N
    type PositionPart = Degree of string*Direction  | Minute of string | Second of string | Sign of string
    type ValidPosition = {degrees: int; minutes: int; seconds: float; sign: float}
    type Position =  Lat of ValidPosition  | Lgt of ValidPosition  | InvalidPosition
    type Coord = ValidCoord of float | InvalidCoord    
    type Coords =  ValidCoords of (float * float) | InvalidCoords    

    let toListOfPositionParts (posistion:string) = 
        Regex
            .Replace(posistion,"[^.\w-]", " ")
            .Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray 

    let toPosition pos sign = function
    | ValidCoord degs::ValidCoord mins::[ValidCoord secs]->
         pos {degrees=int degs;minutes=int mins; seconds=float secs; sign=sign}
    | _ -> InvalidPosition

    let validateWholePositionPart min max parseFunc part = 
        match parseFunc part with
        | true, v when min <= v && v <= max -> Some v
        | _ -> None
    let (|ValidDegree|_|) max = validateWholePositionPart 0 max Int32.TryParse
    let (|ValidMinutes|_|) part = validateWholePositionPart 0 59 Int32.TryParse part
    let (|ValidSeconds|_|) part = validateWholePositionPart 0.0 59.99 Double.TryParse part

    let parsePositionPart  = function             
        | Degree (ValidDegree 180 degs, E) -> ValidCoord( float degs)
        | Degree (ValidDegree 180 degs, W) -> ValidCoord( float degs)
        | Degree (ValidDegree 90 degs, N) -> ValidCoord( float degs)
        | Degree (ValidDegree 90 degs, S) -> ValidCoord( float degs)        
        | Minute (ValidMinutes mins) -> ValidCoord(float mins)
        | Second (ValidSeconds secs) -> ValidCoord secs
        | _ -> InvalidCoord
        
    let parsePositionParts = function
    | degs::mins::secs::["N"] -> 
        (List.map parsePositionPart [Degree (degs, N);Minute mins;Second secs]) |> toPosition Lat 1.0
    | degs::mins::secs::["S"] -> 
        List.map parsePositionPart [Degree (degs, S);Minute mins;Second secs] |> toPosition Lat (-1.0)
    | degs::mins::secs::["E"] -> 
        List.map parsePositionPart [Degree (degs, E);Minute mins;Second secs] |> toPosition Lgt 1.0
    | degs::mins::secs::["W"] ->  
        List.map parsePositionPart [Degree (degs, W);Minute mins;Second secs] |> toPosition Lgt (-1.0)
    | _ -> InvalidPosition

    let parsePosition = (toListOfPositionParts >> parsePositionParts) 

    let getTotalSeconds {degrees=deg; minutes=min; seconds=sec; sign=sign} = 
        ((sec + 60.0 * float min + 60.0 * 60.0 * float deg) * sign)
    let parseCoordinates (latStr, lgtStr) = 
        match parsePosition latStr, parsePosition lgtStr with
        | Lat lat, Lgt lgt -> ValidCoords (getTotalSeconds lat, getTotalSeconds lgt)
        | _ -> InvalidCoords
        
    let getPosition position = 
        let regex = @"(\s?(?:[0-9]*\.*[0-9]*[^\w]*){3} [E|W|S|N])*"
        let m = Regex.Match (position, regex) 
        // let m = Regex.Match ( """14*-59'59.03" S 55*1'47" W""", regex)        
        let captures = captureList m 1        
        // List.map parsePosition captures
        match captures with
        | lat::[lgt] -> parseCoordinates (lat, lgt)
        | _ -> InvalidCoords

module Client = 
    
    match E.getPosition """14*-59'59.03" S 55*1'47" W""" with
    | E.InvalidCoords -> printfn "Invalid"
    | E.ValidCoords (lat, lgt) -> printfn "%A %A" lat lgt
