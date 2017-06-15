namespace Exercises19To21
// #load "01Helpers.fs"
open Helpers.L

module E = 
    (* 4.19

        Evaluation of the expression areNbm c1 c2 may traverse the map m twice. Explain why and
        give an alternative declaration for areNb which avoids this problem.

        let rec isMember x = function
        | y::ys -> x=y || (isMember x ys)
        | [] -> false


        let areNb m c1 c2 =
            isMember (c1,c2) m || isMember (c2,c1) m
    *)

    // Explanation: if "isMember (c1,c2) m" does not find a pair the second
    // part - isMember (c2,c1) m - will also be evaluated

    let areNb m c1 c2 = 
        let rec areNb = function
            | [] -> false
            | ( x1, x2)::xs ->
                if (c1, c2) = (x1,  x2) || (c1, c2) = (x2, x1)
                then true
                else areNb xs
        
        areNb m

    (* 4.20
        Most of the auxiliary functions for the map-colouring program just assume an arbitrary, but
        fixed, map. The function canBeExtBy, for example, just passes m on to areNb, which again
        passes m on to isMember. The program can therefore be simplified by declaring (most of) the
        auxiliary functions locally as sketched here:
        let colMap m =
            let areNb c1 c2 = ...
            let canBeExtBy col c = ...
            ...

            Revise the program by completing this skeleton.
    *)
    module ColMap_4_20 =

        type Country = Country of string
        type Map = Map of (Country * Country) list
        type Colour = Colour of Country list
        type Colouring = Colouring of Colour list
        let colMap m = 
            let rec isMember x = function
                | y::ys -> x=y || (isMember x ys)
                | [] -> false

            let areNb country1 country2 = 
                let rec areNb = function
                | Map [] -> false
                | Map ((x1, x2)::xs) ->
                    if (country1, country2) = (x1,  x2) || (country1, country2) = (x2, x1)
                    then true
                    else areNb (Map xs)
                
                areNb m

            let rec canBeExtBy color country =
                match color with
                | Colour [] -> true
                | Colour (country'::col') -> not(areNb country' country) && canBeExtBy (Colour col') country 


            let rec extColouring cols country =
                match cols with
                | Colouring [] -> Colouring[ Colour [country] ]            
                | Colouring (col::cols') -> 
                    if canBeExtBy col country
                    then                
                        let (Colour countryCols) = col 
                        Colouring ((Colour (country::countryCols))::cols')
                    else 
                        let (Colouring colourings) =  extColouring (Colouring cols') country
                        Colouring (col::colourings) 

            let addElem x ys = if isMember x ys then ys else x::ys

            let rec countries = function
                | Map [] ->  []
                | Map ((c1,c2)::ms) -> addElem c1 (addElem c2 (countries (Map ms)))

            let rec colCntrs = function
                | [] -> Colouring []
                | c::cs -> extColouring (colCntrs cs) c

            colCntrs (countries m)

            //let exMap = Map [(Country "a",Country "b"); (Country "c",Country  "d"); (Country "d",Country "a")]

    (* 4.21
        Revise the map-colouring program so that it can cope with countries which are islands (such as
        Iceland) having no neighbours.
    *)

    module ColMap_4_21 =

        // coutry either is country with neighbours or an Island
        // create a map that is consists of counties 
        type Country = Country of string
        type Countries = Neighbours of (Country * Country) | Island of Country        
        type Map = Map of Countries list
        type Colour = Colour of Country list
        type Colouring = Colouring of Colour list
        let colMap m = 
            let areNb country1 country2 = 
                let rec areNb = function
                | Map [] -> false
                | Map ((Island x1)::xs) -> areNb (Map xs)
                | Map ((Neighbours(x1, x2))::xs) ->                
                    if (country1, country2) = (x1,  x2) || (country1, country2) = (x2, x1)
                    then true
                    else areNb (Map xs)
                
                areNb m

            let rec canBeExtendedBy countries country =
                match countries with
                | Colour [] -> true
                | Colour (country'::countries) -> 
                    // List.forall (fun country' -> not(areNb country' country)) (country'::countries)
                    not(areNb country' country) && canBeExtendedBy (Colour countries) country 

            let rec extendColouring cols country =
                match cols with
                | Colouring [] -> Colouring[ Colour [country] ]            
                | Colouring (col::cols') -> 
                    if canBeExtendedBy col country
                    then                
                        let (Colour countryCols) = col 
                        Colouring ((Colour (country::countryCols))::cols')
                    else 
                        let (Colouring colourings) =  extendColouring (Colouring cols') country
                        Colouring (col::colourings) 

            // let rec isMember x = function
            //     | y::ys -> x=y || (isMember x ys)
            //     | [] -> false

            let rec isMember x l = List.contains x l

            let addElem x ys = if isMember x ys then ys else x::ys

            let rec countries = function
                | Map [] ->  []
                | Map (Neighbours(c1,c2)::ms) -> addElem c1 (addElem c2 (countries (Map ms)))
                | Map (Island c1::ms) -> addElem c1 (countries (Map ms))

            let rec colCntrs = function
                | [] -> Colouring []
                | c::cs -> extendColouring (colCntrs cs) c

            colCntrs (countries m)
    