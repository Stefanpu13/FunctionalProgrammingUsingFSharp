namespace Exercises5
module E = 

    (* 5.5
        Consider the map colouring example in Section 4.6. Give declarations for the functions areNb
        canBeExtBy, extColouring, countries and colCntrs using higher-order list functions.
        Are there cases where the old declaration from Section 4.6 is preferable?

        Note: Assume that the results should be the same for both variants.
    *)


    // function returns as soon as  match is found
    let rec isMember x = function
    | y::ys -> x=y || (isMember x ys)
    | [] -> false

    // function always has to traverse the whole list
    let isMember2 x l = List.fold (fun isMem el -> (el = x) || isMem) false l

    let areNb m c1 c2 =
        isMember2 (c1,c2) m || isMember2 (c2,c1) m

    // Can result in stack overflow exception, as function is not tail recursive 
    let rec canBeExtBy m col c =
        match col with
        | [] -> true
        | c'::col' -> not(areNb m c' c) && canBeExtBy m col' c

    // does not have the problem of stack overflow
    let canBeExtBy2 m col c = List.fold (fun canBeExt country -> not (areNb m c country) && canBeExt) true col

    // not tail recursive. Returns as soon as match is found
    let rec extColouring m cols c =
        match cols with
        | [] -> [[c]]
        | col::cols' -> 
            if canBeExtBy m col c
            then (c::col)::cols'
            else col::extColouring m cols' c

    // More complicated algorithm. Also not efficient due to list concant "@"
    let extColouring2 m cols c =
        let (extendedCols, alreadyExtended) =
            List.fold (
                fun (cols', alreadyExtended) currentColor-> 
                    if canBeExtBy2 m currentColor c && not alreadyExtended                        
                    then (cols' @ [(c::currentColor)], true)
                    else (cols' @ [currentColor], alreadyExtended)
            ) ([], false) cols

        if alreadyExtended then extendedCols else extendedCols @ [[c]]

    let addElem x ys = if isMember2 x ys then ys else x::ys

    // stack overflow problems
    let rec countries = function
        | [] -> []
        | (c1,c2)::m -> addElem c1 (addElem c2 (countries m))

    // no stackoverflow problems
    let countries2 m = 
        List.foldBack (
            fun (c1, c2) addedCountries -> addElem c1 (addElem c2 addedCountries)
        ) m [] 

    // stack overflow problems
    let rec colCntrs m = function
    | [] -> []
    | c::cs -> extColouring m (colCntrs m cs) c

    // No stack overflow problems
    let colCntrs2 m cs = List.foldBack (fun c cols-> extColouring2 m cols c) cs []

    let colMap m = colCntrs m (countries m)

    let colMap2 m = colCntrs2 m (countries2 m)

    (*
        Conclusion: 
        1. Original functions are not tail recursive. 
        However, considering that the domain are countries (total count is in the 100s), this will not be a problem
        2. Revised functions more clearly show in which direction the lists are traversed (fold vs foldBack) 
        3. extColouring is more preferable than extColouring2 because:
            a) it is simpler
            b) it is more efficient
        In all other cases revised functions seem better, due to 2
    *)