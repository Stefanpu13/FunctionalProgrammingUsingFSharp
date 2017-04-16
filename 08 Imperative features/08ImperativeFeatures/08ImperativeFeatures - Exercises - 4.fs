namespace Exercises4
module E = 
    (* 8.4
        Declare null to denote the default value of the record type:
        type t = { mutable link : t ; data : int };;
        Declare some other values of type t and use assignment to build chains and circles of values of
        type t. Declare a function to insert an element in the front of a chain of values of type t.
    *)

    type T = { mutable link : T; data : int }
    let defaultT = Unchecked.defaultof<T>    

    let insert link chain = 
        link.link <- chain
        link 
    let createChain ts = 
        List.foldBack insert ts defaultT
    
    let createCircle links = 
        let rec createCircle firstLink previousLink links =
            match links with
            | [] -> 
                firstLink
            | [lastLink] ->
                lastLink.link <- firstLink
                previousLink.link <- lastLink        
                firstLink
            | link::rest ->  
                previousLink.link <- link
                createCircle firstLink link rest
        
        match links with
        | [] -> defaultT
        | [x] -> 
            x.link <- x 
            x
        | x::rest -> createCircle x x rest 
