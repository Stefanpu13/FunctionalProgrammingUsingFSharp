namespace Exercises1
module E = 
    (* 8.4
        Declare null to denote the default value of the record type:
        type t = { mutable link : t ; data : int };;
        Declare some other values of type t and use assignment to build chains and circles of values of
        type t. Declare a function to insert an element in the front of a chain of values of type t.
    *)
    let add x y = x + y
    
    type T = { mutable link : T; data : int }
    
    let ts = [
        {link=Unchecked.defaultof<T>; data=1};
        {link=Unchecked.defaultof<T>; data=2};
        {link=Unchecked.defaultof<T>; data=3};
        {link=Unchecked.defaultof<T>; data=4};
        {link=Unchecked.defaultof<T>; data=5};
    ]

    let insert link chain = 
        link.link <- chain
        link 
    let createChain ts = 
        List.foldBack insert ts Unchecked.defaultof<T>

    createChain ts

    let createCircle ts = 
        let rec createCircle firstLink previousLink ts =
            match ts with
            | [] -> firstLink
            | [lastLink] ->
                lastLink.link <- firstLink
                previousLink.link <- lastLink                 
                createCircle firstLink lastLink []
            | x::rest ->  
                previousLink.link <- x
                createCircle firstLink x rest
        
        match ts with
        | [] -> Unchecked.defaultof<T>
        | x::rest -> createCircle x x rest
  
    let enumCircle links =         
        let rec enumCircle c t = 
            if c < 20 
            then printfn "%A" t.data; enumCircle (c+1) t.link
            else printfn "%A" t.data;
        enumCircle 0 links
    
    enumCircle (createCircle ts)
