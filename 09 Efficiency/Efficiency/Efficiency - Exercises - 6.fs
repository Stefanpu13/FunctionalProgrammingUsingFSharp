namespace Exercises6
module E = 

    (* 9.6
        Declare a continuation-based version of the factorial function and compare the run time with
        the results in Section 9.4.        
    *)

    let rec factA = function
    | (0,m) -> m
    | (n,m) -> factA(n-1,n*m)

    let factACont (n, m) = 
        let rec factCont cont = function
        | (0, m) -> cont m
        | (n, m) -> factCont ((fun m -> cont n*m)) (n - 1,m)

        factCont id (n, m)

    let xs16 = List.init 1000000 (fun i -> 16)

    // #time
    
    for i in xs16 do let _ = factA(i,1) in ()

    for i in xs16 do let _ = factACont(i,1) in ()
