namespace Exercises10
module E = 

    (* 9.10
        Consider the following list-generating function:
        let rec bigListK n k =
            if n=0 then k []
            else bigListK (n-1) (fun res -> 1::k(res))
        The call bigListK 130000 id causes a stack overflow. Analyze this problem.
    *)

    let rec bigListK n k =
        if n=0 then k []
        else bigListK (n-1) (fun res -> 1::k(res))
    
    (*
        The problem comes from the "1::k(res)" part.
        When k [] is called the built up continuation starts executing. 
        Since calling k(res) is not the last operation, a new stack frame is used every time ('::' is the last operation)

        To make the function tail-recursive: '1::k(res)' should become 'k(1::res)'
    *)
        
    // bigListK 53000 id