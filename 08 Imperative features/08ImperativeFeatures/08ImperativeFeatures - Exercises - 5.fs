namespace Exercises5
module E = 
    (* 8.5
        Give a declaration of the gcd function using a while loop instead of recursion (cf. Section
        1.8).
    *)

    // gcd recursive declaration from Section 1.8
    let rec gcdRec = function
        | (0,n) -> n
        | (m,n) -> gcdRec(n % m,m)

    let gcd (m, n) =
        let mutable ( mutM,  mutN) = (m, n)
        
        while mutM > 0 do
            let newM = mutN % mutM 
            mutN <- mutM
            mutM <- newM
        mutN
