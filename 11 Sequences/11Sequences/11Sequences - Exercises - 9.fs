namespace Exercises9

module E =     
    (* 11.9
        Declare a sequence denoting the following enumeration of the integers:
        0,−1, 1,−2, 2,−3, 3, . . .
    *)

    // Also solution to 11.14
    let alternating = seq {
        let rec alt n = seq {             
                yield -n
                yield n
                yield! alt (n+1)
        }
        yield 0 
        yield! alt 1                   
    }

    // Seq.item 6666666 alternating
    