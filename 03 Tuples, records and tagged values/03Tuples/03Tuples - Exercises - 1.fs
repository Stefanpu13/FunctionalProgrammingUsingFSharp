namespace Exercises1
module E = 
    (* 3.1 
        A time of day can be represented as a triple (hours, minutes, f) where f is either AM or PM
        – or as a record. Declare a function to test whether one time of day comes before another. For
        example, (11,59,"AM") comes before (1,15,"PM"). Make solutions with triples as well
        as with records. Declare the functions in infix notation.
    *)

    // Note: Solutions here do not validate entered values. For Times with validation see exercise 6

    // 3.1.1 Solution with triples

    type TimeOfDayTriple = TimeOfDayTriple of int * int * string
    let (<.) t1 t2 =
        // Destructure (Pattern match) params to their parts
        let (TimeOfDayTriple (h1, m1, f1)) = t1
        let (TimeOfDayTriple (h2, m2, f2)) = t2
        match f1, f2 with
        | "AM", "PM" -> true
        | "PM", "AM" -> false
        // order of delcaration in type matters when comparing types
        // Earlier declared parts are compared first.
        // In case of "TimeOfDayTriple" hours are compared first
        | _, _ -> t1 < t2 

    // 3.1.2 Solution with records

    type TimeOfDayRecord = {Hour: int; Minute: int; F:string}
    let (<..) t1 t2 = 
        let {Hour = h1; Minute = m1; F = f1} = t1
        let {Hour = h2; Minute = m2; F = f2} = t2
        match f1, f2 with
        | "AM", "PM" -> true
        | "PM", "AM" -> false
        // order of delcaration in type matters when comparing types
        // Earlier declared parts are compared first.
        // In case of "TimeOfDayRecord" hours are compared first
        | _, _ -> t1 < t2
  