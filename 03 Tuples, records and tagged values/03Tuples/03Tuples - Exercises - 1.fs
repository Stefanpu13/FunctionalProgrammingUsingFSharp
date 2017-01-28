namespace Exercises1
module E = 
    (* 3.1 
        A time of day can be represented as a triple (hours, minutes, f) where f is either AM or PM
        – or as a record. Declare a function to test whether one time of day comes before another. For
        example, (11,59,"AM") comes before (1,15,"PM"). Make solutions with triples as well
        as with records. Declare the functions in infix notation.
    *)

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
    let t1 = TimeOfDayTriple ( 11, 55, "AM")
    let t2 = TimeOfDayTriple (11, 54, "PM")

    t1 <. t2    

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

    let t3 = {Hour= 11; Minute= 24; F = "AM"}
    let t4 = {Hour= 11; Minute= 23; F = "PM"} 

    t3 <.. t4

    // Since order of delcaration matters, declaring record`s properties in the
    // correct order will make comparison automatic
    type PartOfDay = AM | PM  
    type TimeOfDayRecord2 = {F:PartOfDay; Hour: int; Minute: int}

    let t5 = {Hour= 11; Minute= 24; F = PM}
    let t6 = {Hour= 11; Minute= 23; F = PM} 

    t5 < t6

    // Solution with records and validation
    type TimeOfDayRecord3 = {F: PartOfDay; Hour: int; Minute: int}
    type Time = TimeOfDayRecord3 of TimeOfDayRecord3 | InvalidTime
    type IsEarlierTimeResult = InvalidTimeMsg of string | IsEarlier of bool
    let (|Hour|_|) h = 
        match h with 
        | h when h >= 0 && h <= 23 -> Some h
        | _ -> None
    let (|Minute|_|) m = 
        match m with 
        | m when m >= 0 && m <= 59 -> Some m
        | _ -> None
    let (|ValidTime|) t = 
        match t with
        | TimeOfDayRecord3 {F = f; Hour = Hour h; Minute = Minute m} -> 
            TimeOfDayRecord3 ({F = f; Hour = h; Minute = m})    
        | _ -> InvalidTime
    let isEarlier (ValidTime t1, ValidTime t2) = 
        match (t1, t2) with
        | (InvalidTime , InvalidTime) -> InvalidTimeMsg "t1 and t2 are invalid"
        | (_, InvalidTime) ->  InvalidTimeMsg "t2 is invalid"
        | (InvalidTime, _) -> InvalidTimeMsg "t1 is invalid"
        | (TimeOfDayRecord3 t1, TimeOfDayRecord3 t2) -> IsEarlier (t1 < t2)

    let t1' = TimeOfDayRecord3 {F = PM; Hour = 12; Minute = 33}
    let t2' = TimeOfDayRecord3 {F = AM; Hour = 12; Minute = 34}

    isEarlier (t1', t2') 
