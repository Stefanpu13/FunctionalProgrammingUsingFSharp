namespace Exercise6
module E = 
    
    //Solve Exercise 3.1 using tagged values to represent AM and PM.

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
