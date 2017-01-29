namespace Exercises6
module E = 
    
    //Solve Exercise 3.1 using tagged values to represent AM and PM.

    // Since order of delcaration matters, declaring record`s properties in the
    // correct order will make comparison automatic
    type PartOfDay = AM | PM      
    type TimeOfDayRecord = {PartOfDay: PartOfDay; Hour: int; Minute: int}
    type Time = TimeOfDayRecord of TimeOfDayRecord | InvalidTime
    type IsEarlierTimeResult = InvalidTimeMsg of string | IsEarlier of bool
    let (|Hour|_|) h = 
        match h with 
        | h when h >= 0 && h <= 23 -> Some h
        | _ -> None
    let (|Minute|_|) m = 
        match m with 
        | m when m >= 0 && m <= 59 -> Some m
        | _ -> None
    let (|ValidatedTime|) t = 
        match t with
        | TimeOfDayRecord {PartOfDay = f; Hour = Hour h; Minute = Minute m} -> 
            TimeOfDayRecord ({PartOfDay = f; Hour = h; Minute = m})    
        | _ -> InvalidTime
    let (<..) (ValidatedTime t1) (ValidatedTime t2) = 
        match (t1, t2) with
        | (InvalidTime , InvalidTime) -> InvalidTimeMsg "t1 and t2 are invalid"
        | (_, InvalidTime) ->  InvalidTimeMsg "t2 is invalid"
        | (InvalidTime, _) -> InvalidTimeMsg "t1 is invalid"
        | (TimeOfDayRecord t1, TimeOfDayRecord t2) -> IsEarlier (t1 < t2)
