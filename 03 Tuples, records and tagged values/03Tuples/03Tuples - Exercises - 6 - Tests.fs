module Exercises6Tests
open NUnit.Framework
open FsUnitTyped
open Exercises6.E

// "Comes before" is custom operator - (<.) for triples and (<..) for records
[<TestFixture>]
type ``Comes before tests``() = 
    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,56,"AM"), output should be true``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 55; PartOfDay = AM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (IsEarlier true)


    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,54,"AM"), output should be false``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 55; PartOfDay = AM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 54; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (IsEarlier false)

    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,55,"AM"), output should be false``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 0; PartOfDay = AM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 0; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual ( IsEarlier false)

    [<Test>]
    member test.``When t1=(11,55,"AM"), t2=(11,54,"PM"), output should be true``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 55; PartOfDay = AM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 54; PartOfDay = PM} 

        t3 <.. t4 |> shouldEqual (IsEarlier true)

    [<Test>]
    member test.``When t1=(11,55,"PM"), t2=(11,56,"AM"), output should be false``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 55; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (IsEarlier false)

    [<Test>]
    member test.``When t1 minutes are < 0, output should be "t1 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= -1; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t1 is invalid")

    
    [<Test>]
    member test.``When t1 minutes are > 59, output should be "t1 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 60; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t1 is invalid")

    [<Test>]
    member test.``When t1 hours are < 0, output should be "t1 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= -1; Minute= 06; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t1 is invalid")

    [<Test>]
    member test.``When t1 hours are > 23, output should be "t1 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= 24; Minute= 06; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t1 is invalid")

    
    [<Test>]
    member test.``When t2 minutes are < 0, output should be "t2 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 20; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= -1; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t2 is invalid")

    
    [<Test>]
    member test.``When t2 minutes are > 59, output should be "t2 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 46; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 11; Minute= 60; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t2 is invalid")

    [<Test>]
    member test.``When t2 hours are < 0, output should be "t2 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= 11; Minute= 06; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= -1; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t2 is invalid")

    [<Test>]
    member test.``When t2 hours are > 23, output should be "t2 is invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= 22; Minute= 06; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 24; Minute= 56; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t2 is invalid")

    [<Test>]
    member test.``When t1 and t2 are invalid, output should be "t1 and t2 are invalid"``() = 
        let t3 = TimeOfDayRecord {Hour= -22; Minute= 06; PartOfDay = PM}
        let t4 = TimeOfDayRecord {Hour= 24; Minute= 61; PartOfDay = AM} 

        t3 <.. t4 |> shouldEqual (InvalidTimeMsg "t1 and t2 are invalid")
