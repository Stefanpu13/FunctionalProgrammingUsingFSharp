namespace Exercise11.Types

module T = 
    type Sex = Male|Female|Other

    type Sport = Football|Tenis|Baseball|TableTenis|Basketball

    type Music = Jazz|Rock|Pop|Salsa|HipHop|``Classical Music``

    type Reading = Poetry|SciFi|Magazines|Novels|``Techincal Literature``

    type ReadingMap = ReadingMap of Map<Reading, bool>

    type ThemesOfInterest = {
        Sports: Sport Set;
        Music: Music list;         
        Reading: ReadingMap
        }
    type Client = { 
        Id: int
        Name:string; 
        TelephoneNum:string option; 
        Sex:Sex; 
        YearOfBirth:int; 
        ThemesOfInterest: ThemesOfInterest option
        }
