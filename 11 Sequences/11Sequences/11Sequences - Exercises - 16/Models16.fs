namespace Exercise16

module Models = 
    type Sex = Male|Female|Other

    type Sport = Football|Tenis|Baseball|TableTenis|Basketball

    type Music = Jazz|Rock|Pop|Salsa|HipHop|``Classical Music``

    type Reading = Poetry|SciFi|Magazines|Novels|``Techincal Literature``

    type ThemesOfInterest = {
        Sports: Sport list;
        Music: Music list; 
        Reading: Reading list;
        }
    type Client = { 
        Id: int
        Name:string; 
        TelephoneNum:string option; 
        Sex:Sex; 
        YearOfBirth:int; 
        ThemesOfInterest: ThemesOfInterest option        }
