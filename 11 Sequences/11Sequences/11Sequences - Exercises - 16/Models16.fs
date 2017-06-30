namespace Exercise16

module Models = 
    type Sex = Male|Female|Other

    // type Sport = Football|Tenis|Baseball|TableTenis|Basketball

    // type Music = Jazz|Rock|Pop|Salsa|HipHop|``Classical Music``

    // type Reading = Poetry|SciFi|Magazines|Novels|``Techincal Literature``

    type Sport = string

    type Music = string

    type Reading = string


    type InterestType = Sport of Sport|Music of Music|Reading of Reading

    // interests and interest types are dynamic

    // let getInterestTypeName = function
    // | Sport _-> "Sport"
    // | Music _-> "Music"
    // | Reading _ -> "Reading" 

    type ThemesOfInterest = {
        Sports: Sport list;
        Music: Music list; 
        Reading: Reading list;
        }
    type Client = { 
        ClientId: int
        Name:string; 
        TelephoneNum:string; 
        Sex:Sex; 
        YearOfBirth:int; 
        ThemesOfInterest: ThemesOfInterest option        
        }
