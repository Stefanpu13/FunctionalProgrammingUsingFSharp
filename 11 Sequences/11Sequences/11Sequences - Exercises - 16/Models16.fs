namespace Exercise16

module Models = 
    type Sex = Male = 1|Female = 2|Other = 3

    type Sport = string

    type Music = string

    type Reading = string
    type Interest = {Category: string; Name: string}

    type Client = { 
        ClientId: int
        Name:string; 
        TelephoneNum:string; 
        Sex:Sex; 
        YearOfBirth:int; 
        ThemesOfInterest: Interest list        
        }
