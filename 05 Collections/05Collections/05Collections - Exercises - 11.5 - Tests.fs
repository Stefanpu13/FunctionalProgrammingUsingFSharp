module Exercises23Tests

open Exercise11.Helpers.L
open Exercise11.Types.T
open Exercise11.ClientHelpers.H
open Exercise11.ClientsRepo
open DatingBureau.API
open NUnit.Framework
open FsUnitTyped

(* 4.23
    A dating bureau has a file containing name, telephone number, sex, year of birth and themes
    of interest for each client. You may make a request to the bureau stating your own sex, year
    of birth and themes of interest and get a response listing all matching clients, that is, clients
    with different sex, a deviation in age less than 10 years and with at least one common theme
    of interest. The problem is to construct a program for generating the responses from the dating
    bureau.

    Additional Reqs:
        1. User can have no themes of Interests or have any number of Interests; 
        2. Each interest is a Theme and can have several subThemes:
            For example: Theme "Sport" with subthemes "Football" and "Tenis"  
            or
            be empty        
        3. Adding new theme/subtheme should break existing code or be automatically handled
        4. Removing new theme/subtheme should break existing code or be automatically handled
        
*)

let client1 = {
    Id=11;
    Name = "Gregory";
    Sex = Male;
    TelephoneNum = Some "";
    YearOfBirth = 1990;
    ThemesOfInterest = Some {
        Sports = set [Football; Tenis]
        Music = [Rock];
        Reading = [Poetry]
    }
}

[<TestFixture>]
type ``multiply polynomial by number tests``() = 

    [<Test>]
    member test.``Same sex clients should not be matched`` () = 

        let client2 =  {
            Id=12;
            Name = "Milan";
            Sex = client1.Sex;
            TelephoneNum = Some "";
            YearOfBirth = client1.YearOfBirth - 5;
            ThemesOfInterest = client1.ThemesOfInterest            
        }

        areMatching client1 client2 |> shouldEqual false

    [<Test>]
    member test.``Clients with age difference more than 10 years should not be matched`` () = 

        let client2 =  {
            Id=12;
            Name = "Milana";
            Sex = Female;
            TelephoneNum = Some "";
            YearOfBirth = client1.YearOfBirth - 11;
            ThemesOfInterest = client1.ThemesOfInterest
        }

        areMatching client1 client2 |> shouldEqual false

    [<Test>]
    member test.``Clients with no common interests should not be matched`` () = 

        let client2 =  {
            Id=12;
            Name = "Milana";
            Sex = Female;
            TelephoneNum = Some "";
            YearOfBirth = client1.YearOfBirth - 5;
            ThemesOfInterest = Some {
                Sports =set [Baseball]
                Music = [Jazz];
                Reading = [Magazines]
            }
        }

        areMatching client1 client2 |> shouldEqual false

    [<Test>]
    member test.``Clients with different sex, with age difference less than 10 years and common sports interests should be matched`` () = 

        let client2 =  {
            Id=12;
            Name = "Milana";
            Sex = Female;
            TelephoneNum = Some "";
            YearOfBirth = client1.YearOfBirth - 5;
            ThemesOfInterest = Some {
                Sports =set [Football]
                Music = [HipHop];
                Reading = [Novels] 
            }
        }

        areMatching client1 client2 |> shouldEqual true

    [<Test>]
    member test.``Clients with different sex, with age difference less than 10 years and common music interests should be matched`` () = 

        let client2 =  {
            Id=12;
            Name = "Milana";
            Sex = Female;
            TelephoneNum = Some "";
            YearOfBirth = client1.YearOfBirth - 5;
            ThemesOfInterest = Some {
                Sports =set [Baseball]
                Music = [Rock];
                Reading = [Novels] 
            }
        }

        areMatching client1 client2 |> shouldEqual true

    [<Test>]
    member test.``Clients with different sex, with age difference less than 10 years and common reading interests should be matched`` () = 

        let client2 =  {
            Id=12;
            Name = "Milana";
            Sex = Female;
            TelephoneNum = Some "";
            YearOfBirth = client1.YearOfBirth - 5;
            ThemesOfInterest = Some {
                Sports = set [Baseball]
                Music = [Jazz];
                Reading = [Poetry] 
            }
        }

        areMatching client1 client2 |> shouldEqual true