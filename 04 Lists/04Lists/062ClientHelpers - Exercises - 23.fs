
namespace ClientHelpers 

open Microsoft.FSharp.Reflection
open Types.T

module H = 
    let private numberOfInterestsCount = System.Random()
    let private interestRandomIndex = System.Random()
    let private sportCases = FSharpType.GetUnionCases typeof<Sport>
    let private musicCases = FSharpType.GetUnionCases typeof<Music>
    let private readingCases = FSharpType.GetUnionCases typeof<Reading>

    let private randomInterests<'T> (avaialableInterests:UnionCaseInfo []) =
        seq {
            for n in 1..numberOfInterestsCount.Next(3) do
                let randomInterest = avaialableInterests.[interestRandomIndex.Next(avaialableInterests.Length)]     
                yield FSharpValue.MakeUnion(randomInterest, [||]) :?> 'T
        }

    let private getClientWithSomeInterests cl = 
        match cl.ThemesOfInterest with
        | Some i -> cl
        | None -> {cl with ThemesOfInterest = Some {Sports = []; Music = []; Reading = [];}}

    let private clientWithGeneratedSportsInterests interests client  =
        let clientWithSomeInterests = getClientWithSomeInterests client
        {clientWithSomeInterests with ThemesOfInterest = Some { clientWithSomeInterests.ThemesOfInterest.Value with Sports = interests;}}

    let private clientWithGeneratedMusicInterests interests client  =
        let clientWithSomeInterests = getClientWithSomeInterests client
        {clientWithSomeInterests with ThemesOfInterest = Some { clientWithSomeInterests.ThemesOfInterest.Value with Music = interests;}}
        
    let private clientWithGeneratedReadingInterests interests client  =
        let clientWithSomeInterests = getClientWithSomeInterests client
        {clientWithSomeInterests with ThemesOfInterest = Some { clientWithSomeInterests.ThemesOfInterest.Value with Reading = interests;}}

    let private toUniqueSortedList l = (List.ofSeq >> Helpers.L.unique >> Helpers.L.sort) l

    let generateRandomInterests client = 
        let addSportInterests  = randomInterests<Sport> >> toUniqueSortedList >> clientWithGeneratedSportsInterests
        let addMusicInterests = randomInterests<Music> >> toUniqueSortedList >> clientWithGeneratedMusicInterests 
        let addReadingInterests = randomInterests<Reading> >> toUniqueSortedList >> clientWithGeneratedReadingInterests

        client 
            |> addSportInterests sportCases                        
            |> addMusicInterests musicCases
            |> addReadingInterests readingCases

    // let generateRandomInterestsForClients clients  =  Helpers.L.map generateRandomInterests clients 

