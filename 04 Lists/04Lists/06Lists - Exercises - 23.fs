
#load "01Helpers.fs"
#load "061Types - Exercises - 23.fs"
#load "062ClientHelpers - Exercises - 23.fs"
#load "063ClientsRepo - Exercises - 23.fs"
#load "064DaringBureauAPI - Exercises - 23.fs"

open Helpers.L
open Types.T
open ClientHelpers.H
open ClientsRepo
open DatingBureau.API


(* 4.23
    A dating bureau has a file containing name, telephone number, sex, year of birth and themes
    of interest for each client. You may make a request to the bureau stating your own sex, year
    of birth and themes of interest and get a response listing all matching clients, that is, clients
    with different sex, a deviation in age less than 10 years and with at least one common theme
    of interest. The problem is to construct a program for generating the responses from the dating
    bureau.

    Addition Reqs:
        1. User can have no themes of Interests or have any number of Interests; 
        2. Each interest is a Theme and can have several subThemes:
            For example: Theme "Sport" with subthemes "Football" and "Tenis"  
            or
            be empty        
        3. Adding new theme/subtheme should break existing code or be automatically handled
        5. Common themes of interest (if any) are returned as result of comparison? (not necessary)
*)
let getMatchingClients client clients = clients |> filter (areMatching client)
let partitionMatchingAndUnnachingClients client clients =  clients |> partition (areMatching client)

///----------------------
let clientsWithRandomInterests = File.all()
let (firstClient::restClients) = clientsWithRandomInterests  
let matchingClients = getMatchingClients firstClient restClients

let matching, unmatching = partitionMatchingAndUnnachingClients firstClient restClients

firstClient
matching
unmatching