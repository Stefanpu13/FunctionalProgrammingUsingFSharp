namespace Exercises9
module E = 
    open System

    (* 6.9
        A company consists of departments with sub-departments, which again can have sub-departments,
        and so on. (The company can also be considered as a department.)
    *)


    (* 6.9.1
        Assume that each department has a name and a (possibly empty) list of sub-departments.
        Declare an F# type Department. 
    

       6.9.2
        Extend this type so that each department has its own gross income
    *)

    type Name = Name of string
    type GrossIncome = GrossIncome of float
    type Department = Department of (Name * GrossIncome * Department list)
    
    (* 6.9.3
        Declare a function to extract a list of pairs (department name, gross income), for all departments.
    *)

    let allDepartments (Department department) = 
        let rec allDeprartments namesAndIncomes = function
            | Department (Name n, GrossIncome gi, []) -> (Name n, GrossIncome gi)::namesAndIncomes
            | Department (Name n, GrossIncome gi, deps) -> 
                List.fold allDeprartments ((Name n, GrossIncome gi)::namesAndIncomes) deps
        
        allDeprartments [] (Department department)

    (* 6.9.4
        Declare a function to extract the total income for a given department by adding up its gross
        income, including the income of its sub-departments.
    *)

    let totalIncome = 
        allDepartments 
            >> List.sumBy (fun (Name n, GrossIncome gi) -> gi) 
            >> GrossIncome

    (* 6.9.5
        Declare a function to extract a list of pairs (department name, total income) for all departments.
    *)

    let allDepartmentsTotalIncome (Department department) = 
        let rec totalForSubDepts namesAndTotalIncomes = function
            | Department (Name n, GrossIncome gi, []) -> 
                (Name n, GrossIncome (gi))::namesAndTotalIncomes
            | Department (Name n, GrossIncome gi, deps) as d ->
                let subdepsNamesAndTotalIncomes = List.fold totalForSubDepts namesAndTotalIncomes deps                
                (Name n, totalIncome d)::subdepsNamesAndTotalIncomes
        
        totalForSubDepts [] (Department department)

    (* 6.9.6
        Declare a function format of type Department -> string, which can be used to get a
        textual form of a department such that names of sub-departments will occur suitably indented
        (e.g., with four spaces) on separate lines. (Use printf to print out the result. Do not use
        printf in the declaration of format.)
    *)

    let format d = 
        let rec format indentation formattedDeps = function
            | Department (Name n, _, []) -> formattedDeps + indentation + n + Environment.NewLine
            | Department (Name n, _ , subdeps) ->
                let subdepsIndentation = indentation + " " + " "
                let formattedDep = formattedDeps + indentation + n + Environment.NewLine
                List.fold (fun toString d -> format subdepsIndentation toString d) formattedDep subdeps

        format "" Environment.NewLine d
