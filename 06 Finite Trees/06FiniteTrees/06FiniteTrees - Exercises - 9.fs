namespace Exercises9
module E = 

    (* 6.9
        A company consists of departments with sub-departments, which again can have sub-departments,
        and so on. (The company can also be considered as a department.)
    *)


    (* 6.9.1
        Assume that each department has a name and a (possibly empty) list of sub-departments.
        Declare an F# type Department. 
    *)
    (* 6.9.2
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

    let allDepartments2 (Department department) = 
        let rec allDepartments2 departments department = 
            match department with
            | Department (Name n, GrossIncome gi, []) -> (Name n, GrossIncome gi)::departments
            | Department (Name n, GrossIncome gi, d::deps) -> 
                // List.fold allDeprartments ((Name n, GrossIncome gi)::departments) deps
                allDepartments2 (List.fold allDepartments2 ((Name n, GrossIncome gi)::departments) deps) d
                // allDepartments ((Name n, GrossIncome gi)::departments) d (fun deps -> List.fold cont  ) 
        
        allDepartments2 [] (Department department) 

    let dept = Department (Name "Base", GrossIncome 12.0, [])
    let dept2 = Department (Name "Base1", GrossIncome 22.0, [dept]) 
    allDepartments2 dept2

    let deepDept = 
        [1.0..3.0] 
            |> List.fold (fun dept num -> Department (Name ("Base" + num.ToString()), GrossIncome num, [dept; ])) dept
    allDepartments deepDept |> List.length
    (* 6.9.4
        Declare a function to extract the total income for a given department by adding up its gross
        income, including the income of its sub-departments.
    *)

    let totalIncome = 
        allDepartments 
            >> List.sumBy (fun (Name n, GrossIncome gi) -> gi) 
            >> GrossIncome

    totalIncome deepDept

    (* 6.9.5
        Declare a function to extract a list of pairs (department name, total income) for all departments.
    *)

    let allDepartmentsTotalIncome1 (Department department) = 
        let rec totalForSubDepts namesAndTotalIncomes = function
            | Department (Name n, GrossIncome gi, []) -> 
                (Name n, GrossIncome (gi))::namesAndTotalIncomes
            | Department (Name n, GrossIncome gi, deps) as d ->
                let subdepsNamesAndTotalIncomes = 
                    List.fold (fun namesAndIncomes (Department (Name sn, income, _) as sd) -> 
                        totalForSubDepts namesAndIncomes sd
                    ) namesAndTotalIncomes deps
                
                (Name n, totalIncome d)::subdepsNamesAndTotalIncomes
        
        totalForSubDepts [] (Department department)

    let allDepartmentsTotalIncome (Department department) = 
        let rec totalForSubDepts namesAndTotalIncomes deps =
            List.fold (
                fun (GrossIncome previousDeptsTotal, namesAndIncomes) d ->
                    let (GrossIncome deptTotal, subDeptsIncomeAndNames) = 
                        totalForDepartment (GrossIncome previousDeptsTotal, namesAndIncomes) d

                    (GrossIncome (deptTotal+previousDeptsTotal), subDeptsIncomeAndNames)
                ) (GrossIncome 0.0, namesAndTotalIncomes) deps 
        and totalForDepartment (GrossIncome totalIncome, namesAndTotalIncomes) = function
            | Department (Name n, GrossIncome gi, []) -> 
                (GrossIncome(totalIncome + gi), (Name n, GrossIncome (gi))::namesAndTotalIncomes) 
            | Department (Name n, GrossIncome gi, deps) ->
                let (GrossIncome subDeptsTotal, namesAndTotalIncomesForSub) = totalForSubDepts namesAndTotalIncomes deps
                (GrossIncome (subDeptsTotal + gi), ((Name n, GrossIncome (subDeptsTotal + gi))::namesAndTotalIncomesForSub))
        
        let (GrossIncome total, namesAndTotalIncomes) =  totalForDepartment (GrossIncome 0.0, []) (Department department)
        namesAndTotalIncomes

    let dept3  = Department (Name "Base3", GrossIncome 10.0, [Department (Name "Base2", GrossIncome 9.0, []); dept2])
    allDepartmentsTotalIncome deepDept
    allDepartmentsTotalIncome dept2
    allDepartmentsTotalIncome dept3

    allDepartmentsTotalIncome1 deepDept
    allDepartmentsTotalIncome1 dept2
    allDepartmentsTotalIncome1 dept3

    (*
        Declare a function format of type Department -> string, which can be used to get a
        textual form of a department such that names of sub-departments will occur suitably indented
        (e.g., with four spaces) on separate lines. (Use printf to print out the result. Do not use
        printf in the declaration of format.)
    *)

    let format d = 
        let rec format indentation formattedDeps = function
            | Department (Name n, _, []) -> formattedDeps + indentation + n + "\n"
            | Department (Name n, _ , subdeps) ->
                let subdepsIndentation = indentation + " " + " "
                let formattedDep = formattedDeps + indentation + n + "\n"
                List.fold (fun toString d -> format subdepsIndentation toString d) formattedDep subdeps

        format "" "\n" d

    format deepDept