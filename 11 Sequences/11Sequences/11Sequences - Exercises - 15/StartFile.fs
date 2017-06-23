namespace Exercises15

open System.Data.SqlClient

module Startup = 
    let private connString = @"Data Source=.;
        Initial Catalog=Register;
        Integrated Security=True";


    let private execNonQuery s =
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteNonQuery() |> ignore

    let private execQuery s =
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        use reader = comm.ExecuteReader() 
        (seq{
            while reader.Read() do
                let row = seq {
                    for i in 0..reader.FieldCount - 1 do
                        yield reader.GetName(i), reader.GetValue(i)
                }
                yield row |> List.ofSeq
        }) |> List.ofSeq

        
    let private executeScalar s = 
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteScalar ()     
        
    let init ()=         
        let ifTableExsitsQuery tableName = "
                IF (EXISTS (SELECT * 
                        FROM INFORMATION_SCHEMA.TABLES 
                        WHERE TABLE_SCHEMA = 'dbo' 
                		AND TABLE_CATALOG ='Register'
                        AND  TABLE_NAME = '" + tableName + "'))

                    Select 1
                ELSE
                    Select 0
                "
        let tableExists ifTableExsitsQuery = ((executeScalar ifTableExsitsQuery) :?> int) = 1            

        let createRegisterTable () = 
            let ifRegisterTableExists = ifTableExsitsQuery "Register"

            if not (tableExists ifRegisterTableExists)
            then
            (*If table does not exist, create it*)
                execNonQuery "
                    CREATE TABLE Register (
                        ID int primary key IDENTITY(1,1) NOT NULL,
                        ArticleCode varchar(50) NOT NULL,
                        ArticleName varchar(50) NOT NULL,             
                        Price int NOT NULL)
                    "
        createRegisterTable ()


    init ()


