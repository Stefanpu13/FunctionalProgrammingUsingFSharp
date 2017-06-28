namespace Exercises15

open System.Data.SqlClient

module Startup = 
    let private connString = @"Data Source=.;        
        Initial Catalog=Register;
        Integrated Security=True"

    let private execNonQuery connString s =
        use conn  = new SqlConnection (connString)        
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteNonQuery() |> ignore
        
    let private executeScalar connString s = 
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteScalar ()     

    let deleteAllRecords tableName = 
        let deleteTableContents = "
            Truncate table Register.[dbo]." + tableName
        execNonQuery connString deleteTableContents
