namespace Exercise16

open System.Data.SqlClient

module DbOps = 

    let private connString = @"Data Source=.;        
        Initial Catalog=DatingBeruau;
        Integrated Security=True"

    let private execNonQuery connString s =
        use conn  = new SqlConnection (connString)        
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteNonQuery() |> ignore           


    
    let deleteAllRecords tableName = 
        let deleteTableContents = "
            Truncate table Register.[dbo]." + tableName
        execNonQuery connString deleteTableContents

