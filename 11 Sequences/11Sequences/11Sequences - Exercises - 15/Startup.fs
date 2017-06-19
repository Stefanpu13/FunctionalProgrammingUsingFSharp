namespace Exercises15

module Startup = 
    open System.Configuration
    open System.Data
    open System.Data.SqlClient

    let connString = @"Data Source=.;
        Initial Catalog=Register;
        Integrated Security=True"
    let conn = new SqlConnection(connString)
    conn.Open()
    let execNonQuery conn s =
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteNonQuery() |> ignore
        
    let executeScalar conn s = 
            let comm = new SqlCommand(s, conn, CommandTimeout = 10)
            comm.ExecuteScalar () 
    let execRegisterQuery = execNonQuery conn    
    let init ()= 

        let ifRegisterExists = "
            IF (EXISTS (SELECT * 
                    FROM INFORMATION_SCHEMA.TABLES 
                    WHERE TABLE_SCHEMA = 'dbo' 
            		AND TABLE_CATALOG ='Register'
                    AND  TABLE_NAME = 'Register'))

                Select 1
            ELSE
                Select 0
            "
        let tableExists = ((executeScalar conn ifRegisterExists) :?> int) = 1

        (*If table does not exist, create it*)
        execRegisterQuery "CREATE TABLE Register (
            PartId int NOT NULL,
            PartName varchar(50) NOT NULL,
            IsBasic bit NOT NULL,
            PRIMARY KEY (PartId))"


    let connString1 = @"Data Source=.;
    Initial Catalog=ProductRegister;
    Integrated Security=True"
    let conn1 = new SqlConnection(connString1)
    conn1.Open()
    // let execNonQuery conn s =
    //     let comm = new SqlCommand(s, conn, CommandTimeout = 10)
    //     comm.ExecuteNonQuery() |> ignore

    execNonQuery conn1 "CREATE TABLE Part (
    PartId int NOT NULL,
    PartName varchar(50) NOT NULL,
    IsBasic bit NOT NULL,
    PRIMARY KEY (PartId))"
    execNonQuery conn1 "CREATE TABLE PartsList (
    PartsListId int NOT NULL,
    PartId int NOT NULL,
    Quantity int NOT NULL,
    PRIMARY KEY (PartsListId, PartId))"