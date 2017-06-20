namespace Exercises15
// open System.Data

module Startup = 
    open System.Configuration
    open System.Data
    open System.Data.SqlClient

    // open Repository
    
    let connString = @"Data Source=.;
        Initial Catalog=Register;
        Integrated Security=True"

    let execNonQuery s =
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteNonQuery() |> ignore
        
    let executeScalar s = 
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteScalar ()     
    let init ()= 

        let ifRegisterTableExists = "
            IF (EXISTS (SELECT * 
                    FROM INFORMATION_SCHEMA.TABLES 
                    WHERE TABLE_SCHEMA = 'dbo' 
            		AND TABLE_CATALOG ='Register'
                    AND  TABLE_NAME = 'Register'))

                Select 1
            ELSE
                Select 0
            "
        let tableExists = ((executeScalar ifRegisterTableExists) :?> int) = 1

        if not tableExists
        then
        (*If table does not exist, create it*)
            execNonQuery "
                CREATE TABLE Register (
                    ID int primary key IDENTITY(1,1) NOT NULL,
                    ArticleCode varchar(50) NOT NULL,
                    ArticleName varchar(50) NOT NULL,             
                    Price int NOT NULL)
                "


    // let connString1 = @"Data Source=.;
    // Initial Catalog=ProductRegister;
    // Integrated Security=True"
    // let conn1 = new SqlConnection(connString1)
    // conn1.Open()
    // // let execNonQuery conn s =
    // //     let comm = new SqlCommand(s, conn, CommandTimeout = 10)
    // //     comm.ExecuteNonQuery() |> ignore

    // execNonQuery conn1 "CREATE TABLE Part (
    // PartId int NOT NULL,
    // PartName varchar(50) NOT NULL,
    // IsBasic bit NOT NULL,
    // PRIMARY KEY (PartId))"
    // execNonQuery conn1 "CREATE TABLE PartsList (
    // PartsListId int NOT NULL,
    // PartId int NOT NULL,
    // Quantity int NOT NULL,
    // PRIMARY KEY (PartsListId, PartId))"

    init()
    // conn

// Startup.init()