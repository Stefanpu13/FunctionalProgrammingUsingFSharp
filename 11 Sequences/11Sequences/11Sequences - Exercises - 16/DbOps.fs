namespace Exercise16

open System.Data.SqlClient

module DbOps = 
    let  connString = @"Data Source=.;
        Initial Catalog=DatingBureau;
        Integrated Security=True";

    let execNonQuery s =
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteNonQuery() |> ignore

    let execQuery s =
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

        
    let executeScalar s = 
        use conn  = new SqlConnection (connString)
        conn.Open()
        let comm = new SqlCommand(s, conn, CommandTimeout = 10)
        comm.ExecuteScalar ()     
        
    // let init () =         
    let ifTableExsitsQuery dbName tableName = 
        "
        IF (EXISTS (SELECT * 
                FROM INFORMATION_SCHEMA.TABLES 
                WHERE TABLE_SCHEMA = 'dbo' 
        		AND TABLE_CATALOG ='" + dbName + "'
                AND  TABLE_NAME = '" + tableName + "'))
            Select 1
        ELSE
            Select 0
        "
    let tableExists ifTableExsitsQuery = ((executeScalar ifTableExsitsQuery) :?> int) = 1   

    let createTables () =         
        let createInterestCategoryTable () = 
            let ifInterestTypeTableExists = ifTableExsitsQuery "DatingBureau" "InterestCategory"

            if not (tableExists ifInterestTypeTableExists)
            then
            (*If table does not exist, create it*)
                execNonQuery "
                    CREATE TABLE InterestCategory (
                        InterestCategoryId int primary key IDENTITY(1,1) NOT NULL,                            
                        Name varchar(50) NOT NULL
                        )                            
                    "
        
        let createInterestTable () = 
            let ifInterestTableExists = ifTableExsitsQuery "DatingBureau" "Interest"

            if not (tableExists ifInterestTableExists)
            then
            (*If table does not exist, create it*)
                execNonQuery "
                    CREATE TABLE Interest (
                        InterestId int primary key IDENTITY(1,1) NOT NULL,                            
                        Name varchar(50) NOT NULL,             
                        InterestCategoryId int FOREIGN KEY REFERENCES InterestCategory(InterestCategoryId) NOT NULL
                        )
                    "        

        let createSexTable () = 
            let ifSexTableExists = ifTableExsitsQuery "DatingBureau" "Sex"

            if not (tableExists ifSexTableExists)
            then
            (*If table does not exist, create it*)
                execNonQuery "
                    CREATE TABLE Sex (
                        SexId int primary key IDENTITY(1,1) NOT NULL,
                        Name varchar(50) NOT NULL
                        )
                    "

            execNonQuery "
                INSERT INTO Sex 
                VALUES 
                    ('Male'),
                    ('Female'),
                    ('Other');
            "
        let createClientTable () = 
            let ifRegisterTableExists = ifTableExsitsQuery "DatingBureau" "Client"

            if not (tableExists ifRegisterTableExists)
            then
            (*If table does not exist, create it*)
                execNonQuery "
                    CREATE TABLE Client (
                        ClientId int primary key IDENTITY(1,1) NOT NULL,
                        Name varchar(50) NOT NULL,
                        TelephoneNum varchar(50) NULL,
                        SexId int FOREIGN KEY REFERENCES Sex(SexId) NOT NULL,
                        YearOfBirth int NOT NULL
                        )
                    "

        let createClientInterestTable () = 
            let ifClientInterestTableExists = ifTableExsitsQuery "DatingBureau" "ClientInterest"

            if not (tableExists ifClientInterestTableExists)
            then
            (*If table does not exist, create it*)
                execNonQuery "
                    CREATE TABLE ClientInterest (
                        ClientInterestId int primary key IDENTITY(1,1) NOT NULL,                        
                        ClientId int FOREIGN KEY REFERENCES Client(ClientId) NOT NULL,
                        InterestId int FOREIGN KEY REFERENCES Interest(InterestId) NOT NULL,
                        )
                    "                  
                
        createInterestCategoryTable ()             
        createInterestTable ()
        createSexTable ()
        createClientTable ()
        createClientInterestTable ()
    // createTables()





