namespace Exercises15

open Models

#if INTERACTIVE
#r "FSharp.Data.TypeProviders"
#endif
open Microsoft.FSharp.Data.TypeProviders

open System
open Microsoft.FSharp.Linq

module Repository = 
    type DbSchema = SqlDataConnection<"Data Source=.;
        Initial Catalog=Register;
        Integrated Security=True">

    let mutable private db = DbSchema.GetDataContext()  
    let reInit () =
        db.DataContext.Dispose()
        db <- DbSchema.GetDataContext()  

    let private submit () = 
        try            
            db.DataContext.SubmitChanges()            
        with
        | exn -> 
            raise exn

    let findArticle ac =     
        try    
            let article =  query {
                for row in db.Register1 do
                    find (row.ArticleCode=ac) 
            } 

            Some (article.ArticleCode, (article.ArticleName, Price article.Price))
            with 
            | :? InvalidOperationException -> None
    
    let addArticle (ac, (an, (Price p))) =        
        match findArticle ac with
        | None ->
            let newArticle = 
                DbSchema.ServiceTypes.Register1(
                    ArticleCode = ac,
                    ArticleName = an,
                    Price = p
            )
            
            db.Register1.InsertOnSubmit(newArticle)
            submit()
        | Some _ ->         
            ()
    
    let getRegister () =
        
        let articles = query {
            for row in db.Register1 do
                select row
        }

        let register =  articles |> Seq.map (fun r ->
            (r.ArticleCode, (r.ArticleName, (Price r.Price)))) |> List.ofSeq |> Register

        register      
    