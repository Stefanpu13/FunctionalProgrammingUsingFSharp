namespace Exercises15

open Models

#if INTERACTIVE
#r "FSharp.Data.TypeProviders"
#endif
open Microsoft.FSharp.Data.TypeProviders

open Microsoft.FSharp.Linq


module Repository = 
    type DbSchema = SqlDataConnection<"Data Source=.;
        Initial Catalog=Register;
        Integrated Security=True">

    let db = DbSchema.GetDataContext()    

    let rec findArticle ac = function
    | Register ((ac',adesc)::_) when ac=ac' -> adesc
    | Register (_::reg) -> findArticle ac (Register reg)
    | _ ->
        failwith(ac + " is an unknown article code")

    let getRegister () =
        let articles = query {
            for row in db.Register1 do
                select row
        }

        let register =  articles |> Seq.map (fun r ->
            (r.ArticleCode, (r.ArticleName, (Price r.Price)))) |> List.ofSeq |> Register

        register 
        
    // let findArticle2 ac =  query {
    //     for row in db.Register1 do
    //         find (row.ArticleCode = ac)
    //    }        
    
    // findArticle2 "21"  