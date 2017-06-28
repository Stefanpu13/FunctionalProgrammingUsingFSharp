namespace Exercises15

#if INTERACTIVE
#load "./Models.fs"
#load "./Repository.fs"
#endif


open Models
open Repository

module E =     
    (* 11.12
        Give a database-based solution to the cash-register example introduced in Section 4.6.
    *)

    let makeBill purchase =
        // Would be more efficient to get whole register and "findArticle" in-memory,
        // but for the sake of practice I prefer searching with the repository method
        let rec makeBill = function
        | Purchase [] -> Bill ([],Price 0)
        | Purchase (Item (NoPieces np, ac)::pur) -> 
            match Repository.findArticle ac with
            | Some (ac, ( aname, Price aprice)) ->
                let tprice = (np*aprice)
                let (Bill (billtl,Price sumtl)) = makeBill (Purchase pur)
                Bill ((NoPieces np,aname,Price tprice)::billtl,Price (tprice+sumtl))
            | None ->                
                makeBill (Purchase pur)

        makeBill purchase    
