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

    // let reg = 
    //     Register [
    //         ( "a1",( "cheese", Price 25));
    //         (  "a2",(  "herring", Price 4));
    //         ( "a3",( "soft drink",Price 5)) 
    //     ]    

    let rec private findArticle ac = function
    | Register ((ac',adesc)::_) when ac=ac' -> adesc
    | Register (_::reg) -> findArticle ac (Register reg)
    | _ -> failwith(ac + " is an unknown article code")

    let private findArticle2 ac (Register reg) =
        match List.tryFind (fun (ac', _ ) -> ac=ac') reg with
        | Some (_,adesc) -> adesc
        |None -> failwith(ac + " is an unknown article code")

    
    let makeBill purchase =
        let reg = Repository.getRegister ()

        let rec makeBill reg = function

        | Purchase [] -> Bill ([],Price 0)
        | Purchase (Item (NoPieces np, ac)::pur) -> 
            let ( aname, Price aprice) = findArticle ac reg
            let tprice = (np*aprice)
            let (Bill (billtl,Price sumtl)) = makeBill reg (Purchase pur)
            Bill ((NoPieces np,aname,Price tprice)::billtl,Price (tprice+sumtl))

        makeBill reg purchase    
