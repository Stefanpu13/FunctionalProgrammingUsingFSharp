namespace Exercises10
open Types.T
// #nowarn
module E = 

    (* 5.10
        Extend the cash register example to take discounts for certain articles into account. For example,
        find a suitable representation of discounts and revise the function to make a bill accordingly.
    *)

    let x = Disount.create 45.0

    let xv = match x with
        |None -> 0.0 
        |Some xx -> Disount.value xx
    let x2 = Disount.Discount 0.8
    // let 
    // let x = Disount.create 5.0 
    type ArticleCode = ArticleCode of string
    type ArticleName = ArticleName of string
    type NoPieces = NoPieces of int
    type Price = Price of int
    type Discount = Discount of float
    type Register = Register of Map<ArticleCode, ArticleName*Price*Discount option>

    let makeBill3 reg pur =
        let f ac np (infos, billprice) =
            let (aname, aprice, discount) = Map.find ac reg
            let tprice = 
                match discount with
            // int(round(0.85*(float p) for 15% discount
                | Some d -> np * int(round((1.0 - d) * (float aprice)))
                | None -> np * aprice

            (np,aname, tprice)::infos, tprice+billprice
        Map.foldBack f pur ([],0)