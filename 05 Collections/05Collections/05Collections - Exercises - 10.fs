namespace Exercises10
open Exercises9.E
open Exercise9.Types.T
// #nowarn
module E = 

    (* 5.10
        Extend the cash register example to take discounts for certain articles into account. For example,
        find a suitable representation of discounts and revise the function to make a bill accordingly.
    *)    
    // Register is visible single case union constructor, while Purchase is not
    let makeBill3 (Register reg) (pur:Purchase.Purchase) =
        let f ac (NoPieces np) (Bill (Infoseq infos, Price billprice)) =
            let (aname, Price aprice, discount) = Map.find ac reg
            let tprice = 
                match discount with
                | Some d->
                    let dValue = Discount.value d 
                    np * int(round((1.0 - dValue) * (float aprice)))
                | None -> np * aprice

            Bill ((Infoseq (Info (NoPieces np, aname, Price tprice):: infos)), Price (tprice+billprice))
        Map.foldBack f (Purchase.value pur) (Bill (Infoseq [], Price 0))