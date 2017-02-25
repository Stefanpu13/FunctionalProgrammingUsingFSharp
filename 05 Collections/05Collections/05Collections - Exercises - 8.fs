namespace Exercises8
module E = 
    (* 5.8
        Give declarations for makeBill3 using map.fold rather than map.foldBack.
    *)

    let makeBill3FoldBack  reg pur =
        let f ac np (infos, billprice) =
            let (aname, aprice) = Map.find ac reg
            let tprice = np*aprice
            (np,aname, tprice)::infos, tprice+billprice
        Map.foldBack f pur ([],0)

    let makeBill3Fold reg pur =
        let f (infos,billprice) ac np =
            let (aname, aprice) = Map.find ac reg
            let tprice = np*aprice
            ((np,aname,tprice)::infos, tprice+billprice)
        Map.fold f ([],0) pur 
          