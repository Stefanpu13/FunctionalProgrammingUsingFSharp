namespace Exercises8
module E = 
    (* 5.8
        Give declarations for makeBill3 using map.fold rather than map.foldBack.
    *)
    type ArticleCode =ArticleCode of string
    type ArticleName = ArticleName of string
    type NoPieces = NoPieces of int
    type Price = Price of int
    type Register = Register of Map<ArticleCode, ArticleName*Price>
    type Purchase = Purchase of Map<ArticleCode,NoPieces>
    type Info =  Info of NoPieces * ArticleName * Price
    type Infoseq = Infoseq of Info list
    type Bill = Bill of Infoseq * Price

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