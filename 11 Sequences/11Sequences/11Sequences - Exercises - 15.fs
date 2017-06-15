namespace Exercises15

#if INTERACTIVE
#load "11Sequences - Exercises - 1 - 6.fs"
#load "11Sequences - Exercises - 10.fs"
#endif

open Exercises1to6.E
open Exercises10.E

module E =     
    (* 11.12
        Give a database-based solution to the cash-register example introduced in Section 4.6.
    *)

    type ArticleCode = string
    type ArticleName =  string

    type Price = int

    type Register = Register of (ArticleCode * (ArticleName*Price)) list   

    type NoPieces = int
    type Item = Item of (NoPieces * ArticleCode)
    type Purchase = Purchase of Item list

    type Info = Info of NoPieces * ArticleName * Price
    type Infoseq = Infoseq of Info list
    type Bill = Bill of Infoseq * Price

    let reg = Register [
        ( "a1",( "cheese",  25));
        (  "a2",(  "herring",  4));
        ( "a3",( "soft drink", 5)) 
        ]

    let rec findArticle ac = function
    | Register ((ac',adesc)::_) when ac=ac' -> adesc
    | Register (_::reg) -> findArticle ac (Register reg)
    | _ ->

        failwith(ac + " is an unknown article code")

        
    let rec makeBill reg = function
    | Purchase [] -> ([],0)
    | Purchase (Item (np, ac)::pur) -> 
        let ( aname, aprice) = findArticle ac reg
        let tprice = np*aprice
        let (billtl,sumtl) = makeBill reg (Purchase pur)
        ((np,aname,tprice)::billtl,tprice+sumtl)

    findArticle ("") reg
