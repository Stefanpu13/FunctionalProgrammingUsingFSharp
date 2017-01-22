// 4.6 Examples. A model-based approach

// Example: Cash register
type ArticleCode = string
type ArticleName = string

type Price = int
type Register = (ArticleCode * (ArticleName*Price)) list

type NoPieces = int // np where np >= 0
type Item = NoPieces * ArticleCode
type Purchase = Item list

type Info = NoPieces * ArticleName * Price
type Infoseq = Info list
type Bill = Infoseq * Price

let reg = [
    ("a1",("cheese",25));
    ("a2",("herring",4));
    ("a3",("soft drink",5)) ]

let pur = [(3,"a2"); (1,"a1")]

let rec findArticle ac = function
    | (ac',adesc)::_ when ac=ac' -> adesc
    | _::reg -> findArticle ac reg
    | _ ->
        failwith(ac + " is an unknown article code")

let rec makeBill reg = function
    | [] -> ([],0)
    | (np,ac)::pur -> 
        let (aname,aprice) = findArticle ac reg
        let tprice = np*aprice
        let (billtl,sumtl) = makeBill reg pur
        ((np,aname,tprice)::billtl,tprice+sumtl)


// Example: Map colouring

type Country = string
type Map = (Country * Country) list
type Colour = Country list
type Colouring = Colour list

let exMap = [("a", "b"); ("c",  "d"); ("d","a")]

let rec isMember x = function
    | y::ys -> x=y || (isMember x ys)
    | [] -> false

let areNb m c1 c2 =
    isMember (c1,c2) m || isMember (c2,c1) m

let rec canBeExtBy m col c =
    match col with
    | [] -> true
    | c'::col' -> not(areNb m c' c) && canBeExtBy m col' c

let rec extColouring m cols c =
    match cols with
    | [] -> [[c]]
    | col::cols' -> 
        if canBeExtBy m col c
        then (c::col)::cols'
        else col::extColouring m cols' c

let addElem x ys = if isMember x ys then ys else x::ys
let rec countries = function
    | [] -> []
    | (c1,c2)::m -> addElem c1 (addElem c2 (countries m))

countries exMap
let rec colCntrs m = function
    | [] -> []
    | c::cs -> extColouring m (colCntrs m cs) c

let colMap m = colCntrs m (countries m)

colMap exMap

