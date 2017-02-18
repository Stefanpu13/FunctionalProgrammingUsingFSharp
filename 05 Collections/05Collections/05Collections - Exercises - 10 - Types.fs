namespace Types
// http://stackoverflow.com/questions/3740566/f-suppress-warnings see accepted answer
#nowarn "1"
module T = 
    module Discount =         
        [<CompilerMessageAttribute ("Do not create discounts using the \"Discount\" constructor.
        Validation will be skipped.", 1)>] 
        type D = Discount of float
        let create d = 
            if d < 0.0 || d > 0.99
            then None
            else Some (Discount d)
        let apply f (Discount s) = f s
        let value s = apply id s

    type ArticleCode = ArticleCode of string
    type ArticleName = ArticleName of string
    type NoPieces = NoPieces of int
    type Price = Price of int    
    type Register = Register of Map<ArticleCode, ArticleName * Price * Discount.D option>
    type Purchase = Purchase of Map<ArticleCode, NoPieces>
    type Info = Info of NoPieces * ArticleName * Price
    type Infoseq = Infoseq of Info list
    type Bill = Bill of Infoseq * Price

