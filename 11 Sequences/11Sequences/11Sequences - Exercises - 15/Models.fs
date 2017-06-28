namespace Exercises15

module Models = 
    
    type ArticleCode = string
    type ArticleName =  string

    type Price =Price of int

    type Register = Register of (ArticleCode * (ArticleName*Price)) list   

    type NoPieces = NoPieces of int
    type Item = Item of (NoPieces * ArticleCode)
    type Purchase = Purchase of Item list

    type Info =  NoPieces * ArticleName * Price
    type Infoseq =  Info list
    type Bill = Bill of Infoseq * Price
