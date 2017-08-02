namespace Exercises5
module Quest = 
    
    type Country = string

    type BinTree<'a> = 
        |Leaf of Country
        | Node of BinTree<'a> * 'a * BinTree<'a>

    let getQuestTree () = 
        Node( 
            Node(
                Leaf "Cyprus",
                "Is this country an island?",
                Leaf "France"
            ),
            "Is this country in EU?",
            Node(
                Leaf "Turkey",
                "Is this country a muslim coountry?",
                Leaf "Russia"
            )
        )
