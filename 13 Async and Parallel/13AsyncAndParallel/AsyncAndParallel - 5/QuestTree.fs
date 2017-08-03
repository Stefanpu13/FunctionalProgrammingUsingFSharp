namespace Exercises5
module Quest = 
    
    type Country = string

    type BinTree<'a> = 
        |Leaf of Country
        | Node of BinTree<'a> * 'a * BinTree<'a>


    // build tree from questions, associated with european countries
    (*
        From given dataset containg questions build a valid bin tree to play the game
        What is a valid tree?
        A tree where each leave is uniquely determined
        1. Each Question has 2 lists - countries with answer 'yes' and countries with answer 'no'
        2.  
    *)
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
