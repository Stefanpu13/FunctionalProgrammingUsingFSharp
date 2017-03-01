namespace Exercises6
module E = 
    
    (* 6.6
        Consider search trees of type BinTree<’a> as defined in Section 6.4. Declare an F# function
        that can delete an element in such a tree. Hint: Make use of an auxiliary function that deletes
        the smallest element in a non-empty search tree (and returns that value).
    *)

    type BinTree<'a when 'a : comparison> = 
    | Leaf
    | Node of BinTree<'a> * 'a * BinTree<'a>

    let t3 = Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2, Leaf))
    let t4 = Node(t3, 5, Node(Leaf, 7, Leaf))

    let deleteSmallestElement tree = 
        let rec deleteSmallestElement tr = 
            match tr with
            | Leaf -> (Leaf, Leaf)
            | Node(Leaf, x, rightTree) -> (rightTree, Node(Leaf, x, Leaf))
            | Node(leftTree, v, rightTree) -> 
                let (treeAfterDeletion, deletedElement) = deleteSmallestElement leftTree
                (Node (treeAfterDeletion, v, rightTree), deletedElement)
        
        deleteSmallestElement tree

    deleteSmallestElement (Node(Node(Leaf, 1, Leaf), 3, Leaf))
    deleteSmallestElement t3
    deleteSmallestElement t4
    deleteSmallestElement (Node(Leaf, 1, Node(Leaf, 2, Node(Leaf, 3, Leaf))))

    
    let  delete x t =
        let rec delete x tree = 
            match tree with
            | Leaf -> Leaf
            | Node(leftTree,v,rightTree) when x<v -> Node(delete x leftTree, v, rightTree)
            | Node(leftTree,v,rightTree) when x<v -> Node(leftTree, v, delete x rightTree)
            | Node(leftTree,_,rightTree)->
                match (leftTree, rightTree) with
                | (Leaf, Leaf) -> Leaf
                | (Leaf, tree) -> tree
                | (tree, Leaf) -> tree
                | (leftTree , rightTree) ->
                    let (rightTreeAfterDeletion, smallestElement) = deleteSmallestElement rightTree
                    match smallestElement with
                    | Node (Leaf, v, Leaf) -> Node(leftTree, v, rightTreeAfterDeletion)
                    | root -> Leaf
        delete x t

    delete 5 t4
                

        
    
         

    