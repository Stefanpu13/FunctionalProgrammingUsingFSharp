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
    let rec delete x tree =
        match tree with
        | Leaf -> Leaf
        | Node(leftTree,v,rightTree) when x<v -> Node(delete x leftTree, v, rightTree)
        | Node(leftTree,v,rightTree) when x>v -> Node(leftTree, v, delete x rightTree)
        | Node(leftTree,_,rightTree) ->
                // See http://www.algolist.net/Data_structures/Binary_search_tree/Removal
                // to understand why delete and return smallest element from right tree 
                let (rightTreeAfterDeletion, smallestElement) = deleteSmallestElement rightTree
                match smallestElement with
                | Node (Leaf, v, Leaf) -> Node(leftTree, v, rightTreeAfterDeletion)
                | rightTreeWasEmpty -> leftTree   


    // tail recursive with continuation passing style
    (* My notes: 
        Interestingly, passing a function, that is passing a function, that is passing..., 
        also know as continuation passing style, acts as placeholder within the tree as it is traversed.
        Once the end of the traverse process is reached, all the functions(that are nested) are triggered,
        by passing the necessary arguments(some tree structure in that case) to the current(innermost) function.
        The innermost function returns a modified version of the passed tree to its containing function, 
        which also returns a modified version of the argument to its containing function, and so on.
        In effect, the placeholders are replaced by the new tree structure and the new tree is built.
    *)
    let deleteSmallestElementTailRecursive tree = 
        let rec deleteSmallestElement tr f = 
            match tr with
            | Leaf -> (Leaf, Leaf)
            | Node(Leaf, x, rightTree) -> (f rightTree, Node(Leaf, x, Leaf))
            | Node(leftTree, v, rightTree) -> 
                deleteSmallestElement leftTree (fun treeAfterDeletion -> f (Node(treeAfterDeletion, v, rightTree)))
        
        deleteSmallestElement tree id

    // tail recursive with continuation passing style
    let deleteTailRecursive x tree = 
        let rec delete x tree f =
            match tree with
            | Leaf -> f Leaf
            | Node(leftTree,v,rightTree) when x<v -> delete x leftTree (fun tr ->f (Node(tr, v, rightTree)))
            | Node(leftTree,v,rightTree) when x>v -> delete x rightTree (fun tr ->f (Node(leftTree, v,tr)))
            | Node(leftTree,_,rightTree) ->
                    // See http://www.algolist.net/Data_structures/Binary_search_tree/Removal
                    // to understand why delete and return smallest element from right tree 
                    let (rightTreeAfterDeletion, smallestElement) = deleteSmallestElementTailRecursive rightTree
                    match smallestElement with
                    | Node (Leaf, v, Leaf) -> f (Node(leftTree, v, rightTreeAfterDeletion))
                    | rightTreeWasEmpty -> f leftTree  

        delete x tree id

    let generateTree s e = 
        let tree  = Node(Leaf, s, Leaf)
        let rec generateTree x t = 
            match x with
            | max when x = e -> Node(t, (x), Leaf)
            | _ -> generateTree (x+1) (Node(t, (x), Leaf))

        generateTree (s+1) tree    
    // let c = 300000
    // let bigT = generateTree 1 c

    // deleteTailRecursive 1 bigT

    (* examples to visualize result fo continuations
        (fun treeAfterDeletion1 -> 
            (fun treeAfterDeletion2 -> 
                (fun treeAfterDeletion3 ->
                (Node(treeAfterDeletion3, v3, rightTree3))) 
            (Node(treeAfterDeletion2, v2, rightTree2))) 
        (Node(treeAfterDeletion1, v1, rightTree1))) rightTree

        (fun treeAfterDeletion -> (fun treeAfterDeletion -> f (Node(treeAfterDeletion, v, rightTree))) (Node(treeAfterDeletion, v, rightTree)))

        treeAfterDeletion2 = Node(treeAfterDeletion3, v3, rightTree3)

        treeAfterDeletion1 = Node(Node(treeAfterDeletion3, v3, rightTree3), v2, rightTree2)

        Node(Node(Node(treeAfterDeletion3, v3, rightTree3), v2, rightTree2), v1, rightTree1)

        (fun tr1 ->
            (fun tr2 ->
                (fun tr3 -> (Node(leftTree3, v3, tr3))) 
            (Node(tr2, v2, rightTree2))) 
        (Node(tr1, v1, rightTree1)))

        Node(Node(Node(leftTree3, v3, Leaf)), v2, rightTree2), v1, rightTree1)  
    *)
                

        
    
         

    