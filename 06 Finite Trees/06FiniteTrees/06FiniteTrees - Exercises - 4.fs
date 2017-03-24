namespace Exercises4
module E = 
    (* 6.4
        Consider binary trees of type BinTree<’a,’b> as defined in Section 6.3. Declare functions
        1. leafVals: BinTree<’a,’b> -> Set<’a> such that leafVals t is the set of values
        occurring the leaves of t,
        2. nodeVals: BinTree<’a,’b> -> Set<’b> such that nodeVals t is the set of values
        occurring the nodes of t, and
        3. vals: BinTree<’a,’b> -> Set<’a>*Set<’b> such that vals t = (ls, ns), where
        ls is the set of values occurring the leaves of t and ns is the set of values occurring the nodes
        of t
    *)

    type BinTree<'a,'b> =
        | Leaf of 'a
        | Node of BinTree<'a,'b> * 'b * BinTree<'a,'b>


    (* 6.4.1
        1. leafVals: BinTree<’a,’b> -> Set<’a> such that leafVals t is the set of values
        occurring the leaves of t,
    *)
    let leafVals t = 
        let rec leafVals s  = function
            | Leaf v -> Set.add v s 
            | Node(t1,_,t2) -> Set.union (leafVals s t1) (leafVals s t2)
        leafVals Set.empty t

    (* 6.4.2
        2. nodeVals: BinTree<’a,’b> -> Set<’b> such that nodeVals t is the set of values
        occurring the nodes of t.
    *)

    let nodeVals t = 
        let rec nodeVals s  = function
            | Leaf v -> s 
            | Node(t1,v,t2) ->
                let s1 = Set.add v s
                Set.union (nodeVals s1 t1) (nodeVals s1 t2)
        nodeVals Set.empty t

    (* 6.4.3
        3. vals: BinTree<’a,’b> -> Set<’a>*Set<’b> such that vals t = (ls, ns), where
        ls is the set of values occurring the leaves of t and ns is the set of values occurring the nodes
        of t
    *)

    let vals t = (leafVals t), (nodeVals t)
