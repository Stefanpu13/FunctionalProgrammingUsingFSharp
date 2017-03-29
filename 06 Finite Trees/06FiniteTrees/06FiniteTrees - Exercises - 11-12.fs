namespace Exercises11
module E = 
    (* 6.11
        Write the steps of the evaluation of the expression:
        depthFirstFold (fun a x -> x::a) [] t1
        See Page 139.
    *)
    type ListTree<'a> = Node of 'a * (ListTree<'a> list)

    let t7 = Node(7,[])
    let t6 = Node(6,[])
    let t5 = Node(5,[])     
    let t3 = Node(3,[])    
    let t2 = Node(2,[t5]) 
    let t4 = Node(4,[t6; t7])
    let t1 = Node(1,[t2; t3; t4])

    let rec depthFirstFold f e (Node(x,ts)) =
        List.fold (depthFirstFold f) (f e x) ts
    
    (* depthFirstFold (fun a x -> x::a) [] t1

                    1
                  / | \
                 /  |  \
                /   |   \
               2    3    4
               |       /   \    
               5      6     7
        1. depthFirstFold (fun a x -> x::a)              []              t1
            2. depthFirstFold (fun a x -> x::a)          [1]             t2
                3. depthFirstFold (fun a x -> x::a)      [2;1]           t5
                    4. depthFirstFold (fun a x -> x::a)  [5;2;1]         []
            4. depthFirstFold (fun a x -> x::a)          [5;2;1]         t3
                5. depthFirstFold (fun a x -> x::a)      [3;5;2;1]       []
            6. depthFirstFold (fun a x -> x::a)          [3;5;2;1]       t4
                7. depthFirstFold (fun a x -> x::a)      [4;3;5;2;1]     t6
                    8. depthFirstFold (fun a x -> x::a)  [6;4;3;5;2;1]   []
                9. depthFirstFold (fun a x -> x::a)      [6;4;3;5;2;1]   t7
                    10. depthFirstFold (fun a x -> x::a) [7;6;4;3;5;2;1] []
    *)
    
    (* 6.12 
        Declare the functions depthFirstFoldBack and breadthFirstFold on list trees, cf.
        Section 6.6.
    *)
    let rec depthFirstFoldBack f (Node(x,ts)) e = 
        f x (List.foldBack (depthFirstFoldBack f) ts e)    

    let rec breadthFirstFoldBackList f ts e =
        match ts with
        | [] -> e
        | (Node(x,ts))::rest ->
        f x (breadthFirstFoldBackList f (rest@ts) e)

    let breadthFirstFoldBack f t e = breadthFirstFoldBackList f [t] e   

    // fold : folder:('State -> 'T -> 'State) -> state:'State -> list:'T list -> 'State
    let rec breadthFirstFoldList f e ts=
        match ts with
        | [] -> e
        | (Node(x,ts))::rest -> 
            breadthFirstFoldList f (f e x) (rest@ts)
    let breadthFirstFold f e t = breadthFirstFoldList f e [t]

    
