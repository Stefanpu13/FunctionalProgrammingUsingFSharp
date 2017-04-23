namespace Exercises9
module E = 
    open System.Collections.Generic
    (* 8.9
        Make declarations of breadthFirst and breadthFirstFold for list trees using an imperative
        queue.
        Hint: unfold the while-loop in the declaration of breadthFirstIter to a local recursive
        function and use argument and value of this function to build the result.
    *)
    type ListTree<'a> = Node of 'a * (ListTree<'a> list)
    
    let breadthFirstIter f state ltr =
        let mutable finalState = state
        let remains = Queue<ListTree<'a>>()

        remains.Enqueue ltr
        while (remains.Count <> 0) do
            let (Node (x,tl)) = remains.Dequeue()
            List.iter (remains.Enqueue) tl
            finalState <- f finalState x
        finalState

    // recursive breathFirstFold
    let rec breadthFirstFoldList f e ts=
        match ts with
        | [] -> e
        | (Node(x,ts))::rest -> 
            breadthFirstFoldList f (f e x) (rest@ts)
    let breadthFirstFold f e t = breadthFirstFoldList f e [t]
