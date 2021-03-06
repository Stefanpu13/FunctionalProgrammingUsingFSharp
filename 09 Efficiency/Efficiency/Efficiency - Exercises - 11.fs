namespace Exercises11
module E = 
    open Exercises8and9.E

    (* 9.11
        Declare tail-recursive functions leftTree and rightTree. By use of leftTree it should
        be possible to generate a big unbalanced tree to the left containing n+1 values in the nodes so
        that n is the value in the root, n − 1 is the value in the root of the left subtree, and so on. All
        subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a
        big unbalanced tree to the right.
        1. Use these functions to show the stack limit when using count and countA from Exercise
        9.8.        
    *)
        
    let rec leftTree treeDepth =  function
    |tr, x when x >= treeDepth -> Node (tr, x, Leaf)
    |tr, m -> leftTree treeDepth (Node(tr, m, Leaf), m + 1)

    let rec rightTree treeDepth =  function
    |tr, x when x >= treeDepth -> Node (Leaf, x, tr)
    |tr, m -> rightTree treeDepth (Node(Leaf, m, tr), m + 1) 


    type Result = | Success | Fail

    let tryFunc n func = 
        try        
            func (leftTree n (Leaf,0)) |> ignore
            Success        
        with e -> 
            Fail       
            
    let rec findApproximateStackTraceDepth lower upper func = 
        if lower > upper
        then
            lower
        else
            let middle = upper - (upper - lower) / 2

            match tryFunc middle func with
            | Success -> findApproximateStackTraceDepth (middle + 1) upper func
            | Fail -> findApproximateStackTraceDepth lower (middle - 1) func

    (*
        Give sufficiently big stack trace depth to show that the 'countA' function causes
        stack overflow.

        let stackTraceDepth = 150000

        findApproximateStackTraceDepth 0 stackTraceDepth (fun tr -> countA 0 tr)
        findApproximateStackTraceDepth 0 stackTraceDepth (fun tr -> countAC tr 0 id)
    *)


    (*
        2. Use these functions to test the performance of countC and countAC from Exercise 9.9.

        #time
        countAC (rightTree 150000 (Leaf, 0)) 0 id |> ignore
    *)
