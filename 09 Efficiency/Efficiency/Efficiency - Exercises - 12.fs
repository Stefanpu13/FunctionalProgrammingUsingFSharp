namespace Exercises12
module E = 
    open System
    open System.Runtime.CompilerServices
    open Exercises8and9.E
    open Exercises11.E  

    (* 9.12
        Develop a continuation-based version of the function preOrder from Section 6.4, and compare
        the performance of the two functions.
    *)
        
    let rec preOrder = function
    | Leaf -> []
    | Node(tl,x,tr) -> 
        RuntimeHelpers.EnsureSufficientExecutionStack()
        x :: (preOrder tl) @ (preOrder tr)

    let preOrderTail t = 
        let rec preOrder cont = function
        | Leaf -> cont []
        | Node(tl,x,tr) as tree -> 
            preOrder (fun leftTreeList  -> 
                preOrder ( fun rightTreeList -> 
                    cont (x::leftTreeList@rightTreeList)
                ) tr
            ) tl
        
        preOrder id t 

   (*
        For stack trace dept in the interval 6000-8000 speed diff. is neglitible
        After that dept there is not enough space for the 'preOrder' function

        findApproximateStackTraceDepth 0 8500 preOrder
        findApproximateStackTraceDepth 0 85000 preOrderTail 
   *)
    

