namespace Exercises12
module E = 
    open Exercises8and9.E    
    open Exercises11.E
    open System
    open System.Runtime.CompilerServices
     

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

    let t3 = Node(Node(Leaf, -3, Leaf), 0, Node(Leaf, 2, Leaf))
    let t4 = Node(t3, 5, Node(Leaf, 7, Leaf))

    preOrder t4
    preOrderTail t4

    let generateBinTree depth = 
        let rand = Random() 
        let rec generateBinTree = function
        | t, 0 -> t
        | (t, n) -> 
            if rand.Next(10) % 2 = 0
            then generateBinTree (Node(t, n, Leaf), n - 1)
            else generateBinTree (Node(Leaf, n, t), n - 1)
        
        generateBinTree (Leaf, depth)
            
    preOrder (generateBinTree 1000)

    findApproximateStackTraceDepth 0 150000 preOrder
    findApproximateStackTraceDepth 0 150000 preOrderTail 

