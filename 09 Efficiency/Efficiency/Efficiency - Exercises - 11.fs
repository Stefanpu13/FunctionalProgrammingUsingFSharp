namespace Exercises11
module E = 
    open Exercises8and9.E

    // let st = StackTrace()
    // st.FrameCount

    (* 9.11
        Declare tail-recursive functions leftTree and rightTree. By use of leftTree it should
        be possible to generate a big unbalanced tree to the left containing n+1 values in the nodes so
        that n is the value in the root, n − 1 is the value in the root of the left subtree, and so on. All
        subtree to the right are leaves. Similarly, using rightTree it should be possible to generate a
        big unbalanced tree to the right.
        1. Use these functions to show the stack limit when using count and countA from Exercise
        9.8.
        2. Use these functions to test the performance of countC and countAC from Exercise 9.9.
    *)
        
    let rec leftTree n =  function
    |tr, x when x >= n -> Node (tr, x, Leaf)
    |tr, m -> leftTree n (Node(tr, m, Leaf), m + 1)

    let rec rightTree n =  function
    |tr, x when x >= n -> Node (Leaf, x, tr)
    |tr, m -> rightTree n (Node(Leaf, m, tr), m + 1) 
