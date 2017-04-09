
namespace NN
module MM = 
let charCount = [| for n in 'A'..'Z' -> 0 |]

let PrintLines3() =
    seq {
        let finished = ref false
        while not !finished do
            match System.Console.ReadLine() with
            | null -> finished := true
            | s -> yield s
    }

let PrintLines1() =
    let mutable finished = false
    while not finished do
        match System.Console.ReadLine() with
        | null -> finished <- true
        | s -> printfn "line is: %s" s

let PrintLines2() =
    seq {
        let mutable finished = false
        // Compiler error:
        while not finished do
            match System.Console.ReadLine() with
            | null -> finished <- true
            | s -> yield s
    }

open System.Collections.Generic
type ListTree<'a> = Node of 'a * (ListTree<'a> list)
let breadthFirstIter f ltr =
    let remains = Queue<ListTree<'a>>()
    remains.Enqueue ltr
    while (remains.Count <> 0) do
        let (Node (x,tl)) = remains.Dequeue()
        List.iter (remains.Enqueue) tl
        f x

let tr = Node (
            1, 
            [
                Node(
                    2, 
                    []
                ); 
                Node(
                    3, 
                    [ Node(4,[])]
                );
                 Node(
                    5, 
                    []
                ); 
            ])

breadthFirstIter (fun n -> printfn "%A" n) tr