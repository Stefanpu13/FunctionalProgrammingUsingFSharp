namespace Exersices2
open System.Threading
module E = 
    (* 2.
        Make a type extension (cf. Section 7.4) of the class AsyncEventQueue<’T> with an extra
        member Timer: ’T -> int -> unit such that evaluating
        evnq.Timer evnt n
        will start an asynchronous computation that first sleeps n milliseconds and afterwards sends the
        event evnt to the queue evnq.
        Hint: Apply Async.StartWithContinuations to Async.Sleep with suitable continuations.
    *)


    // An asynchronous event queue kindly provided by Don Syme
    type AsyncEventQueue<'T>() =
        let mutable cont = None
        let queue = System.Collections.Generic.Queue<'T>()
        let tryTrigger() =
            match queue.Count, cont with
            | _, None -> ();
            | 0, _ -> ();
            | _, Some d ->
                cont <- None
                d (queue.Dequeue())

        let tryListen(d) =
            if cont.IsSome then invalidOp "multicast not allowed"
            cont <- Some d
            tryTrigger()
        member x.Post msg =               
            queue.Enqueue msg; tryTrigger()
        member x.Receive() =
            Async.FromContinuations (fun (cont,econt,ccont) ->                
                tryListen cont)

        member x.Timer evnt n = 
            Async.StartWithContinuations (
                async{
                    do! Async.Sleep n                    
                },
                (fun el -> printfn "Continuation entered."; x.Post evnt),
                (fun el -> printfn "err:%A" el),
                (fun el -> printfn "canc:%A" el)
            )          

    // let evQueue = AsyncEventQueue()
    // evQueue.Timer "a" 2000

    

