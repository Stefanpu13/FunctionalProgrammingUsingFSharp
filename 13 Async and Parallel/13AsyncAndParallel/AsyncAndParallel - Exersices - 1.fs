namespace Exersices1
open System.Threading
module E = 
    (* 1.
        Make program producing the deadlocked situation described on Page 315.
    *)

    let mutex1 = new Mutex()
    let mutex2 = new Mutex()
    let thread1 = Thread (fun () ->        
        printfn "thread1 start"
        mutex1.WaitOne() |> ignore
        // Transfer control to other thread.
        Thread.Sleep 200
        mutex2.WaitOne() |> ignore
  
        printfn "fin1"
        )
    let thread2 = Thread (fun () ->        
        printfn "thread2 start"
        mutex2.WaitOne() |> ignore        
        // Transfer control to other thread. That way it doesn`t matter witch thread is 
        // started first.
        Thread.Sleep 200       
        mutex1.WaitOne() |> ignore

        printfn "fin2"
    )

    thread1.Start()    
    thread2.Start()
