namespace Exersices7
open System.Threading.Tasks
module E =     
    (* 7.
        In this exercise we shall use data and task parallelism in connection with computation of Fibonacci
        numbers.
            • Consider your solution to Exercise 1.5. Make an array containing the integers from 0 to 40
            and apply Array.map to compute the first 41 Fibonacci numbers. Measure the run time of
            this operation. Make a data parallel solution using Array.Parallel.map and compare
            the solutions.
            • Make a task-parallel solution to Exercise 1.5 and measure the obtained speedup when computing
            big Fibonacci numbers.
            • Compare the obtained results with a sequential solution using accumulation parameters (Exercise
            9.7). Note that the linear speedup obtained using multiple cores does not replace the
            use of good algorithms.            
    *)

    let rec fibonacci1_5 n =
        match n with
        | 0 -> 0
        | 1 -> 1
        | x -> fibonacci1_5 (x-1) + fibonacci1_5 (x-2) 

    let rec fibonacci1_5Parallel n remainingTasks=
        match n with
        | 0 -> 0
        | 1 -> 1
        | x -> 
            if remainingTasks > 0
            then 
                let t1 = 
                    Task.Factory.StartNew(fun _ ->
                        fibonacci1_5Parallel (x - 1) (remainingTasks - 1) 
                    )
                let t2 = 
                    Task.Factory.StartNew(fun _ ->
                        fibonacci1_5Parallel (x - 2) (remainingTasks - 2) 
                    )
                t1.Result + t2.Result                
            else 
                fibonacci1_5 (x-1) + fibonacci1_5 (x - 2)     



    let fibonacciOptimised n = 
        let rec fib fibMinus2 fibMinus1 = function       
            | searched when searched = n -> fibMinus2 + fibMinus1
            | x -> fib fibMinus1 (fibMinus2 + fibMinus1) (x + 1)
      
        match n with 
        | 0 -> 0
        | 1 -> 1
        | x -> fib 0 1 2

    // #time
    // rouhgly 2.5 secs
    [1..40] 
        |> List.map fibonacci1_5
        |> List.iter (printf "%A; ")
    // around 1.2 - 1.3 secs
    [1..40] 
        |> List.map (fun n -> fibonacci1_5Parallel n 8)
        |> List.iter (printf "%A; ")
    // almost instanlty
    [1..40] 
        |> List.map fibonacciOptimised
        |> List.iter (printf "%A; ")