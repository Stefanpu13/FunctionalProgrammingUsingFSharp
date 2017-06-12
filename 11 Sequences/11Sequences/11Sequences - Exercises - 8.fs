namespace Exercises8

module E =     
    (* 11.8
        The Madhava-Leibniz series (also called Gregory-Leibniz series) for π is:
        pi = 4 sum (n=0 to infinity) ((-1)^n)/2n+1
        Use this series to approximate π. (Note that there are other series for π, which converge much
        faster than the above one.)
    *)

    let piApprox n = 
        4.0 * Seq.sumBy (fun n -> (pown -1.0 (int n) )/(2.0 * n + 1.0)) {0.0..n}

    let piApprox2 n = 4.0 * Seq.sum (seq {
        for num in 0.0..n do
            yield (pown -1.0 (int num) )/(2.0 * num + 1.0)                         
    })

    (*
    #time
    piApprox 11111115.0 
    piApprox2 11111115.0 
    *)
       