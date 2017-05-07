namespace Exercises14
module E =     
    open System.Collections.Generic
    (* 9.14
        Comparison of the efficiency of iteration functions for list and sets.
        In this exercise you should declare functions
        iterCollM: (’a -> unit) -> Coll<’a> -> unit
        so that iterCollM f col performs f v0; f v1; . . . ; f vn when col has v0, v1, . . . , vn as the
        elements, and M is the method of traversal that can be based on a tail-recursive function or
        using an enumerator.
    *)

    (* 9.14.1
        Declare a tail-recursive function to iterate a function over the elements of a list.
    *)

    let iterListTail iterator l = 
        let rec iter = function
        | [] -> ()
        | x::xs -> 
            iterator x
            iter xs
        
        iter l

        (* 9.14.2
            Declare a enumerator-based version. See Page 192.
        *)

    let enumerator (m: IEnumerable<'c>) =
        let e = ref (m.GetEnumerator())
        let f () =
            match (!e).MoveNext() with
            | false -> None
            | _ -> Some ((!e).Current)
        f

    let iterListEnumerator iterator l = 
        let enumF = enumerator l

        let rec iter () = 
            match enumF () with
            | None -> ()
            | Some x ->
                iterator x
                iter ()
        iter ()

    (* 9.14.3
        Declare a tail-recursive version that iterate over the elements of a set on the basis of the
        recursion scheme that repeatedly removes the minimal elements from the set. (See e.g. the
        declaration of tryFind on Page 109.)
    *)

    let rec iterSetTail iterator s =
        if Set.isEmpty s
        then ()
        else
            let minEl = Set.minElement s
            iterator minEl
            iterSetTail iterator (Set.remove minEl s)

    (* 9.14.4
        Compare the run times of the above iteration functions and the library functions List.iter
        and Set.iter. Use, for example, sets and lists containing the integers from 0 to 10000000
        and the function ignore.
    *)

    (*
        let someList = [0..10000000]
        let someSet = set someList

        #time

        let iteratorFunc x = 
            x < 100000 |> ignore
            ()

        iterListTail iteratorFunc someList
        iterListEnumerator iteratorFunc someList
        List.iter iteratorFunc someList

        iterSetTail iteratorFunc someSet
        Set.iter iteratorFunc someSet
    *)
    