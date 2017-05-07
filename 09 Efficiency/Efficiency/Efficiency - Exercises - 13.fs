namespace Exercises13
module E = 
    open System.Collections.Generic
    (* 9.13 
        Compare the run times of the two versions of the function tryFind that are declared on
        Page 109 and on Page 191.
    *)

    let rec tryFind109 p s =
        if Set.isEmpty s 
        then None
        else 
            let minE = Set.minElement s
            if p minE 
            then Some minE
            else tryFind109 p (Set.remove minE s)

    let enumerator (m: IEnumerable<'c>) =
        let e = ref (m.GetEnumerator())
        let f () =
            match (!e).MoveNext() with
            | false -> None
            | _ -> Some ((!e).Current)
        f
    let tryFind191 p (s: Set<'a>) =
        let f = enumerator s
        let rec tFnd () =
            match f() with
            | None -> None
            | Some x ->
                if (p x) then Some x 
                else tFnd()
                
        tFnd()

    (*    
        let someSet = set [1..20000]

        #time
        tryFind109 ((=) someSet.Count) someSet
        tryFind191 ((=) someSet.Count) someSet
    *)