namespace Exercises1

    (* 8.1
        *Make a drawing of the environment and store obtained by the following declarations and assignments:
        let mutable x = 1;;
        let mutable y = (x,2);;
        let z = y;;
        x <- 7;;

        Environment               Store
        x -> loc1                 loc1: 1
        y -> loc2                 loc2: (1, 2)
        z -> 1 (automatic application of "contentsOf" operator - see p.177-178)                
        x -> loc1                 loc1:7  
    *)

    (* 8.2
        The sequence of declarations:
            let mutable a = []
            let f x = a <- (x :: a)
            f(1)
        are accepted by F#. Explain why.

        Answer: type inference is based on usage. Since f is used with 1
        its type is inferred to be (int -unit). Since 'x' is int, it follows that (x::a) is list int.
        So, 'let mutable a = []' is monomorphic. See p. 82, where it is explained that:
        "All monomorphic expressions are OK, even non-value expressions,"
    *)


    (* 8.3 
        Make a drawing of the environment and store obtained by the following declarations and assignments:
        type t1 = { mutable a : int }
        type t2 = { mutable b : int ; c : t1 }
        let x = { a = 1 }
        let y = { b = x.a ; c = x }
        x.a <- 3

        Environment                    Store
        x -> {a -> loc1}               loc1:1
        y ->{b->1; c->{a->loc1}}
        x -> {a -> loc2}               loc2:2
        y -> {b->1; c->{a->loc2}}
    *)
