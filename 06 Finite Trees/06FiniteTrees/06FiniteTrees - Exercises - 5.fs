namespace Exercises5
module E = 
    
    (* 6.5
        An ancestor tree contains the name of a person and of some of the ancestors of this person. We
        define the type AncTree by:
        type AncTree = | Unspec
        | Info of AncTree * string * AncTree;;
        The left sub-tree is the ancestor tree of the farther while the right sub-tree is the ancestor tree
        of the mother. Write a value of type ancTree with at least 5 nodes and make a drawing of the
        corresponding tree.
        Declare functions maleAnc and femaleAnc to compute the list of names of male and female
        ancestors of a person in an ancestor tree.
    *)

    type AncTree = 
    | Unspec 
    | Info of AncTree * string * AncTree

    let maleAnc t =
        let rec maleAnc ancestors = function
            | Unspec -> ancestors
            | Info (maleAncestorTree, ancestorName, _) -> maleAnc  (ancestorName::ancestors) maleAncestorTree  
        
        match t with
        | Unspec -> []
        | Info (maleAncestorTree, _, _) -> maleAnc [] maleAncestorTree

    let femaleAnc t =
        let rec femaleAnc ancestors = function
            | Unspec -> ancestors
            | Info (_, ancestorName, femaleAncestorTree) -> femaleAnc  (ancestorName::ancestors) femaleAncestorTree  
        
        match t with
        | Unspec -> []
        | Info (_, _, femaleAncestorTree) -> femaleAnc [] femaleAncestorTree


    let t = 
        Info 
            (Info
                (Info
                    (Info
                        (Info (Unspec, "Dad4", Unspec), 
                        "Dad3", Unspec), 
                    "DadsDad", Unspec), 
                "Dad1", 
                Info(Unspec, "Dad1sMom", Unspec)), 
            "Me",
             Info
                (Info(Unspec, "MomsDad", Info(Unspec, "MomsDadMom", Unspec)),
                 "Mom", Info(Info(Unspec, "AnotherDad", Unspec), "MomsMom", Info(Unspec, "OneMoreMom", Unspec))))

    maleAnc t
    femaleAnc t