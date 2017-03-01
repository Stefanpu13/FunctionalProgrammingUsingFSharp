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
            | Info (Info(grandFatherTree, name, grandMotherTree) as maleTree, _, femaleTree) -> 
                (maleAnc (name::ancestors) maleTree) @ (maleAnc [] femaleTree)
            | Info (_,_,femaleTree) -> (maleAnc ancestors femaleTree)
            
        maleAnc [] t

    let femaleAnc t =
        let rec femaleAnc ancestors = function
            | Unspec -> ancestors
            | Info (maleTree, _, (Info(grandFatherTree, motherName, grandMotherTree) as femaleTree)) -> 
                (femaleAnc [] maleTree) @ femaleAnc (motherName::ancestors) femaleTree
            | Info (maleTree, _, _) -> femaleAnc ancestors maleTree
        
        femaleAnc [] t


    let t = 
        Info(
            Info(
                Info(
                    Info(
                        Info (Unspec, "Dad4", Unspec), 
                        "Dad3", 
                        Unspec
                    ), 
                    "DadsDad", 
                    Unspec
                ), 
                "Dad1", 
                Info(Unspec, "Dad1sMom", Unspec)
            ), 
            "Me",
             Info(
                 Info(
                     Unspec, 
                     "MomsDad", 
                     Info(Unspec, "MomsDadMom", Unspec)
                ),
                "Mom",
                Info(
                    Info(
                        Unspec, "MomsMomsDad", Unspec
                    ), 
                    "MomsMom", 
                    Info(Unspec, "OneMoreMom", Unspec)
                )
            )
        )
        