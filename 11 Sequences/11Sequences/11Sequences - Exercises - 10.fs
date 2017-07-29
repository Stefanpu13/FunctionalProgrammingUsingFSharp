namespace Exercises10

module E =     
    (* 11.10
        Use the functions in the Seq library to declare a function cartesian sqx sqy that gives a
        sequence containing all pairs (x, y) where x is a member of sqx and y is a member of sqy.
        Make an alternative declaration using sequence expressions.
    *)

    
    let cartesian seqx seqy = 
        Seq.collect (fun elx -> 
            Seq.map (fun ely -> elx, ely) seqy
        ) seqx
        

    let cartesian2 seqx seqy = seq {
        for elx in seqx do
            for ely in seqy do  
                yield elx, ely
    }


    //cartesian2 [1;2;3; 5] [3;4;5; 7; 8]

    //cartesian [8;9] (cartesian [1;2;3; 5] [3;4;5; 7; 8])

        

    

    