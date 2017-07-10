module ComputationExpressions

type mySeq<'a> = seq<'a>

type MySeqClass() =
    member bld.Yield a: mySeq<'a> = Seq.singleton a
    member bld.For(sq:mySeq<'a>, f:'a -> mySeq<'b>):mySeq<'b> = 
        Seq.collect f sq

let mySeq = MySeqClass()

let cartesian sqx sqy =  mySeq {
    for x in sqx do
        for y in sqy do
            yield (x,y) 
}

let cartesian2 seqx seqy = 
    Seq.collect (fun elx -> 
        Seq.map (fun ely -> elx, ely) seqy
    ) seqx