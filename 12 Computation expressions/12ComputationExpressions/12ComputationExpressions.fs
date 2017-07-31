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

 
// Maybe 

type Expr = 
    | Num of int | Var of string 
    | Add of Expr*Expr | Div of Expr*Expr
let delay v = fun () -> v
let start m = m()

type maybe<'a> = unit -> option<'a>  
type MaybeClass() =
    member bld.Bind(m:maybe<'a>, f:'a->maybe<'b>):maybe<'b> =
        match start m with
        | None -> delay None
        | Some a -> f a
    member bld.Return a:maybe<'a> = delay(Some a)
    member bld.ReturnFrom v:maybe<'a> = delay v
    member bld.Zero():maybe<'a> = delay None
    member bld.Delay f:maybe<'a> = fun () -> start (f())
let maybe = MaybeClass()


let I e env =
    let rec eval = function
    | Num i -> maybe {return i}
    | Var x -> maybe {return! Map.tryFind x env}
    | Add(e1,e2) -> 
        maybe {
            let! v1 = eval e1
            let! v2 = eval e2
            return (printfn "v1: %i v2: %i" v1 v2 ; v1+v2)
            // return v1+v2
        }
    | Div(e1,e2) -> 
        maybe {
            let! v2 = eval e2
            if v2<>0 then
                let! v1 = eval e1
                return v1/v2
        }
    eval e

let e1 = Add(Div(Num 1, Num 0), Num 2)
let e2 = Add(Add(Var "x", Var "y"), Num 2)
let env = Map.ofList [("x",1);("y",2)]
let v1 = I e1 env
start v1
let v2 = I e2 env
start v2



  //---------------

// let r = 4 in r + 4
//   r