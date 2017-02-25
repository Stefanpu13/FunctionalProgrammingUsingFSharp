namespace Exercises1
module E = 
    (* 6.1
        Declare a function red of type Fexpr -> Fexpr to reduce expressions generated from the
        differentiation program in Section 6.2. For example, sub-expressions of form Const 1.0 * e
        can be reduced to e. (A solution is satisfactory if the expression becomes “nicer”. It is difficult
        to design a reduce function so that all trivial sub-expressions are eliminated.)
    *)

    type Fexpr = 
        | Const of float
        | X
        | Add of Fexpr * Fexpr
        | Sub of Fexpr * Fexpr
        | Mul of Fexpr * Fexpr
        | Div of Fexpr * Fexpr
        | Sin of Fexpr
        | Cos of Fexpr
        | Log of Fexpr
        | Exp of Fexpr

    let rec D = function
        | Const _ -> Const 0.0
        | X -> Const 1.0
        | Add(fe,ge) -> Add(D fe, D ge)
        | Sub(fe,ge) -> Sub(D fe, D ge)
        | Mul(fe,ge) -> Add(Mul(D fe, ge), Mul(fe, D ge))
        | Div(fe,ge) -> Div(Sub(Mul(D fe,ge), Mul(fe,D ge)),Mul(ge,ge))
        | Sin fe -> Mul(Cos fe, D fe)
        | Cos fe -> Mul(Const -1.0, Mul(Sin fe, D fe))
        | Log fe -> Div(D fe, fe)
        | Exp fe -> Mul(Exp fe, D fe)


    // very naive solution
    let rec reduce = function     
    | Add(exp1, exp2) ->
        match (exp1, exp2) with
        | (Const 0.0, exp)| (exp, Const 0.0) -> reduce exp
        | (Const x, Const y) -> Const (x + y)
        | (e1, e2) when e1 = e2 -> Mul (Const 2.0, reduce e1)
        | (e1, e2) when e1 <> reduce e1 || e2 <> reduce e2 -> reduce (Add (reduce exp1, reduce exp2))        
        | (e1, e2) -> (Add (reduce exp1, reduce exp2))
    | Mul (exp1, exp2) ->
        match (exp1, exp2) with
        | (Const 0.0, exp)| (exp, Const 0.0) -> Const (0.0)
        | (Const 1.0, exp)| (exp, Const 1.0) -> reduce exp
        | (Const x, Const y) -> Const (x * y)
        | (e1, e2) when e1 <> reduce e1 || e2 <> reduce e2 -> reduce (Mul (reduce exp1, reduce exp2))
        | (e1, e2) -> (Mul (reduce exp1, reduce exp2)) 
    | Const f -> Const f
    | X -> X
    | Sub(exp1, exp2) -> 
        match exp1, exp2 with 
        | e1, e2 when e1 = e2 -> Const 0.0
        | (e1, e2) when e1 <> reduce e1 || e2 <> reduce e2 -> reduce (Sub (reduce exp1, reduce exp2))
        | exp1, exp2 -> Sub (reduce exp1, reduce exp2)    
    | Div (exp1, exp2) -> 
        match (exp1, exp2) with 
        | Const 0.0, e1 -> Const 0.0 
        | (e1, e2) when e1 <> reduce e1 || e2 <> reduce e2 -> reduce (Div (reduce exp1, reduce exp2))
        | e1, e2 -> Div (reduce e1, reduce e2) 
    | Sin fe -> Sin (reduce fe)
    | Cos fe -> Cos (reduce fe) 
    | Log fe -> Log (reduce fe) 
    | Exp fe -> Exp (reduce fe)
