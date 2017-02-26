namespace Exercises1
module E = 
    open Exercises1Common.Types
    open Exercises1Common.Utils
    (* 6.1
        Declare a function red of type Fexpr -> Fexpr to reduce expressions generated from the
        differentiation program in Section 6.2. For example, sub-expressions of form Const 1.0 * e
        can be reduced to e. (A solution is satisfactory if the expression becomes “nicer”. It is difficult
        to design a reduce function so that all trivial sub-expressions are eliminated.)
    *)
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
