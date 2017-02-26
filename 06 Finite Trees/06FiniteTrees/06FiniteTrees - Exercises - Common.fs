namespace Exercises1Common
module Types = 
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

module Utils = 
    open Types
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