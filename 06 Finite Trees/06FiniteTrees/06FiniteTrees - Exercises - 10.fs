namespace Exercises10
module E = 
    (* 6.10
        Consider expression trees of type ExprTree declared in Section 6.5. Extend the type with an
        if-then-else expression of the form: if b then e1 else e2, where b is a boolean expression
        and e1 and e2 are expressions. An example could be:
        if a*3>b+c && a>0 then c+d else e
        Furthermore, extend the declaration of the eval function accordingly. Hint: make use of a
        mutually recursive type and function declarations.
    *)

    type  ExprTree = 
        | Const of int
        | Ident of string
        | Minus of ExprTree
        | Sum of ExprTree * ExprTree
        | Diff of ExprTree * ExprTree
        | Prod of ExprTree * ExprTree
        | Let of string * ExprTree * ExprTree
        | If of BooleanExpression * ExprTree * ExprTree
    and BooleanExpression =
        | And of BooleanExpression * BooleanExpression
        | Or of BooleanExpression * BooleanExpression
        | Not of BooleanExpression
        | Comparison of ComparisonOperator * ExprTree * ExprTree
        | BooleanConst of bool    
    and ComparisonOperator = 
        | EQ
        | GT
        | GTE
        | ST
        | STE

        // a*3 > b + 5 && c 

    let et =
        Prod(Ident "a",
            Sum(Minus (Const 3),
                Let("x", Const 5, Sum(Ident "x", Ident "a"))))

    let rec eval t env =
        match t with
        | Const n -> n
        | Ident s -> Map.find s env
        | Minus t -> - (eval t env)
        | Sum(t1,t2) -> eval t1 env + eval t2 env
        | Diff(t1,t2) -> eval t1 env - eval t2 env
        | Prod(t1,t2) -> eval t1 env * eval t2 env
        | Let(s,t1,t2) -> 
            let v1 = eval t1 env
            let env1 = Map.add s v1 env
            eval t2 env1
        | If (be, trueExp, falseEpx) -> 
            match evalBoolExp be env with
            | true -> eval trueExp env
            | false -> eval falseEpx env 
    and evalBoolExp be env = 
        match be with
        | And (be1, be2) -> (evalBoolExp be1 env) && (evalBoolExp be2 env) 
        | Or (be1, be2) -> (evalBoolExp be1 env) || (evalBoolExp be2 env) 
        | Not be -> not (evalBoolExp be env)        
        | BooleanConst b -> b
        | comp -> evalComp comp env
    and evalComp comp env = 
        match comp with
        | Comparison (EQ, t1, t2) -> (eval t1 env) = (eval t2 env)
        | Comparison (GT, t1, t2) -> (eval t1 env) > (eval t2 env)
        | Comparison (GTE, t1, t2) -> (eval t1 env) >= (eval t2 env)
        | Comparison (ST, t1, t2) -> (eval t1 env) < (eval t2 env)
        | Comparison (STE, t1, t2) -> (eval t1 env) <= (eval t2 env)
        | be -> evalBoolExp be env


    let env = Map.add "a" -7 Map.empty
    let env1 = Map.add "b" 7 env
    let et2 =
        Prod(Ident "a", If (            
                And(               
                       Comparison( 
                            GT, 
                            Sum(Const 3, Const 4), 
                            Prod( Ident "b", Ident "a")),
                    BooleanConst true), 
                Diff(Const 6, Ident "a"),
                Diff(Const 6, Ident "b")))
    
    // a = -7; b= 7
    //  a * (if (3+4) > (a*b) then (6-a) else (6-b))

    eval et2 env1