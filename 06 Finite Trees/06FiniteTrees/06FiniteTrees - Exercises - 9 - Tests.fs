module Exercises9Tests
open NUnit.Framework
open FsUnitTyped
open Exercises8.E
open Exercises1Common.Types.Expression

// Test intpInstr
[<TestFixture>]
type ``Test intpInstr``() =

    [<Test>]
    member t.``If Stack is someStack and opertation is Push n, stack should be Stack [n::someStack]`` () =         
        intpInstr (Stack []) (PUSH 3.0) |> shouldEqual <| Stack [3.0]        
        intpInstr (Stack[3.0]) (PUSH 2.0) |> shouldEqual <| Stack [2.0; 3.0]

    [<Test>]
    member t.``If Stack is empty and opertation is OneOperand f, stack should be InvalidStack`` () = 
        let stack = Stack []
        intpInstr stack (SIN ) |> shouldEqual <| InvalidStack
        intpInstr stack (COS) |> shouldEqual <| InvalidStack
        intpInstr stack (LOG ) |> shouldEqual <| InvalidStack
        intpInstr stack (EXP ) |> shouldEqual <| InvalidStack

    [<Test>]
    member t.``If Stack is n::someStack and opertation is OneOperand f, stack should be Stack (f n)::someStack`` () = 
        let stack = Stack [2.0; 3.0]
        intpInstr stack (SIN ) |> shouldEqual <| Stack [sin 2.0; 3.0]
        intpInstr stack (COS) |> shouldEqual <| Stack [cos 2.0; 3.0]
        intpInstr stack (LOG ) |> shouldEqual <| Stack [log 2.0; 3.0]
        intpInstr stack (EXP ) |> shouldEqual <| Stack [exp 2.0; 3.0]

    [<Test>]
    member t.``If Stack has less than two elems and opertation is TwoOperand f, stack should be InvalidStack`` () = 
        let stack = Stack [2.0]
        intpInstr stack (ADD ) |> shouldEqual <| InvalidStack
        intpInstr stack (SUB ) |> shouldEqual <| InvalidStack
        intpInstr stack (MULT ) |> shouldEqual <| InvalidStack
        intpInstr stack (DIV ) |> shouldEqual <| InvalidStack

        intpInstr (Stack []) (ADD ) |> shouldEqual <| InvalidStack
        intpInstr (Stack []) (SUB ) |> shouldEqual <|  InvalidStack
        intpInstr (Stack []) (MULT ) |> shouldEqual <|  InvalidStack
        intpInstr (Stack []) (DIV ) |> shouldEqual <|  InvalidStack

    [<Test>]
    member t.``If Stack is a:b::vals and opertation is TwoOperand f, stack should be Stack ((f b a)::vals) `` () = 
        let stack = Stack [2.0; 3.0]
        intpInstr stack (ADD ) |> shouldEqual <| Stack [5.0]
        intpInstr stack (SUB ) |> shouldEqual <| Stack [1.0]
        intpInstr stack (MULT ) |> shouldEqual <| Stack [6.0]
        intpInstr stack (DIV ) |> shouldEqual <| Stack [1.5]

// Test intpProg
[<TestFixture>]
type ``Test intpProg``() =

    [<Test>]
    member t.``If instructions list is empty, result should be None`` () =         
        intpProg [] |> shouldEqual <| None        
    
    [<Test>]
    member t.``If instructions list is InvalidStack, result should be None`` () =         
        intpProg [ADD] |> shouldEqual <| None
        intpProg [PUSH 2.0; MULT] |> shouldEqual <| None
        intpProg [LOG; PUSH 2.0] |> shouldEqual <| None
        intpProg [SIN; PUSH 2.0; PUSH 3.0] |> shouldEqual <| None

    [<Test>]
    member t.``If instructions list is only PUSH ops, result should be last PUSH`` () =         
        intpProg [PUSH 2.0;PUSH 3.0;] |> shouldEqual <| Some 3.0
        intpProg [PUSH 2.0;PUSH 2.0;PUSH 4.0] |> shouldEqual <| Some 4.0
        intpProg [PUSH 2.0;PUSH 2.0;PUSH 0.0; PUSH 4.0] |> shouldEqual <| Some 4.0
        intpProg [PUSH 1.0] |> shouldEqual <| Some 1.0
        
    [<Test>]
    member t.``If instructions list is one operand op, result should be result of op`` () =         
        intpProg [PUSH 2.0;PUSH 3.0;SIN] |> shouldEqual <| Some (sin 3.0)
        intpProg [PUSH 2.0;PUSH 4.0;COS] |> shouldEqual <| Some (cos 4.0)
        intpProg [PUSH 2.0;PUSH 5.0;LOG] |> shouldEqual <| Some (log 5.0)
        intpProg [PUSH 2.0;PUSH 6.0;EXP] |> shouldEqual <| Some (exp 6.0)

    [<Test>]
    member t.``If instructions list is simple two operand op, result should be result of op`` () =         
        intpProg [PUSH 2.0;PUSH 2.0;ADD] |> shouldEqual <| Some 4.0
        intpProg [PUSH 2.0;PUSH 2.0;SUB] |> shouldEqual <| Some 0.0
        intpProg [PUSH 2.0;PUSH 2.0;MULT] |> shouldEqual <| Some 4.0
        intpProg [PUSH 2.0;PUSH 2.0;DIV] |> shouldEqual <| Some 1.0

    [<Test>]
    member t.``If instructions list is complex op, result should be result of  all ops applied cpprectly`` () =         
        intpProg [PUSH 2.0;PUSH 3.0;SIN; ADD;] |> shouldEqual <| Some ((sin 3.0) + 2.0)
        intpProg [PUSH 2.0;PUSH 4.0;COS; SUB] |> shouldEqual <| Some (2.0 - (cos 4.0))
        intpProg [ PUSH 3.0; PUSH 2.0;PUSH 5.0;SUB;PUSH 4.0;MULT; SUB] |> shouldEqual <| Some (3.0 - 4.0 * (2.0 - 5.0))  
        intpProg [PUSH 2.0;PUSH 6.0;EXP; DIV] |> shouldEqual <| Some (2.0 / (exp 6.0))


// Test trans
(* 
    Note: transforming a stack and interpretinga stack are two disting ops. 
    The first transforms expression into stack, the second builds new stack that can be evaluated
*)
[<TestFixture>]
type ``Test trans``() =

    [<Test>]
    member t.``If Fexpr is Const, result should be [PUSH 3.0]`` () =         
        trans (Const 3.0, 2.0) |> shouldEqual <| [PUSH 3.0]     

    [<Test>]
    member t.``If Fexpr is X, result should be [PUSH X] and intpProg should be Some 2`` () =         
        trans (X, 2.0) |> shouldEqual <| [PUSH 2.0]  
        intpProg (trans (X, 2.0)) |> shouldEqual <| Some 2.0 

    [<Test>]
    member t.``If Fexpr Sub(Const 3.0, X) and X = 2.0, result should be [PUSH 3.0;PUSH 2.0; SUB]`` () =         
        trans (Sub(Const 3.0, X), 2.0) |> shouldEqual <| [PUSH 3.0;PUSH 2.0; SUB] 
        intpProg (trans (Sub(Const 3.0, X), 2.0)) |> shouldEqual <| Some 1.0
    
    [<Test>]
    member t.``If Fexpr Mul(X, Sub(Sin(Const 3.0))) and X = 2.0, result should be [PUSH 2.0;PUSH 3.0;SIN;PUSH 2.0; SUB; MULT]`` () =         
        trans (Mul(X, Sub(Sin(Const 3.0), X)), 2.0) |> shouldEqual <| [PUSH 2.0; PUSH 3.0; SIN; PUSH 2.0; SUB; MULT]
        intpProg (trans (Mul(X, Sub(Sin(Const 3.0), X)), 2.0) ) |> shouldEqual <| Some (2.0 * (sin 3.0 - 2.0))
   