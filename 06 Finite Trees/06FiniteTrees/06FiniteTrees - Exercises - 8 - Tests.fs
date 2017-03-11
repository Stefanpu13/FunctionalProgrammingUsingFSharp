module Exercises8Tests
open NUnit.Framework
open FsUnitTyped
open Exercises8.E

// Test intpInstr
[<TestFixture>]
type ``Test intpInstr``() =

    [<Test>]
    member t.``If Stack is someStack and opertation is Push n, stack should be Stack [n::someStack]`` () = 
        
        intpInstr (Stack []) (PUSH 3.0) |> shouldEqual <| Stack [3.0]
        
        intpInstr (Stack[3.0]) (PUSH 2.0) |> shouldEqual <| Stack [2.0; 3.0]

    [<Test>]
    member t.``If Stack is empty and opertation is OneOperand f, stack should be Stack empty`` () = 
        let stack = Stack []
        intpInstr stack (SIN ) |> shouldEqual <| Stack []
        intpInstr stack (COS) |> shouldEqual <| Stack []
        intpInstr stack (LOG ) |> shouldEqual <| Stack []
        intpInstr stack (EXP ) |> shouldEqual <| Stack []

    [<Test>]
    member t.``If Stack is n::someStack and opertation is OneOperand f, stack should be Stack (f n)::someStack`` () = 
        let stack = Stack [2.0; 3.0]
        intpInstr stack (SIN ) |> shouldEqual <| Stack [sin 2.0; 3.0]
        intpInstr stack (COS) |> shouldEqual <| Stack [cos 2.0; 3.0]
        intpInstr stack (LOG ) |> shouldEqual <| Stack [log 2.0; 3.0]
        intpInstr stack (EXP ) |> shouldEqual <| Stack [exp 2.0; 3.0]

    [<Test>]
    member t.``If Stack has less than two elems and opertation is TwoOperand f, stack should remain same`` () = 
        let stack = Stack [2.0]
        intpInstr stack (ADD ) |> shouldEqual <| stack
        intpInstr stack (SUB ) |> shouldEqual <| stack
        intpInstr stack (MULT ) |> shouldEqual <| stack
        intpInstr stack (DIV ) |> shouldEqual <| stack

        intpInstr (Stack []) (ADD ) |> shouldEqual <| Stack []
        intpInstr (Stack []) (SUB ) |> shouldEqual <|  Stack []
        intpInstr (Stack []) (MULT ) |> shouldEqual <|  Stack []
        intpInstr (Stack []) (DIV ) |> shouldEqual <|  Stack []

    [<Test>]
    member t.``If Stack is a:b::vals and opertation is TwoOperand f, stack should be Stack ((f b a)::vals) `` () = 
        let stack = Stack [2.0; 3.0]
        intpInstr stack (ADD ) |> shouldEqual <| Stack [5.0]
        intpInstr stack (SUB ) |> shouldEqual <| Stack [1.0]
        intpInstr stack (MULT ) |> shouldEqual <| Stack [6.0]
        intpInstr stack (DIV ) |> shouldEqual <| Stack [1.5]

  