namespace Exercises8
open Exercises1Common.Types.Expression
module E = 

    (*
        We consider a simple calculator with instructions for addition, subtraction, multiplication and
        division of floats, and the functions: sin, cos, log and exp.
        The instruction set of the calculator is modelled by the following F# type:
        type Instruction = 
            | ADD | SUB | MULT | DIV | SIN
            | COS | LOG | EXP | PUSH of float
        The calculator is a stack machine, where a stack is a list of floats.
        The execution of an instruction maps a stack to a new stack:
        The execution of ADD with stack a b c · · · yields a new stack: (b + a) c · · · , where the top
        two elements a and b on the stack have been replaced by the single element (b + a). Similarly
        with regard to the instructions, SUB, MULT and DIV, which all work on the top two elements
        of the stack.
        The execution of one of the instructions SIN, COS, LOG and EXP applies the corresponding
        function to the top element of the stack. For example, the execution of LOG with stack
        a b c · · · yields the new stack: log(a) b c · · · .
        The execution of PUSH r with the stack a b c · · · pushes r on top of the stack, that is, the
        new stack is: r a b c · · · .
    *)


    (* 6.8.1
        Declare a type Stack for representing the stack, and declare an F# function to interpret the
        execution of a single instruction:
        intpInstr: Stack -> Instruction -> Stack 
    *)

    type Instruction = 
            | ADD | SUB | MULT | DIV | SIN
            | COS | LOG | EXP | PUSH of float

    type Stack = Stack of float list | InvalidStack

    type Operation = 
    | TwoOperands of (float -> float -> float)
    | OneOperand of (float -> float)
    | PushOperand of float 

    let operation = function
        | ADD ->  TwoOperands (+)
        | SUB ->  TwoOperands (-)
        | MULT ->  TwoOperands (*)
        | DIV ->  TwoOperands (/)
        | SIN -> OneOperand sin
        | COS -> OneOperand cos
        | LOG -> OneOperand log
        | EXP -> OneOperand exp
        | PUSH num -> PushOperand num

    let intpInstr stack instr =
        match stack with
        | Stack values ->
            match (operation instr, values) with                        
                | TwoOperands f, (a::b::vals) -> Stack ((f b a)::vals)
                | OneOperand f, (v::vals) -> Stack ((f v)::vals)
                | PushOperand v, _ ->  Stack (v::values)
                | op, s ->  InvalidStack    
        | InvalidStack -> InvalidStack

    (* 6.8.2
        A program for the calculator is a list of instructions [i1, i2, . . . , in]. A program is executed
        by executing the instructions i1, i2, . . . , in one after the other, in that order, starting with an
        empty stack. The result of the execution is the top value of the stack when all instructions
        have been executed.
        Declare an F# function to interpret the execution of a program:
        intpProg: Instruction list -> float
    *)

    let intpProg instructions = 
        let stackAfterInstructions = List.fold intpInstr (Stack []) instructions
        match stackAfterInstructions with
        | Stack (x::xs) -> Some x
        | s -> None

    (* 6.8.3
        Declare an F# function
        trans: Fexpr * float -> Instruction list
        where Fexpr is the type for expression trees declared in Section 6.2. The value of the expression
        trans(fe, x) is a program prg such that intpProg(prg) gives the float value of
        fe when X has the value x.
    *)

    let trans (expr, num) = 
        let rec trans expr instructions = 
            match expr with
            | Add (a, b) -> (trans a (trans b (ADD::instructions)))
            | Sub (a, b) -> (trans a (trans b (SUB::instructions)))
            | Mul (a, b) -> (trans a (trans b (MULT::instructions)))
            | Div (a, b) -> (trans a (trans b (DIV::instructions)))
            | Sin a -> (trans a (SIN::instructions))
            | Cos a -> (trans a (COS::instructions))
            | Log a -> (trans a (LOG::instructions))
            | Exp a -> (trans a (EXP::instructions))
            | X -> (PUSH num)::instructions
            | Const n -> (PUSH n)::instructions
    
        trans expr []

    let expr = 
        Add(
            Add(
                Sub(
                    Const 2.0, 
                    X
                ), 
                Mul(
                    X, 
                    Add(Const 4.0, X)
                )), 
            X)

    
    (expr, 3.0) |> trans |> intpProg
