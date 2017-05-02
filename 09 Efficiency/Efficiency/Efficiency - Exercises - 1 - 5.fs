namespace Exercises1
module E = 

    (* 9.1
        Consider the function g declared on Page 202 and the stack and heap after the evaluation of g 2
        shown in Figure 9.2. Reproduce this resulting stack and heap by a systematic application of push
        and pop operations on the stack, and heap allocations that follow the step by step evaluation of
        g 2.

        let xs = [1; 2]    

        let rec g = function
            | 0 -> xs
            | n -> let ys = n::g(n-1)
            List.rev ys

        
       stack          heap
1. 

        g    -------> "closure for g"
sf0     it   -------> ?
        xs   -------> |1|| | ===> |2|| |
---------------------------------------------
2. 

sf1     n=2
        ys   -------> |2|| | ===> g 1     

        g    -------> "closure for g"
sf0     it   -------> ?
        xs   -------> |1|| | ===> |2|| |
---------------------------------------------        
2. 

sf2     n=1
        ys   -------> |1|| | ===> g 0
                       ^========
                               | 
sf1     n=2                    |
        ys   -------> |2|| | ===      

        g    -------> "closure for g"
sf0     it   -------> ?
        xs   -------> |1|| | ===> |2|| |

---------------------------------------------
3. 


sf3     n=0          
        ys   -------> xs
                      ^        
                      |========
sf2     n=1                   |  
        ys   -------> |1|| | ==
                       ^
                       |======== 
sf1     n=2                    |
        ys   -------> |2|| | ===      

        g    -------> "closure for g"
sf0     it   -------> ?
        xs   -------> |1|| | ===> |2|| |
---------------------------------------------
4. 

sf2     n=1                   
        ys   -------> |1|| | ===> |1|| | ===> |2|| |
        rYs  -------> |2|| | ===> |1|| | ===> |1|| |
                       ^
                       |======== 
sf1     n=2                    |
        ys   -------> |2|| | ===      

        g    -------> "closure for g"
sf0     it   -------> ?
        xs   -------> |1|| | ===> |2|| |
---------------------------------------------
5.                     
                      
                      |1|| | ===> |1|| | ===> |2|| | +


sf1     n=2                    
        ys   -------> |2|| | ===> |2|| | ===> |1|| | ===> |1|| |     
        rYs  -------> |1|| | ===> |1|| | ===> |2|| | ===> |2|| |       

        g    -------> "closure for g"
sf0     it   -------> ?
        xs   -------> |1|| | ===> |2|| |
---------------------------------------------
6. 

                      |1|| | ===> |1|| | ===> |2|| | +    
                      |2|| | ===> |2|| | ===> |1|| | ===> |1|| | +                 
                     

        g    -------> "closure for g"
sf0     it   -------> |1|| | ===> |1|| | ===> |2|| | ===> |2|| |   
        xs   -------> |1|| | ===> |2|| |

    *)


    let add a b = a + b

    (* 9.2
        Show that the gcd function on Page 16 is iterative.

        let rec gcd = function
        | (0,n) -> n
        | (m,n) -> gcd(n % m,m)

        The form of an iterative function is (see p. 209, 9.5):
        let rec g z = if p z then g(f z) else h z
        The idea is that each call to 'g' is tail call

        So 'gcd' can be rewritten as:
        let rec gcd (n, m) = 
            if n <> 0 then gcd( n % m, m)
            else m

        'gcd'can also be rewritten as (slighty differnt, but again in iterative form):
        let rec gcd (n, m) = 
            if n = 0 then m
            else gcd( n % m, m)
        
        Thus, 'gcd' is iterative function.
    *)


    (* 9.3
        Declare an iterative solution to exercise 1.6. (
            1.6 Declare a recursive function sum: int * int -> int, where
            sum(m, n) = m + (m + 1) + (m + 2) + · · · + (m + (n − 1)) + (m + n)        

        
        let sum (m, n)= 
            let rec sum state = function
            | (m, neg) when neg < 0 -> 0
            | (m, 0) -> state + m
            | (m, n) -> sum (state + m + n) (m, n-1)

            sum 0 (m, n)

        let rec sumIter (m, n) = 
            let rec sum state (m, n) = 
                if n < 0 then 0
                else if n = 0 then state + m
                else sum (state + m + n) (m, n-1)
            sum 0 (m, n)
    *)

    (* 9.4
        Give iterative declarations of the list function List.length.

        let length l = 
            let rec length counter l = 
                if (not (List.isEmpty l)) then length (counter + 1) (List.tail l)
                else counter
            
            length 0 l
    *)

    (* 9.5
        Express the function List.fold in terms of an iterative function itfold iterating a function
        of type ’a list * ’b -> ’a list * ’b.

        let fold folder state l =
            let rec itfold cont = function
                | (res, []) -> cont (res, [])
                | (res, x::xs) -> itfold (fun (r, ls) -> cont ((folder r x), ls)) (res, xs)

            (state, l) 
                |> itfold id  
                |> fst


        fold (fun r el ->10 * r + el) 0 [1;2;3;4]
    *)    

