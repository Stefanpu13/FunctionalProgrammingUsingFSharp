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