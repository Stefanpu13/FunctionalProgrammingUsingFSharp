namespace Exersices6

module E = 

    let x = 109 ^^^ 70 ^^^ 20 ^^^ 30


    let xorb1 = (^^^)
    xorb1 ( xorb1 ( xorb1 109 70) 20) 30
    
    let xorb nums = 
        match nums with
        | [] -> failwith "two little heaps of matches"
        | [x] -> failwith "two little heaps of matches"
        | x::xs -> List.fold (^^^) x xs

    xorb [109; 70;20;30]