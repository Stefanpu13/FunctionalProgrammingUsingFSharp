module Exercises2Tests
open NUnit.Framework
open FsUnitTyped
open Exercises2.E


[<TestFixture>]
type ``Addition Tests``() = 

    [<Test>]
    member test.``When pences sum is more than 12, that is m1=(12, 10, 10), m2=(12, 11, 7), output should be (25,1,11)``() = 
        let m1 = MoneyTriple(12, 10, 10)
        let m2 = MoneyTriple(12, 11, 7)

        m1 +& m2 |> shouldEqual (MoneyTriple (25,2,5))    

        let m3 = {Pound = 12; Shilling = 10; Pence = 10}
        let m4 = {Pound = 12; Shilling = 11; Pence = 7}

        m3 +. m4 |> shouldEqual {Pound = 25; Shilling = 2; Pence = 5}

    [<Test>]
    member test.``When shillings sum is more than 20, that is m1=(12, 10, 10), m2=(12, 11, 1), output should be (25,1,11)``() = 
        let m1 = MoneyTriple(12, 10, 10)
        let m2 = MoneyTriple(12, 11, 1)

        m1 +& m2 |> shouldEqual (MoneyTriple (25,1,11))    

        let m3 = {Pound = 12; Shilling = 10; Pence = 10}
        let m4 = {Pound = 12; Shilling = 11; Pence = 1}

        m3 +. m4 |> shouldEqual {Pound = 25; Shilling = 1; Pence = 11}

    [<Test>]
    member test.``When pences sum is more than 24, that is m1=(12, 10, 15), m2=(12, 7, 11), output should be (24,19,2)``() = 
        let m1 = MoneyTriple(12, 10, 15)
        let m2 = MoneyTriple(12, 7, 11)

        m1 +& m2 |> shouldEqual (MoneyTriple (24,19,2))    

        let m3 = {Pound = 12; Shilling = 10; Pence = 15}
        let m4 = {Pound = 12; Shilling = 7; Pence = 11}

        m3 +. m4 |> shouldEqual {Pound = 24; Shilling = 19; Pence = 2}

    [<Test>]
    member test.``When shillings sum is more than 40, that is m1=(12, 30, 10), m2=(12, 21, 1), output should be (26,11,11)``() = 
        let m1 = MoneyTriple(12, 30, 10)
        let m2 = MoneyTriple(12, 21, 1)

        m1 +& m2 |> shouldEqual (MoneyTriple (26,11,11))    

        let m3 = {Pound = 12; Shilling = 30; Pence = 10}
        let m4 = {Pound = 12; Shilling = 21; Pence = 1}

        m3 +. m4 |> shouldEqual {Pound = 26; Shilling = 11; Pence = 11}     

    
    [<Test>]
    member test.``When remainder in pences increases pounds by one, that is m1=(12, 12, 11), m2=(12, 7, 1), output should be (25,0,0)``() = 
        let m1 = MoneyTriple(12, 12, 11)
        let m2 = MoneyTriple(12, 7, 1)

        m1 +& m2 |> shouldEqual (MoneyTriple (25,0,0))    

        let m3 = {Pound = 12; Shilling = 12; Pence = 11}
        let m4 = {Pound = 12; Shilling = 7; Pence = 1}

        m3 +. m4 |> shouldEqual {Pound = 25; Shilling = 0; Pence = 0}     

// Substraction Tests
[<TestFixture>]
type ``Substraction Tests``() = 

    [<Test>]
    member test.``When m1=(12, 10, 10), m2=(12, 9, 7), output should be (0,1,3)``() = 
        let m1 = MoneyTriple(12, 10, 10)
        let m2 = MoneyTriple(12, 9, 7)

        m1 -& m2 |> shouldEqual (MoneyTriple (0,1,3))    

        let m3 = {Pound = 12; Shilling = 10; Pence = 10}
        let m4 = {Pound = 12; Shilling = 9; Pence = 7}

        m3 -. m4 |> shouldEqual {Pound = 0; Shilling = 1; Pence = 3}
        
    [<Test>]
    member test.``When pences2 < pences2, that is m1=(13, 10, 9), m2=(12, 9, 11), output should be (1,0,10)``() = 
        let m1 = MoneyTriple(13, 10, 9)
        let m2 = MoneyTriple(12, 9, 11)

        m1 -& m2 |> shouldEqual (MoneyTriple (1,0,10))    

        let m3 = {Pound = 13; Shilling = 10; Pence = 9}
        let m4 = {Pound = 12; Shilling = 9; Pence = 11}

        m3 -. m4 |> shouldEqual {Pound = 1; Shilling = 0; Pence = 10}

    [<Test>]
    member test.``When shillings1 < shillings2, that is m1=(13, 10, 9), m2=(12, 16, 8), output should be (0,14,1)``() = 
        let m1 = MoneyTriple(13, 10, 9)
        let m2 = MoneyTriple(12, 16, 8)

        m1 -& m2 |> shouldEqual (MoneyTriple (0,14,1))    

        let m3 = {Pound = 13; Shilling = 10; Pence = 9}
        let m4 = {Pound = 12; Shilling = 16; Pence = 8}

        m3 -. m4 |> shouldEqual {Pound = 0; Shilling = 14; Pence = 1}

    [<Test>]
    member test.``When m1 < m2 and pences1 < pences2, that is m1=(12, 9, 8), m2=(12, 9, 9), output should be (0,0,-11)``() = 
        let m1 = MoneyTriple(12, 9, 8)
        let m2 = MoneyTriple(12, 9, 9)        

        m1 -& m2 |> shouldEqual (MoneyTriple (0, 0,-1))    
        
        let m3 = {Pound = 12; Shilling = 9; Pence = 8}
        let m4 = {Pound = 12; Shilling = 9; Pence = 9}

        m3 -. m4 |> shouldEqual {Pound = 0; Shilling = 0; Pence = -1}

    [<Test>]
    member test.``When m1 < m2 and shillings1 < shillings2, that is m1=(12, 9, 8), m2=(12, 10, 8), output should be (0,-1,0)``() = 
        let m1 = MoneyTriple(12, 9, 8)
        let m2 = MoneyTriple(12, 10, 8)        

        m1 -& m2 |> shouldEqual (MoneyTriple (0,-1,0))    
        
        let m3 = {Pound = 12; Shilling = 9; Pence = 8}
        let m4 = {Pound = 12; Shilling = 10; Pence = 8}

        m3 -. m4 |> shouldEqual {Pound = 0; Shilling = -1; Pence = 0}

    [<Test>]
    member test.``When m1 < m2 and pounds1 < pounds2, that is m1=(12, 9, 8), m2=(13, 9, 8), output should be (-1, 0 ,0)``() = 
        let m1 = MoneyTriple(12, 9, 8)
        let m2 = MoneyTriple(13, 9, 8)        

        m1 -& m2 |> shouldEqual (MoneyTriple (-1,0,0))    
        
        let m3 = {Pound = 12; Shilling = 9; Pence = 8}
        let m4 = {Pound = 13; Shilling = 9; Pence = 8}

        m3 -. m4 |> shouldEqual {Pound = -1; Shilling = 0; Pence = 0}

    [<Test>]
    member test.``When m1 < m2 and pences1 > pences1, that is m1=(12, 9, 8), m2=(13, 9, 6), output should be (0, -19 ,-10)``() = 
        let m1 = MoneyTriple(12, 9, 8)
        let m2 = MoneyTriple(13, 9, 6)        

        m1 -& m2 |> shouldEqual (MoneyTriple (0, -19 ,-10))    
        
        let m3 = {Pound = 12; Shilling = 9; Pence = 8}
        let m4 = {Pound = 13; Shilling = 9; Pence = 6}

        m3 -. m4 |> shouldEqual {Pound = 0; Shilling = -19; Pence = -10}

    [<Test>]
    member test.``When m1 < m2 and shillings1 > shillings1, that is m1=(12, 11, 8), m2=(13, 9, 8), output should be (0, -18 , 0)``() = 
        let m1 = MoneyTriple(12, 11, 8)
        let m2 = MoneyTriple(13, 9, 8)        

        m1 -& m2 |> shouldEqual (MoneyTriple (0, -18 , 0))    
        
        let m3 = {Pound = 12; Shilling = 11; Pence = 8}
        let m4 = {Pound = 13; Shilling = 9; Pence = 8}

        m3 -. m4 |> shouldEqual {Pound = 0; Shilling = -18; Pence = 0}

    [<Test>]
    member test.``When m1 < m2 and shillings1 > shillings2 and pences1 > pences1, that is m1=(12, 11, 8), m2=(13, 9, 6), output should be (0, -17 , -10)``() = 
        let m1 = MoneyTriple(12, 11, 8)
        let m2 = MoneyTriple(13, 9, 6)        

        m1 -& m2 |> shouldEqual (MoneyTriple (0, -17 , -10))    
        
        let m3 = {Pound = 12; Shilling = 11; Pence = 8}
        let m4 = {Pound = 13; Shilling = 9; Pence = 6}

        m3 -. m4 |> shouldEqual {Pound = 0; Shilling = -17; Pence = -10}