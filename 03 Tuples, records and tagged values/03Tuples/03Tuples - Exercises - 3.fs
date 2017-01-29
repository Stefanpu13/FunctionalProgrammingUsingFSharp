namespace Exercises3
module E = 


    (* 3.3
        The set of complex numbers is the set of pairs of real numbers. Complex numbers behave almost
        like real numbers if addition and multiplication are defined by:
        (a, b) + (c, d) = (a + c, b + d)
        (a, b) · (c, d) = (ac − bd, bc + ad)
        1. Declare suitable infix functions for addition and multiplication of complex numbers.
        2. The inverse of (a, b) with regard to addition, that is, −(a, b), is (−a,−b), and the inverse of
        (a, b) with regard to multiplication, that is, 1/(a, b), is (a/(a2+b2),−b/(a2+b2)) (provided
        that a and b are not both zero). Declare infix functions for subtraction and division of complex
        numbers.
        3. Use let-expressions in the declaration of the division of complex numbers in order to avoid
        repeated evaluation of identical subexpressions.
    *)

    type Complex = Complex of float * float

    // 3.3.1
    let (&+) (Complex(r1, i1)) (Complex(r2, i2)) = Complex (r1+r2, i1+i2)
    let (&*) (Complex(r1, i1)) (Complex(r2, i2)) = Complex (r1*r2 - i1*i2, i1*r2+r1*i2)
    let (&-) (Complex(r1, i1)) (Complex(r2, i2)) = Complex (r1-r2, i1-i2)

    //Operation 1/(a, b)
    // let (!!!) (Complex(r, i)) = Complex (r / (r**2.0 + i**2.0), -i / (r**2.0 + i**2.0))
    
    // uaing binding expressions
    let (!!!) (Complex(r, i)) = 
        let (rSquared, iSquared) = (r**2.0, i**2.0) in 
            Complex (r / (rSquared + iSquared), -i / (rSquared + iSquared))
    let (&/) c1 c2 = c1 &* (!!! c2)


    // Test complex numbers operations

    let c3 = Complex(2.5, 2.0) 
    let c4 = Complex(2.0, 3.0) 

    c3 &- c4
    c3 &/ c4
