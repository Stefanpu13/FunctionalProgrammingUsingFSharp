namespace Tuples
module E =

    

    (* 3.2
        The former British currency had 12 pence to a shilling and 20 shillings to a pound. Declare
        functions to add and subtract two amounts, represented by triples (pounds, shillings, pence) of
        integers, and declare the functions when a representation by records is used. Declare the functions
        in infix notation with proper precedences, and use patterns to obtain readable declarations.
    *)

    // 3.2.1 Money represented with triples

    type MoneyTriple = MoneyTriple of int * int * int
    (* General Algoritm for operaitons with 
        1. convert money to pence
        2. substract / add 
        3. convert pence to money 
    *)
    let toPencesFromTriple m = 
        let (MoneyTriple(pounds,shillings, pences )) = m 
        pences + (shillings * 12) + (pounds * 12 * 20)
    let fromPencesToTriple pences =     
        let remainingPenses, shillings = pences % 12, pences / 12
        let remainingShillings, pounds = shillings % 20, shillings / 20
        
        MoneyTriple (pounds, remainingShillings, remainingPenses)
    let  (+&) m1 m2 = 
        let pences1 = toPencesFromTriple m1
        let pences2 = toPencesFromTriple m2

        fromPencesToTriple (pences1 + pences2)
    let (~~~) m = 
        let (MoneyTriple(pound1, s1, pence1)) = m
        MoneyTriple(-pound1, -s1, -pence1)
    let (-&) m1 m2 =
        let pences1 = toPencesFromTriple m1
        let pences2 = toPencesFromTriple m2
        
        match pences1 < pences2 with
        | true -> ~~~ (fromPencesToTriple (pences2 - pences1))
        | false -> fromPencesToTriple (pences1 - pences2)

    // 3.2.2 Money represented with records
    // Analogical to 3.2.1
    type MoneyRecord = {Pound: int; Shilling: int; Pence: int}
    let toPencesFromRecord m = 
        let {Pound = pounds; Shilling = shillings; Pence = pences} = m 
        pences + (shillings * 12) + (pounds * 12 * 20)
    let fromPencesToRecord pences =     
        let remainingPenses, shillings = pences % 12, pences / 12
        let remainingShillings, pounds = shillings % 20, shillings / 20

        {Pound = pounds; Shilling = remainingShillings; Pence= remainingPenses}
    let  (+.) m1 m2 = 
        let pences1 = toPencesFromRecord m1
        let pences2 = toPencesFromRecord m2

        fromPencesToRecord (pences1 + pences2)
    let (~-.) m =     
        let {Pound = pound; Shilling = shilling; Pence = pence} = m
        {Pound = -pound; Shilling = -shilling; Pence = pence}
    let (-.) m1 m2 =
        let pences1 = toPencesFromRecord m1
        let pences2 = toPencesFromRecord m2
        
        match pences1 < pences2 with
        | true -> -. fromPencesToRecord (pences2 - pences1)
        | false -> fromPencesToRecord (pences1 - pences2)



    // Test 3.3

    // Test triples
    let mTr1 = MoneyTriple(12, 10, 10)
    let mTr2 = MoneyTriple(12, 11, 1)

    mTr1 +& mTr2

    let mTr3 = MoneyTriple(11, 12, 10)
    let mTr4 = MoneyTriple(12, 12, 1)

    mTr3 -& mTr4

    // Test records
    let m1 = {Pound = 12; Shilling = 10; Pence = 1}
    let m2 = {Pound = 12; Shilling = 15; Pence = 1}

    m1 +. m2

    let m3 = {Pound = 11; Shilling = 12; Pence = 10}
    let m4 = {Pound = 12; Shilling = 13; Pence = 1}

    m3 -. m4

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
    let (&+) c1 c2 = 
        let (Complex(r1, i1)) = c1
        let (Complex(r2, i2)) = c2

        Complex (r1+r2, i1+i2)
    let (&*) c1 c2 = 
        let (Complex(r1, i1)) = c1
        let (Complex(r2, i2)) = c2

        Complex (r1*r2 - i1*i2, i1*r2+r1*i2)
    let (&-) c1 c2 = 
        let (Complex(r1, i1)) = c1
        let (Complex(r2, i2)) = c2

        Complex (r1-r2, i1-i2)

    //Operation 1/(a, b)
    let (!!!) c = 
        let (Complex(r, i)) = c
        Complex (r / (r**2.0 + i**2.0), -i / (r**2.0 + i**2.0))
    let (&/) c1 c2 = c1 &* (!!! c2)


    // Test complex numbers operations

    let c1 = Complex(1.0, 2.0) 
    let c2 = Complex(1.0, 2.0) 

    c1 &+ c2
    c1 &* c2
    let c3 = Complex(2.5, 2.0) 
    let c4 = Complex(2.0, 3.0) 

    c3 &- c4
    c3 &/ c4

    (* 3.4
        A straight line y = ax + b in the plane can be represented by the pair (a, b) of real numbers.
        1. Declare a type StraightLine for straight lines.
        2. Declare functions to mirror straight lines around the x and y-axes.
        3. Declare a function to give a string representation for the equation of a straight line.
    *)
    // 3.4.1
    type StraightLine = StraightLine of float * float

    // 3.4.2
    let mirror (StraightLine (a, b)) = StraightLine(-a, -b)
    // 3.4.3
    let straightLineToString l = 
        let (StraightLine (a, b)) = l    
        sprintf "y = %f * x + %f" a b

    straightLineToString (StraightLine (2.6, 8.0))

    (* 3.5
        Make a type Solution capturing the three capabilities for roots in a quadratic equation: two
        roots, one root and no root (cf. Section 3.5). Declare a corresponding solve function.
    *)

    type Solution = 
    | NoSolutions 
    | OneSolution of float 
    | TwoSolutions of float * float

    type Equation = Equation of float * float * float
    let solve (Equation (a, b, c)) =
        let d = b*b - 4.0*a*c
        match (a, b, c) with   
        | (_, _, _) when d < 0.0 -> NoSolutions
        | (a, _, _) when a = 0.0 -> OneSolution (-c / b) // use c, b from match clause
        | (a, b, _) when d = 0.0 -> OneSolution (-b / (2.0 * a))
        | (a, b, c) -> 
            let x1 = (-b + sqrt d) / (2.0 * a)
            let x2 = (-b - sqrt d) / (2.0 * a)
            TwoSolutions (x1, x2)
    let eq = Equation (0.0, 3.0, 1.0)

    solve eq

    (* 3.7
        Give a declaration for the area function on Page 61 using guarded patterns rather than an
        if...then...else expression.
    *)

    type Shape = 
        | Circle of float
        | Square of float
        | Triangle of float*float*float

    type Area  = 
    | Area of float
    | NotAShape of string

    let isShape = function
        | Circle r -> r > 0.0
        | Square a -> a > 0.0
        | Triangle(a,b,c) ->
        a > 0.0 && b > 0.0 && c > 0.0
        && a < b + c && b < c + a && c < a + b

    let area x =
        if not (isShape x)
        then failwith "not a legal shape" raise
        else match x with
                | Circle r -> System.Math.PI * r * r
                | Square a -> a * a
                | Triangle(a,b,c) ->
                    let s = (a + b + c)/2.0
                    sqrt(s*(s-a)*(s-b)*(s-c))

    let areaWithGuards  = function
        | y when not (isShape y) -> NotAShape "Not a legal shape"
        | x -> match x with
                | Circle r -> Area (System.Math.PI * r * r)
                | Square a ->Area ( a * a)
                | Triangle(a,b,c) ->
                    let s = (a + b + c)/2.0
                    Area (sqrt(s*(s-a)*(s-b)*(s-c)))