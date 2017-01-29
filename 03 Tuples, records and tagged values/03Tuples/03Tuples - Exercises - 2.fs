namespace Exercises2
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
        {Pound = -pound; Shilling = -shilling; Pence = -pence}
    let (-.) m1 m2 =
        let pences1 = toPencesFromRecord m1
        let pences2 = toPencesFromRecord m2
        
        match pences1 < pences2 with
        | true -> -. fromPencesToRecord (pences2 - pences1)
        | false -> fromPencesToRecord (pences1 - pences2)
