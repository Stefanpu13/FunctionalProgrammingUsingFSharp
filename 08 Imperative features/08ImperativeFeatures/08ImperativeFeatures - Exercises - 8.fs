namespace Exercises8
module E = 
    open System.Collections.Generic
    (* 8.8
        Declare a DictionaryFold function. The type should correspond to the type of Map.fold.
    *)
    
    let dictionaryFold folder state (dictionary: IDictionary<'a, 'b>) = 
        let mutable res = state
        
        for KeyValue(key, value) in dictionary do
            res <- folder res key value
        res
