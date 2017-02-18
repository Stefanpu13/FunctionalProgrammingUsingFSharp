namespace Exercises9
#nowarn "2"
open Types.T
module E = 
    (* 5.9
        Declare a function to give a purchase map (see Version 3 on Page 118) on the basis of a list of
        items (from the Versions 1 and 2).
    *)
    module Purchase =         
        [<CompilerMessageAttribute ("Do not create purchase using the \"Purchase\" constructor.
        Incrementation of identical items will be skipped.", 2)>] 
        type P = Purchase of Map<ArticleCode, NoPieces>
        let create items =
            let addItem (itemCode, NoPieces addedPieces) addedItems = 
                let foundItem = Map.tryFind itemCode addedItems
                match foundItem with
                | None -> Map.add itemCode (NoPieces addedPieces) addedItems
                | Some (NoPieces currentPieces) -> 
                    Map.add itemCode (NoPieces(currentPieces + addedPieces)) addedItems
            
            Purchase (List.fold (fun addedItems item -> addItem item addedItems) (Map.ofList []) items)

            // let addedCode = Map.tryFind 
        let apply f (Purchase s) = f s
        let value s = apply id s