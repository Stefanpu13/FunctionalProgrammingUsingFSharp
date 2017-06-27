namespace Exercises15

// open Startup
open Models
open E

module Client = 

    let purchase =
        Purchase [
            Item (NoPieces 3, "21")
            Item (NoPieces 1, "20")
        ]

    makeBill purchase    