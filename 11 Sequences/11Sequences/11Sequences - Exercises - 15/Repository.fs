namespace Exercises15

open Models
// open Startup

module Repository = 


    let rec findArticle ac = function
    | Register ((ac',adesc)::_) when ac=ac' -> adesc
    | Register (_::reg) -> findArticle ac (Register reg)
    | _ ->
        failwith(ac + " is an unknown article code")