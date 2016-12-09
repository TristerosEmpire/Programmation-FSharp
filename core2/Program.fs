namespace Essai

open System
open Bibliotheque

module Programmes =
    [<EntryPoint>]
    let main argv =
        printfn "Premier programme avec utilisation d'une bibliothèque."
        argv |> Array.map (int >> decremente)
             |> Array.iter (printfn "%i")
        0 // code retour