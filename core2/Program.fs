// Learn more about F# at http://fsharp.org
namespace Essai
open System

module Programmes =
    [<EntryPoint>]
    let main argv =
        printfn "Premier programme avec utilisation d'une bibliothèque."
        argv |> Array.map (int >> Array.map Bibliotheque.decremente)
             |> Array.iter (printfn "%i")
        0 // return an integer exit code
