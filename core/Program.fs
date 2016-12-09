open System


[<EntryPoint>]
let main argv =
    printfn "Hello World !"
    match (Array.length argv) with
    | 0 -> printfn "Pas d'arguments fournis au programme"
    | _ -> printfn "Il y a %i" (Array.length argv)
    0
