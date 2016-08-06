// POO APPLIQUEE
open System

// Surcharge des opérateurs
[<Measure>]
type ML

type Bouteille(volume: float<ML>) =
    new() = new Bouteille(0.0<ML>)

    member this.Volume = volume

    override this.ToString() = sprintf "%.1f ml" (float this.Volume)

    static member (+) (bg: Bouteille, bd: Bouteille ) =
        new Bouteille(bg.Volume + bd.Volume)

    static member (-) (bg: Bouteille, bd: Bouteille) = 
        new Bouteille(bg.Volume-bd.Volume)
    