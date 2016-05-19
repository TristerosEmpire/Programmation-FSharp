// PROGRAMMATION ORIENTEE OBJET
open System
open System.IO

// Présentation de System.Object
// surcharge de ToString ()

type Ponctuation =
    | Virgule
    | Point
    | PointInterrogation
    | PointExclamation
    override this.ToString () =
        match this with
        | Virgule               -> "Virgule (,)"
        | Point                 -> "Point (.)"
        | PointInterrogation    -> "PointInterrogation (?)"
        | PointExclamation      -> "PointExclamation (!)"

let x = Virgule
x.ToString ()