// POO APPLIQUEE
open System
open System.Collections.Generic

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

    static member (~-) (b: Bouteille) = 
        new Bouteille(b.Volume * -1.0<1>)

    static member (+) (bg:Bouteille, bd:float<ML>) =
        new Bouteille(bg.Volume+bd)

    //nul besoin de spécifier comme ci-dessus le type
    static member (-) (bg:Bouteille, bd) =
        new Bouteille(bg.Volume-bd)

// les indexeurs
let (|CheckIndex|_|) (i:int) = 
    if i <1 || i > 365 then None
    else Some i

type Annee(annee: int) =
    member this.Item(index: int) =
        match index with
        | CheckIndex index -> DateTime.Parse(sprintf "1-1-%d" annee).AddDays(float (index-1))
        | _ -> failwith "Nombre de jours invalide."

let annee = new Annee(2016)
let jour = annee.[171]
jour.DayOfWeek, jour.Day, jour.Month

// index utilisant deux paramètres distincts
type Annee2(annee: int) =
    member this.Item(mois: string, jour: int) =
        let moisConverti =
            match mois.ToLower() with
            | "janvier" -> 1    | "février" -> 2    | "mars" -> 3
            | "avril"   -> 4    | "mai"     -> 5    | "juin" -> 6
            | "juillet" -> 7    | "août"    -> 8    | "septembre" -> 9
            | "octobre" -> 10   | "novembre"-> 11   | "décembre"-> 12
            | _ -> failwithf "Mois proposé (%s) n'est pas valide" mois
        DateTime.Parse(sprintf "1-1-%d" annee).AddMonths(moisConverti - 1).AddDays(float (jour-1))

let a2 = new Annee2(2015)
let alea = a2.["octobre", 30]
alea.Day, alea.Month, alea.Year;;

// indexeurs en lecture et écriture
type ModificateurDeMots(texte:string) =
    let lstLettres = new List<char>(texte)
    member this.Item
        with get index = lstLettres.[index]
        and set index nvCaractere = lstLettres.[index] <- nvCaractere

    override this.ToString() =
        new String(lstLettres.ToArray());;

let jp = new ModificateurDeMots "Jurassic Park"
jp.[10]
jp.[10] <- 'o'
jp.ToString ();;