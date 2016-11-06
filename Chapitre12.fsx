// Attributs
open System

// exemple simple

[<AbstractClass>]
type Animal()=
    abstract NombreDePattes: int -> unit
    abstract Exosquelette: bool -> unit
    member this.EmissionSonore (emetUnSon: bool) =
        match emetUnSon with
        | true -> printfn "L'animal est capable d'émettre un son"
        | false -> printfn "L'animal n'émet pas de son"




// exemple avec ObsoleteAttribute
type NiveauSécurité =
    | Vert of string
    | Orange of string
    | Rouge of string

type NiveauDeDangerActuel(niveauDeSécurité: NiveauSécurité) =

    let niveau = ref niveauDeSécurité

    member this.NiveauSécurité with get () = !niveau

    // pour éviter l'erreur avec Ionide :
    [<System.ObsoleteAttribute("Dépréciée. Le niveau de sécurité ne peut pas être modifié une fois initialisé.", true)>]
    // sinon avec MonoDevelop :
    //[<Obsolete("Dépréciée. Le niveau de sécurité ne peut pas être modifié une fois initialisé.", true)>]
    member this.NiveauSécurité with set x = niveau := x


let danger = new NiveauDeDangerActuel(Vert "Ok")
danger.NiveauSécurité = Orange "Danger imminent"
printfn "%A" danger.NiveauSécurité
