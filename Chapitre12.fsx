// Attributs
open System
open System.Reflection
open System.Collections.Generic

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

// attributs-cibles
// niveau assembly
[<
    assembly:AssemblyDescription("Hello.exe");
    assembly:AssemblyCompany("NoCompany Corp.");
    assembly:AssemblyCopyright("Public Domain - no \169");
>]
do ()

// Définir ses propres atrributs
/// On fournit une description pour une classe donnée
type DescriptionClasseAttribute(description) = 
    inherit System.Attribute()
    member this.Description = description

/// On fournit une description pour une méthode donnée
type DescriptionMethodeAttribute(desc) = 
    inherit System.Attribute()
    member this.Description = desc

/// Application de nos nouveaux attributs

type Pixels =
     | Rouge
     | Vert
     | Bleu

/// On récrée une sorte de Stack<T> avec une List<T> 

[<DescriptionClasse("Représente une pile de pixels")>]
type PixelStack()=
    let listeDePixels = new List<Pixels>()

    [<DescriptionMethode("Ajoute sur la pile un nouveau pixel")>]
    member this.Push px = listeDePixels.Insert(0, px)

    [<DescriptionMethode("Accède au premier élément de la pile")>]
    member this.First = listeDePixels.[0]

    [<DescriptionMethode("Retire le premier élément de la pile et retourne la valeur rétirée")>]
    member this.Pop () = let px = listeDePixels.[0]
                         listeDePixels.RemoveAt(0)
                         px

    [<DescriptionMethode("Comptage du nombre d'éléments dans la liste")>]
    member this.Count = listeDePixels.Count

let liste = new PixelStack ()
[Bleu; Vert; Rouge] |> Seq.iter (fun x -> liste.Push x ) 
liste.First
liste.Pop ()
printfn "Nombre de pixels présents sur la pile : %A" liste.Count
liste.First

// Type et réflexion (type reflection)
// typeof<_> et GetType()
typeof<PixelStack>

type Moteur() =
    member this.Cylindres = 8
    member this.NumeroDeSerie = "JWM-0123"

let type1 = typeof<Moteur>
let moteur = new Moteur()
let type2 = moteur.GetType()

type1 = type2
type1.Name

let typePixelStack = typeof<PixelStack>
typePixelStack.Name