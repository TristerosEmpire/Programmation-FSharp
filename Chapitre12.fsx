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
[Bleu; Vert; Rouge] |> Seq.iter (liste.Push)
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

// accès aux types génériques
let t1 = typeof<seq<'a>>
let t2 = typedefof<seq<'a>>
let t3 = typeof<seq<float>>

// accès aux méthodes et propriétés d'un type
let m = typeof<Moteur>.GetMethods()

Array.ForEach( m ,(fun element -> printfn "%A" element))

(*
 on va créer une fonction qui prendra une instance d'un type
 et retournera une chaine comprenant les méthodes du type et ses propriétés.
 Signature : descriptionDeType : element:'a -> unit
*)

let descriptionDeType (element:'a)  =
    let e = element.GetType()

    let methodes =
        e.GetMethods() |> Array.fold (fun chaine methode -> chaine + sprintf "\n\t%s" methode.Name) ""

    let proprietes =
        e.GetProperties() |> Array.fold (fun chaine propriete -> chaine + sprintf "\n\t%s" propriete.Name) ""

    let champs =
        e.GetFields() |> Array.fold (fun chaine champs -> chaine + sprintf "\n\t%s" champs.Name) ""

    printfn "Methodes :\n\t%s" methodes
    printfn "\nPropriétés :\n\t%s" proprietes
    printfn "\nChamps :\n\t%s" champs

descriptionDeType moteur

// version alternative : descriptionType' : element:Type -> unit
// le Type est obtenu avec la fonction typeof<_>

let descriptionType' (element:Type) =
    let methodes =
        element.GetMethods() |> Array.fold (fun chaine methode -> chaine + sprintf "\n\t%s" methode.Name) ""

    let proprietes =
        element.GetProperties() |> Array.fold (fun chaine propriete -> chaine + sprintf "\n\t%s" propriete.Name) ""

    let champs =
        element.GetFields() |> Array.fold (fun chaine champs -> chaine + sprintf "\n\t%s" champs.Name) ""

    printfn "Methodes :\n\t%s" methodes
    printfn "\nPropriétés :\n\t%s" proprietes
    printfn "\nChamps :\n\t%s" champs

descriptionType' typeof<Moteur>

// version complète
let descriptionComplete (element : Type) =
    let flags =
        BindingFlags.Public     ||| BindingFlags.NonPublic |||
        BindingFlags.Instance   ||| BindingFlags.Static    |||
        BindingFlags.DeclaredOnly

    let methodes =
        element.GetMethods(flags)
        |> Array.fold (fun chaine methode -> chaine + sprintf " %s" methode.Name) ""

    let proprietes =
        element.GetProperties(flags)
        |> Array.fold (fun chaine prop -> chaine + sprintf " %s" prop.Name) ""

    let champs =
        element.GetFields(flags)
        |> Array.fold (fun chaine champs -> chaine + sprintf " %s" champs.Name) ""

    printfn "Nom : %s" element.Name
    printfn "Méthodes : \n\t%s\n" methodes
    printfn "Propriétés : \n\t%s\n" proprietes
    printfn "Champs : \n\t%s\n" champs

descriptionComplete typeof<Moteur>