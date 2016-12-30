// Attributs
open System
open System.Reflection
open System.Collections.Generic
open System.IO

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

// Inspection des attributs : reprise du type Pixels et de PixelStack
// ATTENTION REPL : 
//bien avoir en "mémoire" les classes attributs DescriptionClasseAttribute et DescriptionMethodeAttribute 
let printDocumentation (t:Type) =
    let objPossedeType t o = (o.GetType() = t)

    let descriptionClasse : string option =
        t.GetCustomAttributes(false)
        |> Seq.tryFind (objPossedeType typeof<DescriptionClasseAttribute>)
        |> Option.map (fun attr -> (attr :?> DescriptionClasseAttribute))
        |> Option.map (fun dca -> dca.Description)

    let descriptionMethode : seq<string * string option> = 
        t.GetMethods()
        |> Seq.map (fun mi -> mi, mi.GetCustomAttributes(false))
        |> Seq.map (fun (infoMethode, attrMethode) -> 
            let attributsDeMethode =
                attrMethode |> Seq.tryFind(objPossedeType typeof<DescriptionMethodeAttribute>)
                            |> Option.map (fun attr -> (attr :?> DescriptionMethodeAttribute))
                            |> Option.map (fun dma -> dma.Description)
            infoMethode.Name, attributsDeMethode)
    let getDescription = function
        | Some d -> d
        | None   -> "Aucune descrption fournie."
    printfn "Info pour la classe %s" t.Name
    printfn "Description de classe : \n\t%s" (getDescription descriptionClasse)
    printfn "Description des méthodes:"
    descriptionMethode |> Seq.iter (fun (methodeNom, desc) -> printfn "\t%15s - %s" methodeNom (getDescription desc))

printDocumentation typeof<PixelStack>

// Réflexion et types F#

type Suite = 
    | Coeur
    | Carreau
    | Trefle
    | Pique
type JeuDeCarte =
    | As of Suite
    | Roi of Suite
    | Reine of Suite
    | Valet of Suite
    | ValeurCarte of int * Suite
    | Joker

descriptionComplete typeof<JeuDeCarte>

// ... avec les tuples
let xenon = ("Xe", 54)
open Microsoft.FSharp.Reflection
let elementsUplet = FSharpType.GetTupleElements(xenon.GetType())

// ... avec les unions discriminées
FSharpType.GetUnionCases typeof<JeuDeCarte> |> Array.iter (fun x -> printfn "%s" x.Name)

// ... avec les enregistrements
[<Measure>]
type cel // degré Celsius
type Observation = Ensoleille | Nuageux | Pluvieux
type Meteo = { Observation: Observation; Haute: float<cel>; Bas: float<cel>}
FSharpType.GetRecordFields typeof<Meteo> 
    |> Array.iter (fun x -> printfn "%A [%s] : %s" x.MemberType x.PropertyType.Name x.Name)


// Instanciation dynamique

//// instanciation dynamique de types
type WriterPoli(flux:TextWriter) =
    member this.EcritureLigne(msg:string)=
        sprintf("%s... et bonne journée.") msg |> flux.WriteLine

let consolePolie = Activator.CreateInstance(typeof<WriterPoli>, [|box Console.Out|])
(consolePolie :?> WriterPoli).EcritureLigne("Salut !")

//// Invocation dynamique
type Livre(titre:string, auteur:string) =

    let mutable (m_pageActuelle:int option) = None
    member this.Auteur = auteur
    member this.Titre = titre
    member this.PageActuelle with get () = m_pageActuelle
                             and set x = m_pageActuelle <- x
    override this.ToString () = 
        match m_pageActuelle with
        | Some(x) -> sprintf "%s de %s ouvert à la page %d" titre auteur x
        | None    -> sprintf "%s de %s n'est pas encore ouvert." titre auteur

let lectureDuSoir = new Livre("Ulysse", "James Joyce")
let pageEnCoursInfo = typeof<Livre>.GetProperty("PageActuelle")
pageEnCoursInfo.SetValue(lectureDuSoir, Some(123), [||])
lectureDuSoir.ToString()

//// Opérateurs "point d'interrogation"
let (?) (chose:obj) (propriete:string) =
    match chose.GetType().GetProperty(propriete) with
    | null -> false
    | _    -> true

// on teste :
"une chaine" ? Length
42 ? IsPrime
("une chaine castée en objet" :> obj) ? Length
lectureDuSoir ? Auteur

// récupération dynamique de la valeur d'une propriété 
let (?) (chose:obj) (propriete:string) :'a =
    let propInfo = chose.GetType().GetProperty(propriete)
    propInfo.GetValue(chose, null) :?> 'a
//important : bien indiquer le type de la valeur de retour (ici int)
let verif : int = "lectureDuSoir" ? Length

// établir une valeur dynamiquement pour une propriété
let (?<-) (chose:obj) (propriete :string) (nvValeur: 'a) =
    let propInfo = chose.GetType().GetProperty(propriete)
    propInfo.SetValue(chose, nvValeur, null)

lectureDuSoir ? PageActuelle <- Some 345
let v : int option = lectureDuSoir?PageActuelle