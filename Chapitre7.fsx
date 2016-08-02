// PROGRAMMATION FONCTIONNELLE APPLIQUEE
open System
open System.IO
open System.Text.RegularExpressions
open System.Collections.Generic

// ACTIVE PATTERNS
// Cas simple :

let (|ToColor|) (nom: string) =
    match nom with
    | "rouge" -> System.Drawing.Color.Red
    | "bleu"  -> System.Drawing.Color.Blue
    | "blanc" -> System.Drawing.Color.White
    | _       -> failwith "Erreur : couleur inconnue"

let (ToColor r) = "rouge"

let (|ExtensionFichier|) chemin = Path.GetExtension chemin

let determineType (chemin:string) =
    match chemin with
    // sans AP:
    | chemin when  Path.GetExtension chemin = ".txt" -> printfn "Fichier texte."
    // avec AP
    | ExtensionFichier ".jpg"
    | ExtensionFichier ".png"
    | ExtensionFichier ".gif" -> printfn "Fichier image."
    | ExtensionFichier inconnu -> printfn "Format de fichier inconnu : %s" inconnu

// cas partiels
// 1er exemple : réécriture du cas simple qui génère une exception en cas partiel
let (|ToInteger|_|) x =
    let succes, rslt = Int32.TryParse x
    if succes then Some(rslt)
    else None

let isFour x =
    match x with
    | ToInteger 4 -> true
    | _       -> false

//2nd exemple (avec refactorisation du code original)

let checkValue (succes, resultat) =
    if succes then Some resultat
    else None

let (|ToBool|_|) x =
    checkValue (Boolean.TryParse x)

let (|ToInt|_|) x =
    checkValue (Int32.TryParse x)

let (|ToFloat|_|) x =
    checkValue (Double.TryParse x)

let convertionString s =
    match s with
    | ToBool  resultat1 -> printfn "%s est un booléen ayant pour valeur %b" s resultat1
    | ToInt   resultat2 -> printfn "%s est un booléen ayant pour valeur %d" s resultat2
    | ToFloat resultat3 -> printfn "%s est un booléen ayant pour valeur %f" s resultat3
    | _                 -> printfn "%s est d'un type inconnu" s

// Active Patterns paramétrés: utilise toujours le type option

let (|RegexMatch3|_|) (pattern:string) (input:string) =
    let result = Regex.Match (input,pattern)
    if result.Success then
        match (List.tail [for g in result.Groups -> g.Value]) with
        | fst::snd::trd::[] -> Some (fst, snd, trd)
        | [] -> failwith <| "Match réussi mais aucun groupe n'a été trouvé.\nUtiliser (.*) pour capturer des groupes."
        | _ -> failwith "Match réussi mais n'a pas trouver exactement ces groupes."
        else
            None

let parseTime input = 
    match input with
    | RegexMatch3 "(\d+)/(\d+)/(\d\d\d\d)" (month, day, year)
    | RegexMatch3 "(\d\d\d\d)-(\d+)-(\d+)" (year, month, day)
        -> Some (new DateTime (int year, int month, int day))
    | _ -> None

// Active Patterns à cas multiples

let (|Pair|Impair|) entier = 
    match (entier % 2) with
    |  0 -> Pair
    | _  -> Impair

let estPair valeur =
    match valeur with
    | Pair v -> true
    | _ -> false

let estImpair valeur =
    not (estPair valeur)

let (|Positif|Negatif|Zero|) input =
    if input >  0 then Positif
    elif input < 0 then Negatif
    else Zero

let controleEtat input =
    match input with
    | Positif input -> printfn "Valeur positive."
    | Negatif input -> printfn "Valeur négative."
    | Zero input    -> printfn "La valeur est égale à zéro."

let (|Repertoire|Fichier|Autre|) (input:string) =

    let d = new DirectoryInfo(input)
    let f = new FileInfo(input)

    match input with
    | _ when d.Exists -> Repertoire d
    | _ when f.Exists -> Fichier f
    | _ -> Autre

let verifFichier input =
    match input with
    | Repertoire obj -> printfn "%A est un répertoire (créé le %A)" obj.Name obj.CreationTime
    | Fichier obj -> printfn "%A est un fichier" obj.Name
    | Autre -> printfn "Autre"

//-------------------------------------------
(*
 on peut utiliser les OU logiques | et ET logiques & avec les AP dans le pattern matching : COMBINAISON 
 ou on peut utiliser les AP en les imbriquant : reprise du code complet
*)
#r "System.Xml.dll"
open System.Xml

// filtre un élément XML avec un AP partiel paramétré
let (|Elem|_|) nom (entree:XmlNode) =
    if entree.Name = nom then Some(entree)
    else None

// récupère les attributs d'un élément avec un AP à cas simple
let (|Attributs|) (entree: XmlNode) = entree.Attributes

// filtre un attribut générique avec un AP à cas simple et paramétré
let (|Attr|) nomAttribut (entree: XmlAttributeCollection) = 
    match entree.GetNamedItem(nomAttribut) with
    | null -> failwithf "Attribut %A non trouvé." nomAttribut
    | attr -> attr.Value

// ce que nous allons parser 
type Part = 
    | Widget of float
    | Sprocket of string*int

let ParseXmlNode element = 
    match element with
    | Elem "Widget" xmlElement ->
        match xmlElement with
        | Attributs xmlElementAttributs ->
            match xmlElementAttributs with
            | Attr "Diametre" diametre
                -> Widget (float diametre)
    // version alternative sans aucun doute plus clair et plus efficace :
    //| Elem "Widget" (Attributs (Attr "Diametre" diametre)) -> Widget (float diametre)
    | Elem "Sprocket" (Attributs (Attr "Modele" modele & Attr "NumeroSerie" ns))
        -> Sprocket (modele, (int ns))
    | _ -> failwith "Elément inconnu"

let xmlDoc = 
    let doc  = new System.Xml.XmlDocument();
    let txtXML = "<?xml version=\"1.0\" encoding=\"utf-8\"?>
        <Parts>
            <Widget Diametre='5.0' />
            <Sprocket Modele='A' NumeroSerie='147' />
            <Sprocket Modele='B' NumeroSerie='302' />
        </Parts>"
    doc.LoadXml(txtXML)
    doc

xmlDoc.DocumentElement.ChildNodes |> Seq.cast<XmlElement> |> Seq.map ParseXmlNode;;

//-------------------------------------------

// Le shadowing intentionnel
// Rappel :
let testShadow() =
    let x = 'a';
    let y = x;
    let x = "chaine";
    let x = 42;
    (x,y)

// dépassement arithmétique
let valeurMax = System.Int32.MaxValue
valeurMax + 1
// shadowing en utilisant le module Checked
open Checked
valeurMax + 1

// controle de l'usage des modules : [<RequireQualifiedAccess>]
[<RequireQualifiedAccess>]
module Truc =
    let valeur1 = 1

[<RequireQualifiedAccess>]
module Bidule = 
    let valeur1 = 2

// MAITRISE DES LISTES NON-MUTABLES
// (::) vs (@) : https://blogs.msdn.microsoft.com/chrsmith/2008/07/10/mastering-f-lists/
// on veut filtrer une liste où des éléments consécutifs ne peuvent pas être similaires 
// fold et @ : faible perfs
let listeUnique1 liste = 

    let fonctionPliage acc item = 
        let dernier, listeNDup = acc
        match dernier with
        | None -> Some(item), [item]
        | Some(valeur) when valeur = item -> Some(valeur), listeNDup
        | Some(valeur) -> Some(item), listeNDup @ [item]
    
    let (_, listeNonDupliquee) = List.fold fonctionPliage (None, []) liste
    listeNonDupliquee

// foldBack et :: : meilleures performances et un code bien plus léger et lisible
let listeUnique2 liste =

    let fonctionPliage item acc = 
        match acc with 
        | [] -> [item]
        | x::xs when x <> item -> item :: acc
        | x::xs -> acc
    
    List.foldBack fonctionPliage liste []

(* RAPPEL : signatures de List.fold et List.foldBack
 ---> fold ('a -> 'b -> 'a) -> 'a -> 'b list -> 'a
 application sur la liste de gauche à droite

 ---> foldBack ('a -> 'b -> 'a) -> 'a list -> 'b -> 'b
 application sur la liste de droite à gauche
*)

// le code précédent est bien plus élégant qu'une structure comme suit :

let supprimeDuplicatas liste =
    let rec aux x xs lst =
        match xs with
        | y::ys when x <> y -> aux y ys (y::lst)
        | y::ys             -> aux y ys lst
        | y::[] when x <> y -> y::lst
        | y::[]             -> lst
        | []                -> lst
    let (premier, queue) = (List.head liste, List.tail liste)
    aux premier queue [premier] |> List.rev

// RECURSION TERMINALE

// stack overflow avec creationListeCrash 100000000
let creationListeCrash (valeurMax) =
    let rec createList (i:int) (max:int) =
        if i = max then
            []
        else i::createList (i+1) max
    createList 0 valeurMax

// aucun crash, aucune erreur.
let creationListeSansCrash(valeurMax) =
    let rec aux tableau zero depart =
        if zero = depart then
            tableau
        else
            aux ((depart-1)::tableau) zero (depart-1)

    aux [] 0 valeurMax

// factorielle avec problème :
let rec factorielle x =
    match x with
    | _ when x <= 1 -> 1
    | _ -> x * factorielle (x-1)


// factorielle avec récursion terminale
let rec factorielleRT x =
    match x with
    | _ when x <= 1 -> 1
    | _ -> let calcul = factorielleRT (x-1)
           let rslt = x * calcul
           rslt

// récursion terminale avec accumulateur
let factorielleRT2 x =
    let rec aux valeur acc =
        match valeur with
        | _ when x <= 1 -> 1
        | _ -> aux (valeur-1) (acc*valeur)
    aux x 1

// Continuations
let decompte liste =
    let rec decompteTR lst continuation =
        match lst with
        | []    -> continuation ()
        | x::xs -> decompteTR xs (fun () -> printf "%d " x 
                                            continuation ())
    decompteTR liste (fun () -> printfn "Fin !")

// continuation : arbre binaire et iteration
type EtapeContinuation<'a> =
    | Finished 
    | Etape of 'a * (unit -> EtapeContinuation<'a>)

type Arbre<'a> =
    | Empty
    | Node of Arbre<'a> * 'a * Arbre<'a>

let iter fonction arbreBinaire =
    let rec linearise arbre continuation =
        match arbre with
        | Empty -> continuation ()
        | Node (g,v,d) -> Etape( v, (fun () -> linearise g (fun () -> linearise d continuation)))

    let etapes  = linearise arbreBinaire (fun () -> Finished)

    let rec traiteEtapes etape =
        match etape with
        | Finished -> ()
        | Etape(v, suite) -> fonction v
                             traiteEtapes (suite ())
    traiteEtapes etapes

// PROGRAMMER AVEC LES FONCTIONS

// l'application partielle de fonction : les fonctions passées en argument
let fctSimple x = x + 1
let liste = [1;2;3;4]

List.map (fun x-> fctSimple x) liste;;
List.map fctSimple liste;;

let f0 b c d x = b ( c (d x));;

let f1 x = x + 1
let f2 x = x + 2
let f3 x = x + 3

List.map (fun x -> f0 f1 f2 f3 x) liste;;

List.map (f0 f1 f2 f3) liste;;

//Code redondant : utilisation des fonctions d'ordre supérieur
[<Measure>]
type euro

type Entree = {
    Nom: string;
    Prix: float<euro>;
    Calories: int}

let leMoinsCher menu = 
    List.reduce (fun acc item -> if item.Prix < acc.Prix then item
                                 else acc) menu

let leMoinsCalorique menu =
    List.reduce (fun acc item -> if item.Calories < acc.Calories then item
                                 else acc) menu

// réduction de la redondance des deux fonctions :
let prendItem fct menu =
    let reduction acc item =
        match fct acc item with
        | true -> acc
        | false -> item
    List.reduce reduction menu

let leMoinsCher2 = prendItem (fun acc item -> item.Prix < acc.Prix)
let leMoinsCalorique2 = prendItem (fun acc item -> item.Calories < acc.Calories) 

// Closures/Fermetures
//ex simple :
let mult liste valeur = List.map (fun x -> x * valeur) liste;;

//ex complexe :
type Set = 
    {
        Ajoute : int -> Set;
        Existe : int -> bool;
    }

    static member Vide =
        let rec creeSet liste =
            {
                Ajoute = (fun item -> creeSet (item :: liste))
                Existe = (fun item -> List.exists ((=) item) liste)
            }
        creeSet []

let s = Set.Vide
let s' = s.Ajoute 1
let s'' = s'.Ajoute 2
let s''' = s''.Ajoute 3;;

s.Existe 2;;
s''.Existe 2;;

// MOTIFS FONCTIONNELS
// Mémoïsation

let memoise (f : 'a -> 'b) =
    let dictionnaire = new Dictionary<'a, 'b>()
    let fonctionMemoisation (entree: 'a) : 'b =
        match dictionnaire.TryGetValue(entree) with
        | true, x -> x
        | false, _ ->
                     // évalue et ajoute le résultat dans le dictionnaire
                    let resultat = f entree
                    dictionnaire.Add(entree, resultat)
                    resultat
    fonctionMemoisation

let dodo x = 
    System.Threading.Thread.Sleep(x * 1000)
    x

#time;;

dodo 5;;
dodo 5;;

// mémoïsation de la fonction dodo
let dodoMemoise = memoise dodo;;

dodoMemoise 5;;
dodoMemoise 5;;

// mémoïsation et fonctions récursives
let mauvaiseFacon =
    let rec fib x =
        match x with
        | 0 | 1 -> 1
        | 2     -> 2
        | n     -> fib (n-1) + fib (n-2)
    memoise fib

let rec bonneFacon =
    let fib x = 
        match x with
        | 0 | 1 -> 1
        | 2     -> 2
        | n     -> bonneFacon (n-1) + bonneFacon (n-2)
    memoise fib

// valeurs fonction mutables
type Widget = 
    | Truc   of string * int
    | Bidule of string * float
     
let mutable genereWidget = 
    let compteur = ref 0
    (fun () -> incr compteur
               Truc ((sprintf "Modèle Truc1-%d" !compteur), !compteur));;
genereWidget ();;
genereWidget ();;

// Mise à jour du générateur
genereWidget <-
    let compteur = ref 0
    (fun () -> incr compteur
               Bidule((sprintf "Modèle Bidule%d" !compteur), 0.0))

genereWidget ();;
genereWidget ();;