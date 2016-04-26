open System
open System.IO

(* Application partielle de fonction *)
let ecritFichier (nomDeFichier : string) (texte:string) =
    use fichier = new StreamWriter(nomDeFichier, true)
    fichier.WriteLine(texte)
    fichier.Close()

ecritFichier @"test.txt" "Traitement événement X..."

let ecritFichierLog = ecritFichier @"test.txt"
ecritFichierLog "Traitement événement Y..."

// fonction non partiellement appliquée : présence d'une fonction lambda
List.iter (fun x -> printfn "%d" x) [1..3];;

// même fonction sans lambda avec application partielle et curryfiée :
List.iter (printfn "%d") [1..3];;

let generePuissance baseVal = (fun exposant -> baseVal ** exposant);;
let puissanceDeDeux = generePuissance 2.0;;
puissanceDeDeux 8.0;;

// récursion
let rec factorielle x =
    if x = 1 then 1
    else x * factorielle (x-1);;

// simulation d'une boucle FOR en pgm fct
let rec forFCT corps boucles =
    if boucles <= 0 then ()
    else
        corps ()
        forFCT corps (boucles-1);;

let rec whileFCT predicat corps =
    if predicat () then corps ()
    else ();;

forFCT (fun () -> printfn "Boucle...") 3;;

// récursion mutuelle

let rec estImpaire x =
    if x = 0 then false
    elif x = 1 then true
    else estPaire (x-1)
and estPaire x =
    if x = 0 then true
    elif x = 1 then false
    else estImpaire (x-1)

estImpaire 314;;
estPaire 314;;

// opérateur pipe |>
[1..3] |> List.iter (printfn "%d")

let tailleRep rep =
    let getFiles chemin =
        Directory.GetFiles(chemin, "*.*", SearchOption.AllDirectories)

    let tailleTotale =
        rep
        |> getFiles
        |> Array.map (fun fichier -> new FileInfo(fichier))
        |> Array.map (fun info -> info.Length)
        |> Array.sum

        (* Le code ci-dessus peut-être refactorisé comme suit avec l'opérateur >>
        Opérateur qui sera utilisé dans la section suivante.
        Cette proposition a été donnée par Ionide sous Atom
        et non avec MonoDevelop

    let tailleTotale =
        rep
        |> getFiles
        |> Array.map ((fun fichier -> new FileInfo(fichier))
                       >> (fun info -> info.Length))
        |> Array.sum
*)
    tailleTotale



// opérateur de composition en avant >>
let tailleRepComposee =
    let getFiles rep = Directory.GetFiles(rep,
                                          "*.*",
                                          SearchOption.AllDirectories)
    getFiles
    >> Array.map (fun fichier -> new FileInfo(fichier))
    >> Array.map (fun info -> info.Length)
    >> Array.sum

// opérateur de pipeline en arrière <|
// Au lieu d'écrire :
printfn "Le résultat du sprintf est %A" (sprintf "(%d, %d)" 1 2)
// il sera plus élégant et lisible d'écrire le code suivant
printfn "Le résultat du sprintf est %A" <| sprintf "(%d, %d)" 1 2

// opérateur de composition en arrière <<
let carre x = x*x
let negatif x  = -x

(carre >> negatif) 10
(carre << negatif) 10

// PATTERN MATCHING

// 1er exemple :

let estPair (x:int) =
    match (x % 2) with
    | 0 -> printfn "%d est pair" x
    | _ -> printfn "%d est impair" x

// 2nd exemple

(*
Table de vérité de AND

      | TRUE | FALSE
 -----+------+------
 TRUE |   T  |   F
 -----+------+------
 FALSE|   F  |   F
      +------+------
*)

let tableDeVeriteAND x y =
    match x, y with
    | true, true -> true
    | _, _ -> false


// Litéraux : sans les littéraux la fonction salut sera toujours
// vérifiée avec Bill
[<Literal>]
let Bill = "Bill Gates"

[<Literal>]
let Alan = "Alan Turing"

let salut nom = match nom with
                | Bill -> printfn "Hello, Bill !"
                | Alan -> printfn "Hello, god of computer !"
                | x    -> printfn "Hello, mister %s" x

// les guardes avec WHEN
// le code ne fonctionne pas avec le prompt interactif de Monodevelop
// il faudra donc le charger depuis le shell
let devineLeNombre () =

    let alea = new Random()
    let nbSecret = alea.Next () % 100

    let rec etapeJeu () =
        printfn "J'ai trouvé un nombre quel est-il ? "
        let choixStr = Console.ReadLine()
        let choix = Int32.Parse(choixStr)

        match choix with
        | _ when choix > nbSecret -> printfn "Votre nombre est trop grand"
                                     etapeJeu ()
        | _ when choix = nbSecret -> printfn "Bravo le nombre était %d" nbSecret
                                     ()
        | _ when choix < nbSecret -> printfn "Votre nombre est trop petit"
                                     etapeJeu ()
        | _ -> printfn "Fin du jeu"
               ()
    //lancement du jeu
    etapeJeu ()

// Regroupement de motifs
let testVoyelles c =
    match c with
    | 'a' | 'e' | 'i' | 'o' | 'u' -> printfn "Voyelle"
    | _ -> printfn "Consonne"

let mot = "Hello"

let checkMot m =
    let rec aux word acc =
        if acc < (String.length word) then
            testVoyelles word.[acc]
            aux word (acc+1)
    aux m 0

checkMot mot

let descriptionNb x y = match x,y with
                        | 1, _ | _, 1 -> "Un des nombres est 1."
                        | (2, _) & (_ ,2) -> "Les deux nombres sont égaux à 2."
                        | _ -> "Autre"

// Syntaxe lambda et filtrage par motif
let rec listLength l =
    match l with
    | [] -> 0
    | tete::queue -> 1+ listLength queue

// peut se réécrire avec function :
let rec listLengthF =
    function
    | [] -> 0
    | tete :: queue -> 1 + listLengthF queue

// UNIONS DISCRIMINEES
type Suite = Coeur | Carreau | Trefle | Pique

let suite = [Coeur; Carreau; Trefle; Pique]

type JeuComplet =
    | As of Suite
    | Roi of Suite
    | Reine of Suite
    | Valet of Suite
    | ValeurCarte of (int * Suite)

// Construction du jeu complet via une compréhension de liste

let jeuComplet = [
    for element in suite do
        yield As element
        yield Roi element
        // on peut aussi écrire Roi(element)
        yield Reine(element)
        yield Valet element
        for valeur in [2..10] do
            yield ValeurCarte (valeur, element)
]

List.length jeuComplet

// Types récursifs
type Declaration =
    | Affiche of string
    | Sequence of Declaration * Declaration
    | Si of Expression * Declaration * Declaration

// on définit ce qu'est une expression :
and Expression =
    | Integer of int
    | PlusPetitQue of Expression * Expression
    | PlusGrandQue of Expression * Expression

let programme =
    Si (
        PlusGrandQue ((Integer 3), (Integer 1)),
        Affiche "3 plus grand que 1",
        Affiche "3 n'est pas plus grand que 1")

// unions et arbres binaires (dans leurs formes la plus simple : pas nécessairement
// équilibrés)

type ArbreBinaire =
    | Noeud of int * ArbreBinaire * ArbreBinaire
    | Vide

// fonction pour traverser notre arbre : récursion et filtrage par motif

let rec traverse (arbre:ArbreBinaire) =
    match arbre with
    | Vide -> ()
    | Noeud (valeur, gauche, droit) ->
        traverse gauche
        printfn "Noeud %d" valeur
        traverse droit


(* Notre arbre exemple
   2
 /  \
1     4
    /  \
   3    5
*)

let arbre =
    Noeud (2,
        Noeud (1, Vide, Vide),
        Noeud (4,
               Noeud (3, Vide, Vide),
               Noeud (5, Vide, Vide)
        )
    )

traverse arbre

// Filtrage et unions

type Employes =
    | Manager of string * Employes list
    | Salarie of string

let rec organisation salaries =
    match salaries with
    | Salarie nom -> printfn "%s (Employé)" nom
    | Manager (nom,[]) -> failwith "Impossible d'avoir un manager sans subalterne !"
    | Manager (nom, salaries) ->
        printfn "Le manager %s a sous son ordre : " nom
        salaries |> List.iter organisation


let tpe = Manager ("Jason", [Salarie "Allen"; Salarie "Brent"])
let tpe2 = Manager ("Alice", [
                                Manager ("Bob", [Salarie "Cathy"; Salarie "Eve"]);
                                Manager ("Greg",[ ])]
                   );;
let artisan = Salarie "Arnold"

// Méthodes, propriétés et unions

type JeuComplet2 =
    | As of Suite
    | Roi of Suite
    | Reine of Suite
    | Valet of Suite
    | Valeur of int * Suite

    member this.Value =
        match this with
        | As _ -> 11
        | Roi _ | Reine _ | Valet _ -> 10
        | Valeur (x, _) when x <=11 && x >=2 -> x
        | Valeur _ -> failwith "Carte sans valeur est impossible"

let carte = As Pique
let valeurCarte = carte.Value

// RECORDS / Enregistrements
type PersonneRec = {
    Nom : string;
    Prenom : string;
    Age: int}

let john = {
    Nom="Doe";
    Prenom="John";
    Age=23}

printfn "%s %s a %d ans" john.Prenom john.Nom john.Age

// Records : clonage
type Voiture = {
    Constructeur: string;
    Modele : string;
    Annee : int
}

let cetteAnnee = {
    Constructeur = "FSharp";
    Modele = "coupé";
    Annee = 2016
}

let prochaineAnnee = {cetteAnnee with Annee=2017}

// Records : pattern matching
let FSharpCars = function
                 | {Constructeur = "FSharp"} -> true
                 | _                   -> false

// Records : inférence de type
type Point = { X: float; Y: float}
type Vecteur3 = {X: float; Y: float; Z: float}
// on annote nos arguments avec Point
// sans annotations F# va prendre comme type de référence Vecteur3
let distance (p1:Point) (p2:Point) =
    let carre x = x * x
    sqrt <| carre (p1.X - p2.X) + carre (p1.Y - p2.Y)

// il faut la pleine qualification des champs avec le bon type : ici Point
let p:Point = { X=1.0; Y=2.0}
let origine:Point = {X=0.0; Y=0.0}

// Records : Ajout d'une méthode
type Vecteur  = {X: float; Y: float; Z: float}
                member this.Length =
                            sqrt <| this.X**2.0 + this.Y**2.0 + this.Z**2.00
let v = {X=1.0; Y=2.0; Z=3.0}
v.Length

// Evaluations paresseuses

let x = Lazy<int>.Create(fun () -> printfn "Evaluation de x..." ; 10)
// autre façon de procéder : let x = lazy (printfn "Evaluation de x ..."; 10)
let y = Lazy<int>.Create(fun () -> printfn "Evaluation de y..."; x.Value*2)
// Autre façon de procéder : let y = lazy (printfn "Evaluation de y... "; x.Value * 2)
y.Value

// SEQUENCES :
let seqEntiers = seq { for i in 1 .. System.Int32.MaxValue -> i}
seqEntiers |> Seq.take 5 |> Seq.iter (printfn "%d")

// Expressions de séquences
let alphabetVerbeux =
    seq {
            for caractere in 'A' .. 'Z' do
                printfn "Production de la lettre %c" caractere
                yield caractere
        }

// Seq.nth est déprécié depuis F#4.0 : on utilisera donc Seq.item
let cinquiemeLettre = Seq.item 5 alphabetVerbeux

//utilisation du YIELD BANG :
let rec tousLesFichiers chemin =
    seq {
            yield! Directory.GetFiles chemin
            for repertoire in Directory.GetDirectories(chemin) do
                yield! tousLesFichiers repertoire
        }
// Seq.take
let seqAleatoire = seq {
        let alea = new Random()
        while true do
            yield alea.Next()
    }

seqAleatoire |> Seq.take 3

// Seq.unfold : séquence et suite de Fibonacci
let generateurValeurs (a, b) =
    // quand la somme de a et b atteint 100 alors retourne None
    // nécessaire à l'arrêt de la fonction unfold
    if a + b > 100 then None
    else
        let valeurSuivante = a+b
        Some (valeurSuivante, (valeurSuivante, a))

Seq.unfold generateurValeurs (0,1) |> Seq.toList

// exemple l'opération d'agrégat sur une séquence avec Seq.map

let phrase = "Je suis obligé de taper cette phrase inepte mais qui se \
doit d'être longue mais alors très longue à n'en plus finir".Split([|' '|])

phrase |> Seq.map (fun mot -> (mot, String.length mot))

// REQUETES
// 1er code utilisant les pipes et les séquences
type Client = {Etat: string; CodePostal: int}

let listeCompleteClients = [
            {Etat="Californie"; CodePostal=55523};
            {Etat="New-York"; CodePostal=33324};
            {Etat="Washington"; CodePostal=23412};
            {Etat="Californie"; CodePostal=12345}
]

let clientsSelonEtat nomEtat =
    listeCompleteClients
    |> Seq.filter (fun client -> client.Etat = nomEtat)
    |> Seq.map (fun client -> client.CodePostal)
    |> Seq.distinct

clientsSelonEtat "Californie"

let clientsSelonEtat2 nomEtat=
    query {
        for client in listeCompleteClients do
            where (client.Etat = nomEtat)
            select client.CodePostal
            distinct
            }

clientsSelonEtat "Californie"

// Expressions de requête
open System.Diagnostics

let compteProcActifs = 
    query {
        for procActif in Process.GetProcesses () do
            count
            }

let consoMemoire =
    query {
        for procActif in Process.GetProcesses () do
            sortByDescending procActif.WorkingSet64
            head
            }

// sous Linux consoMemoire.MainWindowTitle -> null donc nous utiliserons le nom du processus
printfn "'%s' utilise un espace de travail de : \n%d octets" 
        consoMemoire.ProcessName 
        consoMemoire.WorkingSet64