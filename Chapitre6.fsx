// PROGRAMMATION .NET
//présentation de l'interface IDisposable
open System
open System.IO
open System.Collections.Generic

type MultiFileLogger() = 
    do printfn "En construction..."
    let m_logs = new List<StreamWriter>()

    member this.AttacheLogFichier fichier =
        let nvFichierLog = new StreamWriter(fichier, true)
        m_logs.Add(nvFichierLog)

    member this.MessageLog (msg:string) =
        m_logs |> Seq.iter (fun writer -> writer.WriteLine (msg))

    interface IDisposable with
        member this.Dispose () = 
            printfn "Nettoyage..."
            m_logs |> Seq.iter (fun writer -> writer.Close () )
            m_logs.Clear ()

let tache1 = 
    use log = new MultiFileLogger()
    log.AttacheLogFichier  (Directory.GetCurrentDirectory () + Path.DirectorySeparatorChar.ToString () + "test.txt")
    log.MessageLog "Hello, world."
    printfn "Sortie de la tâche 1..."
    ()

tache1

// Les Interfaces

type Appreciation =
    | Delicieux
    | CaVa
    | EssayonsAutreChose

type IConsommable =
    interface
        abstract Mange : unit -> unit
        abstract Appreciation : Appreciation
    end

type Pomme() =
    interface IConsommable with
        member this.Mange ()  = printfn "Beurk..."
        member this.Appreciation = EssayonsAutreChose

let pomme = new Pomme()
let iPomme = pomme :> IConsommable
iPomme.Mange ()
iPomme.Appreciation

// Deux façons de créer une interface
// 1) on laisse faire le système d'inférence
type IFaireUnTruc = 
    abstract member FaitLeTruc : unit -> unit

// 2) création explicite
type IFaireEncoreAutreChose = 
    interface
        abstract member FaitUnAutreTruc : unit -> unit
    end

// Héritage d'interface
type IHeriteDeFaitUnTruc =
    inherit IFaireUnTruc 
    abstract FaitUnTrucNouveau : unit -> unit

(* instanciation, interface et héritage
Il faudra bien veiller à implémenter TOUTES les méthodes et propriétés *)

type Truc() =
    interface IFaireUnTruc with
        override this.FaitLeTruc () = printfn "Je fais le truc !"

    interface IHeriteDeFaitUnTruc with
        override this.FaitUnTrucNouveau () = printfn "Là, je fais un truc nouveau..."

let truc = new Truc();;
(truc :> IFaireUnTruc).FaitLeTruc ();;
(truc :> IHeriteDeFaitUnTruc).FaitUnTrucNouveau ();;

// Expression d'objets pour interfaces
type Personne = 
    {
        Prenom : string; Nom :string 
    }
    override this.ToString() = sprintf "%s, %s" this.Nom this.Prenom

let groupe = new List<_> (
                            [|
                                {Prenom = "Greg"; Nom="Gamma"}
                                {Prenom = "Alice"; Nom="Alpha"}
                                {Prenom = "Bob" ; Nom="Beta"}
                            |]
)

let printPersonne () = Seq.iter (fun personne -> printfn "%s\n" (personne.ToString ())) groupe
printPersonne ()
printfn "Tri initial sur les prénoms : "
groupe.Sort(
    {
        new IComparer<Personne> with
            member this.Compare(l, r) =
                if l.Prenom > r.Prenom then   1
                elif l.Prenom = r.Prenom then 0
                else                         -1
     }
)
printfn "Résultat : "
printPersonne ()

printfn "Nouveau tri sur les noms :"
groupe.Sort(
    {
        new IComparer<Personne> with
            member this.Compare(l, r) =
                if l.Nom > r.Nom then   1
                elif l.Nom = r.Nom then 0
                else                   -1
    }
)
printfn "Résultat :"
printPersonne ()

// Expressions d'objets et classes dérivées
[<AbstractClass>]
type Sandwich() =
    abstract member Ingredients : string list
    abstract member Calories : int

// création d'un objet via une expression d'objet:
let dejeuner = {
    new Sandwich() with
        member this.Ingredients = ["Jambon"; "Beurre"]
        member this.Calories = 400
}
dejeuner.Ingredients

// Méthodes d'extension 


// (1024).ToHexString()
// ne fonctionne pas donc on va étendre le module :

type System.Int32 with
    member this.ToHexString () = sprintf "0x%x" this


// Etendre les modules

module List =
    let rec skip n liste = 
        match n, liste with
        | _, [] ->    []
        | 0, liste -> liste
        | n, x::xs -> skip (n-1) xs

module Seq =
    let rec reverse (s: seq<'a>) =
        let stack = new Stack<'a>()
        s |> Seq.iteri stack.Push
        seq {
                while stack.Count > 0 do
                        yield stack.Pop ()
            }

// ENUMERATIONS 

// -> Création
type PieceEchec =
    | Vide = 0
    | Pion = 1
    | Cavalier = 3
    | Fou = 4
    | Tour = 5
    | Reine = 8
    | Roi = 1000000

// -> Utilisation
int PieceEchec.Cavalier;;

let creationEchiquier () =
    // création échiquier vide
    let echiquier = Array2D.init 8 8 ( fun _ _ -> PieceEchec.Pion)
    // placement des pièces
    for i = 0 to 7 do
        echiquier.[1,i] <- PieceEchec.Pion
        echiquier.[6,i] <- enum<PieceEchec> (-1* int PieceEchec.Pion)

    // placement des pièces noires
    [|PieceEchec.Tour; PieceEchec.Cavalier; PieceEchec.Fou; 
    PieceEchec.Reine; PieceEchec.Roi; PieceEchec.Fou; 
    PieceEchec.Cavalier; PieceEchec.Tour|] |> Array.iteri (fun index piece -> echiquier.[0, index] <- piece)

    //placement des blancs
    [|PieceEchec.Tour; PieceEchec.Cavalier; PieceEchec.Fou; 
    PieceEchec.Roi; PieceEchec.Reine; PieceEchec.Fou; 
    PieceEchec.Cavalier; PieceEchec.Tour|] |> Array.iteri 
                (fun index piece -> echiquier.[7, index] <- enum<PieceEchec>(-1 * int piece))

    // retourne l'échiquier
    echiquier

let isPion piece =
    match piece with
    | PieceEchec.Pion -> true
    | _               -> false

let isPion2 (piece:PieceEchec) =
    match int piece with
    | 1 -> true
    | -1 -> true
    | _ -> false

let pieceInvalide = enum<PieceEchec> (42)
System.Enum.IsDefined(typeof<PieceEchec>, pieceInvalide)
System.Enum.IsDefined(typeof<PieceEchec>, PieceEchec.Roi)

// Structs

// -> Création
// avec l'attribut [<Struct>]
[<Struct>]
type StructPrixTuring (nom: string, annee: int)=
    member this.Nom = nom
    member this.Annee = annee
    override this.ToString () = sprintf "%s, prix décerné en %d" this.Nom this.Annee

// avec les mots struct ... end

type StructLivre (livre: string, page: int) =
    struct
        member this.Livre = livre
        member this.Page = page
        override this.ToString () = sprintf "%A : %d pages" this.Livre this.Page
    end

// -> Initialisation
let livre1 = new StructLivre("Programming in F#", 473)
let livreNul = new StructLivre()

livre1.ToString ()
livreNul.ToString ()

// Mutabilité
[<Struct>]
type StructMarquePage =
    val mutable livre : string
    val mutable page : int

    override this.ToString () =
        sprintf "Pause en page %d du livre %A" this.page this.livre

let mutable progression1 = new StructMarquePage()
progression1.livre <- "Proramming in F#"
progression1.page <- 180
progression1.ToString ();;

// en revanche sans mutable :
// let progression2 = new StructMarquePage()
// progression2.livre <- "blahblah" impossible