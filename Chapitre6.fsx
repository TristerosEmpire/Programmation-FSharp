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