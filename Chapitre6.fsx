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