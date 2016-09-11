// POO APPLIQUEE
open System
open System.Collections.Generic
open System.IO

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

// Les slices
// unidimensionnels

type ListeDeMots(txt :string) =
    let mots = txt.Split([|' '|])
    member this.GetSlice (borneINF:int option, borneSUP: int option) =
        let tableau =
            match borneINF, borneSUP with
            | Some borneINF, Some borneSUP -> mots.[borneINF .. borneSUP]
            | Some borneINF, None -> mots.[borneINF ..]
            | None, Some borneSUP -> mots.[.. borneSUP]
            | None, None -> mots.[*]
        tableau

        member this.Comptage () = mots.Length

let ldm = new ListeDeMots("Portez ce vieux whisky au juge blond qui fume")
ldm.[*]
ldm.Comptage ()
ldm.[..5]
ldm.[2..7]

// bidimensionnels
type Points2D(points: seq<float * float>) =
    member this.GetSlice (xinf, xsup, yinf, ysup) =
        let getValue option valeurParDefaut =
            match option with
            | Some x -> x
            | None   -> valeurParDefaut

        let minX = getValue xinf Double.MinValue
        let maxX = getValue xsup Double.MaxValue

        let minY = getValue yinf Double.MinValue
        let maxY = getValue ysup Double.MaxValue

        let intervalle (x,y) = (minX < x && x < maxX && minY < y && y < maxY)

        Seq.filter intervalle points

let points = seq {
    let alea = new Random()
    for i=0 to 1000 do
        let x = alea.NextDouble ()
        let y = alea.NextDouble ()
        yield (x,y)
};;
points;;
let ensemblePoints = new Points2D(points)
ensemblePoints.[0.5 .., 0.5 ..]
ensemblePoints.[0.9 .. 0.99, *]

// Contraintes sur les types génériques

type PlusGrandQueListe<'a when 'a :> IComparable<'a> >(minVal: 'a, liste: List<'a>) =

    let resultat = new List<'a>()

    member private this.Add (nvItem: 'a)=
        let ic = nvItem :> IComparable<'a>
        if ic.CompareTo(minVal) > 0 then
            resultat.Add(nvItem)

    member this.Check () =
        for i in liste do
            this.Add(i)

    member this.Items = resultat

let p = new PlusGrandQueListe<int>(2, new List<int>([1;2;3;3;4;2;1;5;6]))
p.Check ()
p.Items

// DELEGUES ET EVENEMENTS

[<Measure>]
type ml

type TasseDeCafe(volume: float<ml>) =
    let mutable volumeRestant = volume
    let mutable partiesInteressees = List<TasseDeCafe -> unit>()

    member this.Boire(quantite) =
        printfn "Quantité bue %.1f" (float quantite)
        volumeRestant <- max (volumeRestant-quantite) 0.0<ml>
        if volumeRestant <= 0.0<ml> then
            this.AlerteTasseVide ()

    member this.Remplir(nvVolume) =
        printfn "Tasse remplie de %.1f ml" (float nvVolume)
        volumeRestant <- volumeRestant + nvVolume

    member private this.AlerteTasseVide() =
        printfn "Tasse vidée. Avertissement en cours de diffusion"
        for element in partiesInteressees do
            element(this)

    member this.AppelQuandTasseVide(func) =
        partiesInteressees.Add(func)


let tasse = new TasseDeCafe(100.0<ml>)

tasse.AppelQuandTasseVide(
    fun tasse -> printfn "Merci pour l'avertissement..."
                 tasse.Remplir(10.0<ml>))

tasse.Boire(75.0<ml>)
tasse.Boire(75.0<ml>)

// Exemple simple de création et d'utilisation de délégués
type PremierDelegue = delegate of int * int -> int

let delegue = new PremierDelegue(fun x y -> printfn "x = %d, y = %d" x y
                                            x+y)

printfn "Le résultat de l'addition est  %d" (delegue.Invoke(1,2))

// Second exemple d'utilisation de délégués
type IntDelegate = delegate of int -> unit
type TestListe =
    static member ApplicationDelegue(l: int list, d: IntDelegate) =
        l |> List.iter (fun x -> d.Invoke(x))

// création d'un délégué explicite
TestListe.ApplicationDelegue([1..10], new IntDelegate(fun x -> printfn "valeur = %d" x))
//création d'un délégué implicite
TestListe.ApplicationDelegue([1..10], (fun x -> printfn "valeur = %d" x))

// Revenons sur le code TasseDeCafe : peut être entièr
type MugDelegate<'a> = delegate of 'a -> unit

type Mug(volume:float<ml>, d:MugDelegate<_>) =
    let mutable volumeEnCours = volume

    member val delegue:MugDelegate<_> = d with get, set

    member this.VolumeActuel with get() = volumeEnCours

    member this.Boire(volume) =
        printfn "Quantité bue %.1f." (float volume)
        volumeEnCours <- volumeEnCours - volume
        if (volumeEnCours - volume ) <= 0.0<ml> then
            printfn "Mug vide ! Action en cours"
            this.delegue.Invoke(this)
        printfn "Reste %.1f" (float volumeEnCours)

    member this.Remplir(volume) =
        volumeEnCours <- volumeEnCours + volume
        printfn "Mug rempli avec %.1f ml" (float volume)

let d = new MugDelegate<Mug>(fun mug -> mug.Remplir(150.0<ml>))
let mug = new Mug(100.0<ml>, d)
mug.delegue <- new MugDelegate<Mug>(fun mug -> mug.Remplir(75.0<ml>))
mug.Boire(150.0<ml>)

// Combiner des délégués
type LogMessage = delegate of string -> unit

let afficheSurConsole = LogMessage(fun msg -> printfn "Message de log : %s ..." msg)
let ecritureSurFichierLog = LogMessage(fun msg -> printfn "Ecriture du log dans le fichier : %s..." msg
                                                  use fichier = new StreamWriter("log.txt", true)
                                                  fichier.WriteLine(msg))

// "re-typage" obligatoire car Combine retourne un Delegate et non un LogMessage
let combinaison = LogMessage.Combine(afficheSurConsole, ecritureSurFichierLog) :?> LogMessage
combinaison.Invoke("Nouveau message en date du " +  DateTime.Now.ToString())

// controle de l'écriture du fichier : rappel sur des éléments de System.IO
let checkLogFile () = if File.Exists("log.txt") then
                        use lecture = new StreamReader("log.txt", true)
                        printfn "Nous nous trouvons : %s" (Directory.GetCurrentDirectory())
                        printfn "%s" (lecture.ReadToEnd())
                      else
                        failwith "Le fichier n'existe pas"
checkLogFile ()

// EVENEMENTS

// on crée un type de données algébriques simple
type Action = Ajouté | Supprimé

// on crée une classe héritant de system.EventArgs
type ArgumentsEvenement<'a>(valeur: 'a, action: Action) =
    inherit System.EventArgs()

    member this.Valeur = valeur
    member this.Action = action

// création d'un délégué prenant 2 paramètres : un objet et une instance d'ArgumentsEvement
type DelegueOperation<'a> = delegate of obj * ArgumentsEvenement<'a> -> unit

// classe qui utilisera comme strucutre de données un Set sur lequel sera accolé des événements
// pour le constructeur, il y a obligation d'utiliser une contrainte :
type SetAlternatif<'a when 'a: comparison>() =
    let mutable m_set = Set.empty:Set<'a>

    // création d'un événement à l'ajout d'un item avec d'une part le délégué et d'autre part les arguments
    let m_itemAjouté = new Event<DelegueOperation<'a>, ArgumentsEvenement<'a>>()

    // création d'un événement au retrait d'un item
    let m_itemRetiré = new Event<DelegueOperation<'a>, ArgumentsEvenement<'a>>()

    member this.Ajoute(x) =
        m_set <- m_set.Add(x)
        //lancement de l'événement associé
        m_itemAjouté.Trigger(this, new ArgumentsEvenement<_>(x, Ajouté))

    member this.Retire(x) =
        m_set <- m_set.Remove(x)
        //lancement de l'événement associé
        m_itemRetiré.Trigger(this, new ArgumentsEvenement<_>(x, Supprimé))

    // publication des événements
    member this.ItemAjoutéEv = m_itemAjouté.Publish
    member this.ItemRetiréEv = m_itemRetiré.Publish
// test
let sa = new SetAlternatif<int>()
let GestionOperations = new DelegueOperation<int>(
                                fun sender args -> printfn "La valeur %d a été %A" args.Valeur args.Action)
sa.ItemAjoutéEv.AddHandler(GestionOperations)
sa.ItemRetiréEv.AddHandler(GestionOperations)
sa.Ajoute(9)
sa.Retire(9)

// EventDelegate: alternative à Event sans recourir à EventArgs
type ChronoDelegue = delegate of int * int * int -> unit

// création d'une classe Chrono
type Chrono() =
    let eve = new DelegateEvent<ChronoDelegue>()

    member this.Demarre () =
        printfn "Démarrage..."
        // utilisation d'une fonction interne récursive terminale plutôt qu'un while
        let rec boucle iteration =
            //Pause d'une seconde
            Threading.Thread.Sleep(1000)
            // pattern matching afin d'éviter une boucle infinie
            match iteration with
            | 10 -> printfn "...Fin"
            | _  -> let h, m, s = DateTime.Now.Hour, DateTime.Now.Minute, DateTime.Now.Second
                    eve.Trigger([|box h; box m; box s|])
                    boucle (iteration + 1)
        boucle 0

    // Publication
    member this.ChronoUpdate = eve.Publish

//test de la classe
let c = new Chrono()
c.ChronoUpdate.AddHandler(
    new ChronoDelegue(
        fun h m s -> printfn "[%d:%d:%d]" h m s
    )
)
c.Demarre()

// remarque sur le boxing/unboxing
// box : 'a -> obj
// unbox : 'a -> 'b
// pour détecter le type on utilisera l'opérateur :?

(box 42) :? int;;

let detecteEntiers t =
    match box t with
    | :? int -> printfn "%A est un entier" t
    | _ -> printfn "Autre chose"

detecteEntiers 3;;
detecteEntiers "3";;

//Module Observable
[<Measure>]
type minute

[<Measure>]
type bpm = 1/minute

type GenreMusical = Classique | Pop |Rock |Electro | Indie | Country

type Extrait = {Titre: string; Genre: GenreMusical; BPM: int<bpm>}

//Création d'un type classe héritant de System.EventArgs
type ExtraitEventArgs(titre: string, genre: GenreMusical, bpm: int<bpm>) =
    inherit System.EventArgs()

    member this.Titre = titre
    member this.Genre = genre
    member this.BPM = bpm

// création du délégué
type DelegueExtrait = delegate of obj * ExtraitEventArgs -> unit

// création du type classe JukeBox
type Jukebox() =
    let m_extraitDemarreEvent = new Event<DelegueExtrait, ExtraitEventArgs>()

    member this.Interpretation(extrait:Extrait) =
        m_extraitDemarreEvent.Trigger(this,
            new ExtraitEventArgs(
                    extrait.Titre, extrait.Genre, extrait.BPM
        ))

    [<CLIEvent>]
    member this.evExtraitLancé = m_extraitDemarreEvent.Publish

// rappel de l'utilisation des délégués et leur ajout à l'événement
let chant1:Extrait = {Titre="Extrait 1"; Genre=Electro; BPM=152<bpm>}
let chant2:Extrait = {Titre="Extrait 2"; Genre = Classique; BPM=130<bpm>}
let juke = new Jukebox()
let del = new DelegueExtrait(fun objet args -> 
    printfn "Titre joué %s" args.Titre
    )
juke.evExtraitLancé.AddHandler(del);
juke.Interpretation(chant1)

// Utilisation du module Observable
let juke2 = new Jukebox()

let evTitreRapide, evTitreLent =
    juke2.evExtraitLancé 
    |> Observable.filter(
                fun argsExtrait -> match argsExtrait.Genre with
                                   | Classique -> false
                                   | _ -> true)
    |> Observable.partition (
                fun argsNvExtrait -> argsNvExtrait.BPM >= 120<bpm> )

// ajout d'événements aux types Observables
evTitreRapide.Add(fun extrait -> printfn "En écoute : '%s' (rythme rapide)" extrait.Titre)
evTitreLent.Add(fun extrait -> printfn "En écoute : '%s' (rythme lent)" extrait.Titre)

// test :
juke2.Interpretation(chant1)
juke2.Interpretation(chant2)