// POO APPLIQUEE
open System
open System.Collections.Generic

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
type MugDelegate = delegate of unit -> unit

type Mug(volume:float<ml>) =
    let mutable volumeEnCours = volume
    let partiesInteressees = List<MugDelegate>()

    member this.VolumeActuel with get() = volumeEnCours

    member this.Boire(volume) =
        printfn "Quantité bue %.1f." (float volume)
        volumeEnCours <- volumeEnCours - volume
        if (volumeEnCours - volume ) <= 0.0<ml> then
            printfn "Mug vide ! Action en cours"
            for delegue in partiesInteressees do
                delegue.Invoke()
        printfn "Reste %.1f" (float volumeEnCours)

    member this.Remplir(volume) =
        printfn "Mug rempli avec %.1f ml" (float volume)
        volumeEnCours <- volumeEnCours + volume

    member this.AppelQuandTasseVide(func) =
        partiesInteressees.Add(func)

let mug = new Mug(100.0<ml>)
// on souhaite remplir notre mug avec une quantité de 50ml lorsque le mug est vide
let mugVide = new MugDelegate(fun () -> mug.Remplir(mug.VolumeActuel * -1.0 + 50.0<ml>))
mug.AppelQuandTasseVide(mugVide)
mug.Boire(150.0<ml>)