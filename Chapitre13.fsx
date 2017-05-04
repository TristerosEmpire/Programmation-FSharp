open System
open System.IO

// séquences et expressions :
let joursAnnee = 
    seq {
        let mois = ["Janvier"; "Février"; "Mars";
                    "Avril"; "Mai"; "Juin";
                    "Juillet"; "Août"; "Septembre";
                    "Octobre"; "Novembre"; "Décembre"]
        let joursMois m =
            match m with 
            |"Février" -> 28
            | "Avril" | "Juin" | "Septembre" | "Novembre" -> 30
            | _ -> 31
        for m in mois do
            for jour = 1 to joursMois m do
                yield(jour, m)
    }

Seq.length joursAnnee


// Evolution d'un code 
// exemple : calcul de trois résistances en parallèle :
// 1/r1 + 1/r2 + 1/r3

type Resultat<'T> = Succes of 'T | DivisionParZero
let divise y =
    match y with 
    | 0.0 -> DivisionParZero
    | _ -> Succes(1.0/y)

// un code difficilement lisible, long, potentiellement source de bugs
let totalResistance r1 r2 r3 =
    let resultat1 = divise r1
    match resultat1 with
    | DivisionParZero -> DivisionParZero
    | Succes(val1) -> let resultat2 = divise r2
                      match resultat2 with
                      | DivisionParZero -> DivisionParZero
                      | Succes(val2) -> let resultat3 = divise r3
                                        match resultat3 with
                                        | DivisionParZero -> DivisionParZero
                                        | Succes(val3) -> let resultatFinal = divise (val1+val2+val3)
                                                          resultatFinal

// code alternatif
// création d'une fonction prenant 2 args : la valeur de la résistance et une fonction 

let associeEtVerifie resultat resteACalculer =
    match resultat with
    | DivisionParZero -> DivisionParZero
    | Succes(x) -> resteACalculer x

let totalResistance2 r1 r2 r3 =
    associeEtVerifie (divise r1) (
        fun val1 -> associeEtVerifie 
                        (divise r2)
                        (fun val2 -> associeEtVerifie 
                                        (divise r3) 
                                        (fun val3 -> divise (val1+val2+val3))
                        )
    )

// Utilisation des constructeurs d'expression 
// qui effectue la même chose que précédemment
// mais en utilisant un sucre syntaxique

type Constructeur() =
    member this.Bind((x:Resultat<float>), (division : float -> Resultat<float>) ) =
        match x with
        | Succes(x) -> division x
        | _ -> DivisionParZero
    
    member this.Return (x:'a) = x

let constructeur = Constructeur()

let totalResistance3 r1 r2 r3 =
    constructeur {
        let! x = divise r1
        let! y = divise r2
        let! z = divise r3
        return divise (x+y+z)
    }

totalResistance 0.75 0.3 0.4
totalResistance2 0.75 0.3 0.4
totalResistance3 0.75 0.3 0.4

// exemple de constructeur/Builder avec la programmation asynchrone
// PGM asynchrone : chapitre 9
let traitementFichierAsync (fichier: string) (traitementOct: byte[] -> byte[]) =
    async {
        printfn "fichier en traitement [%s]" (Path.GetFileName(fichier))
        let streamFichier = new FileStream(fichier, FileMode.Open)
        let octetsALire = int streamFichier.Length

        let! donnees = streamFichier.AsyncRead(octetsALire);
        printfn "[%s] ouvert, lecture : [%d] octets" (Path.GetFileName(fichier)) donnees.Length
        let donnees' = traitementOct donnees
        let fichierFinal = new FileStream(fichier+".rslt", FileMode.Create)

        do! fichierFinal.AsyncWrite(donnees', 0, donnees'.Length)

        printfn "Fichier finalisé [%s]" <| Path.GetFileName(fichier)
    } |> Async.Start

// Autre exemple : workflow de précision mathématique

type PrecisionArithmetique(chiffresRetenus: int) =
    let arrondi(x: float) = Math.Round(x, chiffresRetenus)

    member this.Bind((valeur: float), (fctDeCalcul: float-> float)) =
        let rslt = arrondi valeur
        fctDeCalcul rslt
    
    member this.Return(valeur: float) = arrondi valeur

let precision x = PrecisionArithmetique(x)

let test = 
    precision 3 {
        let! x = 2.0/12.0
        // ne pas utiliser %f pour le formatage
        printfn "x vaut %A" x
        let! y = 3.5
        printfn "y vaut %A" y
        return (x/y)
    }