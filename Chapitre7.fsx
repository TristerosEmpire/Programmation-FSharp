// PROGRAMMATION FONCTIONNELLE APPLIQUEE
open System
open System.IO
open System.Text.RegularExpressions;

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

// Active Patterns paramétrés

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

let (|Pair|Impair|) entier = if entier % 2 = 0 then Pair else Impair

let estPaire valeur =
    match valeur with
    | Pair v -> true
    | _ -> false

let estImpaire valeur =
    not (estPaire valeur)

let (|Positif|Negatif|Zero|) input =
    if input >  0 then Positif
    elif input < 0 then Negatif
    else Zero

let controleEtat input =
    match input with
    | Positif input -> printfn "Valeur positive."
    | Negatif input -> printfn "Valeur négative."
    | Zero input    -> printfn "La valeur est égale à zéro."