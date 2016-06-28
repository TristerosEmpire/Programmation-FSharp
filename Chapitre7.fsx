// PROGRAMMATION FONCTIONNELLE APPLIQUEE
open System
open System.IO

// ACTIVE PATTERNS  
// Cas simple :

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