// CHAPITRE 14 : QUOTATIONS
open System
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

// les bases
<@ 1 + 1 @>

<@@ fun x-> "Hello" + x @@>

// décomposition d'un arbre AST
let rec descriptionCode (expr : Expr) =
    match expr with

    // valeur litterale
    | Int32(i)  -> printfn "Donnée entière ayant pour valeur : %d" i
    | Double(f) -> printfn "Donnée à virgule flottante ayant pour valeur %f" f
    | String(s) -> printfn "Donnée de type chaine de caractère : '%s'" s

    // appel de méthode
    |Call(appelSurObjet, infoMethode, args) ->
        let appel = match appelSurObjet with
                       | Some x -> sprintf "%A" x
                       | None   -> "(appel d'une méthode statique)"
        printfn "Méthode appelée : %s \nSur la valeur : %s \nAvec les arguments : %A" 
                infoMethode.Name appel args

    // expressions lambda
    | Lambda(arg, corps) -> printfn "Expression lambda - Valeur %s de type %s" arg.Name arg.Type.Name
                            printfn "Traitement du corps de la fonction..."
                            descriptionCode corps

    | _ -> printfn "Expression inconnue de forme '%A'" expr

// quelques essais

descriptionCode <@ 1 @>
descriptionCode <@ 1.0 + 2.0 @>
let valeurLocale = "chaine"
descriptionCode <@ valeurLocale.ToUpper() @>
descriptionCode <@ valeurLocale.[1..3]@>
descriptionCode <@ fun x y -> (y, x) @>
descriptionCode <@ fun x y -> x + y @>