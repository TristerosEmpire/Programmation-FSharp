// "SystemeLivraison.Core.dll"
// pour la compilation : fsharpc --out:PigeonVoyageurPlugin.dll --target:library PigeonVoyageurPlugin.fs --reference:SystemeLivraison.Core.dll 

namespace Pigeon

open Livraison.Core

type PigeonVoyageur() =
    interface Livraison.Core.IMethodeLivraison with
        member this.NomMethode = "Pigeon Voyageur"
        member this.PaquetALivrer(colis, destination) = ()
    end