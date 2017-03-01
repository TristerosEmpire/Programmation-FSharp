// SystemeLivraison.Core.dll
// pour la compilation : 
// fsharpc --out:SystemeLivraison.Core.dll --target:library SystemeLivraison.fs
namespace Livraison.Core

// Représente un colis devant être livré
type Paquet =
    class
        (*...*)
    end

// Destination du paquet
type Destination = 
    {
        Adresse: string
        Ville: string
        CodePostal: int
        Pays: string
    }

// on crée une interface qui devra implémenter les plugins
type IMethodeLivraison =
    interface
        abstract NomMethode: string
        abstract PaquetALivrer: Paquet * Destination -> unit
    end