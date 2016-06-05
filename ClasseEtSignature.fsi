namespace DecouvertesEtEssais

type internal MaClasse =
        new : unit -> MaClasse
        member public Propriete1 : int
        member private Methode1 : int * int -> int
