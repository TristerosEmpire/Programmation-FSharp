namespace DecouvertesEtEssais

type MaClasse () =
    member this.Propriete1 = 10
    member this.Propriete2 with set (x:int) = ()
    member this.Methode1 (x, y) = x + y
    member this.Methode2 () = true
