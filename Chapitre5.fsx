// PROGRAMMATION ORIENTEE OBJET
open System
open System.Globalization
open System.IO
open System.Windows.Forms
open System.Drawing

// Présentation de System.Object
// surcharge de ToString ()

type Ponctuation =
    | Virgule
    | Point
    | PointInterrogation
    | PointExclamation
    override this.ToString () =
        match this with
        | Virgule               -> "Virgule (,)"
        | Point                 -> "Point (.)"
        | PointInterrogation    -> "PointInterrogation (?)"
        | PointExclamation      -> "PointExclamation (!)"

let x = Virgule
x.ToString ()

// GetType ()
let chaine = "Une chaine".GetType ()
chaine.AssemblyQualifiedName

// Egalité des objets
// --> type de valeur
let x = 42
let y = 42
x = y

// --> type de référence
type ClassType (x:int) =
    member this.Value = x

let x = new ClassType(42)
let y = new ClassType(42)
x = x
y = x

// --> customisation de Object.Equals
type ClassType2 (x:int) =
    member this.Value = x
    override this.Equals (o:obj) =
        match o with
        | :? ClassType2 as other -> (other.Value = this.Value)
        | _ -> false
    override this.GetHashCode () = x

let x1 = new ClassType2 (21)
let y1 = new ClassType2 (21)
let z1 = new ClassType2 (1234567)

x1=x1
x1=z1

// Egalités générées
// --> tuples
let xt = (1, 'k', "chaine")
let yt = (1, 'k', "chaine")
let zt = (1, "k", "chaine")

xt = yt
xt = zt

// --> Enregistrements
type R = {Element1 : string; Element2 : int }
let xr = {Element1 = "chaine"; Element2 = 1}
let yr = {Element1 = "chaine"; Element2 = 1}
let zr = {Element1 = "Chaine"; Element2 = 1}
xr=yr
xr=zr

// --> Unions discriminées
type UD =
    | A of int*int
    | B

let xUD = A(4,2)
let yUD = A(4,2)
let zUD = B

xUD = yUD
xUD = zUD

// --> attribut ReferenceEquality
[<ReferenceEquality>]
type UD2 =
    | A of int*int
    | B

let xUD2 = A(4,2)
let yUD2 = A(4,2)

xUD2 = xUD2
yUD2 = xUD2

// Classes
// construction explicite

// premier exemple
type Point =
    val px : float
    val py : float

    //Premier constructeur sans argument
    new () = {px=0.0; py=0.0}

    //Second constructeur avec 2 arguments
    new (x, y) = {px=x ; py=y}

    member this.Length =
        let sqr x = x * x
        sqrt <| sqr this.px + this.py

let p1 = new Point ()
p1.Length

let p2  = new Point (4.0, 5.0)
p2.Length

// Second exemple avec un traitement AVANT initialisation des champs

type Point2 =
    val p1 : int
    val p2 : int

    new (arg:string) as this =
        // prétraitement
        if arg = null then
            raise <| new ArgumentException("Argument non valide")
        let parties = arg.Split [|','|]
        let style = NumberStyles.Float

        let (succesX, x) = Int32.TryParse parties.[0]
        let (succesY, y) = Int32.TryParse parties.[1]

        if not succesX || not succesY then
            raise <| new ArgumentException("Le parsing ne s'est pas exécuté correctement : ne prend que des chaines e.g. \"1,2,3\"")

        //initialisation des arguments
        {p1 = x; p2 = y}

        //post-traitement
        then
            printfn "Initialisation du point à [%i,%i]" this.p1 this.p2


let p1v2 = new Point2("1,2")

// création implicite
type Point3 (x:float, y:float) =
    let longueur =
        let carre x = x * x
        sqrt <| carre x + carre y

    do printfn "Initialisation des valeurs à [%f, %f]" x y

    member this.X = x
    member this.Y = y
    member this.Longueur = longueur

    // définition d'un constructeur customisé vide avec valeurs par défaut
    new() =  new Point3(0.0,0.0)

    // autre constructeur
    new (txt : string) =
        if txt = null then
            raise <| new ArgumentException("Pas de texte fourni en paramètre")
        let parties = txt.Split [|','|]
        let (succesX, x) = Double.TryParse parties.[0]
        let (succesY, y) = Double.TryParse parties.[1]

        if not succesX || not succesY then
            raise <| new ArgumentException("Texte non valide.")

        // Appel du constructeur primaire
        new Point3 (x, y)

// Classes génériques
type CreationTableau<'a>(x: 'a) =
    member this.Vide : 'a[] = [||]
    member this.CreationDeTaille (taille:int) : 'a [] =
        Array.create taille x

// création d'un tableau en laissant le compilateur inféré le type
let t = new CreationTableau<_> ("une chaine")
t.CreationDeTaille 5

let t2 = new CreationTableau<int*char> ((42, 'r'))
t2.CreationDeTaille 3

// Enregistrements et unions discriminées génériques
type GenRec<'a, 'b> = {
    Champs1 : 'a;
    Champs2 : 'b}
let t3 = {Champs1 = "Bleu"; Champs2 = 'B'}

type UDGen<'a> =
    | Tag1 of 'a
    | Tag2 of string * 'a list

Tag2("Couleurs", ['R';'G';'B'])

// Auto-identificateur : this n'est qu'une convention

type Cercle =
    val rayon : float
    new (r) = {rayon = r}
    member truc.Rayon = truc.rayon
    member bidule.Aire = Math.PI * bidule.rayon * bidule.rayon

let cercle = new Cercle(2.0)
cercle.Rayon
cercle.Aire

// Mutateurs/Accesseurs

[<Measure>]
type ml

type BouteilleEau() =
    let mutable volume = 0.0<ml>

    //propriété en lecture seule
    member this.Vide = (volume = 0.0<ml>)

    //propriété en lecture/écriture
    member this.Volume with get ()              = volume
                       and set nouveauVolume    = volume <- nouveauVolume

let bouteille = new BouteilleEau()
bouteille.Vide
bouteille.Volume
bouteille.Volume <- 1000.0<ml>
bouteille.Vide

// on peut bénéficier de l'implémentation automatique des accesseurs/mutateurs
type BouteilleEau2() =
    member this.Vide = (this.Volume = 0.0<ml>)
    member val Volume = 0.0<ml> with get, set

// autre petit test
type BouteilleEau3(vol:float<ml>)=
    member this.Vide = (this.Volume = 0.0<ml>)
    member val Volume = vol with get

// accès aux propriétés via le constructeur
//1er exemple
let bouteilleX = new BouteilleEau(
                    Volume = 10.0<ml>
)
bouteilleX.Vide

//2nd exemple : pour Linux et Windows
let f2 = new Form(Text="Window Title", TopMost=true, Width=640, Height=480)
f2.ShowDialog ()

// Méthodes de classes
// Convention: Pascal case : cf. ligne 285

type Television =

    val mutable m_chaine : int
    val mutable m_allume : bool

    new () = {m_chaine = 7; m_allume=true}

    member this.Allume () =
        printfn "Télévision allumé - cerveau éteint."
        this.m_allume = true

    member this.Eteint () =
        printfn "Télévision éteinte - cerveau rallumé."
        this.m_allume = false

    (* Il est conseillé d'utiliser des tuples pour regrouper
    les valeurs dans un tuple car l'application partielle n'est supportée
    que par F#
    *)
    member this.ChangeChaine (canal:int) =
        if this.m_allume = false then
            failwith "Euh comment changer une chaine quand la télé est éteinte ?!"
        printfn "La chaîne %d va changer..." this.m_chaine
        this.m_chaine <- canal
        printfn "... pour la chaîne n°%d" this.m_chaine

// Méthodes statiques, propriétés et champs
type UneClasse () =
    static member ProprieteStatique = 5;;

UneClasse.ProprieteStatique;;

// exemple plus poussé : le pattern Singleton
[<AllowNullLiteral>]
type ClasseSingleton private () =
    //static let tab:ClasseSingleton [] = Array.singleton (ClasseSingleton())
    static let tab2:ClasseSingleton [] = [|null|]
    static member private Add () = tab2.[0] <- ClasseSingleton()
    static member GetInstance () = 
        if tab2.[0] = null then 
            ClasseSingleton.Add ()
            printfn "Nouvelle instance ajoutée"
        else printfn "Instance déjà présente"

// y a plus simple : http://www.fssnip.net/7p

// Surcharge de méthode
type Rubis private (poids, brillance) =

    let mutable m_poids = poids
    let mutable m_brillance = brillance

    // le polissage augmente la brillance et réduit le poids
    member this.Polissage () =
        this.Poids <- this.Poids - 0.1
        m_brillance <- m_brillance + 0.1

    // création d'un accesseur privé
    member private this.Poids with get () = m_poids

    // création d'un mutateur public
    member public this.Poids with set nouveauPoids = m_poids <- nouveauPoids

    member this.Brillance = m_brillance

    // Surcharge
    public new () = 
        let rng = new Random()
        let poids = float (rng.Next () % 100) * 0.01
        let brillance = float (rng.Next () % 100) + 0.1
        new Rubis(poids, brillance)

    public new (carat) =
        new Rubis(carat, (new Random()).Next () % 100 |> float |> (*) 0.01)

(* 
Les fichiers signature en F# : se reporter aux fichiers ClasseEtSignature.fsi
et ClassesEtSignature.fs. A noter: il convient d'abord d'ouvrir le fichier signature FSI
puis le fichier source FS
*)

// L'héritage

type ClasseMere =                               //constructeur explicite

    val m_champs1 : int
    new (champs1) = {m_champs1 = champs1}

    member this.Champs1Mere = this.m_champs1

// classe fille avec constructeur implicite
type ClasseFille(champs1, champs2) =
    inherit ClasseMere(champs1)

    member this.Champs2 = champs2
    member this.Champs1 = this.m_champs1

    member this.PrintValues = printfn "Valeurs : %d (directement de la classe mère) et %d de la classe fille" 
                                       this.Champs1 this.Champs2

// classe fille avec constructeur explicite
type ClasseFilleExplicite =
    inherit ClasseMere

    val m_champs2 : int

    new (champs1, champs2) = {
        inherit ClasseMere(champs1)

        m_champs2 = champs2

    }

    member this.Champs2 = this.m_champs2

    member this.Champs1 = this.Champs1Mere

    member this.PrintValues = printfn "Valeurs : %d (directement de la classe mère) et %d de la classe fille" 
                                       this.Champs1 this.Champs2

let fille = new ClasseFilleExplicite(1, 2);;
fille.Champs1;;
fille.Champs2;;
fille.PrintValues;;

