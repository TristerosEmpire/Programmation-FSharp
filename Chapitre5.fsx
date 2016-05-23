// PROGRAMMATION ORIENTEE OBJET
open System
open System.IO

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