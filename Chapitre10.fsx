// Scritping
open System
open System.IO
open System.Collections.Generic
open System.Windows.Forms
open System.Drawing

// Directives génériques
let afficheMoi () =
    printfn "Je suis %s et je me trouve dans %s" __SOURCE_FILE__ __SOURCE_DIRECTORY__
    printfn "Chemin complet : %s" (Path.Combine [|__SOURCE_DIRECTORY__; __SOURCE_FILE__|] )

// Directives spécifiques

// #r
#r "System.Windows.Forms.dll"
#r "System.Drawing.dll"

// on veut retourner un type seq<string * Drawing.Bitmap>
let images chemin =
    Directory.GetFiles(chemin, "*.jpg") |> Seq.map (fun fichier ->
            Path.GetFileName(fichier), Bitmap.FromFile(fichier))

// création d'un datagrid
let dg = new System.Windows.Forms.DataGridView(Dock=DockStyle.Fill)
dg.DataSource <- new List<_>(images "/home/patrice/Téléchargements/")
dg.DataSource <- new List<_>(images (Environment.GetFolderPath(Environment.SpecialFolder.MyPictures)) )

let f = new Form()
f.Controls.Add(dg)
f.ShowDialog()

// coloration en mode console
let colorisation (couleur:ConsoleColor) texte =
    // utilisation de la fonction Printf.kprintf de signature :
    // (string -> 'a) -> Printf.StringFormat<'b,'a> -> 'b
    Printf.kprintf (fun chaine -> let original = Console.ForegroundColor
                                  Console.ForegroundColor <- couleur
                                  Console.WriteLine chaine
                                  Console.ForegroundColor <- original)
                   texte

colorisation ConsoleColor.Blue "hello en bleu."

let rotationCouleurs =
// aura pour signature :rotationCouleurs : seq<ConsoleColor>
    // récupération des valeurs prédéfinies de ConsoleColor
    let couleursPossibles = Enum.GetValues(typeof<ConsoleColor>)
    // obligation de contraindre le type de retour : ConsoleColor sinon erreur d'ambiguité 
    let couleurs (index:int) : ConsoleColor = enum (index % couleursPossibles.Length)
    Seq.initInfinite couleurs

// application de rotationCouleurs
"Faire l'expérience d'un arc-en-ciel de possibilité !"
|> Seq.zip rotationCouleurs
|> Seq.iter (fun (couleur, caractere) -> colorisation couleur "%c" caractere) 

//Production de son
let victoire () = 
    for frequence in [100 .. 50 .. 2000] do
        Console.Beep(frequence, 25)

let defaite () =
    for frequence in [2000 .. 50 .. 37] do
        Console.Beep(frequence, 25)

// Parcours d'un répertoire pour en lister tous les fichiers sous la forme d'une séquence
// listeDesFichiersSous : repertoire:string -> seq<string>
let rec listeDesFichiersSous repertoire =
    seq {
        yield! Directory.GetFiles(repertoire)
        for sousRepertoire in Directory.GetDirectories(repertoire) do
            yield! listeDesFichiersSous sousRepertoire
    }

__SOURCE_DIRECTORY__ |> listeDesFichiersSous |> Seq.iter (fun fichier -> printfn "%A" fichier)

__SOURCE_DIRECTORY__ |> listeDesFichiersSous
                     |> Seq.filter (fun fichier -> fichier.ToUpper().EndsWith("FSX"))
                     |> Seq.iter (fun fichier -> printfn "%A" fichier)

let backupFSX repertoire repertoireBackup =
    repertoire |> listeDesFichiersSous 
               |> Seq.filter (fun fichier -> fichier.ToUpper().EndsWith("FSX"))
               |> Seq.iter (fun fichier -> let nomFichier = Path.GetFileName(fichier)
                                           let destination = Path.Combine(repertoireBackup, nomFichier)
                                           File.Copy(fichier, destination)
                                           printfn "%s a été copié ici : %s" nomFichier destination)
