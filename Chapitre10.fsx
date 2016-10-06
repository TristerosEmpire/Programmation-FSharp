// Scritping
open System
open System.IO
open System.Collections.Generic
open System.Windows.Forms
open System.Drawing

// Directives génériques
let affiche_moi () = 
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