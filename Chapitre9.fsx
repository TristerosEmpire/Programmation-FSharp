// Programmation Asynchrone et Parallèle
open System
open System.Threading
open System.Threading.Tasks
open System.IO
open System.Net
open System.Drawing
open System.Drawing.Imaging
open System.Drawing.Drawing2D

// THREADS
// Création et lancement de threads
let corpsDuThread () =
    for i in 1..5 do
        // pause d'un dixième de seconde
        Thread.Sleep(100)
        printfn "[Thread %d] -> %d" Thread.CurrentThread.ManagedThreadId i

let lanceThread () =
    let thread = new Thread(corpsDuThread)
    thread.Start ()

lanceThread ()
lanceThread ()

// Pool de threads

// on passe une fonction lambda sans avoir besoin d'instancier le délégué WaitCallback
ThreadPool.QueueUserWorkItem(fun _ -> for i in 1..5 do printfn "%d" i)

//soit un crée une instance de WaitCallback
let afficheNombre (max : obj) =
    for i in 1 .. (max :?> int) do
        printfn "%d" i
// soit on utilise explicitement le délégué avec la nécessité de faire un boxing sur le type int
ThreadPool.QueueUserWorkItem(new WaitCallback(afficheNombre), box 5)

// Problèmes : compétitions / race conditions
let sommeTableauErreur (tab: int[]) =
    let total = ref 0

    let thread1Achevé = ref false

    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = 0 to tab.Length / 2 - 1 do
                    total := tab.[i] + !total
                 thread1Achevé := true
    ) |> ignore

    let thread2Achevé = ref false

    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = tab.Length  / 2 to tab.Length - 1 do
                    total := tab.[i] + !total
                 thread2Achevé := true
    ) |> ignore

    while not !thread1Achevé || not !thread2Achevé do
        Thread.Sleep(0)

    !total

let millionUn = Array.create 1000000 1

sommeTableauErreur millionUn

// résolution sans blocage:

let sommeTableauNonBloquée (tab:int[]) =
    let sstotal1 = ref 0
    let sstotal2 = ref 0
    let thread1Achevé = ref false

    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = 0 to tab.Length / 2 - 1 do
                    sstotal1 := tab.[i] + !sstotal1
                 thread1Achevé := true
    ) |> ignore

    let thread2Achevé = ref false

    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = tab.Length  / 2 to tab.Length - 1 do
                    sstotal2 := tab.[i] + !sstotal2
                 thread2Achevé := true
    ) |> ignore

    // attendre que les 2 threads aient fini leur travail respectif
    while not !thread1Achevé || not !thread2Achevé do
        Thread.Sleep(0)

    !sstotal1 + !sstotal2

sommeTableauNonBloquée millionUn
sommeTableauNonBloquée millionUn

// résolution avec la fonction lock : ('a -> (unit -> 'b) -> 'b) when 'a : not struct

let sommeTableauBloqué (tab:int[]) =
    let total = ref 0

    let thread1Achevé = ref false
    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = 0 to tab.Length / 2 - 1 do
                    lock total (fun () -> total := tab.[i] + !total)
                 thread1Achevé := true
    ) |> ignore

    let thread2Achevé = ref false
    ThreadPool.QueueUserWorkItem(
        fun _ -> for i = tab.Length / 2 to tab.Length - 1 do
                    lock total (fun () -> total := tab.[i] + !total)
                 thread2Achevé := true
    ) |> ignore

    while not !thread1Achevé || not !thread2Achevé do
        Thread.Sleep(0)

    !total

sommeTableauBloqué millionUn

// Problème des deadlocks ou interblocage : attention le FSI sera bloqué !
type CompteBancaire = {
    IDCompte: int;
    NomPropriétaire: string;
    mutable Balance : int
}

// on crée deux comptes
let alice:CompteBancaire = {
    IDCompte=1;
    NomPropriétaire="Alice";
    Balance=2000
}

let bob:CompteBancaire = {
    IDCompte=2;
    NomPropriétaire="Bob"
    Balance=1000
}

let transfert montant duCompte versCompte =
    printfn "Blocage du compte de  %s : dépôt de fonds." duCompte.NomPropriétaire
    lock duCompte
        (fun () ->
                    printfn "Blocage du compte de %s - retrait de fonds."
                            duCompte.NomPropriétaire
                    lock versCompte
                         (fun () ->
                                    duCompte.Balance <- duCompte.Balance - montant
                                    printfn "Compte de %s - balance = %d" duCompte.NomPropriétaire duCompte.Balance
                                    versCompte. Balance <- versCompte.Balance + montant
                                    printfn "Compte de %s - balance = %d" versCompte.NomPropriétaire versCompte.Balance
                         )
                    printfn("Opération achevée.")
        )

ThreadPool.QueueUserWorkItem(fun _ -> transfert 100 alice bob)
ThreadPool.QueueUserWorkItem(fun _ -> transfert 100 bob alice)

// une solution possible proposée par C.Smith : utilisé le lock sur l'ID le plus petit

let transfert2 montant duCompte versCompte =
    printfn "Blocage du compte de  %s : dépôt de fonds." duCompte.NomPropriétaire
    lock (min duCompte versCompte)
        (fun () ->
                    printfn "Blocage du compte de %s - retrait de fonds."
                            duCompte.NomPropriétaire
                    duCompte.Balance <- duCompte.Balance - montant
                    printfn "Compte de %s - balance = %d" duCompte.NomPropriétaire duCompte.Balance
                    versCompte. Balance <- versCompte.Balance + montant
                    printfn "Compte de %s - balance = %d" versCompte.NomPropriétaire versCompte.Balance
                    printfn("Opération achevée.")
        )

ThreadPool.QueueUserWorkItem(fun _ -> transfert2 100 alice bob)
ThreadPool.QueueUserWorkItem(fun _ -> transfert2 100 bob alice)

// Programmation Asynchrone
// selon le modèle APM

let traitementAsynchroneFichier (cheminFichier: string) (traitementOctets: byte[] -> byte[]) =
    // callback appelé quand l'écriture asynchrone est achevée
    let écritureAsynchroneCallback =
        new AsyncCallback(
            fun (iar:IAsyncResult) ->
                //récupère l'état depuis le résultat asynchrone
                let fluxEnEcriture = iar.AsyncState :?> FileStream

                //achève l'opération d'écriture asynchrone via EndWrite
                let octetsEcrits = fluxEnEcriture.EndWrite(iar)
                fluxEnEcriture.Close()
                printfn "Traitement du fichier achevé [%s]"
                        (Path.GetFileName(fluxEnEcriture.Name))
            )

    // callback appelé quand l'écriture asynchrone est achevée
    let lectureAsynchroneCallback =
        new AsyncCallback(
            fun (iar:IAsyncResult) ->
                //récupère l'état depuis le résultat asynchrone
                let (lectureFlux:FileStream), (données:byte[]) =
                            iar.AsyncState :?> (FileStream * byte[])

                //achève la lecture asynchrone par l'appel de EndRead
                let octetsLus = lectureFlux.EndRead(iar)
                lectureFlux.Close()
                //Traitement du résultat
                printfn "Traitement du fichier [%s], lecture [%d] octets"
                        (Path.GetFileName(lectureFlux.Name))
                        octetsLus

                let octetsMAJ = traitementOctets données
                let fichierFinal = new FileStream(lectureFlux.Name + ".result", FileMode.Create)
                let _ = fichierFinal.BeginWrite(octetsMAJ, 0, octetsMAJ.Length,
                                                écritureAsynchroneCallback,
                                                fichierFinal)
                ()
        )

    // démarrage de la lecture asynchrone
    let fs = new FileStream(cheminFichier, FileMode.Open, FileAccess.Read, FileShare.Read,2048,
                            FileOptions.Asynchronous)

    let longueurFichier = int fs.Length
    let tampon = Array.zeroCreate longueurFichier
    // Etat passé à la lecture asynchron e
    let état = (fs, tampon)
    printfn "Traitement du fichier [%s]" (Path.GetFileName(cheminFichier))
    let _ = fs.BeginRead(tampon,0,tampon.Length, lectureAsynchroneCallback, état)
    ()

// programmation asynchrone : worflow asynchrone en F# (solution alternative)
let traitementAsynchroneFichier2 (cheminFichier:string) (traitementOctets: byte[] -> byte[]) =
    async {
        printfn "Traitement du fichier [%s]" (Path.GetFileName(cheminFichier))
        use fs = new FileStream(cheminFichier, FileMode.Open)
        let octetsALire = int fs.Length
        let! données = fs.AsyncRead(octetsALire)

        printfn "[%s] ouvert, [%i] octets lus." (Path.GetFileName(cheminFichier)) données.Length
        let données' = traitementOctets données
        use fichierRésultat = new FileStream(cheminFichier + ".resultat", FileMode.Create)
        do! fichierRésultat.AsyncWrite(données', 0, données'.Length)

        printfn "Traitement du fichier [%s] achevé." <| Path.GetFileName(cheminFichier)
    }|> Async.Start

// démarrage de tâches asynchrones et récupération de valeur
let recupContenuHTML (url:string) =
    async{
            let requete = WebRequest.Create(url)
            let! reponse = requete.AsyncGetResponse()

            use stream = reponse.GetResponseStream()
            use lecture = new StreamReader(stream)

            return lecture.ReadToEndAsync().Result
    }

let html = recupContenuHTML "http://www.github.com/" |> Async.RunSynchronously
// utilisation de Async.RunSynchronously avec Async.Parallel
let htmls = ["http://www.google.com";"http://yahoo.com";"http://www.bing.com"]
            |> List.map recupContenuHTML
            |> Async.Parallel
            |> Async.RunSynchronously

// workflow et exceptions : reprise du code précédent
let recupContenuHTML2 (url:string) =
    async{
        try
            let requete = WebRequest.Create(url)
            let! reponse = requete.AsyncGetResponse()

            use stream = reponse.GetResponseStream()
            use lecture = new StreamReader(stream)

            return lecture.ReadToEndAsync().Result
        with
        | :? IOException as ioe -> return "Erreur IO : " + ioe.Message
        | :? WebException as we -> return "Erreur Web : " +  we.Message
    }
// test
let htmls2 = ["http://www.google.com";"http://yahoo.com";"http://www.bing.com"; "http://www.fhouzhf.com"]
            |> List.map recupContenuHTML2
            |> Async.Parallel
            |> Async.RunSynchronously
htmls2.[3]

//annulation
let tacheAnnulable =
    async {
        printfn "Attendre 10 secondes..."
        for i =0 to 10 do
            printfn "%d..." i
            do! Async.Sleep(1000)

        printfn "Tâche terminée !"
    }

let gestionAnnulation (oce : OperationCanceledException) =
    printfn "Tâche annulée !"


//Arrêt immédiat :
//Async.CancelDefaultToken()

let annulationDifférée =
    Async.TryCancelled(tacheAnnulable, gestionAnnulation) |> Async.Start
    let n = (new Random()).Next 10
    printfn "%d secondes avant annulation" n
    (*
    la valeur aléatoire est le nombre de secondes maximum avant annulation
    *)
    for i = 0 to n do
        Thread.Sleep(1000)
    Async.CancelDefaultToken()

// utilisation de CancellationTokenSource

let annulationTraquee =
    let traitement = Async.TryCancelled(tacheAnnulable, gestionAnnulation)
    let sourceAnnulation = new CancellationTokenSource()
    Async.Start(traitement, sourceAnnulation.Token)

    // rerpise du code précédent
    let n = (new Random()).Next 10
    printfn "%d secondes avant annulation" n
    (*
    la valeur aléatoire est le nombre de secondes maximum avant annulation
    *)
    for i = 0 to n do
        Thread.Sleep(1000)

    sourceAnnulation.Cancel()

// Utilisation de Async.StartWithContinuations
let tacheAsync =
    let n = (new Random()).Next 15
    printfn "Possibilité d'annulation au bout de %d secondes." n
    async {
        printfn "Démarrage des 10 secondes..."
        for i =0 to 10 do
            printfn "%d..." i
            do! Async.Sleep(1000)
            // si le choix aléatoire est inférieur ou égal à 5 alors on lève une exception
            if n<=5 && i = n then
                failwith "Durée trop courte"
            // si i est égal au choix aléatoire alors on annule
            elif i=n then
                Async.CancelDefaultToken()
    }

// 1 - On exécute le code suivant

Async.StartWithContinuations(
    tacheAsync,
    (fun _ ->  printfn "Tâche finalisée sans problème"),
    (fun exc -> printfn "Exception levée : %s" exc.Message),
    (fun annule -> printfn "Annulation : %s" annule.Message)
)
(*
2 - on exécute la ligne suivante devra être exécutée après le message final
pour pouvoir sortir et récupérer le prompt
*)
Async.CancelDefaultToken()

(*
pour un autre snippet avec Async.StartWithContinuations : http://www.fssnip.net/ob
avec utilisation d'une boucle récursive

doc MSDN + exemple : https://technet.microsoft.com/fr-fr/library/ee370487(v=vs.110).aspx
*)

// Customisation des primitives Async
type System.IO.Directory with
    static member GetFilesAsync(chemin:string, motif:string) =
        let delegue = new Func<string*string, string[]>(Directory.GetFiles)
        Async.FromBeginEnd(
            (chemin, motif),
            delegue.BeginInvoke,
            delegue.EndInvoke
        )

type System.IO.File with
    static member CopyAsync(source: string, dest: string) =
        let delegue = new Func<string*string, unit>(File.Copy)
        Async.FromBeginEnd(
            (source, dest),
            delegue.BeginInvoke,
            delegue.EndInvoke
        )

let sauvegardeAsynchrone source destination motifDeRecherche =
    async {
        let! fichiers = Directory.GetFilesAsync(source, motifDeRecherche)
        for fichier in fichiers do
            let nom = Path.GetFileName(fichier)
            do! File.CopyAsync(fichier, Path.Combine(destination, nom))
    }

// Programmation Parallèle

// Parallel.For : calcul parallèle de deux matrices
let multiplierMatrice (matrice1: float[,]) (matrice2: float[,]) =
    let m1Ligne, m1Col = Array2D.length1 matrice1, Array2D.length2 matrice1
    let m2Ligne, m2Col = Array2D.length1 matrice2, Array2D.length2 matrice2
    if m1Ligne <> m2Col then failwith "Les dimensions entre les matrices ne conviennent pas."

    // création d'un espace pour la matrice des résultats : final

    let (final:float[,]) = Array2D.zeroCreate m1Col m2Ligne
    let fLigne, fCol = m1Col, m2Ligne

    // calcule pour une ligne finalisée
    let calculeLigne idLigne =
        for idCol=0 to fCol-1 do
            for x = 0 to m1Ligne-1 do
                final.[idCol, idLigne] <- final.[idCol, idLigne] + matrice1.[x, idCol] * matrice2.[idLigne, x]

        ()

    let _ = Parallel.For(0, fLigne, new Action<int>(calculeLigne))
    // on retourne la matrice finalisée
    final

// test :
let x = array2D [| [|1.0;0.0|] ; [|0.0;1.0|] |]
let y = array2D [| [|1.0;2.0|] ; [|7.0;8.0|] |]
let matriceNV = multiplierMatrice y x

// module Array.Parallel
let rechercheFichiersSecrets motclé répertoire =
    let fichiersSecrets = Directory.GetFiles(répertoire, "*.secret")

    // ('T -> unit) -> 'T [] -> unit
    // décrypte les données
    Array.Parallel.iter File.Decrypt fichiersSecrets

    let donnéesSecrètes = fichiersSecrets 
                          |> Array.Parallel.map (fun accèsFichier -> File.ReadAllText accèsFichier)
                          |> Array.Parallel.choose (fun contenu ->
                                    if contenu.Contains motclé then
                                            Some contenu
                                    else None)
    // réencrypte les fichiers
    Array.Parallel.iter File.Encrypt fichiersSecrets
    // affiche le contenu des fichiers possédant le mot-clé désiré
    donnéesSecrètes

let redimensionneIMG (largeur:int, hauteur:int) (fichier:string) =
    let original = Bitmap.FromFile(fichier)
    let redim = new Bitmap(largeur,hauteur)
    use g = Graphics.FromImage(redim)

    g.InterpolationMode <- InterpolationMode.HighQualityBicubic
    g.DrawImage(redim, 0,0, largeur, hauteur)
    let nomFichier = Path.GetFileNameWithoutExtension(fichier) 
    let repertoire = Path.GetDirectoryName(fichier)

    redim.Save(
            Path.Combine(
                Path.GetDirectoryName(fichier), Path.GetFileNameWithoutExtension(fichier)+".redim.jpg"
            ), ImageFormat.Jpeg)

let mesFichiersImagesJPG (répertoire:string) =
    Directory.GetFiles(répertoire, "*.jpg")

let redimensionne repertoire = mesFichiersImagesJPG repertoire 
                               |> Array.map (fun fichier -> Task.Factory.StartNew(new Action(fun () -> redimensionneIMG (640, 480) fichier)))

// remplacer XXX par le nom d'utilisateur
Task.WaitAll(redimensionne "/home/XXX/Images")

// annulations et la TPL
let tacheLongueCTS = new CancellationTokenSource()

// code original de Smith pas franchement fonctionnel
let longueTâche () = 
    let mutable i = 1
    let mutable loop = true

    while i <= 10 && loop do
        printfn "%d..." i
        i <- i + 1
        Thread.Sleep(1000)

        // on vérifie si la tâche a été annulée
        if tacheLongueCTS.IsCancellationRequested then
            printfn "Tâche annulée ; arrêt prématuré"
            loop <- false
    printfn "Tache complétée."

// version fonctionnelle du code original
let longueTâcheF () =
    let rec aux(i,loop) =
        match (i, loop) with
        | (x, true) 
              when x <= 10 -> printfn "%d..." i
                              Thread.Sleep(1000)
                              if tacheLongueCTS.IsCancellationRequested then 
                                aux ((i+1), false)
                              else 
                                aux ((i+1), true)
        | (x, false)       -> printfn "Tâche annulée; arrêt prématuré."
        | _                -> printfn "Tâche complétée."
    aux(1, true)
                
// test : longueTâcheF ()
let alea = (new Random()).Next 20
let lancementTaches () = Task.Factory.StartNew(longueTâcheF, tacheLongueCTS.Token)

let t = lancementTaches ()

if alea < 10 then
    tacheLongueCTS.Cancel()

// pour le REPL : pour retourner au prompt
// pour retester le code : il faudra relancer TOUTES LES ETAPES dès let tacheLongueCTS = ...
tacheLongueCTS.Cancel()

// TPL et exceptions
type Caverne =
    | YetiDevoreHumain of string
    | Crevasse
    | Tresor
    | SansIssue
    | Croisement of Caverne * Caverne

let rec trouveTousLesTrésors noeud =
    match noeud with
    | YetiDevoreHumain nom -> failwithf "Humain dévoré par le Yéti %s." nom
    | Crevasse -> failwith "Humain tombé dans une profonde crevasse mortelle."
    | SansIssue -> 0
    | Tresor -> 1
    | Croisement (gauche, droite) -> let aGauche = Task.Factory.StartNew<int>(
                                                        fun () -> trouveTousLesTrésors gauche
                                                )
                                     let aDroite = Task.Factory.StartNew<int>(
                                                        fun () -> trouveTousLesTrésors droite
                                                )
                                     aGauche.Result + aDroite.Result

// test du code avec gestion des exceptions

let carte = 
    Croisement (
        Croisement(
            SansIssue,
                Croisement (Tresor, Crevasse)
        ),
        Croisement(
            YetiDevoreHumain "Umaro",
            Croisement(
                YetiDevoreHumain "Choko",
                Tresor
            )
        )
    )

try 
    let trouverTrésor =
        Task.Factory.StartNew<int>(fun () -> trouveTousLesTrésors carte)
    printfn "%d trésors trouvés." trouverTrésor.Result
with
| :? AggregateException as ae ->
    // récupération de toutes les exceptions internes à AggregateException
    let rec decompositionException (ae : AggregateException) =
        seq {
            for excInterne in ae.InnerExceptions do
                match excInterne with
                | :? AggregateException as ae -> yield! decompositionException ae
                | _ -> yield excInterne
        }
    printfn "AggregateException : "
    decompositionException ae |> Seq.iter (fun ex -> printfn "\nMessage: %s" ex.Message)
(* 
Résultat :
AggregateException : 
Message: Humain tombé dans une profonde crevasse mortelle.
val it : unit = ()
*)