// Programmation Asynchrone et Parallèle
open System
open System.Threading
open System.Collections.Generic
open System.IO

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