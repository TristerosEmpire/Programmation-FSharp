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
