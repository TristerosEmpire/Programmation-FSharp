open System

# Découverte de Liste

let proches x = [
    yield x-1;
    yield x;
    yield x+1;
    ]

# opérateurs d'agrégats avec List
List.map (function x -> x*x) (proches 3)

let insertVirgule acc item = acc+ ", " + item;;
List.reduce insertVirgule ["Jean";"Jacques";"Pierre";"Jeanne";"Michelle";"Benoit"]

let somme acc i = acc + i
List.fold somme 0 [1;2;3]

let compteVoyelles (str : string) =
    let carListe = List.ofSeq str

    let accumule (As,Es,Is,Os,Us) caractere = 
        match caractere with
        | 'a' -> (As+1,Es,Is,Os,Us)
        | 'e' -> (As,Es+1,Is,Os,Us)
        | 'i' -> (As,Es,Is+1,Os,Us)
        | 'o' -> (As,Es,Is,Os+1,Us)
        | 'u' -> (As,Es,Is,Os,Us+1)
        | _ -> (As,Es,Is,Os,Us)
    
    List.fold accumule (0,0,0,0,0) carListe


# OPTION
let isInteger str = 
    let succes, rslt = Int32.TryParse(str)

    if succes then Some(rslt)
    else None

let recupNegatif liste =
    let listeFiltree = List.filter (fun x -> x < 0) liste
    if List.length listeFiltree > 0 then Some(listeFiltree)
    else None

let neg = recupNegatif [1;-2; -3; 4; 5;-8; 9];
printfn "Résultat de l'extraction = %A" (Option.get neg)

