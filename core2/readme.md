# .NET Core 1 et F# 4.1 : 2<sup>nd</sup> exemple

Objectif : création d'une bibliothèque et utilisation de celle-ci dans un programme.

```bash
mkdir core2
cd core2
dotnet new -l f#
ls
dotnet restore
donet run
```

Tout fonctionne. On crée un nouveau fichier :

```bash
touch Bibliotheque.fs
atom Bibliotheque.fs
```

On crée un certain de fonctions. On modifie **project.json** :

```json
"compile": {
  "includeFiles": [
    "Bibliotheque.fs",
    "Program.fs"
  ]
}
```
:warning: L'ordonnancement des fichiers importe. On compile d'abord la bibliothèque puis le programme contenant l'EntryPoint !


On passe à la dernière modification : celle de **Program.fs**


Une fois les modifications effectuées, on passe à la compilation et à l'éxecution du code.

```bash
dotnet run 1 2 3 4 5
```

## Utilisation avec le REPL (fsharpi) :

```fsharp
#load "Bibliotheque.fs";;
open FSI_OOXX.Essai.Bibliotheque;;
decremente 3;;
```

FSI_OOXX où XX doit être remplacé par la valeur fournie par le REPL
