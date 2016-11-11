# .Net Core et F# 4.1

## Installation

<https://www.microsoft.com/net/core>

et suivre selon son OS

## Premier test

Au prompt du shell

```bash
mkdir testCore
cd testCore

dotnet new -l f#
```

Le système va télécharger les dépendances.

```bash
ls
```

On verra un certain nombre de fichiers : si NuGet.Config existe alors il faudra le supprimer. On bidouillera le code de Program.fs

```bash
dotnet restore
dotnet build
dotnet run
```
