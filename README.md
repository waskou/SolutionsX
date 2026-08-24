# SolutionsX

<p align="center">
  <img src="images/apples.jpg" width="400" alt="Two apples comparing notes under a burst of curvature">
</p>

A Wolfram Language paclet for **storing, verifying and reusing
supergravity solutions**, built on the [xAct](http://www.xact.es) tensor
computer algebra suite.

A *solution* in SolutionsX is a validated association of typed objects —
manifolds, metrics, frames, spin structures, covariant derivatives,
charts, forms, tensors, spinors, functions, rules — that can be saved to
disk, retrieved, and turned back into live xAct definitions with a
single `Load`. Component values are computed once, stored with their
recipes, and replayed instead of re-derived.

## Requirements

- Wolfram Language **13.3 or later** (Mathematica or the free Wolfram
  Engine).
- The **xAct** suite, installed from [xact.es](http://www.xact.es).
  Installation is a copy-paste: download from the
  [xAct download page](http://www.xact.es/download.html), unzip, and
  place the `xAct` folder in your Wolfram `Applications` directory
  (the download page lists the exact location per operating system).
- **FieldsX** (Markus Fröb), installed from
  [github.com/mfroeb/FieldsX](https://github.com/mfroeb/FieldsX):
  place `FieldsX.m` inside the installed `xAct` folder. FieldsX
  further requires **`Multisets.m`**, downloaded from the
  [Wolfram Library Archive](https://library.wolfram.com/infocenter/MathSource/8115/)
  and placed in the `Applications` directory beside `xAct`.

SolutionsX refuses to load with xAct or FieldsX missing and says
exactly what is absent.

## Installing

**As a user** — install the released paclet (available from the first
public release, via the Wolfram Paclet Repository or the `.paclet` asset
on a GitHub Release):

```wolfram
PacletInstall["VasilDimitrov/SolutionsX"]
Needs["VasilDimitrov`SolutionsX`"]
```

**From a clone** (developers, curators) — build and install with the
setup script, then load the same way:

```sh
wolframscript -file setup.wls
```

## First steps

```wolfram
Needs["VasilDimitrov`SolutionsX`"]
Welcome[]
```

`Welcome[]` guides the first session: where your data lives, your
alias, and which curated entries to copy under it. The curated corpus
ships inside the paclet, so this works with or without a clone.
Headless (or by hand), the same steps are:

```wolfram
SetAlias["you"]                (* once; your entries live under Data/you *)
CopyData["Curated"]            (* copy the published entries under your alias *)
sol = GetData["Thr__4dL__Minimal-gauged"];
Load[sol]
```

The full documentation lives in the Wolfram Documentation Center after
installation: search for **SolutionsX** to reach the guide page, 39
reference pages and the tech notes (all examples are executed, never
asserted). `NewData[]` opens a fresh working notebook; `SaveData[sol]`
stores your own entries.

## Data

Entries live under `Data/<alias>/`, one directory per entry, holding the
machine-readable record (`.m`) and its evaluation notebook (`.nb`).
`Data/Curated/` is the published corpus, maintained by the curator;
everything you save goes under your own alias and is never touched by a
`git pull`.

## License

MIT — see [LICENSE](LICENSE). Contact: vasildimi@gmail.com.
