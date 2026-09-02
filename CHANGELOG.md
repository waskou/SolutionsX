# Changelog

The paclet version in `PacletInfo.wl` is the single source of truth and
changes only when an artifact leaves the machine. MAJOR breaks stored
data or the API; MINOR adds capability; PATCH is fixes and docs.

## [2.1.0] — 2026-09-02

Everything since the launch release, gathered into one: the reworked
compute engine, the data browser, the agent verbs, and the license
change.

### License

- **The code is now licensed AGPL-3.0-or-later** (was MIT; versions up
  to and including 2.0.0 remain MIT). The curated data corpus
  `Data/Curated/` is licensed CC BY 4.0, with per-entry attribution via
  the `Author` field of each record's `$info`. `CITATION.cff` added;
  `Welcome[]` closes with a citation pointer.

### Added

- **`ShowData`**, the data browser: every stored entry across aliases
  and the bundled curated corpus, with the entry verbs (`OpenData`,
  `CopyData`, `CurateData`, `DeleteData`) live exactly where the verb
  itself would act.
- **`WorkAs`**, the session identity verb for registered agents, and a
  `Compute` progress channel (experimental, documented in `agent/`).
- **The agent toolkit ships in `agent/`** (experimental): the `sxk`
  persistent kernel, the notebook transcriber and author, the
  METHODS/TRAPS/LADDER notes and the `kit.conf` configuration.
- **`Data/Vasko-bot/` ships**: an agent-produced corpus (CC BY 4.0,
  like `Data/Curated/`), published so the companion paper's claims can
  be checked against the notebooks that produced them.
- Two curated entries: `Thr__4dE__Kahler-base` and
  `Sol__4dE__Kahler-base__Orthotoric`.

### Changed

- **`Compute` is a reworked engine** (the previous engine is retired in
  place): the grid of unfinished components is built eagerly, dependency
  analysis picks the independent ones before resolving, and `ToValues`
  iterates to a fixed point. A chain entry now fills the whole orbit of
  the configuration it names under the signed symmetry group — a
  single-slot raise is correct and complete. Stricter contracts: a slot
  specification on a slot-less object is an error (`Compute::scalar`),
  never decoration; a non-`System`` symbol in the `ParallelMap`
  simplifier falls back to a serial run with `Compute::serial` naming
  the symbols, instead of silently storing unresolved values.

### Fixed

- Literal-array chain entries into a multi-configuration orbit stored
  wrong components silently; the array branch is now configuration-aware
  and correct by symmetry.
- Parallel simplification lost the record's assumptions in the
  subkernels (radicals stopped recombining); every loaded symbol is now
  marked in the subkernels before each parallel map, with a load-time
  tripwire (`General::solxmarker`) should the mechanism ever go stale.

### Removed

- **`EnableParallelComputations`**, symbol and documentation page — a
  no-op with nothing left to enable; `NewData[]` writes a two-line init
  cell.

## [2.0.0] — 2026-08-24

The renamed continuation of SolutionsY: fresh tree, fresh history, and a
data corpus curated anew. Highlights relative to the last SolutionsY:

### Added

- **Multi-user Data**: identity (`$Alias`) and location in a per-user
  configuration file; `SetAlias`, `SetDataDirectory`; read from any
  alias (`"alias/name"`), write only to your own.
- **Curation framework**: the `Curated` corpus as a role, never an
  identity — `SetCurator`/`$Curator`, `CopyData` (foreign → own),
  `CurateData` (own → Curated), `Author` provenance surviving every
  path.
- **The Grade guard**: undefined applications get `Indeterminate` grade
  instead of xAct's scalar-classifying 0, so products of not-yet-defined
  forms survive storage and reload unchanged.
- **`$info` stamp** on every save: SolutionsX version, author, date,
  Wolfram and full xAct versions (with build dates).
- **Notebook workflow verbs**: `NewData`, `OpenData`, `LocateData` (data
  root derived from the notebook's own position), `ResumeAs` (resume
  from disk or re-show the load banner, same notebook both ways).
- **Documentation**: a guide, 39 reference pages and 4 tech notes, all
  generated from source files with executed examples.

### Changed

- Tensorial expressions are stored naked — the metric-`HoldForm`
  storage workaround is gone; records are written exactly as they live
  and never mutated on read.
- Version tolerance lives in the validator; stored records are never
  rewritten.

### Removed

- The stored `$self` property (the chain placeholder `$self` remains).
- The public `$Loaded` flag.
- The dead `Version`, `Label` and `Backup` options.
