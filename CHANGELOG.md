# Changelog

The paclet version in `PacletInfo.wl` is the single source of truth and
changes only when an artifact leaves the machine. MAJOR breaks stored
data or the API; MINOR adds capability; PATCH is fixes and docs.

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
