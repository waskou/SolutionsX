# TRAPS — silent failures of the CAS and the package, keyed by symptom

EXPERIMENTAL. These are not physics and not style: they are places where
the kernel or the package can give you a WRONG or EMPTY answer with no
error. A human squints at a suspicious output; an agent records it as
truth — this list is the antidote. Curated by Vasil + Claude only
(2026-08-26); an entry enters only with the run that earned it.

## Symptom: what you stored is not there, or is 0

- **`IncludeTo` drops a whole malformed batch silently.** `n\[Minus]`,
  `n\[Plus]`: `\[Minus]` and `\[Plus]` are OPERATOR characters, not
  letters, so the list parsed into garbage and NOTHING was stored -- not
  even the well-formed elements beside them. No message.
  **Read back `Keys@sol[$constant]` (or `$rule`, `$function`) after every
  batch `IncludeTo`.** *(2106.05571 sec 2, ~3 min, 2026-08-26)*
- **`IncludeTo` on a GetData'd but not Loaded entry returns UNEVALUATED**
  and stores nothing, with no message. Writes need the Loaded/`ResumeAs`
  entry. Caught by the same read-back guard.
  *(2106.05571 sec 3, 2026-08-26)*
- **Text that a front end wrapped is not WL you can re-parse.** `nb2txt`
  breaks expressions at display width and does not mark which newlines
  were typed; a product wrapped after a syntactically complete prefix
  becomes TWO expressions under `Get` or `sxk do <file>`. In the BTZ run
  `VarD` then acted on 1, returned 0, and `IncludeTo` stored a zero
  equation of motion -- surfacing only as `Solve::naqs` far downstream
  (repair: `DropFrom` + re-run). **Author transcripts and `.wl` one
  top-level expression per line; never split a product across lines.**
  `tests/roundtrip.sh` is blind to this -- it is the identity on the
  typed layer while the `.txt` is still unsafe as input.
  *(BTZ campaign, ~4 min + repair, 2026-08-26)*

## Symptom: $RecursionLimit blowup, ValidateObject failure, or SaveData
## throwing Hold[Throw[$rule]]

- **A scratch handle is bound to a symbol the entry itself uses** -- a
  constant, a rule key, a function name. `m2rp = Gen@sol[$rule, m2rp]`
  corrupted the `Keys` display and made `SaveData` throw; binding `a`
  (the rotation parameter) in a verification script made stored
  expressions evaluate back into the whole solution object. Same trap
  twice. Name handles after the ENTRY (`pd`, `ind`, `cft`), never after
  its physics. The same hygiene catches built-in collisions -- `Cot` is
  Protected and silently blocked a Cotton-tensor name.
  *(2106.05571 sec 3, 2026-08-26)*

## Symptom: Solve returns {} (possibly after minutes) on a solvable system

- **Load installs the entry's assumptions into $Assumptions, and Solve
  consults them.** Chart-scalar conditions + parameter positivity can
  make Solve return {} silently. Fix: every Solve in entry-driving
  scripts runs inside `Block[{$Assumptions = True}, ...]`. Downstream
  poison: `First[{}]` propagates quietly.
  *(accelerating-AdS4 03, ~20 min, 2026-08-25)*

## Symptom: a residual contains an unresolved Derivative[1][f][x] head

- **You substituted value-rules (f[x] -> expr) where derivative heads
  enter.** Build FUNCTION-rules (f -> Function[...]), bottom-up in solve
  order. *(accelerating-AdS4 04, gamma-chichi component, 2026-08-25)*

## Symptom: a check prints 0 suspiciously easily

- **Zeros can be vacuous.** An empty Sum (sign-flipped Exponent made the
  range {k, 2, 1}), a dead symbol, a skipped substitution — all print 0.
  Demand a NONZERO companion target (e.g. the paper's boundary metric).
  *(accelerating-AdS4 04, three of four pullbacks silently zero,
  2026-08-25)*
- **SeriesCoefficient[0, k] stays inert** on plain zeros and poisons
  downstream algebra. Guard the extraction.
  *(accelerating-AdS4 07, 2026-08-25)*
- **Series orders are per-object, not uniform.** The inverse metric may
  lead at z^2 while the extrinsic curvature leads at z^-2; requesting the
  same order everywhere truncates some objects below what their
  combination needs, and the validity window silently collapses. The
  result is `SeriesCoefficient`-shaped junk, not an error. Request per
  object and assert the leading coefficient you expect.
  *(2106.05571 sec 3.1, two false starts, 2026-08-26)*

## Symptom: an error seems to come from the wrong function / Quiet hides it

- **GetData/ValidateObject failures surface as Throw::nocatch**, not a
  clean message — read it as "validation failed" and look above it.
- **DeleteData on a missing entry Throws; Check does NOT catch Throws.**
  The re-runnability guard is `Quiet@Catch[DeleteData[...]]`.
  *(accelerating-AdS4 03/05, ~15 min incl. bisect, 2026-08-25)*

## Symptom: parallel Apply seems to run but results are unchanged

- **`EnableParallelComputations[]` no longer exists** (removed 2026-08-28,
  with the engine it patched). There is nothing to switch on: `Compute`
  evaluates `ParallelMap` entries of `Apply` across the parallel kernels on
  its own. If you meet it in an old notebook or an old transcript, delete
  the line. *(It used to be a silent no-op class: the Echo still claimed
  "Applied Simplify to ..." while every Apply was dropped. BTZ rehearsal
  LOG §5; promoted to blocker in the agent-kit design, then fixed at the
  root.)*
- **A `ParallelMap` simplifier mentioning a non-`System`` symbol is run
  serially, and says so** (`Compute::serial`). It is not dropped and the
  values are correct, but the parallelism is gone. The fix is to do the
  substitution in a `Map` entry first and leave a built-in simplifier under
  `ParallelMap`. Before 2026-08-28 this stored UNRESOLVED values silently —
  if you are reading an older transcript, distrust it.

## Symptom: kernel-driving weirdness (sxk / wolframscript)

- **Needs[...] needs its own submission** — a symbol named beside its own
  Needs *in one parsed unit* stays shadowed in Global`. Scope: submissions
  typed as a unit. A FILE handed to `sxk do` is parsed incrementally, so
  `Needs` on its own line followed by package symbols is safe there, and
  `%` chains across the file's lines work. *(narrowed on evidence, BTZ
  campaign, 2026-08-26)*
- **% chains live within ONE submission** (notebook-cell granularity);
  across submissions % is the plumbing token's Null.
- **SIGINT KILLS a -noprompt kernel** — there is no graceful abort; use
  budgets (sxk try) in advance, `sxk stop` + `sxk again` after.
  *(tested 2026-08-26)*
- **wolframscript eats leading-dash script arguments** — flags for .wls
  scripts must be dashless. *(nb2txt io flag, 2026-08-26)*
- **xTerior then FieldsX loses the Wedge-Diff grade rule** — never load a
  partial xAct stack; use the package's nine, in its order.

## Symptom: the notebook you delivered is not the one you authored

- **Its input cells have line breaks nobody typed.** You modeled your
  transcript on `nb2txt` output. That text comes from the FE's
  `ExportPacket["InputText"]`, which hard-wraps at display width; and
  `nb-author` parses each `<input>` block whole-cell with **line breaks
  preserved in the boxes** (deliberate, so real typed breaks survive). So
  every display wrap becomes a hard break in your notebook. Read a corpus
  cell for its IDIOM, never for its line shape, and **write your own
  transcript one top-level expression per line** — display wrapping is
  the reader's front end's job. Tested 2026-08-26 and both automatic
  repairs FAIL: `PageWidth` on the cell does not suppress the wrap
  (None/Infinity/500/60 byte-identical), and joining lines until they
  parse re-splits `1/Sqrt[-Detgg[]]` from its `VarD[...]` factor — the
  export destroys the distinction and nothing downstream recovers it.
  *(BTZ campaign, flagged by Vasil on first open, 2026-08-26)*
- **Headless `SaveData` CAN rewrite the entry's `.nb`** — it keeps the
  typed layer and merges in the session's evaluation cells (74k -> 522k,
  +24 Output +26 Echo in one incident). The opposite belief, carried in
  from an earlier campaign, is WRONG. **Author the notebook LAST, after
  the final `SaveData`**, and `ls -lT` the entry directories at the end of
  a run. *(2106.05571 sec 3, 2026-08-26)*
- **Raw UTF-8 in an `nb-author` `<text>` cell** is written by `Put` as
  byte-escape garbage (`\[AHat]\200\224`) and the front end renders
  mojibake. Input cells are immune (the FE parser handles `\[...]`).
  Keep text cells ASCII-only; the `nb -> txt` roundtrip is what catches
  it. *(2106.05571 sec 2, ~10 min — the largest single setback of that
  run, 2026-08-26)*
- **`nb-author` entry mode REGENERATES, inputs-only** — running it on an
  entry whose notebook carries the human's evaluated cells destroys them.
  To extend such a notebook, append at cell level,
  `Put[Notebook[Join[First@old, First@new], Sequence @@ Rest@old], file]`,
  and compare the Output/Echo cell counts before and after.
  *(2106.05571 sec 3, 2026-08-26)*
