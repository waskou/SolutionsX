# agent/ — the SolutionsX agent kit (EXPERIMENTAL)

Tooling for AI agents working with SolutionsX on behalf of a human user.
The mission: *read the existing corpus for hints, methods, and how your
user writes their own stuff; read a paper; author something your user
could have created, to aid them.*

Status: experimental, under active design; macOS/Linux, bash, shell+WL
agents only (an evaluator-only agent cannot restart a poisoned xAct slate
— the one reliable recovery — and is not supported yet). Design record:
`SolutionsX_Developement/lab/agent-tools/DESIGN.md` (private).

| tool | what |
|---|---|
| `sxk` | persistent Wolfram kernel driver: cells as submissions, budgets (`try`), monitoring (`bg`/`watch`), the user-note channel (`tell`/`ack`), honest `status`, `log` (the replayable session), `again` (restart + prefix replay), `chan on|off` -- the Compute progress channel (below) -- and `set`/`config`, the configuration levers. `sxk` with no args prints the full contract, including the rules the transport imposes |
| `kit.conf` | the kit's configuration: every lever explained in place, each marked [enforced] (sxk acts on it) or [advisory] (sxk displays it, LADDER.md tells the agent to act on it). Precedence: `sxk set` (session) > env `SXK_<KEY>` > `kit.conf` > built-in; `sxk config` shows effective values with provenance. An owner's `sxk set` mid-run is surfaced to the agent like a user note until acked |
| `LADDER.md` | the stall ladder: what counts as a shape and a verified fact, when to stop spending (STALL_SHAPES / STALL_MINUTES from the config), and the STALLED.md report that stopping produces |
| `nb2txt.wls` | notebook -> text by the authorship rule: everything the human typed (headings, Text cells, Input cells with comments, verbatim), nothing the kernel generated; `io` flag adds generated cells trimmed to Short skeletons |
| `nb-author.wls` | typed sources (a transcript .txt or a sources .wl) -> inputs-only .nb the user can run and tweak; entry mode prepends the standard init cell (position-derived identity) and asserts the entry's .m untouched; bare-.nb mode is the exact inverse of `nb2txt.wls` |
| `reporter` | cold-replays a script on a fresh, isolated kernel and reports: full output, every message-looking line, diff vs a reference if given. No verdicts — judging the report is your job |
| `METHODS.md` | physics/maths approach hints, symptom-keyed, each with the cost record that earned it |
| `TRAPS.md` | silent failures of the CAS and the package — wrong or empty answers with no error; symptom-keyed |
| `tests/smoke.sh` | the sxk guard: 43 checks incl. the failure paths (try timeout, external kernel kill, again-recovery), the Kernel-side verbs (WorkAs, the compute channel), and the config chain (precedence, refusals, surfacing, enforcement); run on both kernels after any change |
| `tests/roundtrip.sh` | the reader/author guard: nb -> txt -> nb -> txt' must be the identity on the typed layer (modulo FE indentation normalization; CellLabels are session history, not content) |

The working contract, in one line: **cheap attempts, loud failures,
auditable claims** — budget atomic calls, monitor loops, consult the hint
files when something hiccups, and when you have tried a few shapes on one
obstacle and are still stuck, STOP and write a report for your user
instead of grinding — the full protocol, thresholds, and report format
are in `LADDER.md` (they can reach you any time via `sxk tell`).

Identity: agents write to their own alias, never the human's. The first
thing an agent runs after loading the package is `WorkAs["<owner>-bot"]`
(e.g. `WorkAs["Vasko-bot"]` — one agent alias per human): it sets `$Alias`,
drops the curator flag, never writes the user's configuration file (the
switch dies with the kernel), and verifies itself — a refused alias or a
switch that does not take aborts instead of proceeding under the wrong
identity.

Monitoring a long `Compute`: after loading the package, `sxk chan on`
points the package's progress channel at this session's `compute.log`;
`Compute` then appends one tab-separated line per component —
`unixtime <TAB> event <TAB> tensor <TAB> detail...`, events `entry`
(chain entry starts: `array|slots|scalar`), `sym`/`val`/`map` (component
i of N in symmetries / independent values / apply-maps), `maps` (entering
apply-maps), `pmap` (handed to ParallelMap, opaque from there), `done`
(chain entry finished, seconds) — and `sxk status` / `sxk watch` show a
"compute pulse" with the age of the newest line. A pulse that stops aging
forward while the kernel is BUSY is your stall signal: consult the hints,
consider whether the approach is the right shape. With the channel unset
(the default) Compute is byte-identical to before — the channel is
strictly passive, and a restart (`sxk again`) resets it to unset.
