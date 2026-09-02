# LADDER.md — the stall ladder (EXPERIMENTAL)

When you are stuck, the expensive failure is not being wrong — it is
grinding: burning hours and tokens circling one obstacle while your user
watches, unable to help because nothing legible comes out. This ladder
turns "stuck" into a cheap, structured stop. It exists because exactly
that grind happened (the accelerating-AdS4 campaign's failure ledger);
the thresholds are tunable defaults, not doctrine.

## The loop

    attempt -> hiccup -> consult the hints (METHODS.md, TRAPS.md)
            -> reformulate as a DIFFERENT SHAPE -> attempt again

- A **hiccup** is any loud failure: a `try` timeout, a stale compute
  pulse, a kill, a wrong or suspicious result.
- A **shape** is a structurally different approach: a different
  substrate (abstract functions vs explicit forms), a different method
  (order-by-order vs all-orders, L'Hôpital vs Series), a different
  decomposition. A retry, a bigger budget, or a parameter tweak is NOT
  a new shape — it is the same shape costing more.
- A **verified fact** is a statement you established by a check that
  actually ran and returned a definitive answer (including a definitive
  negative: "this route provably fails because X"). Progress is measured
  in facts, not in effort spent.

## When to stop

Check `sxk config` for the current thresholds; the owner may change
them mid-session (`sxk set`), and changes are surfaced to you like user
notes until you ack them. Stop spending when EITHER:

- **STALL_SHAPES** distinct shapes have failed on the same obstacle, or
- **STALL_MINUTES** minutes have passed without a new verified fact
  (a busy kernel whose compute pulse is older than this is flagged by
  `sxk status`/`sxk watch` — a report, not a verdict; judge it).

## A budgeted failure is a purchase

Field record (2106.05571, two runs, 2026-08-26): the kernel spent as much
time on shapes that timed out as on shapes that worked — 480 s of budgets
on one obstacle, 3 x 110 s on another. Neither was waste. The 480 s
located the obstacle precisely enough to find the method that made the
whole section tractable; the 330 s established a definitive negative — the
route was not merely slow — which is a verified fact. **What is waste is
the unbudgeted attempt**, not the budgeted one that fails.

Two corollaries earned on the same runs:

- **At the threshold, switch from grinding to diagnosis.** Do not spend
  the next budget on shape #4 chosen blind. Ask, once and in prose before
  touching the kernel, what the computation is a check OF and why it is
  expensive. That question dissolved the hardest obstacle of the sec 2
  run with no new machinery — the check held for arbitrary structure
  functions, so the expensive substitution was gratuitous.
- **The channel is the other half of the ladder.** The single largest
  speed-up of the campaign — three orders of magnitude — came from the
  human interrupting with a method, not from the agent's next shape.
  Reaching the threshold is a reason to make the obstacle legible;
  `sxk tell` runs both ways.

## What stopping means

STOP SPENDING. Do not launch one more speculative attempt. Write
`STALLED.md` in your working directory:

    # STALLED — <one-line obstacle>
    Goal: what this session is trying to produce, one sentence.
    Obstacle: the specific thing that blocks it.
    Shapes tried:
      1. <shape> — <cost (time/budget)> — <how it failed, verbatim
         error or wrong value, not a paraphrase>
      2. ...
    Verified facts so far: what IS established, with how to re-run each.
    Current hypothesis: your best guess at why this is hard.
    THE QUESTION: the single specific question whose answer unblocks
    you. This is the most important line in the file.

Then end loudly: say you are stalled, point at STALLED.md, and stop.
A precise question after 30 wasted minutes beats a vague success claim
after four wasted hours — the expected yield of ideas is mostly-wrong,
and stopping on schedule is what makes attempts cheap.

## Report only what you measured

A timestamp you did not read is a fabrication that looks like data. In
the sec 2 run the headings of a running log were written from a sense of
elapsed time; they drifted +60 min by the end, and the report's headline
wall-clock figure — computed from them — was wrong by 2.5x. Shell out for
the clock at each entry, or stamp entries from file mtimes. The same rule
governs every number you hand your user: mark it **exact** (filesystem or
in-kernel measurement), **measured** (a counter you read), or **modelled**
(reconstructed, assumptions stated) — and say which. *(2026-08-26)*

## The two-way channel

Your user can reach into your loop at any moment with `sxk tell`; every
sxk command shows the note until you `sxk ack` it. **A user note
outranks everything: respond before proceeding.** An `sxk stop` with a
note attached is an explained brake-slam — on recovery (`sxk again`),
read the note BEFORE diagnosing the kill.
