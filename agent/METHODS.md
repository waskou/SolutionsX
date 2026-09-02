# METHODS — physics/maths approach hints, keyed by symptom

EXPERIMENTAL. Consult when a computation hiccups (timeout, stall, blow-up):
the question is never "how do I wait longer" but **"is my approach the
right shape?"**

**Read every entry as a suggestion, not a theorem.** Each was earned on
one or two real problems: the heading is the symptom, the bullet is a
DEFAULT worth trying first, and the italic tail is the evidence — a cost
record, not a proof. Several entries name their own counterexample; that
is the file working as intended. A method that pays on one geometry can
lose on the next, so probe before committing a long chain, and when an
entry fails you in a new regime, record that rather than silently working
around it.

Curated by Vasil + Claude only (2026-08-26). An entry enters ONLY with a
cost record attached.

## Symptom: Simplify/Together on components is slow or never returns

- **Polynomialize the angle.** Mathematica is slow on trigonometric — and
  equally on hyperbolic — rationals even with good assumptions. Replace
  the angular coordinate by a polynomial one. **There is no fixed
  recipe**: cos θ = ±x, sin θ = x, cosh χ = x are all candidates, and the
  choice is decided by two requirements, not by convention.
  - **No residual radical in the metric.** Whichever function you set to
    x, its partner survives as a `Sqrt` — set cos θ = −x and sin θ becomes
    √(1−x²). The substitution only pays if that radical cancels, which it
    does when the partner appears in even powers or paired with its
    differential (sin θ dθ = −d cos θ = dx). **Read the metric to choose,
    not the coordinate**: if the surviving partner appears in an odd power
    on its own, pick the other substitution.
  - **No accidental orientation flip.** cos θ = −x sends θ: 0→π to
    x: −1→+1; cos θ = +x reverses it.
  Store conversion rules back to θ so the result can be compared with the
  paper. Cost record, same machine, same checks: Ricci apply-map
  1 m 05 s -> 1 s; Einstein 16 s -> 0.1 s; Maxwell 3 x 110 s TIMEOUT ->
  0.1 s — the obstacle did not get faster, it stopped existing. Corpus
  model: `Sol__5dL__CCLP__Boyer-Lindquist`, where the angle is already the
  polynomial variable. *(2106.05571 sec 2, 2026-08-26)*
- **The cheapest simplification is a stronger theorem.** Before tuning a
  check, ask what it is a check OF. Maxwell for the Plebanski-Demianski
  ansatz holds for ARBITRARY structure functions, so substituting the
  explicit quartics was gratuitous work; keeping them symbolic turned
  3 x 110 s of timeouts into 7 s -- and verified more.
  *(2106.05571 sec 2, 2026-08-26)*
- **Try the metric functions SYMBOLIC through Compute** (with
  polynomial-degree assumptions, e.g. `Derivative[3][P][r] == 0`),
  substituting explicit forms only in the final check. On a 4d geometry
  chain: 32 s -> 24 s and 3x smaller stored components; on bigger chains
  the difference between minutes and hours.
  **Known counterexample, and the reason matters more than the case:**
  `Sol__5dL__CCLP__Boyer-Lindquist` (the non-susy CCLP) is better done
  with the functions EXPLICIT, because **generic functions hide the
  simplifications that only the explicit form makes available** — the
  cancellations are properties of the particular polynomials, invisible
  while they are abstract heads. So the trade is real in both directions:
  symbolic keeps expressions small, explicit lets them collapse. Which
  wins is a property of the functions, not of the method.
  *(accelerating-AdS4 00b, 2026-08-25; CCLP counterexample and mechanism,
  Vasil, 2026-08-26)*
- **Reach for Together on series coefficients before Simplify.** Simplify
  pays a rationalization cost per coefficient, and Together is often the
  work actually needed. Supporting case: ~6-8 min -> seconds. If Together
  leaves the coefficients in an unusable form, Simplify is still there.
  *(accelerating-AdS4 07, 2026-08-25)*
- **Sqrt of a determinant: peel the perfect square yourself.** Mathematica
  does not recognize Sqrt[bigRational] as a perfect square; factor the
  determinant (FactorSquareFree), take the root of the square part once
  under stated positivity assumptions, and series-expand (1+u)^(1/2) as a
  rational series. *(accelerating-AdS4 07 bdySer diagnosis, 2026-08-25)*
  Corollary: evaluate a quantity DEFINED as a square root squared -- a
  cone angle at a simple zero of W is `c^2 = W'^2/(4 N W)`, a rational
  limit with no assumption gymnastics. In a polynomialized chart the
  perfect squares are everywhere (boundary density, horizon area element,
  charge measure) and all peel by hand. *(2106.05571, 2026-08-26)*

## Symptom: a limit or series at a (double) root is not returning

- **Take limits by polynomial arithmetic (explicit L'Hopital) rather than
  Series at the root.** Numerator/denominator derivatives + evaluation,
  with a regularity guard. Cost of the wrong shapes: ~25 min (full
  contraction) + ~15 min (Series at the 0/0) vs seconds.
  *(accelerating-AdS4 03, surface gravity, 2026-08-25)*

## Symptom: a perturbative matching/expansion problem explodes or
## an all-orders Solve stalls

- **Solve strictly order by order; keep series algebra on formal heads;
  go explicit only per-order.** All-orders explicit Solve and
  polynomialize-then-shift both die (2.1M-leaf intermediates). Watch for
  formally-overdetermined leading orders whose compatibility is an
  identity of the explicit coefficients — probe the unknown-content of
  each equation. *(accelerating-AdS4 04, six shapes ~2 h, 2026-08-25)*
- **Boundary/holography: the boundary lives in the expansion, not in the
  exact slice.** Parameterize the induced object with an abstract ansatz
  capturing the perturbative solution; compute with abstract functions;
  substitute the known series at extraction time. Cost of the wrong
  shape: ~70 min of Simplify on 10^4-term bivariate rationals, stopped by
  the human. *(accelerating-AdS4 06, THE central failure, 2026-08-25)*
- **...and for a COMPOSITE object, expand formally first.** Substituting
  the exact slice data and then expanding dies in every arrangement: the
  combination forces one enormous multivariate rational normalization
  even though each ingredient is small. Two stages instead. (1) Formal:
  replace each abstract slice function by a Laurent ansatz with small
  formal coefficient heads (`htt0[x]`, `htt1[x]`, ...) and expand -- every
  order of the bracket becomes a compact formula in those heads, and the
  divergent orders cancel FORMALLY. (2) Explicit: one cheap `Series` per
  slice function (coefficients `Together`-ed rather than `Simplify`-ed),
  substituted as FUNCTION-rules into the per-order formulas only. Cost of
  the wrong shape: 3 shapes x 120-240 s of timeouts -> 7 s formal + 48 s
  + 105 s explicit. Watch the per-object orders -- see TRAPS.
  *(2106.05571 sec 3.1, the T_ij extraction, 2026-08-26)*

## Symptom: an integral over a horizon/on-shell surface hangs

- **Apply the on-shell rules FIRST** (e.g. m -> m(r+) from Q(r+)=0):
  without them determinants do not collapse and integrands are
  unintegrable Sqrt monsters. *(accelerating-AdS4 03 entropy, ~10 min,
  2026-08-25)*

## Symptom: Simplify returns a big non-zero-looking expression that a
## numerical spot-check says is zero

- **The CAS is the obstacle, not the physics -- find the one thing it is
  trying to rationalize.** If the expression is rational in everything
  except a single overall radical, keep the radical SYMBOLIC, supply its
  variation through its rational square (`dk = d(k^2)/(2k)`), then trade
  even powers of the radical for that square in the expanded numerator
  and `Together`. Cost of the wrong shape: 37 s of 10^4-leaf garbage ->
  2.9 s of exact zeros, on an 8-parameter first law.
  *(2106.05571 sec 3.3, 2026-08-26)*

## Choosing an apply-map

- **One measurement, on one problem — a starting guess, not a rule.**
  Abstract functions were fastest with `Map -> Simplify` (3d geometry
  chain ~1 s); explicit rationals with `Map -> Together` then
  `ParallelMap -> Simplify` (boundary chain 31 s). `ParallelMap` also paid
  directly on component extractions and on independent residuals.
  *(2106.05571 sec 3, 2026-08-26)*

## Discipline that makes wrong shapes cheap

- **The Compute recipe is an art, discovered by cheap experiment — it is
  not looked up, and nothing here is hard-coded.** Substrate (symbolic vs
  explicit functions), apply-map, and simplifier are per-problem choices
  that depend on the actual functions in front of you. Probing ONE
  component costs seconds; inheriting a recipe from this file costs a
  whole chain. Print LeafCount and compare — the number argues where prose
  does not. Every Compute-shape entry above is a starting guess for that
  experiment, never its conclusion.
- Pair every must-be-ZERO check with a must-be-NONZERO target: a zero can
  be vacuous (empty Sum, dead symbol). *(caught a silently-zero pullback,
  2026-08-25)*
- Ask the NUMERICAL question before the symbolic one: random rational
  values answer "is this zero?" in under a second, and only then is it
  worth spending `Simplify` on proving it. *(the one avoidable burn of
  the 2106.05571 sec 3 run: 37 s, 2026-08-26)*
- Budget atomic calls (sxk try; Simplify's TimeConstraint option) — a
  2-minute verdict beats a silent half-hour.
