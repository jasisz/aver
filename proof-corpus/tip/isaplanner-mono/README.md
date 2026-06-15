# isaplanner-mono — monomorphized renderings of the higher-order TIP problems

The published IsaPlanner benchmark is 85 problems. Eight of them are
**higher-order** — they pass a function as an argument:

| prop | conjecture | higher-order fn |
|------|------------|-----------------|
| 12 | `drop n (map f xs) = map f (drop n xs)` | `map f` |
| 14 | `filter p (xs ++ ys) = filter p xs ++ filter p ys` | `filter p` |
| 35 | `dropWhile (λx. False) xs = xs` | `dropWhile` + λ |
| 36 | `takeWhile (λx. True) xs = xs` | `takeWhile` + λ |
| 41 | `take n (map f xs) = map f (take n xs)` | `map f` |
| 43 | `takeWhile p xs ++ dropWhile p xs = xs` | `takeWhile`/`dropWhile p` |
| 66 | `len (filter p xs) ≤ len xs` | `filter p` |
| 73 | `rev (filter p xs) = filter p (rev xs)` | `filter p` |

Aver is **first-order with no closures** (whole-program monomorphized — the
property that buys exact effect footprints, provable `!`, etc.). The stdlib has
no `List.map`/`filter`/`takeWhile`/`dropWhile`. So these 8 **cannot be written
natively** — they are *inexpressible*, not unproven, and are absent from
`../isaplanner/` (which holds the 77 natively-expressible standard problems +
one extra, `prop_86`).

This directory renders those 8 **the Aver way**: the function/predicate is
*fixed to a concrete monomorphic instance*, turning the higher-order operator
into an ordinary first-order recursive function.

## The asterisk (read before quoting any number)

A monomorphized rendering is a **strictly weaker statement** than the
higher-order original: it loses the `∀`-over-function quantifier (one concrete
`f`/`p` instead of all of them). It is **not** apples-to-apples with a
higher-order prover that proves the full `∀ f. …`.

It *is* the same **inductive problem**: in all 8, `f`/`p` is parametric — it is
carried opaquely or cased on identically — so the induction is the same shape
the higher-order proof would use. Whole-program monomorphization also means a
real Aver program never has a generic `map f`; it has `mapSucc`, `filterZero`.
So this is the *idiomatic* form, not a degraded one.

**Non-degeneracy is required.** A constant `f = id` would collapse the problem
(`map id xs = xs`) and prove nothing. Instances chosen here:

- `map f` → `mapS` applies `Nat.S` (successor) — genuinely transforms each element.
- `filter p` / `takeWhile p` / `dropWhile p` → predicate `isZ` (is-zero) — keeps a
  proper, non-trivial sublist.
- prop_35 / prop_36 are **faithful by construction**: their predicate *is* the
  constant `False` / `True`, so `constFalse` / `constTrue` is the exact original,
  no instance choice involved.

This is a **separate, asterisked metric** — excluded from `run.sh`'s headline
`coverage (union)` so it can never be conflated with the strict
natively-expressible number.

## Result (measured 2026-06-15, Lean kernel-genuine, no discovery)

**6 / 8 universal:** prop_12, prop_14, prop_35, prop_36, prop_41, prop_43.

Open: prop_66 (needs a `le`/`S` helper lemma), prop_73 (needs
`filter (xs++ys) = filter xs ++ filter ys`, i.e. prop_14, as a helper to push
`filter` through `rev`'s append). Both are exactly the cases a higher-order
prover closes via **lemma discovery / theory exploration** — honestly open here
in the no-discovery baseline.

## Denominators (apples-to-apples with the published 85)

- **Strict native:** 41 / 85 — Aver proves 41 standard problems; the 8
  higher-order ones count as not-done because they are inexpressible.
- **With this asterisk:** 47 / 85 — adding the 6 monomorphized closures.

For comparison: IsaPlanner 47/85 (rippling + lemma discovery), Dafny 45/85 (Z3,
prover-accepts). Aver's numbers are Lean kernel-genuine
(`#print axioms ⊆ {propext, Classical.choice, Quot.sound}`) and discovery-free —
a trust × no-discovery cell no published tool occupies. The asterisked 47
landing on IsaPlanner's 47 is a coincidence of counts, **not** of statement
strength: 6 of Aver's are the weaker fixed-instance form.
