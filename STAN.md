# Project state snapshot

One page for anyone (human or agent) landing in this repository cold. SNAPSHOT SEMANTICS:
this file describes state as of its last commit date — trust the code, manifests, and CI
over any prose here that has drifted. Canonical order when documents disagree:
committed proof manifests > module doc-comments (`.av` intents) > `projects/*/README.md`
> this file > anything else.

## What this project is

Aver is a small, total, verification-first language. Every `law` in a module is judged by
two independent backends — the Lean 4 kernel (core only, no Mathlib) and Dafny/Z3 — and
credit is recorded per law in a committed `proof_manifest.json`: `universal` (a real
∀-theorem, axiom-whitelisted), `bounded`/`sampled` (honestly weaker tiers), and an optional
`credit` marker distinguishing a labeled hand-written proof from a fully engine-derived
one. The design bet: proofs are composed from previously proven laws by a small set of
shape-keyed, name-blind engine strategies — domain content lives in the laws, never in
the engine.

## Where the frontier is (proof corpus)

- `projects/k5_fdiv/` — the flagship corpus: a faithful model of the 1996 AMD K5
  floating-point division proof. Stages 0-2 (rationals, float-as-rational, Newton-Raphson
  bounds) are fully proven on the Lean kernel; the rounding module holds 24 universal
  laws; the end-to-end theorems run and are sample-verified but are deliberately NOT
  claimed as laws — the remaining named holes are listed in `projects/k5_fdiv/README.md`
  (stage table) and in `domain/kernel.av`'s intent comment.
- One law (the trunc-sticky rounding composition) is universal via a labeled hand proof
  (`projects/k5_fdiv/proofs/lean/`); replacing it with a fully generic mechanism was
  attempted three times in 2026-07 and measured not viable with the current engine — the
  measured reason (saturation cannot invent terms absent from both sides of a goal) is
  documented next to the law in `domain/round.av`. It stays a labeled hand proof until a
  proposer exists that can introduce those intermediate terms.
- `proof-corpus/` tracks external benchmark coverage (TIP); `tests/proof_spec` is the
  behavioral gate for the proof pipeline (live Lean+Dafny in CI via the Proof workflow).

## How work happens here

- Direction and review live with the maintainer; implementation legs are routinely done
  by AI agents under written briefs with adversarial review; every substantive claim in a
  report must carry verbatim tool output. Failed attempts are recorded with their exact
  residuals — a documented negative result is a normal, valued outcome in this repo.
- `aver proof <file> --backend lean --check --check-json` is the ground truth for proof
  claims (use `--module-root projects/k5_fdiv` for the K5 corpus and remove any stale
  `out/` directory first). The JSON reports sorries, hard build errors, and Dafny
  timeouts separately — a green result means exactly what it says.
- The maintainer's working notes (plans, decision logs, research verdicts) live in a
  PRIVATE notes repository; the `prompts/` directory is intentionally gitignored here.
  This snapshot plus the K5 README plus commit messages are the public trail. If this
  file looks stale, `git log --oneline -30` is the fastest way to reorient.

## Five questions a cold reader should be able to answer after this page

1. What is proven vs open in the K5 corpus? → K5 README stage table + manifests.
2. Why does one rounding law have a hand-written proof? → measured engine limitation,
   documented at the law site; not a hidden shortcut (its manifest credit says so).
3. What judges a proof? → two independent backends + an axiom whitelist; the manifest
   is the record; nothing else grants credit.
4. How do I check a claim myself? → the `aver proof --check` invocation above.
5. Where did the plans go? → private notes by design; the public trail is this file,
   the READMEs, and commit messages.
