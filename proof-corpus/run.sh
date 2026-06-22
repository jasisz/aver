#!/usr/bin/env bash
#
# proof-corpus coverage runner — measures how many proof TASKS Aver CLOSES (the
# universal law verified), as a NUMBER, not a gate. Measures BOTH backends:
# Lean (kernel-checked, trustworthy, but only Aver's hand-rolled strategies) and
# Dafny (Z3 automation — more reach, less trust). A task is COVERED if EITHER
# backend proves it; the two are also reported separately because they differ a
# LOT (Z3's automated induction proves far more of this corpus than the Lean
# strategies). The union is the honest "what Aver can prove".
#
# NOT a regression suite: most tasks are expected to be OPEN by design; an open
# task lowers the number, never fails. The must-prove regression set lives in the
# compiler repo (examples/data, examples/formal + proof_spec.rs assert_eq!).
#
# Metric per backend: `aver proof <f> --backend <b> --check --check-json`.
#   - Dafny keys on "passed": true — its check already folds the "universal
#     lemma omitted" / `assume {:axiom}` degradations into `passed`, so a
#     passing Dafny check IS an honest universal closure.
#   - Lean keys on "universal": true, NOT "passed". Lean's `passed` stays
#     lenient (a bounded `native_decide` verify-on-domain is a legitimate but
#     weaker check the regression corpus relies on); "universal" is the
#     `#print axioms`-gated signal that the ∀-claim is genuinely kernel-proved
#     (no `Lean.ofReduceBool`). Coverage must count only genuine closures.
# Retries once (lake/Z3 transient failures produce false "open", never false
# "proved", so the reported numbers are lower bounds).
#
# Usage:  ./run.sh            (uses ../target/debug/aver — build it first)
#         AVER=/path/to/aver ./run.sh
set -u

ROOT="$(cd "$(dirname "$0")" && pwd)"
AVER="${AVER:-$ROOT/../target/debug/aver}"

if [ ! -x "$AVER" ]; then
  echo "error: aver binary not found at $AVER (run: cargo build --bin aver)" >&2
  exit 1
fi

# proves <file> <backend> -> "proved" | "open" (retry once to absorb
# flakiness). The honest key differs per backend: Dafny -> "passed":true,
# Lean -> "universal":true (see the metric note above).
proves() {
  local f="$1" backend="$2" key attempt out json
  case "$backend" in
    lean) key='"universal":true' ;;
    *)    key='"passed":true' ;;
  esac
  for attempt in 1 2; do
    out="$(mktemp -d)"
    json="$("$AVER" proof "$f" --backend "$backend" --check --check-json -o "$out" 2>/dev/null | grep '"passed"' | tail -1)"
    rm -rf "$out"
    printf '%s' "$json" | grep -q "$key" && { echo "proved"; return; }
  done
  echo "open"
}

lean=0
dafny=0
union=0
total=0
# `decomposed/` holds LLM helper-law-augmented copies of OPEN tip/ tasks (a
# SEPARATE "loop reach" metric, see decomposed/README.md). `isaplanner-mono/`
# holds monomorphized renderings of the 8 higher-order isaplanner problems Aver
# cannot express natively (a SEPARATE asterisked metric, see
# isaplanner-mono/README.md). Both are excluded here so the baseline
# `coverage (union)` stays "what the engine proves UNAIDED on bare, natively
# expressible tip/" — folding either in would inflate the headline.
for f in $(find "$ROOT" -name '*.av' -not -path '*/decomposed/*' -not -path '*/isaplanner-mono/*' | sort); do
  [ -e "$f" ] || continue
  total=$((total + 1))
  l=$(proves "$f" lean)
  d=$(proves "$f" dafny)
  [ "$l" = proved ] && lean=$((lean + 1))
  [ "$d" = proved ] && dafny=$((dafny + 1))
  if [ "$l" = proved ] || [ "$d" = proved ]; then
    union=$((union + 1))
    tag="lean+dafny"
    [ "$l" = open ] && tag="dafny-only"
    [ "$d" = open ] && tag="lean-only"
    printf '  proved  [%-10s] %s\n' "$tag" "${f#"$ROOT"/}"
  fi
done

echo ""
echo "coverage (union): ${union} / ${total}   |   lean: ${lean}   dafny: ${dafny}"
