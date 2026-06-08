#!/usr/bin/env bash
#
# proof-corpus coverage runner — measures how many proof TASKS Aver's engine
# CLOSES (the law's universal theorem, kernel-checked by Lean), as a NUMBER,
# not a gate.
#
# This is NOT a regression suite: most tasks are expected to be OPEN (out of the
# engine's current reach) by design. An open task lowers the coverage %, it is
# never a failure. The must-prove regression set lives in the compiler repo
# (examples/data, examples/formal + tests/proof_spec.rs assert_eq!), separate.
#
# Metric: a task is COVERED when `aver proof <f> --check --check-json` reports
# "passed": true — i.e. the Lean export of the universal law kernel-checks with
# no sorry/axiom over budget, via any of Aver's auto-mode proof strategies
# (structural induction, accumulator-fold spec-equivalence, etc.). Lake-gated.
#
# NOTE: this counts the AUTO-mode strategies. The `--discover` lemma-discovery
# class (codec roundtrips, list-homomorphism enumeration) is NOT counted here;
# run those separately if needed. A file that fails to compile counts as open.
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

covered=0
total=0
for f in "$ROOT"/*/*.av; do
  [ -e "$f" ] || continue
  total=$((total + 1))
  out="$(mktemp -d)"
  json="$("$AVER" proof "$f" --check --check-json -o "$out" 2>/dev/null | grep '"passed"' | tail -1)"
  rm -rf "$out"
  if printf '%s' "$json" | grep -q '"passed":true'; then
    covered=$((covered + 1))
    printf '  proved   %s\n' "${f#"$ROOT"/}"
  else
    printf '  open     %s\n' "${f#"$ROOT"/}"
  fi
done

echo ""
echo "coverage: ${covered} / ${total}"
