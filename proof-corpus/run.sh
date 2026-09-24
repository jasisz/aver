#!/usr/bin/env bash
#
# proof-corpus coverage runner — measures how many proof TASKS Aver CLOSES (the
# universal law verified by the Lean kernel), as a NUMBER, not a gate.
#
# NOT a regression suite: most tasks are expected to be OPEN by design; an open
# task lowers the number, never fails. The must-prove regression set lives in the
# compiler repo (examples/data, examples/formal + proof_spec.rs assert_eq!).
#
# Metric: `aver proof <f> --check --check-json`, keyed on "universal": true, NOT
# "passed". `passed` stays lenient (a bounded `native_decide` verify-on-domain
# is a legitimate but weaker check the regression corpus relies on);
# "universal" is the `#print axioms`-gated signal that the ∀-claim is genuinely
# kernel-proved (no `Lean.ofReduceBool`). Coverage must count only genuine
# closures. Retries once (lake transient failures produce false "open", never
# false "proved", so the reported number is a lower bound).
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

# proves <file> -> "proved" | "open" (retry once to absorb flakiness).
proves() {
  local f="$1" attempt out json
  for attempt in 1 2; do
    out="$(mktemp -d)"
    json="$("$AVER" proof "$f" --check --check-json -o "$out" 2>/dev/null | grep '"passed"' | tail -1)"
    rm -rf "$out"
    printf '%s' "$json" | grep -q '"universal":true' && { echo "proved"; return; }
  done
  echo "open"
}

proved=0
total=0
# `decomposed/` holds LLM helper-law-augmented copies of OPEN tip/ tasks (a
# SEPARATE "loop reach" metric, see decomposed/README.md). `isaplanner-mono/`
# holds monomorphized renderings of the 8 higher-order isaplanner problems Aver
# cannot express natively (a SEPARATE asterisked metric, see
# isaplanner-mono/README.md). Both are excluded here so the baseline
# `coverage` stays "what the engine proves UNAIDED on bare, natively
# expressible tip/" — folding either in would inflate the headline.
for f in $(find "$ROOT" -name '*.av' -not -path '*/decomposed/*' -not -path '*/isaplanner-mono/*' | sort); do
  [ -e "$f" ] || continue
  total=$((total + 1))
  if [ "$(proves "$f")" = proved ]; then
    proved=$((proved + 1))
    printf '  proved  %s\n' "${f#"$ROOT"/}"
  fi
done

echo ""
echo "coverage: ${proved} / ${total}"
