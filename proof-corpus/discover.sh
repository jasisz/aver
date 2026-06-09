#!/usr/bin/env bash
#
# Lemma-discovery REACH baseline — how many corpus TASKS `aver proof --discover`
# proves >= 1 auxiliary lemma for, and the total proved-lemma count. This is the
# CONJECTURE+PROVE reach of the discovery layer (a leading indicator), NOT the
# main-law coverage number.
#
# ⚠️ IMPORTANT — this is NOT the same metric as run.sh:
#   - run.sh measures whether the TASK'S UNIVERSAL LAW closes (currently 57/154).
#   - this measures whether discovery CONJECTURES + KERNEL-PROVES any lemma for
#     the task. Discovery does NOT yet feed those lemmas back into the main law's
#     proof (the `ProofStrategy::SimpOverLemmas` re-entry is still pending), so a
#     proved discovered lemma does NOT (yet) move the run.sh coverage number.
#     This baseline exists to quantify discovery's reach BEFORE the structural
#     homomorphism conjecturer (B1) lands — so its effect is measurable.
#
# Expensive: each `--discover` enumerates, VM-filters, and runs up to ~8 `lake
# build`s. No retry (a transient lake failure only UNDER-counts → lower bound).
#
# Usage:  ./discover.sh            (uses ../target/debug/aver — build it first)
#         AVER=/path/to/aver ./discover.sh
set -u

ROOT="$(cd "$(dirname "$0")" && pwd)"
AVER="${AVER:-$ROOT/../target/debug/aver}"

if [ ! -x "$AVER" ]; then
  echo "error: aver binary not found at $AVER (run: cargo build --bin aver)" >&2
  exit 1
fi

tasks_with=0
total_lemmas=0
total=0
for f in $(find "$ROOT" -name '*.av' | sort); do
  [ -e "$f" ] || continue
  total=$((total + 1))
  out="$(mktemp -d)"
  # Count the per-lemma "  ✓ <lemma>" lines discovery prints under
  # "PROVED (Lean, kernel-checked): N".
  n="$("$AVER" proof "$f" --discover -o "$out" 2>/dev/null | grep -c '✓')"
  rm -rf "$out"
  if [ "${n:-0}" -gt 0 ]; then
    tasks_with=$((tasks_with + 1))
    total_lemmas=$((total_lemmas + n))
    printf '  %3d lemma(s)  %s\n' "$n" "${f#"$ROOT"/}"
  fi
done

echo ""
echo "discovery reach: ${tasks_with} / ${total} tasks proved >=1 lemma   |   ${total_lemmas} lemmas total"
echo "(reach, NOT main-law coverage — see header; run.sh is the coverage metric)"
