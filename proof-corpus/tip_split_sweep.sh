#!/usr/bin/env bash
#
# Per-task Lean-universal sweep over the TIP corpus, SPLIT by source suite
# (isaplanner-78 vs prod-74). Mirrors run.sh's metric exactly: a task is
# CLOSED iff `aver proof <f> --backend lean --check --check-json` reports
# "universal":true (the #print-axioms-gated kernel-genuine signal).
#
# Lean-only on purpose: Lean elaboration has no wall-clock timeout, so a
# heavy machine only makes this SLOW, never wrong (unlike Dafny/Z3). The
# retry-once absorbs transient lake-lock failures. So the per-task verdicts
# are trustworthy even under load.
#
# Output is PERSISTED (home dir, not /tmp) so "which tasks pass?" is forever
# a grep, not a re-measurement.
set -u

ROOT="$(cd "$(dirname "$0")" && pwd)"
AVER="${AVER:-$ROOT/../target/debug/aver}"
OUT="${OUT:-$HOME/aver-tip-split-sweep-2026-06-15.txt}"

: > "$OUT"
echo "# TIP per-task Lean-universal sweep (split isaplanner/prod)" >> "$OUT"
echo "# aver: $AVER  HEAD: $(cd "$ROOT/.." && git rev-parse --short HEAD)" >> "$OUT"
echo "" >> "$OUT"

prove_lean() {
  local f="$1" out json
  for attempt in 1 2; do
    out="$(mktemp -d)"
    json="$("$AVER" proof "$f" --backend lean --check --check-json -o "$out" 2>/dev/null | grep '"passed"' | tail -1)"
    rm -rf "$out"
    printf '%s' "$json" | grep -q '"universal":true' && { echo universal; return; }
  done
  echo open
}

sweep_dir() {
  local dir="$1" label="$2" n=0 closed=0
  echo "== $label ==" >> "$OUT"
  for f in $(find "$ROOT/$dir" -name '*.av' | sort); do
    n=$((n + 1))
    r="$(prove_lean "$f")"
    [ "$r" = universal ] && closed=$((closed + 1))
    printf '%-10s %s\n' "$r" "${f#"$ROOT/$dir/"}" >> "$OUT"
  done
  echo "-- $label: $closed / $n universal --" >> "$OUT"
  echo "" >> "$OUT"
  echo "$label: $closed / $n"
}

a="$(sweep_dir tip/isaplanner isaplanner)"
b="$(sweep_dir tip/prod prod)"
{
  echo "=== SUMMARY ==="
  echo "$a"
  echo "$b"
} | tee -a "$OUT"
echo "saved -> $OUT"
