#!/usr/bin/env bash
# WITH-DECOMPOSE union sweep: a TIP task is CLOSED iff its base file proves
# universal no-discovery OR a decomposed/<suite>/<name>.av entry proves
# universal self-contained (same #print-axioms kernel-genuine bar, NO --discover).
set -u
ROOT="$(cd "$(dirname "$0")" && pwd)"
AVER="${AVER:-$ROOT/../target/debug/aver}"
OUT="${OUT:-$HOME/aver-tip-with-decompose-after-structrung.txt}"
: > "$OUT"
echo "# WITH-DECOMPOSE union sweep  aver: $AVER  HEAD: $(cd "$ROOT/.." && git rev-parse --short HEAD)" >> "$OUT"
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
  local suite="$1" label="$2" n=0 base=0 viadec=0 closed=0
  echo "== $label ==" >> "$OUT"
  for f in $(find "$ROOT/tip/$suite" -name '*.av' | sort); do
    n=$((n + 1)); name="$(basename "$f")"; dec="$ROOT/decomposed/$suite/$name"
    r="$(prove_lean "$f")"
    if [ "$r" = universal ]; then base=$((base+1)); closed=$((closed+1)); printf '%-16s %s\n' base-universal "$name" >> "$OUT"
    elif [ -f "$dec" ]; then
      d="$(prove_lean "$dec")"
      if [ "$d" = universal ]; then viadec=$((viadec+1)); closed=$((closed+1)); printf '%-16s %s\n' via-decompose "$name" >> "$OUT"
      else printf '%-16s %s (decomposed present but OPEN)\n' open "$name" >> "$OUT"; fi
    else printf '%-16s %s\n' open "$name" >> "$OUT"; fi
  done
  echo "-- $label: $closed / $n  (base $base + via-decompose $viadec) --" >> "$OUT"
  echo "$label: $closed / $n  (base $base + decompose $viadec)"
}
a="$(sweep_dir isaplanner isaplanner)"; b="$(sweep_dir prod prod)"
{ echo "=== SUMMARY (with decompose) ==="; echo "$a"; echo "$b"; } | tee -a "$OUT"
echo "saved -> $OUT"
