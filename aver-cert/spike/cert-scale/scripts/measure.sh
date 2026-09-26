#!/bin/bash
# Elaborate the given modules one at a time in the kept build dir, each under
# /usr/bin/time -l and lean --profile; append a one-line summary per module.
S=/private/tmp/claude-502/-Users-szymon-tezewski-PycharmProjects-lumen-rs/209eaacb-8ad3-40eb-85c7-5d60cb39ef76/scratchpad/cert-scale
B=${BUILD:-$S/build}
OUT=${OUT:-$S/measure.tsv}
mkdir -p "$S/prof"
cd "$B" || exit 1
for m in "$@"; do
  log="$S/prof/$m.log"
  /usr/bin/time -l lake env lean --profile -Dprofiler.threshold=1 -DmaxHeartbeats=0 "$m.lean" > "$log" 2>&1
  rc=$?
  real=$(awk '/ real /{print $1}' "$log")
  rss=$(awk '/maximum resident set size/{printf "%.0f", $1/1048576}' "$log")
  foot=$(awk '/peak memory footprint/{printf "%.0f", $1/1048576}' "$log")
  tc=$(/usr/bin/grep -E '^\s+type checking ' "$log" | awk '{print $3 $4}')
  el=$(/usr/bin/grep -E '^\s+elaboration ' "$log" | awk '{print $2 $3}')
  simp=$(/usr/bin/grep -E '^\s+simp ' "$log" | awk '{print $2 $3}')
  tac=$(/usr/bin/grep -E '^\s+tactic execution ' "$log" | awk '{print $3 $4}')
  printf '%s\trc=%s\treal=%ss\tmaxrss=%sMB\tfootprint=%sMB\ttypecheck=%s\telab=%s\tsimp=%s\ttactic=%s\n' \
    "$m" "$rc" "$real" "$rss" "$foot" "$tc" "$el" "$simp" "$tac" | tee -a "$OUT"
done
