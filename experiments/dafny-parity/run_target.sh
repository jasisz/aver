#!/usr/bin/env bash
# Run one proof entry through both backends and keep every raw output.
#
#   run_target.sh <label> <module-root> <entry.av> <results-dir>
#
# Lean:  aver proof --check-json (the ratchet's per-law manifest), then a
#        replayed `lake build` log for per-theorem fallback mapping.
# Dafny: aver proof --backend dafny (export only), then `dafny verify` run
#        directly with the flags aver --check uses (--verify-included-files,
#        default 30 s limit) plus a per-implementation text log, so every
#        lemma gets its own outcome instead of one whole-file verdict.
# Nothing here fails the job: a crash is recorded and the analyzer reports it.
set -uo pipefail

label="$1"; root="$2"; entry="$3"; results="$4"
AVER="${AVER:-aver}"
LEAN_TIMEOUT="${LEAN_TIMEOUT:-150m}"
DAFNY_TIMEOUT="${DAFNY_TIMEOUT:-150m}"
DAFNY_LIMIT="${DAFNY_LIMIT:-30}"
DAFNY_CORES="${DAFNY_CORES:-4}"
BACKENDS="${BACKENDS:-lean dafny}"

mkdir -p "$results"
work="$RUNNER_TEMP/proof-$label"
rm -rf "$work"; mkdir -p "$work"
echo "$label $root $entry" > "$results/target.txt"

# Source inventory for the analyzer: every .av in the module root.
tar -C "$root" -czf "$results/sources.tgz" --exclude='.git' --exclude='target' $(cd "$root" && find . -name '*.av' -not -path './target/*' | sed 's|^\./||')

for backend in $BACKENDS; do
  if [ "$backend" = lean ]; then
    out="$work/lean"
    start=$(date +%s)
    "$AVER" proof "$entry" --module-root "$root" -o "$out" > "$results/lean.export.log" 2>&1
    echo "export_exit=$?" > "$results/lean.status"
    if [ -f "$out/lean-toolchain" ]; then
      tc="$(cat "$out/lean-toolchain")"
      for attempt in 1 2 3 4; do elan toolchain install "$tc" && break; sleep 15; done
    fi
    timeout "$LEAN_TIMEOUT" "$AVER" proof "$entry" --module-root "$root" -o "$out" \
      --check-json --sorry-budget 100000 --declined-budget 100000 \
      > "$results/lean.json" 2> "$results/lean.stderr"
    echo "check_exit=$?" >> "$results/lean.status"
    echo "check_seconds=$(( $(date +%s) - start ))" >> "$results/lean.status"
    cp "$out/proof_manifest.json" "$results/lean.manifest.json" 2>/dev/null || true
    # Replayed build log: per-theorem sorry / error lines for the fallback map.
    (cd "$out" && timeout 60m lake build > "$results/lake.log" 2>&1; echo "lake_exit=$?" >> "$results/lean.status")
    tar -C "$out" -czf "$results/lean.export.tgz" --exclude='.lake' .
    # Isolation pass: a hard error in one law fails the whole build, so no
    # law gets audited. Strip the laws that own the errors and check again,
    # up to three rounds; the stripped laws keep their first-pass verdict.
    if ! grep -q "lake_exit=0" "$results/lean.status"; then
      copy="$work/root-isolated"; rm -rf "$copy"; cp -r "$root" "$copy"
      rel="$(realpath --relative-to="$root" "$entry")"
      log="$results/lake.log"; out2="$out"
      : > "$results/lean.stripped"
      for round in 1 2 3; do
        python3 "$(dirname "$0")/strip_laws.py" "$copy" "$log" "$out2" | tee -a "$results/lean.stripped"
        out2="$work/lean-isolated-$round"
        timeout "$LEAN_TIMEOUT" "$AVER" proof "$copy/$rel" --module-root "$copy" -o "$out2" \
          --check-json --sorry-budget 100000 --declined-budget 100000 \
          > "$results/lean2.json" 2> "$results/lean2.stderr"
        cp "$out2/proof_manifest.json" "$results/lean2.manifest.json" 2>/dev/null || true
        (cd "$out2" && timeout 60m lake build > "$results/lake2.log" 2>&1; echo "lake_exit=$?" > "$results/lean2.status")
        echo "round=$round" >> "$results/lean2.status"
        log="$results/lake2.log"
        grep -q "lake_exit=0" "$results/lean2.status" && break
      done
      tar -C "$out2" -czf "$results/lean2.export.tgz" --exclude='.lake' .
    fi
  else
    out="$work/dafny"
    echo "== dafny export $(date -u +%T)"
    "$AVER" proof "$entry" --module-root "$root" --backend dafny -o "$out" > "$results/dafny.export.log" 2>&1
    echo "export_exit=$?" > "$results/dafny.status"
    mod="$(grep -m1 '^module ' "$entry" | awk '{print $2}')"
    dfy="$mod.dfy"
    [ -f "$out/$dfy" ] || dfy="$(cd "$out" && ls *.dfy 2>/dev/null | grep -v '^common.dfy$' | head -1)"
    echo "entry_dfy=$dfy" >> "$results/dafny.status"
    echo "== dafny verify $(date -u +%T) ($(find "$out" -name '*.dfy' | wc -l) files, $(cat $(find "$out" -name '*.dfy') | wc -l) lines)"
    start=$(date +%s)
    (cd "$out" && timeout "$DAFNY_TIMEOUT" dafny verify --verify-included-files \
        --verification-time-limit "$DAFNY_LIMIT" --cores "$DAFNY_CORES" \
        --log-format "text;LogFileName=$results/dafny.verification.txt" \
        --log-format "csv;LogFileName=$results/dafny.verification.csv" \
        "$dfy" 2>&1 | tee "$results/dafny.log" | grep --line-buffered -E "Error:|finished with" | cut -c1-200
        exit "${PIPESTATUS[0]}")
    echo "verify_exit=$?" >> "$results/dafny.status"
    echo "verify_seconds=$(( $(date +%s) - start ))" >> "$results/dafny.status"
    tar -C "$out" -czf "$results/dafny.export.tgz" .
  fi
done
ls -la "$results"
