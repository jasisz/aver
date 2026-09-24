#!/usr/bin/env bash
# Measure one certificate package: produce it, run `aver-cert check` and
# `verify` with timings, keep the verifier's staged build, export the
# CheckerWitness environment and replay it through every kernel under test,
# on the original export and on one with a single artifact byte flipped.
#
# usage: run-package.sh PACKAGE AVER AVER_CERT TOOLS OUT
#   PACKAGE is one of: certkit-clockrange, k5-main, k5-laws
set -uo pipefail

PACKAGE="$1"
AVER="$(realpath "$2")"
AVER_CERT="$(realpath "$3")"
TOOLS="$(realpath "$4")"
OUT="$(realpath -m "$5")"
HERE="$(cd "$(dirname "$0")" && pwd)"
REPO="$(cd "$HERE/../.." && pwd)"
LEAN_TOOLCHAIN="leanprover/lean4:v4.34.0"
KERNEL_TIMEOUT="${KERNEL_TIMEOUT:-3600}"

mkdir -p "$OUT/logs"
WORK="$(mktemp -d)"
RESULTS="$OUT/results.tsv"
printf 'step\texit\twall_s\tmaxrss_kb\n' > "$RESULTS"

# run_timed STEP CMD... — wall time and peak RSS of CMD, output to logs/STEP.log.
run_timed() {
  local step="$1"; shift
  local log="$OUT/logs/$step.log"
  local stats="$WORK/$step.time"
  echo "::group::$step"
  echo "+ $*" | tee "$log"
  /usr/bin/time -f '%e %M' -o "$stats" timeout "$KERNEL_TIMEOUT" "$@" >>"$log" 2>&1
  local status=$?
  local wall rss
  read -r wall rss < <(tail -n 1 "$stats" 2>/dev/null || echo "NA NA")
  tail -n 40 "$log"
  echo "::endgroup::"
  printf '%s\t%s\t%s\t%s\n' "$step" "$status" "$wall" "$rss" >> "$RESULTS"
  echo "$step: exit $status, ${wall}s, ${rss} KiB"
  return $status
}

# run_stdin STEP INPUT CMD... — same, with INPUT on stdin.
run_stdin() {
  local step="$1" input="$2"; shift 2
  local log="$OUT/logs/$step.log"
  local stats="$WORK/$step.time"
  echo "::group::$step"
  echo "+ $* < $input" | tee "$log"
  /usr/bin/time -f '%e %M' -o "$stats" timeout "$KERNEL_TIMEOUT" "$@" <"$input" >>"$log" 2>&1
  local status=$?
  local wall rss
  read -r wall rss < <(tail -n 1 "$stats" 2>/dev/null || echo "NA NA")
  tail -n 40 "$log"
  echo "::endgroup::"
  printf '%s\t%s\t%s\t%s\n' "$step" "$status" "$wall" "$rss" >> "$RESULTS"
  echo "$step: exit $status, ${wall}s, ${rss} KiB"
  return $status
}

TC_PREFIX="$(elan run --install "$LEAN_TOOLCHAIN" lean --print-prefix)"
TC_BIN="$TC_PREFIX/bin"
echo "Lean toolchain: $TC_PREFIX"

# 1. Produce the package.
PKG="$WORK/out"
case "$PACKAGE" in
  certkit-clockrange)
    run_timed compile "$AVER" compile "$REPO/tools/certkit/fixtures/clockrange.av" \
      --target wasm-gc --certify -o "$PKG"
    ;;
  k5-main)
    run_timed compile "$AVER" compile "$REPO/projects/k5_fdiv/main.av" \
      --module-root "$REPO/projects/k5_fdiv" --target wasm-gc --certify -o "$PKG"
    ;;
  k5-laws)
    # A leaf that depends on every K5 law module, with one trivial export.
    cp -r "$REPO/projects/k5_fdiv" "$WORK/k5"
    deps=$(for f in "$WORK"/k5/domain/*.av; do
      sed -n 's/^module \([A-Za-z0-9]*\).*/Domain.\1/p' "$f" | head -n 1
    done | paste -sd, - | sed 's/,/, /g')
    cat > "$WORK/k5/lawsentry.av" <<EOF
module LawsEntry
    intent =
        "Measurement leaf: depends on every K5 law module so one certificate"
        "package carries all of their laws."
    depends [$deps]
    exposes [one]
    effects []

fn one() -> Int
    ? "Trivial export, so the package certifies one function."
    1
EOF
    cat "$WORK/k5/lawsentry.av"
    cp "$WORK/k5/lawsentry.av" "$OUT/lawsentry.av"
    run_timed compile "$AVER" compile "$WORK/k5/lawsentry.av" \
      --module-root "$WORK/k5" --target wasm-gc --certify -o "$PKG"
    ;;
  *) echo "unknown package $PACKAGE"; exit 2 ;;
esac

WASM="$(ls "$PKG"/*.wasm 2>/dev/null | grep -v '\.optimized\.wasm$' | head -n 1)"
if [ -z "$WASM" ] || [ ! -d "$PKG/cert" ]; then
  echo "no package produced"; exit 1
fi
echo "artifact: $WASM ($(stat -c %s "$WASM") bytes)"
{
  echo "artifact_bytes=$(stat -c %s "$WASM")"
  echo "package_lean_files=$(find "$PKG/cert" -name '*.lean' | wc -l)"
  echo "package_lean_bytes=$(find "$PKG/cert" -name '*.lean' -exec cat {} + | wc -c)"
} > "$OUT/package.txt"
cp "$WASM" "$OUT/artifact.wasm"
tar -C "$PKG" -czf "$OUT/cert-package.tgz" cert

# 2. check and verify, with the verifier's own step timings. The verify run's
# final replay goes through a hook that copies the staged build away first,
# then runs exactly the stock `leanchecker --fresh CheckerWitness`.
export AVER_CERT_TIMINGS=1
export AVER_CERT_PHASE_TIMEOUT_SECS="${AVER_CERT_PHASE_TIMEOUT_SECS:-5400}"
run_timed check "$AVER_CERT" check "$WASM" "$PKG/cert"

KEEP="$WORK/kept-build"
HOOK="$WORK/keep-and-replay.sh"
cat > "$HOOK" <<EOF
#!/bin/bash
/usr/bin/rm -rf "$KEEP"
/usr/bin/mkdir -p "$KEEP"
/usr/bin/cp -a . "$KEEP/"
exec "$TC_BIN/leanchecker" --fresh CheckerWitness
EOF
chmod +x "$HOOK"
run_timed verify env AVER_CERT_PARALLEL_REPLAY="$HOOK" "$AVER_CERT" verify "$WASM" "$PKG/cert"

# The stock pipeline on the tampered artifact, with the manifest's hash updated
# to match, so the rejection has to come from Lean rather than the hash pin.
python3 "$HERE/tamper.py" wasm "$WASM" "$WORK/tampered.wasm" > "$OUT/tamper-wasm.json"
cp -r "$PKG/cert" "$WORK/tampered-cert"
old_hash=$(sha256sum "$WASM" | cut -d' ' -f1)
new_hash=$(sha256sum "$WORK/tampered.wasm" | cut -d' ' -f1)
grep -rl "$old_hash" "$WORK/tampered-cert" | xargs -r sed -i "s/$old_hash/$new_hash/g"
run_timed verify-tampered-artifact "$AVER_CERT" verify "$WORK/tampered.wasm" "$WORK/tampered-cert"

if [ ! -d "$KEEP" ]; then
  echo "verify never reached the replay step; no staged build to export"
  exit 0
fi
cd "$KEEP"
rm -rf .aver-cert-tmp-*
export PATH="$TC_BIN:$PATH"
find . -maxdepth 1 -name '*.lean' -printf '%f\t%s\n' | sort > "$OUT/kept-lean-files.tsv"

# 3. The stock kernel replay, once more, outside the verifier.
run_timed leanchecker-fresh lake env leanchecker --fresh CheckerWitness

# 4. Export and replay through each kernel.
EXPORT="$WORK/witness.ndjson"
echo "::group::export"
/usr/bin/time -f '%e %M' -o "$WORK/export.time" \
  lake env "$TOOLS/lean4export" CheckerWitness > "$EXPORT" 2> "$OUT/logs/export.log"
status=$?
read -r wall rss < "$WORK/export.time"
printf 'export\t%s\t%s\t%s\n' "$status" "$wall" "$rss" >> "$RESULTS"
echo "export: exit $status, ${wall}s, $(stat -c %s "$EXPORT") bytes, $(wc -l < "$EXPORT") lines"
{
  echo "export_bytes=$(stat -c %s "$EXPORT")"
  echo "export_lines=$(wc -l < "$EXPORT")"
  echo "export_decls=$(grep -c -E '"(axiom|def|thm|opaque|quot|inductive)"' "$EXPORT")"
  echo "export_natlits=$(grep -c '"natVal"' "$EXPORT")"
  echo "export_max_natlit_digits=$(grep -o '"natVal":"[0-9]*"' "$EXPORT" | awk '{ n = length($0) - 11; if (n > m) m = n } END { print m + 0 }')"
} >> "$OUT/package.txt"
head -c 600 "$EXPORT"; echo
echo "::endgroup::"

TAMPERED="$WORK/witness-tampered.ndjson"
python3 "$HERE/tamper.py" export "$WASM" "$EXPORT" "$TAMPERED" > "$OUT/tamper-export.json" 2>&1 \
  || rm -f "$TAMPERED"
cat "$OUT/tamper-export.json"

kernels() { # INPUT SUFFIX
  local input="$1" suffix="$2"
  run_timed "official-export$suffix" "$TOOLS/official-export-kernel" "$input"
  run_stdin "nanoda$suffix" "$input" "$TOOLS/nanoda" "$TOOLS/arena-config.json"
  run_stdin "nanoda-strict$suffix" "$input" "$TOOLS/nanoda" "$TOOLS/strict-config.json"
  run_stdin "sokonanoda$suffix" "$input" "$TOOLS/sokonanoda" "$TOOLS/arena-config.json"
  run_stdin "mathgraph$suffix" "$input" "$TOOLS/mathgraph" "$TOOLS/mathgraph-config.json"
}
kernels "$EXPORT" ""
if [ -s "$TAMPERED" ]; then
  kernels "$TAMPERED" "-tampered"
fi

# 5. Kernel versus elaboration on the heaviest modules of the verify build.
python3 "$HERE/profile.py" heavy "$OUT/logs/verify.log" 8 > "$OUT/heavy-modules.tsv"
cat "$OUT/heavy-modules.tsv"
mkdir -p "$OUT/profile"
while IFS=$'\t' read -r module took; do
  file="${module//.//}.lean"
  [ -f "$file" ] || { echo "no source for $module"; continue; }
  run_timed "profile-$module" lake env lean -Dprofiler=true -Dprofiler.threshold=100 \
    -DElab.async=false "$file"
  cp "$OUT/logs/profile-$module.log" "$OUT/profile/$module.log"
done < "$OUT/heavy-modules.tsv"
python3 "$HERE/profile.py" cumulative "$OUT"/profile/*.log > "$OUT/profile.jsonl" || true
cat "$OUT/profile.jsonl"

cd /
rm -rf "$WORK"
exit 0
