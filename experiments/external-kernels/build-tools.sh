#!/usr/bin/env bash
# Build the exporter and the kernels under test into $1 (default: ./tools).
#
# Every source is pinned to a commit. The kernel revisions and build recipes
# follow the Lean Kernel Arena's checker definitions (checkers/*.yaml at
# ARENA_REV), with two deviations noted in README.md: no `-C target-cpu=native`
# and no profile-guided build, because the binaries run on another runner.
set -euo pipefail

TOOLS="$(realpath -m "${1:-tools}")"
LEAN_TOOLCHAIN="leanprover/lean4:v4.34.0"
LEAN4EXPORT_REV="076e8e57707e813375e8f9da8bf989799ace9680"   # tag v4.34.0
ARENA_REV="fa66d8bd1adcd1ced5d820fb93c37327aa3ccc57"
NANODA_REV="4c544ed4099c8227f07d5de77ad1e69fb0740a27"
SOKONANODA_REV="28c03d0103e004610e4d47a4828965efb2b70af9"
MATHGRAPH_REV="78c7502bac8a5ba000057b3f083bc0595ac65750"

mkdir -p "$TOOLS"
SRC="$TOOLS/src"
mkdir -p "$SRC"

fetch() { # url rev dir
  local url="$1" rev="$2" dir="$3"
  rm -rf "$dir"
  git init -q "$dir"
  git -C "$dir" remote add origin "$url"
  git -C "$dir" fetch -q --depth 1 origin "$rev"
  git -C "$dir" checkout -q FETCH_HEAD
}

# lean4export, at the tag for this Lean release.
fetch https://github.com/leanprover/lean4export "$LEAN4EXPORT_REV" "$SRC/lean4export"
echo "$LEAN_TOOLCHAIN" > "$SRC/lean4export/lean-toolchain"
(cd "$SRC/lean4export" && lake build lean4export)
cp "$SRC/lean4export/.lake/build/bin/lean4export" "$TOOLS/lean4export"

# The arena's "official" checker: parse the export with lean4export's parser
# and replay it into an empty environment through the stock Lean kernel. It is
# the reference verdict for an export that has no .olean (the tampered one).
fetch https://github.com/leanprover/lean-kernel-arena "$ARENA_REV" "$SRC/arena"
OFFICIAL="$SRC/arena/checkers/official"
echo "$LEAN_TOOLCHAIN" > "$OFFICIAL/lean-toolchain"
# Pin the parser to the same lean4export revision as the exporter.
python3 - "$OFFICIAL" "$LEAN4EXPORT_REV" <<'EOF'
import json, sys, pathlib
root, rev = pathlib.Path(sys.argv[1]), sys.argv[2]
toml = root / "lakefile.toml"
toml.write_text(toml.read_text().replace('rev = "master"', f'rev = "{rev}"'))
manifest = root / "lake-manifest.json"
data = json.loads(manifest.read_text())
for package in data["packages"]:
    if package["name"] == "lean4export":
        package["rev"] = rev
        package["inputRev"] = rev
manifest.write_text(json.dumps(data, indent=1))
EOF
(cd "$OFFICIAL" && lake build kernel)
cp "$OFFICIAL/.lake/build/bin/kernel" "$TOOLS/official-export-kernel"

# nanoda, with the arena's stack-size patch.
fetch https://github.com/ammkrn/nanoda_lib "$NANODA_REV" "$SRC/nanoda"
grep -q 'STACK_SIZE: usize = 16_777_216;' "$SRC/nanoda/src/lib.rs"
sed -i 's/STACK_SIZE: usize = 16_777_216;/STACK_SIZE: usize = 268_435_456;/' "$SRC/nanoda/src/lib.rs"
(cd "$SRC/nanoda" && cargo build --release --quiet)
cp "$SRC/nanoda/target/release/nanoda_bin" "$TOOLS/nanoda"

# sokonanoda and the MathGraph checker (a sokonanoda descendant).
fetch https://github.com/intgrah/sokonanoda "$SOKONANODA_REV" "$SRC/sokonanoda"
(cd "$SRC/sokonanoda" && cargo build --release --quiet)
cp "$SRC/sokonanoda/target/release/sokonanoda" "$TOOLS/sokonanoda"

fetch https://github.com/metalogiclabs/mathgraph-lean-kernel "$MATHGRAPH_REV" "$SRC/mathgraph"
(cd "$SRC/mathgraph" && cargo build --release --quiet)
cp "$SRC/mathgraph/target/release/sokonanoda" "$TOOLS/mathgraph"

# The arena's configuration (all axioms admitted, Nat and String extensions on),
# plus a strict variant that admits only Lean's three standard axioms.
cat > "$TOOLS/arena-config.json" <<'EOF'
{
  "use_stdin": true,
  "nat_extension": true,
  "string_extension": true,
  "unpermitted_axiom_hard_error": false,
  "unsafe_permit_all_axioms": true,
  "num_threads": 4
}
EOF
cat > "$TOOLS/mathgraph-config.json" <<'EOF'
{
  "use_stdin": true,
  "nat_extension": true,
  "string_extension": true,
  "unpermitted_axiom_hard_error": false,
  "unsafe_permit_all_axioms": true,
  "num_threads": 2
}
EOF
cat > "$TOOLS/strict-config.json" <<'EOF'
{
  "use_stdin": true,
  "nat_extension": true,
  "string_extension": true,
  "permitted_axioms": ["propext", "Classical.choice", "Quot.sound"],
  "unpermitted_axiom_hard_error": false,
  "num_threads": 4
}
EOF

cat > "$TOOLS/REVISIONS" <<EOF
lean-toolchain $LEAN_TOOLCHAIN
lean4export $LEAN4EXPORT_REV
lean-kernel-arena $ARENA_REV
nanoda_lib $NANODA_REV
sokonanoda $SOKONANODA_REV
mathgraph-lean-kernel $MATHGRAPH_REV
EOF
rm -rf "$SRC"
ls -la "$TOOLS"
