#!/usr/bin/env bash
# Release the next version of Aver.
#
# Usage:
#   scripts/release.sh <X.Y.Z> [--dry-run]
#
# Steps:
#   1. sanity (clean tree, on `main`, in sync with origin, CHANGELOG section exists, tag free)
#   2. version bumps in Cargo.toml, aver-lsp/Cargo.toml, tools/website/index.html
#   3. `cargo build` to regenerate Cargo.lock
#   4. commit "Release X.Y.Z", tag vX.Y.Z
#   5. push commit + tag
#   6. `gh release create` with the X.Y.Z section of CHANGELOG.md as the body
#
# Pass --dry-run to print the planned diff and skip every mutation.

set -euo pipefail

if [[ $# -lt 1 ]]; then
    echo "usage: $0 <X.Y.Z> [--dry-run]" >&2
    exit 2
fi

VERSION="$1"
DRY_RUN=false
if [[ "${2:-}" == "--dry-run" ]]; then DRY_RUN=true; fi

if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "error: version must be X.Y.Z (got: $VERSION)" >&2
    exit 2
fi

MAJOR_MINOR="${VERSION%.*}"
TAG="v$VERSION"
ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

run() {
    if $DRY_RUN; then
        echo "[dry-run] $*"
    else
        eval "$@"
    fi
}

# 1. sanity checks ---------------------------------------------------------

if [[ -n "$(git status --porcelain)" ]]; then
    echo "error: working tree not clean" >&2
    exit 1
fi

BRANCH="$(git rev-parse --abbrev-ref HEAD)"
if [[ "$BRANCH" != "main" ]]; then
    echo "error: not on main (on $BRANCH)" >&2
    exit 1
fi

git fetch --quiet origin main
if [[ "$(git rev-parse HEAD)" != "$(git rev-parse origin/main)" ]]; then
    echo "error: local main out of sync with origin/main" >&2
    exit 1
fi

if git rev-parse "$TAG" >/dev/null 2>&1; then
    echo "error: tag $TAG already exists" >&2
    exit 1
fi

if ! grep -q "^## $VERSION " CHANGELOG.md; then
    echo "error: no \"## $VERSION ...\" section in CHANGELOG.md" >&2
    exit 1
fi

CURRENT="$(grep -m1 '^version = ' Cargo.toml | sed -E 's/version = "([^"]+)"/\1/')"
echo "Releasing $CURRENT -> $VERSION (tag $TAG)"

# 2. version bumps ---------------------------------------------------------

run "sed -i '' 's/^version = \"$CURRENT\"/version = \"$VERSION\"/' Cargo.toml"
run "sed -i '' 's/version = \"=$CURRENT\"/version = \"=$VERSION\"/' aver-lsp/Cargo.toml"

# website hero strip is "vMAJOR.MINOR" (e.g. v0.13)
PREV_MM="${CURRENT%.*}"
run "sed -i '' 's|&middot; v$PREV_MM &middot;|\\&middot; v$MAJOR_MINOR \\&middot;|' tools/website/index.html"

# 3. regenerate Cargo.lock -------------------------------------------------

run "cargo build --quiet"

# 4. commit + tag ----------------------------------------------------------

if $DRY_RUN; then
    echo "--- planned diff:"
    git --no-pager diff
    echo "--- planned commit message: Release $VERSION"
    echo "--- planned tag: $TAG"
    exit 0
fi

git add Cargo.toml Cargo.lock aver-lsp/Cargo.toml tools/website/index.html
git commit -m "Release $VERSION"
git tag "$TAG"

# 5. push ------------------------------------------------------------------

git push origin main
git push origin "$TAG"

# 6. github release --------------------------------------------------------

# extract the section between "## X.Y.Z" and the next "## " header
NOTES="$(awk -v ver="## $VERSION " '
    $0 ~ "^"ver { in_section = 1; next }
    in_section && /^## / { exit }
    in_section { print }
' CHANGELOG.md)"

# title pattern matches what gh release list shows for prior releases
TITLE="Aver $VERSION"
CODENAME="$(grep -m1 "^## $VERSION " CHANGELOG.md | sed -E 's/^## [0-9.]+ "([^"]+)".*/\1/')"
if [[ -n "$CODENAME" && "$CODENAME" != "## $VERSION "* ]]; then
    TITLE="Aver $VERSION \"$CODENAME\""
fi

gh release create "$TAG" --title "$TITLE" --notes "$NOTES"

echo "released $TAG: $TITLE"
