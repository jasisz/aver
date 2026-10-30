#!/usr/bin/env python3
"""Every file a published crate `include_str!`s or `include_bytes!`s is in its package.

The repository builds such a file from the checkout, so a path missing from
`Cargo.toml`'s `include` list only shows when the crates.io package is built:
0.31.0 stopped half published on five proof kernel modules. This lists each
package (`cargo package --list`, no build) and checks every literal path the
crate's sources include.
"""

from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
CRATES = {
    "aver-lang": ROOT,
    "aver-rt": ROOT / "aver-rt",
    "aver-memory": ROOT / "aver-memory",
    "aver-cert": ROOT / "aver-cert",
    "aver-lsp": ROOT / "aver-lsp",
}
INCLUDE = re.compile(r'include_(?:str|bytes)!\(\s*"([^"]+)"\s*\)')
TEST_ITEM = re.compile(r"#\[cfg\(test\)\]\s*(?:#\[[^\]]*\]\s*)*(?:pub(?:\([^)]*\))?\s+)?mod\s+\w+\s*\{")


def without_tests(text: str) -> str:
    """The source with its `#[cfg(test)] mod … { … }` blocks cut out: test
    code is not built from the package."""
    out, at = [], 0
    for m in TEST_ITEM.finditer(text):
        if m.start() < at:
            continue
        depth, i = 1, m.end()
        while i < len(text) and depth:
            depth += {"{": 1, "}": -1}.get(text[i], 0)
            i += 1
        out.append(text[at : m.start()])
        at = i
    out.append(text[at:])
    return "".join(out)


def packaged(crate: str) -> set[str]:
    out = subprocess.run(
        ["cargo", "package", "-p", crate, "--list", "--allow-dirty"],
        cwd=ROOT,
        check=True,
        capture_output=True,
        text=True,
    ).stdout
    return {line.strip() for line in out.splitlines() if line.strip()}


def included(root: Path, files: set[str]) -> list[tuple[str, str]]:
    """(source file, included path relative to the crate root) for every
    literal include in a packaged Rust source."""
    found = []
    for rel in sorted(files):
        parts = rel.split("/")
        if not rel.endswith(".rs") or parts[-1] == "tests.rs" or "tests" in parts[:-1]:
            continue
        source = root / rel
        for path in INCLUDE.findall(without_tests(source.read_text())):
            target = (source.parent / path).resolve()
            try:
                found.append((rel, target.relative_to(root.resolve()).as_posix()))
            except ValueError:
                found.append((rel, str(target)))
    return found


def main() -> int:
    missing = []
    for crate, root in CRATES.items():
        files = packaged(crate)
        for source, target in included(root, files):
            if target not in files:
                missing.append(f"{crate}: {source} includes {target}, not in the package")
    for line in missing:
        print(line)
    if missing:
        print("add the files to `include` in that crate's Cargo.toml")
        return 1
    print(f"every included file is packaged ({', '.join(CRATES)})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
