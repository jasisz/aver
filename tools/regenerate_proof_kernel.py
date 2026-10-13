#!/usr/bin/env python3
"""Regenerate or verify the Aver code the aver binary embeds for proofs.

Two entries under `tools/proof-kernel/` are compiled with
`aver compile --target rust`:

- `embed.av`, the proof kernel, installed as `src/proof_kernel/`;
- `embed_std.av`, the standard proof plans (`Kernel.Std`), installed as
  `src/proof_std/`. They are not trusted: the kernel checks every proof a
  plan writes. They live in their own tree so the kernel's stays exactly
  the kernel.

Before either, the kernel's table of builtin signatures and of the type
shapes whose `==` is equality (`tools/proof-kernel/kernel/builtins.av`) is
written from the compiler's own type checker by `aver kernel-builtins`, so
the kernel carries no second copy of them.

Each one's generated modules land in `aver_generated/` plus
`runtime_support.rs`, with every `crate::` path re-rooted under the tree's
own module. The hand-written `mod.rs` of each tree is its only interface:
`proof_kernel::verdict(text)` checks one step script, and
`proof_std::run(plan, goal)` runs one standard plan on a goal.

    python3 tools/regenerate_proof_kernel.py          # regenerate in place
    python3 tools/regenerate_proof_kernel.py --check  # fail if stale
"""

from __future__ import annotations

import argparse
import filecmp
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
KERNEL_DIR = REPO_ROOT / "tools" / "proof-kernel"
GENERATED_PARTS = ["aver_generated", "runtime_support.rs"]
# (entry under tools/proof-kernel, the crate module it is installed as)
TREES = [("embed.av", "proof_kernel"), ("embed_std.av", "proof_std")]
BUILTINS = KERNEL_DIR / "kernel" / "builtins.av"


def builtins_source(aver_bin: Path) -> str:
    """The kernel's builtin table, as the compiler writes it."""
    return subprocess.run(
        [str(aver_bin), "kernel-builtins"], cwd=REPO_ROOT, check=True, capture_output=True, text=True
    ).stdout


def run(command: list[str], cwd: Path = REPO_ROOT) -> None:
    subprocess.run(command, cwd=cwd, check=True)


def resolve_aver_bin(explicit: str | None) -> Path:
    if explicit is not None:
        path = Path(explicit)
        if not path.is_absolute():
            path = REPO_ROOT / path
        if not path.exists():
            raise SystemExit(f"aver binary does not exist: {path}")
        return path
    path = REPO_ROOT / "target" / "debug" / "aver"
    if not path.exists():
        print("Building current aver...")
        run(["cargo", "build", "--bin", "aver"])
    return path


def reroot(text: str, module: str) -> str:
    # The kernel's macros come from aver_rt; a glob import does not carry
    # them into a nested module, so each file names the one it uses.
    text = text.replace("use crate::*;", f"use crate::{module}::*;\nuse ::aver_rt::aver_list_match;")
    text = text.replace("crate::aver_generated", f"crate::{module}::aver_generated")
    text = text.replace("crate::cancel_checkpoint", f"crate::{module}::cancel_checkpoint")
    leftover = [line for line in text.splitlines() if "crate::" in line and f"crate::{module}" not in line]
    if leftover:
        raise SystemExit(f"generated {module} has a crate path this script does not re-root: {leftover[0]}")
    return text


def generate(aver_bin: Path, entry: str, module: str, destination: Path) -> None:
    """Write the generated parts of src/<module> into `destination`."""
    with tempfile.TemporaryDirectory(prefix="aver-proof-kernel-") as temp:
        project = Path(temp) / "out"
        run(
            [
                str(aver_bin),
                "compile",
                entry,
                "--target",
                "rust",
                "--module-root",
                ".",
                "-o",
                str(project),
            ],
            cwd=KERNEL_DIR,
        )
        source = project / "src"
        destination.mkdir(parents=True, exist_ok=True)
        for part in GENERATED_PARTS:
            src = source / part
            dst = destination / part
            if src.is_dir():
                shutil.copytree(src, dst)
            else:
                shutil.copy2(src, dst)
    files = [destination / p for p in rs_files(destination)]
    for path in files:
        path.write_text(reroot(path.read_text(), module))
    files = [str(p) for p in files]
    run(["rustfmt", "--edition", "2024", *files])


def rs_files(root: Path) -> set[Path]:
    found: set[Path] = set()
    for part in GENERATED_PARTS:
        base = root / part
        if base.is_dir():
            found |= {p.relative_to(root) for p in base.rglob("*.rs")}
        elif base.exists():
            found.add(base.relative_to(root))
    return found


def check(aver_bin: Path) -> None:
    differences = []
    if builtins_source(aver_bin) != BUILTINS.read_text():
        differences.append(f"content differs: {BUILTINS.relative_to(REPO_ROOT)}")
    for entry, module in TREES:
        installed = REPO_ROOT / "src" / module
        with tempfile.TemporaryDirectory(prefix="aver-proof-kernel-check-") as temp:
            fresh = Path(temp) / module
            generate(aver_bin, entry, module, fresh)
            mine, theirs = rs_files(fresh), rs_files(installed)
            differences += [f"only generated: src/{module}/{p}" for p in sorted(mine - theirs)]
            differences += [f"only checked in: src/{module}/{p}" for p in sorted(theirs - mine)]
            differences += [
                f"content differs: src/{module}/{p}"
                for p in sorted(mine & theirs)
                if not filecmp.cmp(fresh / p, installed / p, shallow=False)
            ]
    if differences:
        preview = "\n".join(f"  - {d}" for d in differences[:10])
        raise SystemExit(
            "checked-in proof kernel or standard plans are stale:\n"
            f"{preview}\n"
            "Regenerate them with:\n"
            "  python3 tools/regenerate_proof_kernel.py"
        )
    print("Checked-in proof kernel and standard plans are fresh.")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="compare without modifying the worktree")
    parser.add_argument("--aver-bin", help="compiler binary (defaults to target/debug/aver)")
    args = parser.parse_args()
    aver_bin = resolve_aver_bin(args.aver_bin)
    if args.check:
        check(aver_bin)
        return
    BUILTINS.write_text(builtins_source(aver_bin))
    print(f"Wrote {BUILTINS.relative_to(REPO_ROOT)}")
    for entry, module in TREES:
        installed = REPO_ROOT / "src" / module
        for part in GENERATED_PARTS:
            target = installed / part
            if target.is_dir():
                shutil.rmtree(target)
            elif target.exists():
                target.unlink()
        generate(aver_bin, entry, module, installed)
        print(f"Installed the generated {module} in {installed.relative_to(REPO_ROOT)}")


if __name__ == "__main__":
    sys.exit(main())
