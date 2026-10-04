#!/usr/bin/env python3
"""Regenerate or verify the proof kernel the aver binary embeds.

`tools/proof-kernel/embed.av` (the kernel written in Aver) is compiled with
`aver compile --target rust`, and its generated modules are installed as
`src/proof_kernel/aver_generated/` plus `runtime_support.rs`, with every
`crate::` path re-rooted under `crate::proof_kernel::`. The hand-written
`src/proof_kernel/mod.rs` is the only interface: `verdict(text)` checks one
step script in process, which `aver proof --backend aver` calls.

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
INSTALLED = REPO_ROOT / "src" / "proof_kernel"
GENERATED_PARTS = ["aver_generated", "runtime_support.rs"]


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


def reroot(text: str) -> str:
    # The kernel's macros come from aver_rt; a glob import does not carry
    # them into a nested module, so each file names the one it uses.
    text = text.replace("use crate::*;", "use crate::proof_kernel::*;\nuse ::aver_rt::aver_list_match;")
    text = text.replace("crate::aver_generated", "crate::proof_kernel::aver_generated")
    text = text.replace("crate::cancel_checkpoint", "crate::proof_kernel::cancel_checkpoint")
    leftover = [line for line in text.splitlines() if "crate::" in line and "crate::proof_kernel" not in line]
    if leftover:
        raise SystemExit(f"generated kernel has a crate path this script does not re-root: {leftover[0]}")
    return text


def generate(aver_bin: Path, destination: Path) -> None:
    """Write the generated parts of src/proof_kernel into `destination`."""
    with tempfile.TemporaryDirectory(prefix="aver-proof-kernel-") as temp:
        project = Path(temp) / "out"
        run(
            [
                str(aver_bin),
                "compile",
                "embed.av",
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
        path.write_text(reroot(path.read_text()))
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
    with tempfile.TemporaryDirectory(prefix="aver-proof-kernel-check-") as temp:
        fresh = Path(temp) / "kernel"
        generate(aver_bin, fresh)
        mine, theirs = rs_files(fresh), rs_files(INSTALLED)
        differences = [f"only generated: {p}" for p in sorted(mine - theirs)]
        differences += [f"only checked in: {p}" for p in sorted(theirs - mine)]
        differences += [
            f"content differs: {p}"
            for p in sorted(mine & theirs)
            if not filecmp.cmp(fresh / p, INSTALLED / p, shallow=False)
        ]
    if differences:
        preview = "\n".join(f"  - {d}" for d in differences[:10])
        raise SystemExit(
            "checked-in proof kernel is stale:\n"
            f"{preview}\n"
            "Regenerate it with:\n"
            "  python3 tools/regenerate_proof_kernel.py"
        )
    print("Checked-in proof kernel is fresh.")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true", help="compare without modifying the worktree")
    parser.add_argument("--aver-bin", help="compiler binary (defaults to target/debug/aver)")
    args = parser.parse_args()
    aver_bin = resolve_aver_bin(args.aver_bin)
    if args.check:
        check(aver_bin)
        return
    for part in GENERATED_PARTS:
        target = INSTALLED / part
        if target.is_dir():
            shutil.rmtree(target)
        elif target.exists():
            target.unlink()
    generate(aver_bin, INSTALLED)
    print(f"Installed the generated proof kernel in {INSTALLED.relative_to(REPO_ROOT)}")


if __name__ == "__main__":
    sys.exit(main())
