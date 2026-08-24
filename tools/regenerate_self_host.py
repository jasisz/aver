#!/usr/bin/env python3
"""Regenerate or verify the checked-in Rust self-host compiler."""

from __future__ import annotations

import argparse
import filecmp
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]
CHECKED_IN_SRC = REPO_ROOT / "src" / "self_host"
DEFAULT_OUTPUT = REPO_ROOT / "self_hosted" / "out"


def run(command: list[str], *, check: bool = True) -> subprocess.CompletedProcess:
    return subprocess.run(command, cwd=REPO_ROOT, check=check)


def resolve_aver_bin(explicit: str | None) -> Path:
    if explicit is not None:
        path = Path(explicit)
        if not path.is_absolute():
            path = REPO_ROOT / path
        if not path.exists():
            raise SystemExit(f"aver binary does not exist: {path}")
        return path

    path = REPO_ROOT / "target" / "debug" / "aver"
    print("Building current aver...")
    run(["cargo", "build", "--bin", "aver"])
    return path


def compile_self_host(aver_bin: Path, output: Path) -> None:
    if output.exists():
        shutil.rmtree(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    run(
        [
            str(aver_bin),
            "compile",
            "self_hosted/main.av",
            "--target",
            "rust",
            "--output",
            str(output),
            "--module-root",
            "self_hosted",
            "--with-self-host-support",
            "--guest-entry",
            "runGuestCliProgram",
            "--with-replay",
            "--policy",
            "runtime",
        ]
    )


def prepare_generated_source(output: Path) -> Path:
    source = output / "src"
    if not source.is_dir():
        raise SystemExit(f"self-host generation produced no source tree: {source}")

    verify_rs = source / "verify.rs"
    if verify_rs.exists():
        verify_rs.unlink()

    main_rs = source / "main.rs"
    text = main_rs.read_text()
    if "#![allow(" not in text:
        raise SystemExit(f"generated self-host main has no allow block: {main_rs}")
    text = text.replace("#![allow(", "#![allow(clippy::all, ", 1)
    verify_module = "\n#[cfg(test)]\nmod verify;\n"
    if verify_module not in text:
        raise SystemExit(f"generated self-host main has no verify module: {main_rs}")
    main_rs.write_text(text.replace(verify_module, "\n", 1))

    manifest = output / "Cargo.toml"
    for attempt in range(1, 6):
        run(["cargo", "fmt", "--manifest-path", str(manifest)])
        check = run(
            ["cargo", "fmt", "--manifest-path", str(manifest), "--", "--check"],
            check=False,
        )
        if check.returncode == 0:
            return source
        if attempt == 5:
            raise SystemExit(
                "cargo fmt did not reach a fixed point on the regenerated "
                "self-host after 5 passes"
            )
    raise AssertionError("fixed-point loop must return or fail")


def rust_source_difference(left: Path, right: Path) -> list[str]:
    left_files = {path.relative_to(left) for path in left.rglob("*.rs")}
    right_files = {path.relative_to(right) for path in right.rglob("*.rs")}
    differences = [f"only generated: {path}" for path in sorted(left_files - right_files)]
    differences.extend(
        f"only checked in: {path}" for path in sorted(right_files - left_files)
    )
    differences.extend(
        f"content differs: {path}"
        for path in sorted(left_files & right_files)
        if not filecmp.cmp(left / path, right / path, shallow=False)
    )
    return differences


def install_generated_source(source: Path) -> None:
    if CHECKED_IN_SRC.exists():
        shutil.rmtree(CHECKED_IN_SRC)
    shutil.copytree(source, CHECKED_IN_SRC)
    count = sum(1 for _ in CHECKED_IN_SRC.rglob("*.rs"))
    print(f"Copied {count} files to src/self_host/")


def install_generated_project(project: Path, destination: Path) -> None:
    if destination.exists():
        shutil.rmtree(destination)
    destination.parent.mkdir(parents=True, exist_ok=True)
    shutil.copytree(project, destination)


def check_freshness(aver_bin: Path) -> None:
    with tempfile.TemporaryDirectory(prefix="aver-self-host-freshness-") as temp:
        output = Path(temp) / "out"
        compile_self_host(aver_bin, output)
        generated = prepare_generated_source(output)
        differences = rust_source_difference(generated, CHECKED_IN_SRC)
    if differences:
        preview = "\n".join(f"  - {item}" for item in differences[:10])
        remainder = len(differences) - 10
        if remainder > 0:
            preview += f"\n  - ... and {remainder} more"
        raise SystemExit(
            "checked-in self-host output is stale:\n"
            f"{preview}\n"
            "Regenerate it with:\n"
            "  python3 tools/regenerate_self_host.py"
        )
    print("Checked-in self-host output is fresh.")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument(
        "--check",
        action="store_true",
        help="regenerate in a temporary directory and compare without modifying the worktree",
    )
    mode.add_argument(
        "--output",
        type=Path,
        help="generate a normalized standalone project at this path without installing it",
    )
    parser.add_argument(
        "--aver-bin",
        help="compiler binary to use (defaults to target/debug/aver, building it if absent)",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    aver_bin = resolve_aver_bin(args.aver_bin)
    if args.check:
        check_freshness(aver_bin)
        return

    output = args.output
    if output is not None:
        if not output.is_absolute():
            output = REPO_ROOT / output
        with tempfile.TemporaryDirectory(prefix="aver-self-host-output-") as temp:
            project = Path(temp) / "out"
            compile_self_host(aver_bin, project)
            prepare_generated_source(project)
            install_generated_project(project, output)
        print(f"Generated normalized self-host project at {output}")
        return

    # Format outside the repository workspace. A generated Cargo.toml placed
    # directly under `self_hosted/out` is intentionally not a workspace member,
    # so `cargo fmt --manifest-path` rejects it while it is nested under the
    # repository Cargo.toml. Install only after the standalone project is at a
    # verified formatting fixed point.
    with tempfile.TemporaryDirectory(prefix="aver-self-host-regenerate-") as temp:
        project = Path(temp) / "out"
        compile_self_host(aver_bin, project)
        prepare_generated_source(project)
        install_generated_project(project, DEFAULT_OUTPUT)
    install_generated_source(DEFAULT_OUTPUT / "src")


if __name__ == "__main__":
    main()
