#!/usr/bin/env python3
"""
Aver release script.

Usage:
    python3 tools/release.py 0.9.7
    python3 tools/release.py 0.9.7 --dry-run
    python3 tools/release.py 0.9.7 --skip-publish
    python3 tools/release.py 0.9.7 --skip-playground
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]

# Cargo.toml files and the version keys to bump in each.
# (file, [(key_pattern, replacement_template)])
CRATE_ORDER = ["aver-rt", "aver-memory", "aver-lang", "aver-lsp"]

VERSION_FILES = {
    "aver-rt": REPO_ROOT / "aver-rt" / "Cargo.toml",
    "aver-memory": REPO_ROOT / "aver-memory" / "Cargo.toml",
    "aver-lang": REPO_ROOT / "Cargo.toml",
    "aver-lsp": REPO_ROOT / "aver-lsp" / "Cargo.toml",
}


def run(cmd: list[str], cwd: Path = REPO_ROOT, check: bool = True, capture: bool = False) -> subprocess.CompletedProcess:
    if capture:
        return subprocess.run(cmd, cwd=cwd, text=True, capture_output=True, check=check)
    return subprocess.run(cmd, cwd=cwd, check=check)


def current_version() -> str:
    text = (REPO_ROOT / "Cargo.toml").read_text()
    m = re.search(r'^version\s*=\s*"([^"]+)"', text, re.MULTILINE)
    if not m:
        raise SystemExit("Cannot find version in Cargo.toml")
    return m.group(1)


def bump_toml_version(path: Path, old: str, new: str) -> None:
    """Replace version = "old" with version = "new" in a Cargo.toml."""
    text = path.read_text()
    # Package version
    text = text.replace(f'version = "{old}"', f'version = "{new}"', 1)
    path.write_text(text)


def bump_dep_pin(path: Path, dep_name: str, old: str, new: str) -> None:
    """Replace version = "=old" pin for a dependency."""
    text = path.read_text()
    text = text.replace(
        f'{dep_name}' + f', version = "={old}"',
        f'{dep_name}' + f', version = "={new}"',
    )
    # Also handle the pattern without leading comma context
    text = text.replace(f'version = "={old}"', f'version = "={new}"')
    path.write_text(text)


def bump_patch(version: str) -> str:
    """Increment patch version: 0.4.1 -> 0.4.2"""
    parts = version.split(".")
    parts[-1] = str(int(parts[-1]) + 1)
    return ".".join(parts)


def compute_new_versions(old_versions: dict[str, str], new_main: str) -> dict[str, str]:
    """Compute new versions: aver-lang gets the specified version,
    subcrates with changes get a patch bump."""
    new = {}
    new["aver-lang"] = new_main
    for crate in CRATE_ORDER:
        if crate == "aver-lang":
            continue
        # Check if crate has changes since last tag
        last_tag = subprocess.run(
            ["git", "describe", "--tags", "--abbrev=0"],
            cwd=REPO_ROOT, capture_output=True, text=True,
        ).stdout.strip()
        result = subprocess.run(
            ["git", "log", f"{last_tag}..HEAD", "--oneline", "--", f"{VERSION_FILES[crate].parent}/"],
            cwd=REPO_ROOT, capture_output=True, text=True,
        )
        has_changes = bool(result.stdout.strip())
        if has_changes:
            new[crate] = bump_patch(old_versions[crate])
        else:
            new[crate] = old_versions[crate]
    return new


def bump_all_versions(old_versions: dict[str, str], new_versions: dict[str, str], dry_run: bool) -> None:
    """Bump all crate versions."""
    for crate in CRATE_ORDER:
        path = VERSION_FILES[crate]
        old = old_versions[crate]
        new = new_versions[crate]
        if old == new:
            print(f"  {crate}: {old} (unchanged)")
            continue
        print(f"  {crate}: {old} -> {new}" if not dry_run else f"  [dry-run] {crate}: {old} -> {new}")
        if not dry_run:
            bump_toml_version(path, old, new)

    if not dry_run:
        # Cross-references (dep pins)
        main_toml = VERSION_FILES["aver-lang"]
        bump_dep_pin(main_toml, "aver-rt", old_versions["aver-rt"], new_versions["aver-rt"])
        bump_dep_pin(main_toml, "aver-memory", old_versions["aver-memory"], new_versions["aver-memory"])
        mem_toml = VERSION_FILES["aver-memory"]
        bump_dep_pin(mem_toml, "aver-rt", old_versions["aver-rt"], new_versions["aver-rt"])
        lsp_toml = VERSION_FILES["aver-lsp"]
        bump_dep_pin(lsp_toml, "aver-lang", old_versions["aver-lang"], new_versions["aver-lang"])


def read_crate_version(crate: str) -> str:
    path = VERSION_FILES[crate]
    text = path.read_text()
    m = re.search(r'^version\s*=\s*"([^"]+)"', text, re.MULTILINE)
    if not m:
        raise SystemExit(f"Cannot find version in {path}")
    return m.group(1)


def bump_website_version(new_version: str, dry_run: bool) -> None:
    """Bump the version badge shown in the landing hero (tools/website/index.html)."""
    path = REPO_ROOT / "tools" / "website" / "index.html"
    major_minor = ".".join(new_version.split(".")[:2])
    short = f"v{major_minor}"

    text = path.read_text()
    pattern = r"(MIT licensed &middot; Written in Rust &middot; )v\d+(?:\.\d+)+( &middot;)"
    new_text, count = re.subn(pattern, lambda m: f"{m.group(1)}{short}{m.group(2)}", text)

    if count == 0:
        print("  WARN: hero-proof version badge not found, skipping")
        return

    if dry_run:
        print(f"  [dry-run] website hero badge -> {short}")
    else:
        path.write_text(new_text)
        print(f"  Updated website hero badge -> {short}")


def regenerate_self_host(dry_run: bool) -> None:
    print("Regenerating self-host...")
    if dry_run:
        print("  [dry-run] would run: aver compile self_hosted/main.av --target rust ...")
        return

    aver_bin = REPO_ROOT / "target" / "debug" / "aver"
    if not aver_bin.exists():
        print("  Building aver first...")
        run(["cargo", "build"])

    run([
        str(aver_bin), "compile", "self_hosted/main.av",
        "--target", "rust",
        "--output", "self_hosted/out",
        "--module-root", "self_hosted",
        "--with-self-host-support",
        "--guest-entry", "runGuestCliProgram",
        "--with-replay",
        "--policy", "runtime",
    ])

    # Copy generated src to src/self_host/
    src_dest = REPO_ROOT / "src" / "self_host"
    src_source = REPO_ROOT / "self_hosted" / "out" / "src"
    if src_dest.exists():
        shutil.rmtree(src_dest)
    shutil.copytree(src_source, src_dest)

    # Remove verify.rs (test-only, causes compile errors as [[bin]] in aver-lang)
    verify_rs = src_dest / "verify.rs"
    if verify_rs.exists():
        verify_rs.unlink()

    # Patch main.rs: add clippy::all allow, remove verify module
    main_rs = src_dest / "main.rs"
    text = main_rs.read_text()
    text = text.replace(
        "#![allow(",
        "#![allow(clippy::all, ",
    )
    text = text.replace("\n#[cfg(test)]\nmod verify;\n", "\n")
    main_rs.write_text(text)

    # Format generated code
    run(["cargo", "fmt"])

    print(f"  Copied {sum(1 for _ in src_dest.rglob('*.rs'))} files to src/self_host/")


def regenerate_playground(dry_run: bool) -> None:
    print("Regenerating playground WASM artifacts...")
    if dry_run:
        print("  [dry-run] would run: python3 tools/website/rebuild_playground.py --skip-compiler")
        return

    # Build release binary for playground
    run(["cargo", "build", "--release", "--features", "wasm"])
    run([
        sys.executable,
        str(REPO_ROOT / "tools" / "website" / "rebuild_playground.py"),
        "--skip-compiler",
        "--aver-bin", str(REPO_ROOT / "target" / "release" / "aver"),
    ])


def verify(dry_run: bool) -> None:
    print("Running verification...")
    if dry_run:
        print("  [dry-run] would run: cargo fmt --check, clippy, test")
        return

    run(["cargo", "fmt"])
    # Skip generated self-host code in clippy (same as CI)
    run(["cargo", "clippy", "--workspace", "--all-targets", "--exclude", "aver-lang", "--", "-D", "warnings"])
    run(["cargo", "clippy", "-p", "aver-lang", "--lib", "--bin", "aver", "--features", "wasm", "--", "-D", "warnings"])
    run(["cargo", "test", "--features", "wasm"])


def publish(new_versions: dict[str, str], old_versions: dict[str, str], dry_run: bool) -> None:
    print("Publishing to crates.io...")
    for crate in CRATE_ORDER:
        if new_versions[crate] == old_versions[crate]:
            print(f"  {crate}: skipped (unchanged)")
            continue
        cmd = ["cargo", "publish", "-p", crate, "--allow-dirty"]
        if dry_run:
            print(f"  [dry-run] {' '.join(cmd)}")
        else:
            print(f"  Publishing {crate} {new_versions[crate]}...")
            run(cmd)


def git_commit_tag_push(version: str, dry_run: bool) -> None:
    msg = f"Release {version}"
    tag = f"v{version}"

    if dry_run:
        print(f"  [dry-run] git add + commit: {msg}")
        print(f"  [dry-run] git tag {tag}")
        print(f"  [dry-run] git push + push tags")
        print(f"  [dry-run] gh release create {tag}")
        return

    run(["git", "add", "-A"])
    run(["git", "commit", "-m", msg])
    run(["git", "tag", tag])
    run(["git", "push"])
    run(["git", "push", "--tags"])

    # GitHub release from CHANGELOG
    changelog = (REPO_ROOT / "CHANGELOG.md").read_text()
    # Extract section for this version
    pattern = rf"## {re.escape(version)}.*?\n(.*?)(?=\n## |\Z)"
    m = re.search(pattern, changelog, re.DOTALL)
    notes = m.group(1).strip() if m else f"Release {version}"

    run(["gh", "release", "create", tag, "--title", f"Aver {version}", "--notes", notes])


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Release a new version of Aver")
    parser.add_argument("version", help="New version number (e.g. 0.9.7)")
    parser.add_argument("--dry-run", action="store_true", help="Show what would be done without executing")
    parser.add_argument("--skip-publish", action="store_true", help="Skip crates.io publish")
    parser.add_argument("--skip-playground", action="store_true", help="Skip playground WASM rebuild")
    parser.add_argument("--skip-self-host", action="store_true", help="Skip self-host regeneration")
    return parser.parse_args()


def main() -> int:
    args = parse_args()
    new_version = args.version
    dry_run = args.dry_run

    if dry_run:
        print(f"=== DRY RUN: release {new_version} ===\n")
    else:
        print(f"=== Releasing {new_version} ===\n")

    # 0. Editor grammars must match editors/keywords.json (sublime + vscode +
    #    playground highlight.js). Drift here means a new keyword shipped to
    #    one place and not the others — block the release.
    print("Checking editor grammar sync...")
    if dry_run:
        print("  [dry-run] would run: python3 editors/sync.py --check")
    else:
        run([sys.executable, "editors/sync.py", "--check"])
    print()

    # 1. Read current versions
    old_versions = {crate: read_crate_version(crate) for crate in CRATE_ORDER}
    print("Current versions:")
    for crate, ver in old_versions.items():
        print(f"  {crate}: {ver}")
    print()

    # 2. Compute and bump versions
    new_versions = compute_new_versions(old_versions, new_version)
    print("\nTarget versions:")
    for crate, ver in new_versions.items():
        changed = " (changed)" if ver != old_versions[crate] else ""
        print(f"  {crate}: {ver}{changed}")
    print("\nBumping versions...")
    bump_all_versions(old_versions, new_versions, dry_run)
    print()

    # 2.5 Bump landing-page version badge
    print("Bumping website badge...")
    bump_website_version(new_version, dry_run)
    print()

    # 3. Regenerate self-host
    if not args.skip_self_host:
        regenerate_self_host(dry_run)
        print()

    # 4. Regenerate playground
    if not args.skip_playground:
        regenerate_playground(dry_run)
        print()

    # 5. Verify
    verify(dry_run)
    print()

    # 6. Publish
    if not args.skip_publish:
        publish(new_versions, old_versions, dry_run)
        print()

    # 7. Commit, tag, push, GitHub release
    git_commit_tag_push(new_version, dry_run)
    print()

    print(f"{'[dry-run] ' if dry_run else ''}Done! Released {new_version}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
