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
import hashlib
import json
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


def build_runtime_artifacts(version: str, dry_run: bool) -> None:
    """Emit per-version runtime artifacts to tools/website/runtime/v{X}/.

    These are what averlang.dev distributes for `--target edge-wasm`
    consumers — the shared aver_runtime.wasm + aver_to_wasi.wasm bridge,
    plus human-readable .wat companions, sha256 checksums, and a
    README. Also refreshes `latest/` (mirror, not symlink — Cloudflare
    Pages doesn't follow symlinks) and the top-level manifest.json
    used by the runtime listing page.
    """
    print(f"Publishing runtime artifacts for v{version}...")
    runtime_root = REPO_ROOT / "tools" / "website" / "runtime"
    target_dir = runtime_root / f"v{version}"
    if dry_run:
        print(f"  [dry-run] would emit aver_runtime.wasm + aver_to_wasi.wasm to {target_dir}")
        return

    aver_bin = REPO_ROOT / "target" / "release" / "aver"
    if not aver_bin.exists():
        run(["cargo", "build", "--release", "--features", "wasm"])

    target_dir.mkdir(parents=True, exist_ok=True)

    # Build the two artifacts with --optimize size + .wat companion.
    runtime_path = target_dir / "aver_runtime.wasm"
    bridge_path = target_dir / "aver_to_wasi.wasm"
    run([
        str(aver_bin), "wasm-runtime",
        "--artifact", "runtime",
        "--optimize", "size",
        "--wat",
        "--output", str(runtime_path),
    ])
    run([
        str(aver_bin), "wasm-runtime",
        "--artifact", "wasi-bridge",
        "--optimize", "size",
        "--wat",
        "--output", str(bridge_path),
    ])

    # sha256 manifest. Stable order so diffs are clean across releases.
    sha_file = target_dir / "CHECKSUMS.txt"
    lines = []
    for path in sorted(target_dir.iterdir()):
        if path.is_file() and path.name not in ("CHECKSUMS.txt", "README.md"):
            sha = hashlib.sha256(path.read_bytes()).hexdigest()
            lines.append(f"{sha}  {path.name}\n")
    sha_file.write_text("".join(lines))

    # Per-version README — short, points users at the import surface
    # they care about and lists the byte sizes for at-a-glance compare.
    runtime_size = runtime_path.stat().st_size
    bridge_size = bridge_path.stat().st_size
    readme = (
        f"# Aver runtime — v{version}\n"
        f"\n"
        f"Standalone WebAssembly modules for the Aver language runtime.\n"
        f"Pair these with a thin `user.wasm` produced by\n"
        f"`aver compile --target edge-wasm --optimize size`:\n"
        f"\n"
        f"- `aver_runtime.wasm` ({runtime_size:,} B) — alloc, GC, hashmap,\n"
        f"  string/list/vector ops. Imported by every Aver program as the\n"
        f"  `aver_runtime` module. Cached once per session.\n"
        f"- `aver_to_wasi.wasm` ({bridge_size:,} B) — translation shim that\n"
        f"  satisfies a program's `aver/*` host imports against\n"
        f"  `wasi_snapshot_preview1.fd_write`. Optional, only needed if you\n"
        f"  want to run a thin user.wasm under wasmtime / Cloudflare\n"
        f"  Workers / Fastly Compute.\n"
        f"\n"
        f"`.wat` files are human-readable disassemblies, not required at\n"
        f"runtime — they're shipped so you can inspect what the runtime\n"
        f"actually contains.\n"
        f"\n"
        f"`CHECKSUMS.txt` lists sha256 sums of every binary file in this\n"
        f"directory.\n"
    )
    (target_dir / "README.md").write_text(readme)

    # Refresh latest/ as a flat mirror of this version. Cloudflare Pages
    # serves static dirs and won't follow symlinks reliably, so we copy.
    latest_dir = runtime_root / "latest"
    if latest_dir.exists():
        shutil.rmtree(latest_dir)
    shutil.copytree(target_dir, latest_dir)

    # Top-level manifest enumerating every published version. Consumers
    # (CDN listing page, future installer) read this instead of crawling
    # directory listings, which static hosts often disable.
    manifest_path = runtime_root / "manifest.json"
    versions = []
    for entry in sorted(runtime_root.iterdir(), reverse=True):
        if entry.is_dir() and entry.name.startswith("v"):
            ver = entry.name[1:]
            rt = entry / "aver_runtime.wasm"
            br = entry / "aver_to_wasi.wasm"
            if rt.exists() and br.exists():
                versions.append({
                    "version": ver,
                    "runtime_bytes": rt.stat().st_size,
                    "bridge_bytes": br.stat().st_size,
                    "path": entry.name,
                })
    manifest_path.write_text(json.dumps({
        "latest": versions[0]["version"] if versions else None,
        "versions": versions,
    }, indent=2) + "\n")

    # Public listing page at /runtime/ — static HTML so static hosts
    # (Cloudflare Pages) can serve it without JS or directory indexing.
    rows = []
    for v in versions:
        ver = v["version"]
        marker = "  <small>(latest)</small>" if ver == versions[0]["version"] else ""
        rows.append(
            f'      <tr>\n'
            f'        <td><a href="v{ver}/">v{ver}</a>{marker}</td>\n'
            f'        <td><a href="v{ver}/aver_runtime.wasm">aver_runtime.wasm</a>'
            f' <small>({v["runtime_bytes"]:,} B)</small></td>\n'
            f'        <td><a href="v{ver}/aver_to_wasi.wasm">aver_to_wasi.wasm</a>'
            f' <small>({v["bridge_bytes"]:,} B)</small></td>\n'
            f'        <td><a href="v{ver}/aver_runtime.wat">.wat</a> · '
            f'<a href="v{ver}/CHECKSUMS.txt">sha256</a></td>\n'
            f'      </tr>'
        )
    rows_html = "\n".join(rows) if rows else (
        '      <tr><td colspan="4"><em>No releases yet.</em></td></tr>'
    )
    latest_ver = versions[0]["version"] if versions else "?"
    index_html = f"""<!doctype html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>Aver runtime artifacts</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="icon" type="image/svg+xml" href="../favicon.svg">
    <link rel="stylesheet" href="../style.css">
</head>
<body>
<main class="container" style="max-width: 880px; padding: 3rem 1.5rem;">
    <p><a href="/">← averlang.dev</a></p>
    <h1>Aver runtime artifacts</h1>
    <p class="section-sub">
        Standalone WebAssembly modules for the Aver language runtime.
        Pair these with a thin <code>user.wasm</code> built via
        <code>aver compile --target edge-wasm --optimize size</code> —
        the runtime is cached once and shared across every program.
    </p>

    <h2>Quick start</h2>
    <pre><code>$ aver compile mygame.av --target edge-wasm --optimize size -o dist/

# Browser / wasmtime: instantiate runtime first, point user.wasm at it
const runtime = await WebAssembly.instantiateStreaming(
    fetch("https://averlang.dev/runtime/latest/aver_runtime.wasm"),
    {{}}
);
const user = await WebAssembly.instantiateStreaming(
    fetch("/dist/mygame.wasm"),
    {{ aver_runtime: runtime.instance.exports, aver: hostImports }}
);
user.instance.exports._start();</code></pre>

    <h2>Releases</h2>
    <p>
        Stable URL for the most recent release:
        <code>/runtime/latest/aver_runtime.wasm</code>.
        Per-version URLs below are immutable — pin to one for
        reproducible deployments.
    </p>
    <table style="width: 100%; border-collapse: collapse;">
      <thead>
        <tr style="text-align: left; border-bottom: 1px solid var(--border, #ccc);">
          <th>Version</th><th>Runtime</th><th>WASI bridge</th><th>Extras</th>
        </tr>
      </thead>
      <tbody>
{rows_html}
      </tbody>
    </table>

    <h2>What's in here</h2>
    <ul>
        <li><strong>aver_runtime.wasm</strong> — alloc, GC, hashmap (HAMT),
            string/list/vector ops. Imported as <code>aver_runtime</code>
            by every <code>edge-wasm</code> binary.</li>
        <li><strong>aver_to_wasi.wasm</strong> — translation shim that
            satisfies a program's <code>aver/*</code> host imports
            against <code>wasi_snapshot_preview1.fd_write</code>.
            Optional, for wasmtime / Cloudflare Workers / Fastly.</li>
        <li><strong>.wat</strong> — human-readable disassembly. Inspect
            what's actually in the binary.</li>
        <li><strong>CHECKSUMS.txt</strong> — sha256 sums for every file
            in the directory.</li>
    </ul>

    <p class="section-sub">
        Latest: <strong>v{latest_ver}</strong>.
        Manifest: <a href="manifest.json">manifest.json</a>.
    </p>
</main>
</body>
</html>
"""
    (runtime_root / "index.html").write_text(index_html)

    print(
        f"  aver_runtime.wasm  {runtime_size:>6,} B"
        f"  + aver_to_wasi.wasm  {bridge_size:>5,} B"
        f"  → tools/website/runtime/v{version}/"
    )


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

    # 4.5 Publish per-version runtime artifacts (aver_runtime.wasm +
    #     aver_to_wasi.wasm + .wat companions + checksums + manifest)
    #     under tools/website/runtime/v{version}/. Skipped together with
    #     playground because both depend on a release-built `aver` binary.
    if not args.skip_playground:
        build_runtime_artifacts(new_version, dry_run)
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
