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
import datetime
import os
import re
import shutil
import subprocess
import sys
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[1]

# Cargo.toml files and the version keys to bump in each.
# (file, [(key_pattern, replacement_template)])
CRATE_ORDER = ["aver-rt", "aver-memory", "aver-cert", "aver-lang", "aver-lsp"]

VERSION_FILES = {
    "aver-rt": REPO_ROOT / "aver-rt" / "Cargo.toml",
    "aver-memory": REPO_ROOT / "aver-memory" / "Cargo.toml",
    "aver-cert": REPO_ROOT / "aver-cert" / "Cargo.toml",
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


# Cascade cases where a downstream MUST bump because skipping it would
# block `cargo publish aver-lang` on a dep-resolution conflict:
# `aver-lang` resolves both `aver-rt` and `aver-memory`. If `aver-rt`
# bumps but `aver-memory` stays at its already-published version, that
# version still pins the old `aver-rt` exactly — cargo can't satisfy
# both. Other downstream crates (`aver-lsp`) are leaves and never
# block publish, so they bump only on their own changes.
PUBLISH_BLOCKERS = {
    "aver-rt": ["aver-memory"],
}

# `aver-cert` has its own public version line. Its first published package is
# 0.1.0 even when it ships alongside aver-lang 0.27; subsequent source changes
# patch-bump it independently.
FIRST_PUBLIC_VERSIONS = {
    "aver-cert": "0.1.0",
}


def is_unpublished_first_version(crate: str, version: str) -> bool:
    return FIRST_PUBLIC_VERSIONS.get(crate) == version and version not in published_versions(crate)


def crate_source_changed_since_version_set(crate: str, version: str) -> bool:
    """True if the crate's source changed since the commit that last set its
    current package version.

    More robust than diffing `last_tag..HEAD`: a crate can change in an earlier
    release window without ever being version-bumped (and thus never published),
    so the published version is stale even though nothing changed since the most
    recent tag. We baseline against "where this crate's version was last set"
    instead, which tracks the last time it was actually (re)published.

    Manifest/lockfile churn (version bumps, dep-pin updates) is ignored.
    """
    toml = VERSION_FILES[crate]
    crate_dir = toml.parent
    ver_commit = subprocess.run(
        ["git", "log", "-n", "1", "--format=%H", "-S", f'version = "{version}"', "--", str(toml)],
        cwd=REPO_ROOT, capture_output=True, text=True,
    ).stdout.strip()
    if not ver_commit:
        return True  # cannot establish a baseline -> bump to be safe
    changed = subprocess.run(
        ["git", "diff", "--name-only", f"{ver_commit}..HEAD", "--", str(crate_dir)],
        cwd=REPO_ROOT, capture_output=True, text=True,
    ).stdout.strip().splitlines()
    return any(not f.endswith(("Cargo.toml", "Cargo.lock")) for f in changed)


def published_versions(crate: str) -> set[str]:
    """Versions of `crate` already on crates.io. Empty set on any network error
    (the caller then skips the already-published guard, as before)."""
    import json
    import urllib.request

    url = f"https://crates.io/api/v1/crates/{crate}"
    req = urllib.request.Request(
        url, headers={"User-Agent": "aver-release-script (https://github.com/jasisz/aver)"}
    )
    try:
        with urllib.request.urlopen(req, timeout=20) as resp:
            data = json.load(resp)
        return {v["num"] for v in data.get("versions", [])}
    except Exception as e:  # noqa: BLE001 — best-effort guard, never block release on it
        print(f"  warning: could not query crates.io for {crate} ({e}); skipping already-published check")
        return set()


def _cascade(new: dict[str, str], old_versions: dict[str, str]) -> bool:
    """One pass of the publish-blocker cascade. Returns whether anything bumped."""
    changed = False
    for upstream, blockers in PUBLISH_BLOCKERS.items():
        if new[upstream] == old_versions[upstream]:
            continue
        for ds in blockers:
            if new[ds] == old_versions[ds]:
                new[ds] = bump_patch(old_versions[ds])
                changed = True
    return changed


def compute_new_versions(old_versions: dict[str, str], new_main: str) -> dict[str, str]:
    """Compute new versions for every crate.

    aver-lang gets the requested version. Other crates patch-bump independently if either
    (a) their source changed since their version was last set, or (b) skipping
    their bump would create a publish-time resolution conflict for aver-lang
    (see PUBLISH_BLOCKERS). Finally, any computed version that is already on
    crates.io is bumped past — so a re-run cleanly recovers from a partial
    publish instead of failing on "crate version already uploaded".
    """
    new = dict(old_versions)
    new["aver-lang"] = new_main

    # Pass 1: bump any non-main crate whose SOURCE changed since its version
    # was last set (not merely since the last tag).
    for crate in CRATE_ORDER:
        if crate == "aver-lang":
            continue
        if is_unpublished_first_version(crate, old_versions[crate]):
            continue
        if crate_source_changed_since_version_set(crate, old_versions[crate]):
            new[crate] = bump_patch(old_versions[crate])

    # Pass 2: forced cascade for publish-blockers (fixpoint).
    for _ in range(len(CRATE_ORDER)):
        if not _cascade(new, old_versions):
            break

    # Pass 3: never assign a version already on crates.io. Bump past taken
    # versions, then re-run the cascade so downstreams re-pin to the final
    # upstream version. (Recovers from a partial publish: e.g. aver-memory
    # 0.2.10 already uploaded -> land on 0.2.11.)
    for _ in range(len(CRATE_ORDER) * 4):
        bumped = False
        for crate in CRATE_ORDER:
            if new[crate] == old_versions[crate]:
                continue  # not being published this run
            taken = published_versions(crate)
            while new[crate] in taken:
                new[crate] = bump_patch(new[crate])
                bumped = True
        _cascade(new, old_versions)
        if not bumped:
            break

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
        bump_dep_pin(main_toml, "aver-cert", old_versions["aver-cert"], new_versions["aver-cert"])
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
        print("  [dry-run] would run: python3 tools/website/rebuild_playground.py")
        return

    # `rebuild_playground.py` restores the wasm-capable aver bin
    # itself after `wasm-pack` invalidates cargo's feature cache
    # (the script's `main` re-runs `cargo build --release --bin aver
    # --features wasm` between `build_compiler` and `build_wasm`).
    # Pre-build once so the wasm-pack invocation has a warm cache
    # to invalidate — saves a few minutes on cold runs.
    run(["cargo", "build", "--release", "--bin", "aver", "--features", "wasm"])
    run([
        sys.executable,
        str(REPO_ROOT / "tools" / "website" / "rebuild_playground.py"),
    ])


def verify(dry_run: bool) -> None:
    print("Running verification...", flush=True)
    if dry_run:
        print("  [dry-run] would run: cargo fmt --check, clippy, test, bench scenarios, edge compile")
        return

    print("  verify: cargo fmt", flush=True)
    run(["cargo", "fmt"])
    # Skip generated self-host code in clippy (same as CI)
    print("  verify: cargo clippy --workspace (no aver-lang)", flush=True)
    run(["cargo", "clippy", "--workspace", "--all-targets", "--exclude", "aver-lang", "--", "-D", "warnings"])
    print("  verify: cargo clippy -p aver-lang --features wasm,wasip2", flush=True)
    run(["cargo", "clippy", "-p", "aver-lang", "--lib", "--bin", "aver", "--features", "wasm,wasip2", "--", "-D", "warnings"])
    # `wasip2` is a separate feature; with `wasm` alone every
    # `#![cfg(feature = "wasip2")]` test file reports 0 tests, so the
    # wasip2 codegen / component-model surface was never gated by the
    # release check. Enable both (they share `wasm-compile`) so the
    # wasip2_* suites actually run.
    print("  verify: cargo test --features wasm,wasip2", flush=True)
    run(["cargo", "test", "--features", "wasm,wasip2"])

    # Bench smoke. Runs every scenario in `bench/scenarios/` end-to-end on
    # the VM target — catches pipeline / VM regressions that the unit tests
    # miss (e.g. a real program that compiles fine but crashes in the
    # bytecode dispatch). The release script doesn't gate on numbers (the
    # CI gate is 0.15.2 work) but the run must succeed; any scenario that
    # errors out blocks the release.
    run(["cargo", "build", "--release", "--bin", "aver", "--features", "wasm"])
    aver_bin = REPO_ROOT / "target" / "release" / "aver"
    run([str(aver_bin), "bench", str(REPO_ROOT / "bench" / "scenarios"), "--json"])

    # `--target wasm-gc --handler X` (and `--preset cloudflare`) smoke.
    # The 0.17.2 release surfaced three compounding wasm-gc handler-mode
    # bugs that lived from 0.16 onward because the path was never gated
    # on a release. Compiling `tools/edge/app.av` with `--preset
    # cloudflare` exercises every codepath the live edge demo touches —
    # handler synthesis, builtin record dedup, data-count snapshot,
    # caller_fn collector — and `wasm-tools validate` proves the bytes
    # pass an external validator (not just our internal one).
    print("  edge: aver compile tools/edge/app.av --preset cloudflare …")
    edge_out = REPO_ROOT / "target" / "release-edge-smoke"
    if edge_out.exists():
        shutil.rmtree(edge_out)
    edge_out.mkdir(parents=True, exist_ok=True)
    run([
        str(aver_bin), "compile",
        str(REPO_ROOT / "tools" / "edge" / "app.av"),
        "--preset", "cloudflare",
        "--handler", "handler",
        "--module-root", str(REPO_ROOT / "tools" / "edge"),
        "-o", str(edge_out),
    ])
    # External validation — wasmtime's wasm-tools is the same validator
    # the workerd / wasmtime CLI use; if this passes the bytes will
    # instantiate cleanly anywhere wasm-gc + tail-calls are supported.
    if shutil.which("wasm-tools") is not None:
        run(["wasm-tools", "validate", "--features", "all", str(edge_out / "app.wasm")])
    else:
        print("  edge: wasm-tools not on PATH — skipping external validation")
    shutil.rmtree(edge_out)


def publish(new_versions: dict[str, str], old_versions: dict[str, str], dry_run: bool) -> None:
    print("Publishing to crates.io...")
    for crate in CRATE_ORDER:
        first_public = is_unpublished_first_version(crate, new_versions[crate])
        if new_versions[crate] == old_versions[crate] and not first_public:
            print(f"  {crate}: skipped (unchanged)")
            continue
        cmd = ["cargo", "publish", "-p", crate, "--allow-dirty"]
        if dry_run:
            print(f"  [dry-run] {' '.join(cmd)}")
        else:
            print(f"  Publishing {crate} {new_versions[crate]}...")
            run(cmd)


def stamp_changelog_release_date(new_version: str, dry_run: bool) -> None:
    """Replace `## {new_version} "{codename}" (unreleased)` (or the no-
    codename variant) with `## {new_version} "{codename}" — YYYY-MM-DD`.

    Idempotent: if the header already carries a date instead of
    `(unreleased)`, this is a no-op so re-running the release script
    after a partial failure doesn't double-stamp. The codename (if any)
    is preserved verbatim — the regex captures whatever sits between
    the version and `(unreleased)`.
    """
    path = REPO_ROOT / "CHANGELOG.md"
    text = path.read_text()
    today = datetime.date.today().isoformat()
    pattern = rf'^(## {re.escape(new_version)}(?: "[^"]+")?) \(unreleased\)$'
    new_text, n = re.subn(
        pattern, rf"\1 — {today}", text, count=1, flags=re.MULTILINE
    )
    if n == 0:
        # Either already stamped or the section header wasn't found in
        # the expected shape — both are harmless to skip; verify() runs
        # right before publish and would catch a missing section anyway.
        print(f"  CHANGELOG: no `## {new_version} ... (unreleased)` line — skipping")
        return
    if dry_run:
        print(f"  [dry-run] would stamp CHANGELOG `## {new_version}` with {today}")
        return
    path.write_text(new_text)
    print(f"  CHANGELOG: stamped `## {new_version}` with {today}")


def codename_for(version: str, changelog: str) -> str | None:
    """Find the codename declared on this version's CHANGELOG header.

    Only thematic releases (X.Y.0) carry a codename — patches stay plain
    so the release-list signal stays tight: codename = milestone, no
    codename = bugfix in that era.
    """
    m = re.search(rf'^## {re.escape(version)} "([^"]+)"', changelog, re.MULTILINE)
    return m.group(1) if m else None


def git_commit_tag_push(version: str, dry_run: bool) -> None:
    msg = f"Release {version}"
    tag = f"v{version}"
    changelog = (REPO_ROOT / "CHANGELOG.md").read_text()
    codename = codename_for(version, changelog)
    title = f'Aver {version} "{codename}"' if codename else f"Aver {version}"

    if dry_run:
        print(f"  [dry-run] git add + commit: {msg}")
        print(f"  [dry-run] git tag {tag}")
        print(f"  [dry-run] git push + push tags")
        print(f"  [dry-run] gh release create {tag} --title {title!r}")
        return

    run(["git", "add", "-A"])
    run(["git", "commit", "-m", msg])
    run(["git", "tag", tag])
    run(["git", "push"])
    run(["git", "push", "--tags"])

    # Extract release notes for this version's section
    pattern = rf"## {re.escape(version)}.*?\n(.*?)(?=\n## |\Z)"
    m = re.search(pattern, changelog, re.DOTALL)
    notes = m.group(1).strip() if m else f"Release {version}"

    run(["gh", "release", "create", tag, "--title", title, "--notes", notes])


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Release a new version of Aver")
    parser.add_argument("version", help="New version number (e.g. 0.9.7)")
    parser.add_argument("--dry-run", action="store_true", help="Show what would be done without executing")
    parser.add_argument("--skip-publish", action="store_true", help="Skip crates.io publish")
    parser.add_argument("--skip-playground", action="store_true", help="Skip playground WASM rebuild")
    parser.add_argument("--skip-self-host", action="store_true", help="Skip self-host regeneration")
    parser.add_argument(
        "--deploy-edge",
        action="store_true",
        help="After publish, rebuild tools/edge/dist with --preset cloudflare, "
        "wrangler deploy, and curl-smoke /, /api, /fractal. Requires "
        "wrangler on PATH (or npx + node>=22) and an authenticated "
        "Cloudflare account.",
    )
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

    # 5.5 Stamp the CHANGELOG header for this release with today's date.
    #     Done after verify so a failed verification doesn't dirty the
    #     working tree with a half-finished release commit.
    print("Stamping CHANGELOG release date...")
    stamp_changelog_release_date(new_version, dry_run)
    print()

    # 6. Publish
    if not args.skip_publish:
        publish(new_versions, old_versions, dry_run)
        print()

    # 7. Commit, tag, push, GitHub release
    git_commit_tag_push(new_version, dry_run)
    print()

    # 8. Optional: rebuild + deploy `tools/edge` (Cloudflare Workers
    #    Mandelbrot demo). Live deploy of a public endpoint, so opt-in.
    if args.deploy_edge:
        deploy_edge(dry_run)
        print()

    print(f"{'[dry-run] ' if dry_run else ''}Done! Released {new_version}")
    return 0


def deploy_edge(dry_run: bool) -> None:
    """Rebuild `tools/edge/dist/` from `tools/edge/app.av` with
    `--preset cloudflare`, `wrangler deploy`, then smoke-test the
    landing / `/api` / `/fractal` endpoints. Mismatched ABI between
    the wasm-gc emit and the worker.js stubs would silently break a
    deploy on prior releases — this step turns that into a
    fail-fast gate."""
    import urllib.request

    print("Deploying tools/edge to Cloudflare...")
    if dry_run:
        print("  [dry-run] would run: aver compile --preset cloudflare, wrangler deploy, curl smoke")
        return

    aver_bin = REPO_ROOT / "target" / "release" / "aver"
    edge_dir = REPO_ROOT / "tools" / "edge"
    dist_dir = edge_dir / "dist"

    print("  rebuild dist/ from app.av...")
    run([
        str(aver_bin), "compile",
        str(edge_dir / "app.av"),
        "--preset", "cloudflare",
        "--handler", "handler",
        "--module-root", str(edge_dir),
        "-o", str(dist_dir),
    ])

    print("  wrangler deploy...")
    # wrangler 4.x needs Node >=22; if the default node is too old, prefer
    # `~/.nvm/versions/node/v22.*/bin` if present. Fall back to PATH `wrangler`.
    env_path = os.environ.get("PATH", "")
    nvm_dir = Path.home() / ".nvm" / "versions" / "node"
    if nvm_dir.exists():
        v22 = sorted([p for p in nvm_dir.iterdir() if p.name.startswith("v22.")])
        if v22:
            env_path = f"{v22[-1] / 'bin'}:{env_path}"
    deploy_env = {**os.environ, "PATH": env_path}
    subprocess.run(
        ["npx", "wrangler@latest", "deploy"],
        cwd=dist_dir, env=deploy_env, check=True,
    )

    # Smoke. Endpoint URL comes from wrangler's deploy output, but for the
    # canonical demo it's the production hostname workers.dev assigns.
    # Hard-coded here matches what the worker.js / wrangler.toml shipped
    # in `tools/edge/dist/` declare.
    base = "https://aver-edge-gc-demo.jasisz.workers.dev"
    for path in ("/", "/api", "/fractal"):
        url = base + path
        with urllib.request.urlopen(url, timeout=10) as resp:
            if resp.status != 200:
                raise SystemExit(
                    f"edge smoke: {url} returned HTTP {resp.status} (expected 200)"
                )
            print(f"  {url} HTTP {resp.status} ({resp.headers.get('content-length', '?')} bytes)")


if __name__ == "__main__":
    raise SystemExit(main())
