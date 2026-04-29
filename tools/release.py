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


# Reverse dep map: which crates pin which (downstream pins upstream).
# Whenever a crate bumps, every downstream that pins it must bump too —
# otherwise its already-published version on crates.io still references
# the old upstream version, and resolving the new aver-lang against it
# fails (`required by package X, all possible versions conflict`).
DOWNSTREAM_PINS = {
    "aver-rt": ["aver-memory", "aver-lang"],
    "aver-memory": ["aver-lang"],
    "aver-lang": ["aver-lsp"],
}


def compute_new_versions(old_versions: dict[str, str], new_main: str) -> dict[str, str]:
    """Compute new versions for every crate.

    aver-lang gets the requested version. Other crates patch-bump if
    either (a) they have direct changes since last tag, or (b) any crate
    they pin (transitively) is bumping — because their published copy
    pins the old upstream version and would block resolution.
    """
    last_tag = subprocess.run(
        ["git", "describe", "--tags", "--abbrev=0"],
        cwd=REPO_ROOT, capture_output=True, text=True,
    ).stdout.strip()

    new = dict(old_versions)
    new["aver-lang"] = new_main

    # Pass 1: direct changes since last tag
    for crate in CRATE_ORDER:
        if crate == "aver-lang":
            continue
        result = subprocess.run(
            ["git", "log", f"{last_tag}..HEAD", "--oneline", "--", f"{VERSION_FILES[crate].parent}/"],
            cwd=REPO_ROOT, capture_output=True, text=True,
        )
        if result.stdout.strip():
            new[crate] = bump_patch(old_versions[crate])

    # Pass 2: cascade — any crate whose upstream bumps must also bump.
    # Iterate to fixpoint (chain length ≤ #crates).
    for _ in range(len(CRATE_ORDER)):
        changed = False
        for upstream, downstreams in DOWNSTREAM_PINS.items():
            if new[upstream] == old_versions[upstream]:
                continue
            for ds in downstreams:
                if new[ds] == old_versions[ds]:
                    new[ds] = bump_patch(old_versions[ds]) if ds != "aver-lang" else new_main
                    changed = True
        if not changed:
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
    cards = []
    for v in versions:
        ver = v["version"]
        is_latest = ver == versions[0]["version"]
        latest_pill = (
            '<span class="rt-pill">latest</span>' if is_latest else ""
        )
        cards.append(
            f"""
        <article class="rt-release">
            <header class="rt-release-head">
                <h3>v{ver}</h3>{latest_pill}
            </header>
            <ul class="rt-downloads">
                <li>
                    <a href="v{ver}/aver_runtime.wasm" class="rt-dl">
                        <span class="rt-dl-name">aver_runtime.wasm</span>
                        <span class="rt-dl-size">{v["runtime_bytes"]:,} B</span>
                    </a>
                </li>
                <li>
                    <a href="v{ver}/aver_to_wasi.wasm" class="rt-dl">
                        <span class="rt-dl-name">aver_to_wasi.wasm</span>
                        <span class="rt-dl-size">{v["bridge_bytes"]:,} B</span>
                    </a>
                </li>
            </ul>
            <p class="rt-extras">
                <a href="v{ver}/">browse</a>
                · <a href="v{ver}/aver_runtime.wat">.wat</a>
                · <a href="v{ver}/CHECKSUMS.txt">sha256</a>
                · <a href="v{ver}/README.md">README</a>
            </p>
        </article>"""
        )
    cards_html = (
        "".join(cards)
        if cards
        else '\n        <p class="rt-empty">No releases yet.</p>'
    )
    latest_ver = versions[0]["version"] if versions else "?"
    index_html = f"""<!doctype html>
<html lang="en">
<head>
    <meta charset="utf-8">
    <title>Aver runtime · averlang.dev</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="icon" type="image/svg+xml" href="../favicon.svg">
    <link rel="stylesheet" href="../style.css">
    <style>
        .rt-page {{
            max-width: 920px;
            margin: 0 auto;
            padding: 2.5rem 1.5rem 4rem;
        }}
        .rt-back {{
            font-size: 0.9rem;
            color: var(--muted);
            text-decoration: none;
            display: inline-block;
            margin-bottom: 1.5rem;
        }}
        .rt-back:hover {{ color: var(--accent); }}
        .rt-page h1 {{
            font-size: 2.25rem;
            letter-spacing: -0.02em;
            margin-bottom: 0.6rem;
        }}
        .rt-page h2 {{
            font-size: 1.4rem;
            letter-spacing: -0.01em;
            margin: 2.5rem 0 0.9rem;
            padding-top: 1.5rem;
            border-top: 1px solid var(--border);
        }}
        .rt-page h2:first-of-type {{
            border-top: none;
            padding-top: 0;
            margin-top: 2.5rem;
        }}
        .rt-subhead {{
            font-size: 1.05rem;
            font-weight: 650;
            letter-spacing: -0.005em;
            margin: 1.75rem 0 0.6rem;
            color: var(--accent);
        }}
        .rt-design {{
            margin: 1.75rem 0 0;
            padding: 1.15rem 1.35rem;
            background: var(--surface);
            border: 1px solid var(--border);
            border-left: 3px solid var(--accent);
            border-radius: 10px;
            color: var(--text);
            line-height: 1.65;
            font-size: 0.96rem;
        }}
        .rt-design strong {{
            display: block;
            font-size: 0.78rem;
            font-weight: 650;
            text-transform: uppercase;
            letter-spacing: 0.08em;
            color: var(--accent);
            margin-bottom: 0.5rem;
        }}
        .rt-design em {{
            font-style: normal;
            font-weight: 650;
            color: var(--text);
        }}
        .rt-lede {{
            color: var(--muted);
            font-size: 1.05rem;
            line-height: 1.6;
        }}
        .rt-lede code {{
            background: var(--inline-code-bg);
            color: var(--inline-code-text);
            border: 1px solid var(--inline-code-border);
            padding: 0.08rem 0.4rem;
            border-radius: 5px;
            font-family: var(--font-mono);
            font-size: 0.85rem;
        }}
        .rt-page p {{
            color: var(--muted);
            line-height: 1.6;
        }}
        .rt-page p code, .rt-list code {{
            background: var(--inline-code-bg);
            color: var(--inline-code-text);
            border: 1px solid var(--inline-code-border);
            padding: 0.08rem 0.4rem;
            border-radius: 5px;
            font-family: var(--font-mono);
            font-size: 0.85rem;
        }}
        .rt-page pre {{
            background: var(--code-surface);
            color: #e2e8f0;
            border: 1px solid var(--code-border);
            border-radius: 10px;
            padding: 1.1rem 1.25rem;
            overflow-x: auto;
            font-family: var(--font-mono);
            font-size: 0.86rem;
            line-height: 1.55;
            margin: 0.5rem 0;
        }}
        .rt-page pre .rt-comment {{ color: #94a3b8; }}
        .rt-page pre .rt-prompt {{ color: #f59e0b; }}
        .rt-releases {{
            display: grid;
            grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
            gap: 1rem;
            margin-top: 0.75rem;
        }}
        .rt-release {{
            background: var(--surface);
            border: 1px solid var(--border);
            border-radius: 12px;
            padding: 1.1rem 1.25rem;
            transition: border-color 0.18s, box-shadow 0.18s;
        }}
        .rt-release:hover {{
            border-color: var(--accent);
            box-shadow: 0 4px 14px rgba(15, 23, 42, 0.06);
        }}
        .rt-release-head {{
            display: flex;
            align-items: center;
            gap: 0.6rem;
            margin-bottom: 0.85rem;
        }}
        .rt-release-head h3 {{
            font-size: 1.1rem;
            color: var(--text);
            font-weight: 650;
            letter-spacing: -0.01em;
        }}
        .rt-pill {{
            background: var(--accent);
            color: var(--btn-primary-text);
            font-size: 0.7rem;
            font-weight: 650;
            text-transform: uppercase;
            letter-spacing: 0.06em;
            padding: 0.15rem 0.55rem;
            border-radius: 999px;
        }}
        .rt-downloads {{
            list-style: none;
            margin: 0 0 0.85rem;
            padding: 0;
        }}
        .rt-downloads li {{ margin-bottom: 0.35rem; }}
        .rt-dl {{
            display: flex;
            justify-content: space-between;
            align-items: center;
            gap: 0.75rem;
            padding: 0.5rem 0.75rem;
            background: var(--inline-code-bg);
            border: 1px solid var(--inline-code-border);
            border-radius: 8px;
            text-decoration: none;
            color: var(--text);
            font-family: var(--font-mono);
            font-size: 0.82rem;
            transition: border-color 0.18s, color 0.18s;
        }}
        .rt-dl:hover {{
            border-color: var(--accent);
            color: var(--accent);
        }}
        .rt-dl-size {{
            color: var(--muted);
            font-size: 0.75rem;
            white-space: nowrap;
        }}
        .rt-dl:hover .rt-dl-size {{ color: var(--accent); }}
        .rt-extras {{
            font-size: 0.8rem;
            color: var(--muted);
            margin: 0;
        }}
        .rt-extras a {{
            color: var(--muted);
            text-decoration: none;
            border-bottom: 1px dashed var(--border);
        }}
        .rt-extras a:hover {{ color: var(--accent); border-bottom-color: var(--accent); }}
        .rt-list {{
            list-style: none;
            padding: 0;
            margin: 0;
            display: grid;
            gap: 0.85rem;
        }}
        .rt-list li {{
            color: var(--muted);
            line-height: 1.55;
            font-size: 0.93rem;
        }}
        .rt-list strong {{
            color: var(--text);
            font-weight: 650;
        }}
        .rt-foot {{
            margin-top: 2.5rem;
            padding-top: 1.5rem;
            border-top: 1px solid var(--border);
            font-size: 0.85rem;
            color: var(--muted);
        }}
        .rt-foot a {{ color: var(--accent); text-decoration: none; }}
        .rt-foot a:hover {{ text-decoration: underline; }}
        .rt-empty {{
            color: var(--muted);
            font-style: italic;
        }}
        @media (max-width: 600px) {{
            .rt-page h1 {{ font-size: 1.75rem; }}
            .rt-page pre {{ font-size: 0.78rem; padding: 0.85rem 0.95rem; }}
        }}
    </style>
</head>
<body>
<main class="rt-page">
    <a class="rt-back" href="/">← averlang.dev</a>

    <h1>Aver runtime</h1>
    <p class="rt-lede">
        Standalone WebAssembly modules for the Aver language runtime.
        Pair them with a thin <code>user.wasm</code> built via
        <code>aver compile --target edge-wasm --optimize size</code> —
        the runtime is cached once and shared across every program.
    </p>
    <aside class="rt-design">
        <strong>The model in one sentence.</strong>
        Aver emits small, explicit WASM modules. The runtime is shared.
        Effects are host-provided. WASI is just one bridge.
        <em>Packaging</em> (bundled vs edge) and <em>effect host</em>
        (JS / WASI / something else) are independent choices —
        every combination ships.
    </aside>

    <h2>Quick start</h2>
    <p>
        A thin <code>user.wasm</code> imports two modules:
        <code>aver_runtime</code> (from this CDN) and <code>aver</code>
        (host effects you provide — see the next two sections for
        browser and standalone shapes).
    </p>
    <pre><span class="rt-prompt">$</span> aver compile mygame.av --target edge-wasm --optimize size -o dist/

<span class="rt-comment">// Three-step instantiate: runtime → host effects → user program</span>
const runtime = await WebAssembly.instantiateStreaming(
    fetch("https://averlang.dev/runtime/latest/aver_runtime.wasm"),
    {{}}
);
const user = await WebAssembly.instantiateStreaming(
    fetch("/dist/mygame.wasm"),
    {{
        aver_runtime: runtime.instance.exports,
        aver: makeHostImports(runtime.instance.exports.memory),
    }}
);
user.instance.exports._start();</pre>

    <h2>Wiring up effects</h2>
    <p>
        Every <code>user.wasm</code> imports <code>aver/*</code>
        effects (<code>Console.print</code>,
        <code>Time.unixMs</code>, …) — the host environment
        provides them. Three usual paths:
    </p>

    <h3 class="rt-subhead">JS host (browser, Node, Workers, Fastly Compute…)</h3>
    <p>
        Provide the <code>aver/*</code> imports as plain JS
        functions. Strings and other heap values arrive as
        <code>(ptr, len)</code> pairs into the runtime's linear
        memory — read them through a typed-array view of
        <code>runtime.exports.memory</code>.
    </p>
    <pre><span class="rt-comment">// Smallest viable shim — covers Console.* + Time + Random.</span>
<span class="rt-comment">// Add more entries as your program declares more effects.</span>
function makeHostImports(memory) {{
    const decoder = new TextDecoder("utf-8");
    const readString = (ptr, len) =&gt;
        decoder.decode(new Uint8Array(memory.buffer, ptr, len));

    return {{
        console_print: (ptr, len) =&gt; console.log(readString(ptr, len)),
        console_error: (ptr, len) =&gt; console.error(readString(ptr, len)),
        console_warn:  (ptr, len) =&gt; console.warn(readString(ptr, len)),
        time_unixMs:   () =&gt; BigInt(Date.now()),
        random_int:    (lo, hi) =&gt; {{
            const span = hi - lo + 1n;
            return lo + BigInt(Math.floor(Math.random() * Number(span)));
        }},
    }};
}}</pre>

    <h3 class="rt-subhead">WASI host (wasmtime, wasmer, …)</h3>
    <p>
        On runtimes without JS, use the prebuilt
        <code>aver_to_wasi.wasm</code> shim — it satisfies
        <code>aver/*</code> imports against
        <code>wasi_snapshot_preview1.fd_write</code>. The CLI's
        <code>--bridge wasip1</code> bundles it for you in one step:
    </p>
    <pre><span class="rt-prompt">$</span> aver compile mygame.av --target wasm --bridge wasip1 --optimize size -o dist/
<span class="rt-prompt">$</span> wasmtime dist/mygame.wasm
<span class="rt-comment"># No other host wiring needed.</span></pre>
    <p>
        Coverage today: <code>Console.*</code> and
        <code>Format.value</code> / <code>Print.value</code>. Other
        <code>aver/*</code> effects (<code>Disk.*</code>,
        <code>Tcp.*</code>, etc.) trap as <code>unreachable</code>
        in the bridge — wire them through the host directly until
        coverage lands.
    </p>

    <h3 class="rt-subhead">Mixing edge packaging with any host</h3>
    <p>
        The artifacts on this page are the building blocks the CLI
        bundles internally. Publish them once at the URLs below and
        every <code>--target edge-wasm</code> binary you ship can
        share that runtime — paired with whichever effect host fits
        the deployment (browser shim above, the WASI bridge,
        Cloudflare Workers' JS bindings, anything else).
    </p>

    <h2>Releases</h2>
    <p>
        Stable URL for the most recent release:
        <code>/runtime/latest/aver_runtime.wasm</code>.
        Per-version URLs below are immutable — pin to one for
        reproducible deployments.
    </p>
    <div class="rt-releases">{cards_html}
    </div>

    <h2>What's in each release</h2>
    <ul class="rt-list">
        <li><strong>aver_runtime.wasm</strong> — alloc, GC, hashmap (HAMT),
            string / list / vector ops. Imported as <code>aver_runtime</code>
            by every <code>edge-wasm</code> binary.</li>
        <li><strong>aver_to_wasi.wasm</strong> — translation shim that
            satisfies a program's <code>aver/*</code> host imports
            against <code>wasi_snapshot_preview1.fd_write</code>.
            Optional — needed only when running through a WASI host
            such as wasmtime or wasmer. Browser and edge JS hosts
            (Cloudflare Workers, Fastly Compute, Node, Deno) provide
            <code>aver/*</code> directly; they don't need this shim.</li>
        <li><strong>.wat</strong> — human-readable disassembly of each
            <code>.wasm</code>. Inspect what's actually in the binary.</li>
        <li><strong>CHECKSUMS.txt</strong> — sha256 sums for every file
            in the directory.</li>
    </ul>

    <p class="rt-foot">
        Latest: <strong>v{latest_ver}</strong>.
        Programmatic discovery: <a href="manifest.json">manifest.json</a>.
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
