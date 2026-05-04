#!/usr/bin/env python3

from __future__ import annotations

import argparse
import os
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
EXAMPLES_ROOT = REPO_ROOT / "examples" / "games"
PLAYGROUND_ROOT = REPO_ROOT / "tools" / "website" / "playground"
PLAYGROUND_SOURCES_ROOT = PLAYGROUND_ROOT / "sources" / "examples" / "games"
WEBSITE_INDEX = REPO_ROOT / "tools" / "website" / "index.html"
PLAYGROUND_INDEX = PLAYGROUND_ROOT / "index.html"


@dataclass(frozen=True)
class Game:
    slug: str
    source: str
    module_root: str | None = None


GAMES = [
    Game("life", "life.av"),
    Game("snake", "snake.av"),
    Game("tetris", "tetris/main.av", "tetris"),
    Game("checkers", "checkers/main.av", "checkers"),
    Game("rogue", "rogue/main.av", "rogue"),
    Game("doom", "doom/main.av", "doom"),
    Game("wumpus", "wumpus.av"),
]

DIR_MIRRORS = ["checkers", "doom", "rogue", "tetris"]
FILE_MIRRORS = ["life.av", "snake.av", "wumpus.av"]


def resolve_executable(candidate: str) -> str:
    if "/" in candidate:
        path = Path(candidate).expanduser()
        if path.exists() and os.access(path, os.X_OK):
            return str(path)
        raise SystemExit(f"Executable not found or not runnable: {path}")

    resolved = shutil.which(candidate)
    if resolved:
        return resolved
    raise SystemExit(f"Executable not found on PATH: {candidate}")


def run(cmd: list[str]) -> None:
    result = subprocess.run(cmd, cwd=REPO_ROOT, text=True, capture_output=True)
    if result.returncode == 0:
        return

    if result.stdout:
        sys.stderr.write(result.stdout)
    if result.stderr:
        sys.stderr.write(result.stderr)

    if "WASM target requires --features wasm" in result.stderr:
        raise SystemExit(
            "The selected aver binary does not have WASM support. "
            "Rebuild it first with: cargo build --features wasm"
        )

    raise SystemExit(f"Command failed: {' '.join(cmd)}")


def format_kib(size_bytes: int) -> str:
    return f"{size_bytes / 1024:.1f} KiB"


def sync_sources() -> None:
    PLAYGROUND_SOURCES_ROOT.mkdir(parents=True, exist_ok=True)

    for dirname in DIR_MIRRORS:
        src_dir = EXAMPLES_ROOT / dirname
        dst_dir = PLAYGROUND_SOURCES_ROOT / dirname
        if dst_dir.exists():
            shutil.rmtree(dst_dir)
        dst_dir.mkdir(parents=True, exist_ok=True)
        for src_file in sorted(src_dir.glob("*.av")):
            shutil.copy2(src_file, dst_dir / src_file.name)

    for filename in FILE_MIRRORS:
        shutil.copy2(EXAMPLES_ROOT / filename, PLAYGROUND_SOURCES_ROOT / filename)


WASM_COMPILER_DST = PLAYGROUND_ROOT / "wasm"


def build_compiler() -> None:
    """Rebuild the Aver-to-WASM compiler itself (aver_bg.wasm) via wasm-pack,
    then shrink with wasm-opt -Oz."""
    if shutil.which("wasm-pack") is None:
        raise SystemExit("`wasm-pack` not found. Install it: cargo install wasm-pack")
    if shutil.which("wasm-opt") is None:
        raise SystemExit(
            "`wasm-opt` not found on PATH. Install binaryen before rebuilding the compiler."
        )

    print("Building playground compiler (wasm-pack) ...")
    result = subprocess.run(
        [
            "wasm-pack",
            "build",
            "--target",
            "web",
            "--features",
            "playground",
            "--no-default-features",
        ],
        cwd=REPO_ROOT,
        text=True,
        capture_output=True,
    )
    if result.returncode != 0:
        if result.stderr:
            sys.stderr.write(result.stderr)
        raise SystemExit("wasm-pack build failed")

    pkg_dir = REPO_ROOT / "pkg"
    shutil.copy2(pkg_dir / "aver.js", WASM_COMPILER_DST / "aver.js")

    raw_wasm = pkg_dir / "aver_bg.wasm"
    optimized = WASM_COMPILER_DST / "aver_bg.wasm"
    raw_size = raw_wasm.stat().st_size
    print(f"  aver_bg.wasm (wasm-pack): {format_kib(raw_size)}")

    opt_result = subprocess.run(
        ["wasm-opt", "-Oz", str(raw_wasm), "-o", str(optimized)],
        text=True,
        capture_output=True,
    )
    if opt_result.returncode != 0:
        if opt_result.stderr:
            sys.stderr.write(opt_result.stderr)
        raise SystemExit("wasm-opt -Oz failed on compiler wasm")

    opt_size = optimized.stat().st_size
    ratio = 100.0 * (raw_size - opt_size) / raw_size if raw_size else 0.0
    print(f"  aver_bg.wasm (wasm-opt -Oz): {format_kib(opt_size)} (-{ratio:.1f}%)")


def build_wasm(aver_bin: str) -> None:
    if shutil.which("wasm-opt") is None:
        raise SystemExit("`wasm-opt` not found on PATH. Install binaryen before rebuilding playground WASM.")

    for game in GAMES:
        source = PLAYGROUND_SOURCES_ROOT / game.source
        cmd = [
            aver_bin,
            "compile",
            str(source),
            "--target",
            "wasm-gc",
            "--optimize",
            "size",
            "--name",
            game.slug,
            "-o",
            str(PLAYGROUND_ROOT),
        ]
        if game.module_root:
            cmd.extend(["--module-root", str(PLAYGROUND_SOURCES_ROOT / game.module_root)])
        run(cmd)


def collect_sizes() -> dict[str, str]:
    sizes: dict[str, str] = {}
    for game in GAMES:
        wasm_path = PLAYGROUND_ROOT / f"{game.slug}.wasm"
        sizes[game.slug] = format_kib(wasm_path.stat().st_size)
    return sizes


def replace_once(pattern: str, replacement: str, text: str, *, flags: int = 0) -> str:
    updated, count = re.subn(pattern, replacement, text, count=1, flags=flags)
    if count != 1:
        raise SystemExit(f"Expected to replace exactly once for pattern: {pattern}")
    return updated


def update_main_index(text: str, sizes: dict[str, str]) -> str:
    summary = (
        "Seven games compiled directly from Aver to WebAssembly GC. "
        "Engine handles GC and tail calls — no NaN-boxing, no custom heap. "
        f"Snake ships at {sizes['snake']}; a full roguelike with "
        f"procedural generation is {sizes['rogue']}. "
        "Modern browsers only (Chrome 119+ / Firefox 120+ / Safari 18.2+)."
    )
    text = replace_once(
        r'(<section class="games" id="demo">.*?<p class="section-sub">)(.*?)(</p>)',
        rf"\1{summary}\3",
        text,
        flags=re.S,
    )

    for game in GAMES:
        pattern = rf'(<a href="playground/\?game={re.escape(game.slug)}" class="game-card">.*?<small>)([^<]+)(</small>)'
        text = replace_once(pattern, rf"\g<1>{sizes[game.slug]}\g<3>", text, flags=re.S)

    return text


def update_playground_index(text: str, sizes: dict[str, str]) -> str:
    tiny_binaries = (
        f"Snake ships at {sizes['snake']}. "
        f"Tetris is {sizes['tetris']}. "
        f"A full roguelike with procedural generation is {sizes['rogue']}. "
        "Built with <code>--target wasm-gc --optimize size</code> — engine GC + native tail-calls; per-program binary, no shared runtime to fetch."
    )

    for game in GAMES:
        pattern = rf'(<button data-game="{re.escape(game.slug)}"[^>]*>.*?<small>)([^<]+)(</small>)'
        text = replace_once(pattern, rf"\g<1>{sizes[game.slug]}\g<3>", text, flags=re.S)

    text = replace_once(
        r'(<strong>Tiny binaries</strong>\s*<span>)(.*?)(</span>)',
        rf"\1{tiny_binaries}\3",
        text,
        flags=re.S,
    )
    return text


def update_website_copy(sizes: dict[str, str]) -> None:
    main_index = WEBSITE_INDEX.read_text()
    WEBSITE_INDEX.write_text(update_main_index(main_index, sizes))

    playground_index = PLAYGROUND_INDEX.read_text()
    PLAYGROUND_INDEX.write_text(update_playground_index(playground_index, sizes))


def print_report(sizes: dict[str, str]) -> None:
    print("Playground WASM sizes:")
    for game in GAMES:
        wasm_path = PLAYGROUND_ROOT / f"{game.slug}.wasm"
        print(f"  {game.slug:<8} {wasm_path.stat().st_size:>6} B  {sizes[game.slug]}")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description=(
            "Rebuild the playground: compiler (wasm-pack), game WASM (aver compile "
            "--target wasm-gc --optimize size), mirrored sources, and website size labels."
        )
    )
    parser.add_argument(
        "--aver-bin",
        default=os.environ.get("AVER_BIN", str(REPO_ROOT / "target" / "debug" / "aver")),
        help="Path to the aver binary built with --features wasm",
    )
    parser.add_argument(
        "--skip-source-sync",
        action="store_true",
        help="Do not copy sources from examples/games into playground/sources",
    )
    parser.add_argument(
        "--skip-compiler",
        action="store_true",
        help="Do not rebuild the aver_bg.wasm playground compiler via wasm-pack",
    )
    parser.add_argument(
        "--skip-build",
        action="store_true",
        help="Do not rebuild playground game .wasm artifacts",
    )
    parser.add_argument(
        "--skip-html",
        action="store_true",
        help="Do not refresh size labels in tools/website/index.html and playground/index.html",
    )
    return parser.parse_args()


def main() -> int:
    args = parse_args()

    if not args.skip_compiler:
        build_compiler()

    if not args.skip_source_sync:
        sync_sources()

    if not args.skip_build:
        aver_bin = resolve_executable(args.aver_bin)
        build_wasm(aver_bin)

    sizes = collect_sizes()
    if not args.skip_html:
        update_website_copy(sizes)

    print_report(sizes)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
