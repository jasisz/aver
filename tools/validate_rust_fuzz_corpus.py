#!/usr/bin/env python3
"""Replay a bounded AFL corpus through `aver compile --target rust --check`.

The AFL target keeps rustc out of its hot loop. This companion selects inputs
deterministically, skips programs rejected before codegen, and asks the real
Rust toolchain about up to ``--limit`` accepted programs. All checks share one
Cargo target directory so the runtime dependency graph is paid for once.
"""

from __future__ import annotations

import argparse
import hashlib
import os
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path


MULTIMODULE_MAGIC = bytes((0xA7, 0xE2, 0x40, 0x01))


@dataclass(frozen=True)
class DecodedInput:
    entry: Path
    module_root: Path


def decode_input(data: bytes, destination: Path) -> DecodedInput | None:
    """Materialize one raw or multi-module fuzz input under ``destination``."""
    destination.mkdir(parents=True, exist_ok=True)
    if not data.startswith(MULTIMODULE_MAGIC):
        try:
            source = data.decode("utf-8")
        except UnicodeDecodeError:
            return None
        entry = destination / "main.av"
        entry.write_text(source)
        return DecodedInput(entry=entry, module_root=destination)

    position = len(MULTIMODULE_MAGIC)
    if position >= len(data):
        return None
    file_count = data[position]
    position += 1
    if not 1 <= file_count <= 4:
        return None

    paths: list[Path] = []
    for _ in range(file_count):
        if position >= len(data):
            return None
        name_length = data[position]
        position += 1
        if not 1 <= name_length <= 20 or position + name_length > len(data):
            return None
        name_bytes = data[position : position + name_length]
        position += name_length
        if (
            not all(
                ord("0") <= byte <= ord("9")
                or ord("A") <= byte <= ord("Z")
                or ord("a") <= byte <= ord("z")
                or byte == ord(".")
                for byte in name_bytes
            )
            or name_bytes[0] == ord(".")
            or name_bytes[-1] == ord(".")
        ):
            return None
        try:
            name = name_bytes.decode("ascii").lower()
        except UnicodeDecodeError:
            return None

        if position + 2 > len(data):
            return None
        body_length = int.from_bytes(data[position : position + 2], "little")
        position += 2
        if position + body_length > len(data):
            return None
        try:
            body = data[position : position + body_length].decode("utf-8")
        except UnicodeDecodeError:
            return None
        position += body_length

        parts = name.split(".")
        path = destination.joinpath(*parts[:-1], f"{parts[-1]}.av")
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(body)
        paths.append(path)

    if position >= len(data):
        return None
    entry_index = data[position]
    if entry_index >= len(paths):
        return None
    return DecodedInput(entry=paths[entry_index], module_root=destination)


def stable_candidates(corpus_dirs: list[Path], maximum: int) -> list[tuple[str, bytes]]:
    """Return stable, deduplicated candidates with multi-module seeds first."""
    by_digest: dict[str, tuple[int, bytes]] = {}
    for directory_index, directory in enumerate(corpus_dirs):
        if not directory.is_dir():
            continue
        for path in sorted(directory.iterdir()):
            if not path.is_file() or path.name.startswith("README"):
                continue
            data = path.read_bytes()
            digest = hashlib.sha256(data).hexdigest()
            by_digest.setdefault(digest, (directory_index, data))
    ordered = sorted(
        by_digest,
        key=lambda digest: (
            not by_digest[digest][1].startswith(MULTIMODULE_MAGIC),
            by_digest[digest][0],
            digest,
        ),
    )
    return [(digest, by_digest[digest][1]) for digest in ordered[:maximum]]


def command_output(result: subprocess.CompletedProcess[str]) -> str:
    return (
        f"status: {result.returncode}\n"
        f"stdout:\n{result.stdout}\n"
        f"stderr:\n{result.stderr}"
    )


def run(args: argparse.Namespace) -> int:
    aver = args.aver.resolve()
    candidates = stable_candidates(args.corpus, args.max_candidates)
    if not candidates:
        raise SystemExit("Rust fuzz oracle found no corpus inputs")

    checked = 0
    multimodule_checked = 0
    rejected = 0
    has_multimodule_candidate = any(
        data.startswith(MULTIMODULE_MAGIC) for _, data in candidates
    )
    with tempfile.TemporaryDirectory(prefix="aver-rust-fuzz-oracle-") as temp_text:
        temp = Path(temp_text)
        cargo_target = args.target_dir.resolve() if args.target_dir else temp / "cargo-target"
        cargo_target.mkdir(parents=True, exist_ok=True)
        env = os.environ.copy()
        env["CARGO_TARGET_DIR"] = str(cargo_target)
        # The oracle is about rustc, not a developer-machine compiler cache.
        # A stale or sandboxed wrapper can otherwise turn a valid emitted
        # crate into an infrastructure failure before rustc starts.
        env["RUSTC_WRAPPER"] = ""
        env["RUSTC_WORKSPACE_WRAPPER"] = ""
        env["CARGO_BUILD_RUSTC_WRAPPER"] = ""
        # The workflow builds Aver immediately before this step, so the
        # complete runtime graph is already cached. Keep corpus validity
        # independent of registry availability during the oracle pass.
        env["CARGO_NET_OFFLINE"] = "true"

        for digest, data in candidates:
            case = decode_input(data, temp / f"case-{digest[:16]}" / "source")
            if case is None:
                rejected += 1
                continue

            output = temp / f"case-{digest[:16]}" / "generated"
            compile_check = subprocess.run(
                [
                    str(aver),
                    "compile",
                    str(case.entry),
                    "--module-root",
                    str(case.module_root),
                    "--target",
                    "rust",
                    "--check",
                    "--name",
                    f"fuzz_oracle_{digest[:16]}",
                    "-o",
                    str(output),
                ],
                text=True,
                capture_output=True,
                check=False,
                env=env,
            )
            if compile_check.returncode != 0:
                # Parse/type errors and target refusals happen before a
                # project exists; they are ordinary invalid fuzz inputs. A
                # failure after materialisation is exactly the gap this
                # oracle exists to expose (emitter refusal or rustc error).
                if not (output / "Cargo.toml").is_file():
                    rejected += 1
                    continue
                print(
                    f"Rust fuzz oracle failed for sha256:{digest}\n"
                    f"{command_output(compile_check)}"
                )
                return 1
            checked += 1
            if data.startswith(MULTIMODULE_MAGIC):
                multimodule_checked += 1
            if checked >= args.limit:
                break

    if checked == 0:
        raise SystemExit(
            f"Rust fuzz oracle checked no type-correct inputs ({rejected} rejected)"
        )
    if has_multimodule_candidate and multimodule_checked == 0:
        raise SystemExit(
            "Rust fuzz oracle had multi-module seeds but none reached compile --check"
        )
    print(
        f"Rust fuzz oracle: {checked} emitted project(s) passed --check; "
        f"{multimodule_checked} multi-module; "
        f"{rejected} candidate(s) rejected before codegen"
    )
    return 0


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--aver", type=Path, required=True, help="path to the aver binary")
    parser.add_argument(
        "--corpus",
        type=Path,
        action="append",
        required=True,
        help="AFL queue or fallback corpus directory; repeatable",
    )
    parser.add_argument("--limit", type=int, default=24, help="maximum accepted programs")
    parser.add_argument(
        "--max-candidates",
        type=int,
        default=256,
        help="maximum raw inputs considered before filtering",
    )
    parser.add_argument(
        "--target-dir",
        type=Path,
        help="shared CARGO_TARGET_DIR (temporary when omitted)",
    )
    args = parser.parse_args()
    if args.limit < 1 or args.max_candidates < args.limit:
        parser.error("require 1 <= --limit <= --max-candidates")
    return args


if __name__ == "__main__":
    raise SystemExit(run(parse_args()))
