#!/usr/bin/env python3
"""Certificate coupling ratchet: the emitter must not silently lose certificates.

Compiles the certificate corpus with `aver compile --target wasm-gc --certify`
and reads, per program, what the producer offers in `cert/cert-manifest.json`:
the certified exports (a function is offered only when its printed plan types
and its Rust-side lowering equals its emitted code entry byte for byte), the
plan-equals-source bridges and the law-claims. `--certify` writes the package
without building it, so no Lean is needed.

The result is compared with the committed baseline `tools/cert-baseline.json`:

- anything in the baseline that is gone (a function, a level L3 -> L1, a
  bridge, a law-claim) fails and is named;
- anything new also fails, with the command that records it, so an intended
  improvement lands together with its baseline update:

      python3 tools/cert_ratchet.py --update

- a deliberate loss needs `--update --allow-drop`, which a reviewer sees as a
  baseline diff that removes lines.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_BASELINE = REPO_ROOT / "tools" / "cert-baseline.json"

# Programs outside the certkit fixtures directory: (entry, module root).
PROJECTS = [
    ("examples/data/json.av", None),
    ("projects/k5_fdiv/main.av", "projects/k5_fdiv"),
    ("projects/payment_ops/main.av", "projects/payment_ops"),
]


def corpus(repo_root: Path) -> list[tuple[str, str | None]]:
    fixtures = sorted(
        (f"tools/certkit/fixtures/{p.name}", None)
        for p in (repo_root / "tools" / "certkit" / "fixtures").glob("*.av")
    )
    return fixtures + PROJECTS


def summarize(manifest: dict) -> dict:
    """The part of a manifest the ratchet holds: stable names, no indices."""
    return {
        "certified": {
            entry["name"]: entry.get("level", "")
            for entry in manifest.get("certified", [])
        },
        "bridges": sorted(b["export"] for b in manifest.get("sourceBridges", [])),
        "laws": sorted(law["label"] for law in manifest.get("laws", [])),
    }


def measure(aver: Path, entry: str, module_root: str | None) -> dict:
    with tempfile.TemporaryDirectory(prefix="aver-ratchet-") as out:
        cmd = [str(aver), "compile", entry]
        if module_root:
            cmd += ["--module-root", module_root]
        cmd += ["--target", "wasm-gc", "--certify", "-o", out]
        run = subprocess.run(cmd, cwd=REPO_ROOT, capture_output=True, text=True)
        if run.returncode != 0:
            raise RuntimeError(f"{entry}: compile --certify failed\n{run.stdout}{run.stderr}")
        manifest = json.loads((Path(out) / "cert" / "cert-manifest.json").read_text())
    return summarize(manifest)


def compare(baseline: dict, current: dict) -> tuple[list[str], list[str]]:
    """Return (drops, gains) as human-readable lines, program by program."""
    drops: list[str] = []
    gains: list[str] = []
    for program in sorted(set(baseline) | set(current)):
        old = baseline.get(program)
        new = current.get(program)
        if old is None:
            gains.append(f"{program}: new program in the corpus")
            continue
        if new is None:
            drops.append(f"{program}: no longer in the corpus")
            continue
        old_cert, new_cert = old["certified"], new["certified"]
        for name in sorted(set(old_cert) - set(new_cert)):
            drops.append(f"{program}: `{name}` is no longer certified")
        for name in sorted(set(new_cert) - set(old_cert)):
            gains.append(f"{program}: `{name}` is newly certified ({new_cert[name]})")
        for name in sorted(set(old_cert) & set(new_cert)):
            if old_cert[name] != new_cert[name]:
                line = f"{program}: `{name}` level {old_cert[name]} -> {new_cert[name]}"
                (drops if old_cert[name] == "L3" else gains).append(line)
        for key, what in (("bridges", "bridge"), ("laws", "law-claim")):
            before, after = set(old[key]), set(new[key])
            drops += [f"{program}: {what} `{n}` is gone" for n in sorted(before - after)]
            gains += [f"{program}: {what} `{n}` is new" for n in sorted(after - before)]
    return drops, gains


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--aver", type=Path, default=REPO_ROOT / "target" / "debug" / "aver")
    parser.add_argument("--baseline", type=Path, default=DEFAULT_BASELINE)
    parser.add_argument("--update", action="store_true", help="record the current state")
    parser.add_argument("--allow-drop", action="store_true", help="with --update: accept losses")
    parser.add_argument("--jobs", type=int, default=4)
    args = parser.parse_args(argv)
    aver = args.aver.resolve()
    if not aver.is_file():
        print(f"{aver}: no aver binary; build it with `cargo build --bin aver --features wasm`", file=sys.stderr)
        return 2

    programs = corpus(REPO_ROOT)
    current: dict = {}
    errors: list[str] = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {pool.submit(measure, aver, e, r): e for e, r in programs}
        for future in concurrent.futures.as_completed(futures):
            try:
                current[futures[future]] = future.result()
            except RuntimeError as error:
                errors.append(str(error))
    if errors:
        print("\n\n".join(sorted(errors)), file=sys.stderr)
        return 2

    baseline = json.loads(args.baseline.read_text()) if args.baseline.exists() else {}
    drops, gains = compare(baseline, current)

    if args.update:
        if drops and not args.allow_drop:
            print("refusing to record losses without --allow-drop:", file=sys.stderr)
            print("\n".join(drops), file=sys.stderr)
            return 1
        args.baseline.write_text(json.dumps(current, indent=1, sort_keys=True) + "\n")
        print(f"recorded {len(current)} programs in {args.baseline}")
        return 0

    total = sum(len(p["certified"]) for p in current.values())
    print(f"{len(current)} programs, {total} certified functions")
    if drops:
        print("certificates lost against tools/cert-baseline.json:", file=sys.stderr)
        print("\n".join(drops), file=sys.stderr)
    if gains:
        print(
            "certificates gained; record them in this commit with "
            "`python3 tools/cert_ratchet.py --update`:",
            file=sys.stderr,
        )
        print("\n".join(gains), file=sys.stderr)
    return 1 if drops or gains else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
