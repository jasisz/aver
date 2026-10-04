#!/usr/bin/env python3
"""Proof-level ratchet: what closes each law must not silently change.

Every corpus file that declares a law (`examples/`, `proof-corpus/`,
`projects/*`) is proved, and per file the baseline `tools/steps-baseline.json`
records two levels:

- `steps`: laws closed by proof steps. Measured by default with
  `aver proof --backend aver`, where the kernel written in Aver checks the
  steps in process, so this needs no Lean and runs on every pull request.
- `tactic`: laws Lean closes with tactics and not with steps. Measured only
  with `--lean`, which runs `aver proof --check-json` per file and needs Lean;
  it also re-measures `steps` under Lean, which must agree with the default.

A law that leaves a level it is recorded at fails and is named: a law that
reopens, and a law that falls from steps back to tactics. Withdrawing a tactic
portfolio that steps replaced is therefore a visible baseline change. A gain
also fails, with the command that records it, so an intended improvement lands
with its baseline update:

    python3 tools/steps_ratchet.py --update          # the steps level
    python3 tools/steps_ratchet.py --lean --update   # both levels

A deliberate loss needs `--allow-drop` as well, which a reviewer sees as a
baseline diff that removes lines. Every run prints the totals.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import re
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_BASELINE = REPO_ROOT / "tools" / "steps-baseline.json"
CORPUS_DIRS = ["examples", "proof-corpus", "projects"]
LAW = re.compile(r"^verify \S+ law ", re.M)
LEVELS = ("steps", "tactic")

# Files whose modules resolve from a root other than their own directory.
MODULE_ROOTS = {
    "examples/refinement/natural_app.av": "examples",
}


def corpus(repo_root: Path) -> list[str]:
    """Every corpus file that declares a law, as a path from the repository root."""
    found = []
    for top in CORPUS_DIRS:
        for path in (repo_root / top).rglob("*.av"):
            if LAW.search(path.read_text(encoding="utf-8")):
                found.append(path.relative_to(repo_root).as_posix())
    return sorted(found)


def module_root(entry: str) -> str:
    """A project resolves from its own directory; any other file from its directory."""
    if entry in MODULE_ROOTS:
        return MODULE_ROOTS[entry]
    parts = entry.split("/")
    if parts[0] == "projects":
        return "/".join(parts[:2])
    return "/".join(parts[:-1])


def summarize(report: dict) -> dict:
    """The part of a `--check-json` report the ratchet holds."""
    closed_by = report.get("closed_by", {})
    by = lambda level: sorted(law for law, how in closed_by.items() if how == level)
    return {"steps": by("steps"), "tactic": by("tactic"), "laws": len(closed_by)}


def measure(aver: Path, entry: str, lean: bool) -> dict:
    with tempfile.TemporaryDirectory(prefix="aver-steps-ratchet-") as out:
        cmd = [str(aver), "proof", entry, "--module-root", module_root(entry)]
        if not lean:
            cmd += ["--backend", "aver"]
        cmd += ["-o", out, "--check-json", "--sorry-budget", "1000000", "--declined-budget", "1000000"]
        run = subprocess.run(cmd, cwd=REPO_ROOT, capture_output=True, text=True)
    lines = [line for line in run.stdout.splitlines() if line.startswith("{")]
    if not lines:
        raise RuntimeError(f"{entry}: {' '.join(cmd[1:])} failed\n{run.stdout}{run.stderr}")
    report = json.loads(lines[-1])
    if report.get("steps_rejected"):
        raise RuntimeError(f"{entry}: a step proof was refused: {', '.join(report['steps_rejected'])}")
    return summarize(report)


def compare(baseline: dict, current: dict, levels: tuple[str, ...]) -> tuple[list[str], list[str]]:
    """Return (drops, gains) as human-readable lines, file by file, for the levels measured."""
    drops: list[str] = []
    gains: list[str] = []
    for entry in sorted(set(baseline) | set(current)):
        old = baseline.get(entry, {})
        new = current.get(entry)
        if new is None:
            if any(old.get(level) for level in levels):
                drops.append(f"{entry}: no longer in the corpus")
            continue
        for level in levels:
            before, after = set(old.get(level, [])), set(new[level])
            for law in sorted(before - after):
                now = next((other for other in LEVELS if law in new[other]), None)
                where = f"is closed by {now} now" if now else "no longer closes"
                drops.append(f"{entry}: `{law}` {where} (was {level})")
            gains += [f"{entry}: `{law}` newly closes by {level}" for law in sorted(after - before)]
    return drops, gains


def recorded(baseline: dict, current: dict, levels: tuple[str, ...], full: bool) -> dict:
    """The baseline with the measured levels replaced and files with nothing
    left out. After a full run a file no longer in the corpus goes too."""
    out: dict = {}
    for entry in sorted(set(baseline) | set(current)):
        if entry not in current and full:
            continue
        row = {level: list(baseline.get(entry, {}).get(level, [])) for level in LEVELS}
        if entry in current:
            for level in levels:
                row[level] = current[entry][level]
        row = {level: laws for level, laws in row.items() if laws}
        if row:
            out[entry] = row
    return out


def main(argv: list[str]) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--aver", type=Path, default=REPO_ROOT / "target" / "debug" / "aver")
    parser.add_argument("--baseline", type=Path, default=DEFAULT_BASELINE)
    parser.add_argument("--lean", action="store_true", help="measure with Lean: both levels")
    parser.add_argument("--update", action="store_true", help="record the current state")
    parser.add_argument("--allow-drop", action="store_true", help="with --update: accept losses")
    parser.add_argument("--jobs", type=int, default=4)
    parser.add_argument("--only", action="append", default=[], help="measure only this corpus file")
    args = parser.parse_args(argv)
    aver = args.aver.resolve()
    if not aver.is_file():
        print(f"{aver}: no aver binary; build it with `cargo build --bin aver`", file=sys.stderr)
        return 2
    levels = LEVELS if args.lean else ("steps",)

    entries = [e for e in corpus(REPO_ROOT) if not args.only or e in args.only]
    current: dict = {}
    errors: list[str] = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {pool.submit(measure, aver, entry, args.lean): entry for entry in entries}
        for future in concurrent.futures.as_completed(futures):
            try:
                current[futures[future]] = future.result()
            except RuntimeError as error:
                errors.append(str(error))
    if errors:
        print("\n\n".join(sorted(errors)), file=sys.stderr)
        return 2

    laws = sum(s["laws"] for s in current.values())
    counts = ", ".join(f"{sum(len(s[level]) for s in current.values())} by {level}" for level in levels)
    print(f"{len(current)} files, {laws} laws: {counts}")

    baseline = json.loads(args.baseline.read_text()) if args.baseline.exists() else {}
    scope = {e: baseline[e] for e in baseline if not args.only or e in args.only}
    drops, gains = compare(scope, current, levels)

    if args.update:
        if drops and not args.allow_drop:
            print("refusing to record losses without --allow-drop:", file=sys.stderr)
            print("\n".join(drops), file=sys.stderr)
            return 1
        merged = recorded(baseline, current, levels, full=not args.only)
        args.baseline.write_text(json.dumps(merged, indent=1, sort_keys=True) + "\n")
        print(f"recorded {len(merged)} files in {args.baseline}")
        return 0

    if drops:
        print("laws that left a level they are recorded at in tools/steps-baseline.json:", file=sys.stderr)
        print("\n".join(drops), file=sys.stderr)
    if gains:
        update = "python3 tools/steps_ratchet.py" + (" --lean" if args.lean else "") + " --update"
        print(f"laws newly closed; record them in this commit with `{update}`:", file=sys.stderr)
        print("\n".join(gains), file=sys.stderr)
    return 1 if drops or gains else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
