#!/usr/bin/env python3
"""Proof-step coverage ratchet: laws closed by steps must not silently reopen.

Runs `aver proof --backend aver` on every corpus file that declares a law
(`examples/`, `proof-corpus/`, `projects/*`): the step producers write each
law's proof as data and the proof kernel written in Aver checks it in
process, so no Lean is needed. Per file it records which laws close by steps.

The result is compared with the committed baseline `tools/steps-baseline.json`:

- a law in the baseline that no longer closes by steps fails and is named;
- a newly closed law also fails, with the command that records it, so an
  intended improvement lands together with its baseline update:

      python3 tools/steps_ratchet.py --update

- a deliberate loss needs `--update --allow-drop`, which a reviewer sees as a
  baseline diff that removes lines.

Every run prints the totals: laws closed by steps out of laws declared.
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
    return {
        "closed": sorted(law for law, by in closed_by.items() if by == "steps"),
        "laws": len(closed_by),
    }


def measure(aver: Path, entry: str) -> dict:
    with tempfile.TemporaryDirectory(prefix="aver-steps-ratchet-") as out:
        cmd = [
            str(aver), "proof", entry, "--module-root", module_root(entry),
            "--backend", "aver", "-o", out, "--check-json", "--sorry-budget", "1000000",
        ]
        run = subprocess.run(cmd, cwd=REPO_ROOT, capture_output=True, text=True)
    lines = [line for line in run.stdout.splitlines() if line.startswith("{")]
    if run.returncode != 0 or not lines:
        raise RuntimeError(f"{entry}: {' '.join(cmd[1:])} failed\n{run.stdout}{run.stderr}")
    return summarize(json.loads(lines[-1]))


def compare(baseline: dict, current: dict) -> tuple[list[str], list[str]]:
    """Return (drops, gains) as human-readable lines, file by file."""
    drops: list[str] = []
    gains: list[str] = []
    for entry in sorted(set(baseline) | set(current)):
        old = baseline.get(entry)
        new = current.get(entry)
        if old is None:
            if new["closed"]:
                gains.append(f"{entry}: new file closing {len(new['closed'])} law(s) by steps")
            continue
        if new is None:
            if old:
                drops.append(f"{entry}: no longer in the corpus")
            continue
        before, after = set(old), set(new["closed"])
        drops += [f"{entry}: `{law}` no longer closes by steps" for law in sorted(before - after)]
        gains += [f"{entry}: `{law}` newly closes by steps" for law in sorted(after - before)]
    return drops, gains


def recorded(current: dict) -> dict:
    """The baseline form: closed laws per file, files with none left out."""
    return {entry: s["closed"] for entry, s in sorted(current.items()) if s["closed"]}


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
        print(f"{aver}: no aver binary; build it with `cargo build --bin aver`", file=sys.stderr)
        return 2

    current: dict = {}
    errors: list[str] = []
    entries = corpus(REPO_ROOT)
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {pool.submit(measure, aver, entry): entry for entry in entries}
        for future in concurrent.futures.as_completed(futures):
            try:
                current[futures[future]] = future.result()
            except RuntimeError as error:
                errors.append(str(error))
    if errors:
        print("\n\n".join(sorted(errors)), file=sys.stderr)
        return 2

    closed = sum(len(s["closed"]) for s in current.values())
    laws = sum(s["laws"] for s in current.values())
    print(f"{len(current)} files, {closed} of {laws} laws closed by steps on the aver backend")

    baseline = json.loads(args.baseline.read_text()) if args.baseline.exists() else {}
    with_closed = {e: s for e, s in current.items() if s["closed"] or e in baseline}
    drops, gains = compare(baseline, with_closed)

    if args.update:
        if drops and not args.allow_drop:
            print("refusing to record losses without --allow-drop:", file=sys.stderr)
            print("\n".join(drops), file=sys.stderr)
            return 1
        args.baseline.write_text(json.dumps(recorded(current), indent=1, sort_keys=True) + "\n")
        print(f"recorded {len(recorded(current))} files in {args.baseline}")
        return 0

    if drops:
        print("laws reopened against tools/steps-baseline.json:", file=sys.stderr)
        print("\n".join(drops), file=sys.stderr)
    if gains:
        print(
            "laws newly closed by steps; record them in this commit with "
            "`python3 tools/steps_ratchet.py --update`:",
            file=sys.stderr,
        )
        print("\n".join(gains), file=sys.stderr)
    return 1 if drops or gains else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
