#!/usr/bin/env python3
"""Proof-level ratchet: what closes each law must not silently change.

Every corpus file that declares a law (`examples/`, `proof-corpus/`,
`projects/*`) is proved, and per file the baseline `tools/steps-baseline.json`
records three levels:

- `steps`: laws closed by proof steps the kernel written in Aver checks,
  with `aver proof --backend aver`, in process. This needs no Lean and runs
  on every pull request.
- `obligations`: for a law with `because` lines that is not at the steps
  level, each obligation of its chain (`<law>.because<k>`, `<law>.implication`)
  closed by steps the kernel checks, so partial progress is held too. A law at
  the steps level is not counted again by its parts.
- `tactic`: the other laws Lean closes (with tactics, or with steps that
  cite a law only tactics close). Measured only with `--lean`, which also
  runs `aver proof --check-json` per file and needs Lean.

A law that leaves a level it is recorded at fails and is named: a law that
reopens, and a law that falls from steps back to tactics. Withdrawing a tactic
portfolio that steps replaced is therefore a visible baseline change. A gain
also fails, with the command that records it, so an intended improvement lands
with its baseline update:

    python3 tools/steps_ratchet.py --update          # the steps level
    python3 tools/steps_ratchet.py --lean --update   # both levels

A deliberate loss needs `--allow-drop` as well, which a reviewer sees as a
baseline diff that removes lines. Every run prints the totals.

A law recorded at the steps level was closed by the kernel written in Aver.
So that the kernel alone never earns the credit, every law a baseline change
adds to that level is checked under Lean as well, file by file and only for
the files that gained: `--update` does it when `lake` is installed, and the
proof workflow does it on every pull request:

    python3 tools/steps_ratchet.py --lean-gains <the base branch's baseline>

It fails when Lean does not close such a law by its step proof.
"""

from __future__ import annotations

import argparse
import concurrent.futures
import json
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_BASELINE = REPO_ROOT / "tools" / "steps-baseline.json"
CORPUS_DIRS = ["examples", "proof-corpus", "projects"]
LAW = re.compile(r"^verify \S+ law ", re.M)
LEVELS = ("steps", "obligations", "tactic")

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


def own(law: str) -> bool:
    """A law the proved file declares itself. A report also lists the laws of
    every module the file imports, under their module path (`Domain.Fprep.…`);
    those are counted at the file that declares them, which is an entry too."""
    return not law[:1].isupper()


def summarize(report: dict) -> dict:
    """The part of a `--check-json` report the ratchet holds."""
    closed_by = {law: how for law, how in report.get("closed_by", {}).items() if own(law)}
    by = lambda level: sorted(law for law, how in closed_by.items() if how == level)
    steps = set(by("steps"))
    # Obligations of a `because` chain closed by steps, for laws that are
    # not: a law at the steps level is not counted again by its parts.
    obligations = sorted(
        ob
        for ob, how in report.get("obligations_closed_by", {}).items()
        if own(ob) and how == "steps" and ob.rsplit(".", 1)[0] not in steps
    )
    return {
        "steps": sorted(steps),
        "obligations": obligations,
        "tactic": by("tactic"),
        "laws": len(closed_by),
    }


def report(aver: Path, entry: str, lean: bool) -> dict:
    """One `--check-json` report, with the Aver kernel or with Lean. A Lean
    report also lists, under `universal`, every law its manifest credits."""
    with tempfile.TemporaryDirectory(prefix="aver-steps-ratchet-") as out:
        cmd = [str(aver), "proof", entry, "--module-root", module_root(entry)]
        if not lean:
            cmd += ["--backend", "aver"]
        cmd += ["-o", out, "--check-json", "--sorry-budget", "1000000", "--declined-budget", "1000000"]
        run = subprocess.run(cmd, cwd=REPO_ROOT, capture_output=True, text=True)
        lines = [line for line in run.stdout.splitlines() if line.startswith("{")]
        if not lines:
            raise RuntimeError(f"{entry}: {' '.join(cmd[1:])} failed\n{run.stdout}{run.stderr}")
        found = json.loads(lines[-1])
        manifest = Path(out) / "proof_manifest.json"
        if lean and manifest.exists():
            laws = json.loads(manifest.read_text()).get("laws", [])
            found["universal"] = sorted(l["law"] for l in laws if l.get("tier") == "universal")
    if found.get("steps_rejected"):
        raise RuntimeError(f"{entry}: a step proof was refused: {', '.join(found['steps_rejected'])}")
    return found


def measure(aver: Path, entry: str, lean: bool) -> dict:
    steps = summarize(report(aver, entry, False))
    if not lean:
        return steps
    by_lean = report(aver, entry, True)
    closed = {law for law in by_lean.get("universal", []) if own(law)}
    return {
        "steps": steps["steps"],
        "obligations": steps["obligations"],
        "tactic": sorted(closed - set(steps["steps"])),
        "laws": steps["laws"],
    }


def steps_gains(before: dict, after: dict) -> dict[str, list[str]]:
    """The laws `after` records at the steps level that `before` does not, by file."""
    out: dict[str, list[str]] = {}
    for entry, row in after.items():
        old = before.get(entry, {})
        new = sorted(
            (set(row.get("steps", [])) - set(old.get("steps", [])))
            | (set(row.get("obligations", [])) - set(old.get("obligations", [])) - set(old.get("steps", [])))
        )
        if new:
            out[entry] = new
    return out


def lean_confirms(aver: Path, gains: dict[str, list[str]], jobs: int) -> list[str]:
    """Check under Lean each file that gained steps-level laws; the laws Lean
    does not close by their step proof, as lines to report."""
    failures: list[str] = []

    def one(entry: str) -> list[str]:
        try:
            closed = report(aver, entry, True).get("closed_by", {})
        except RuntimeError as error:
            return [str(error)]
        return [
            f"{entry}: `{law}` closes by steps in the Aver kernel but by {closed.get(law, 'nothing')} in Lean"
            for law in gains[entry]
            if closed.get(law) != "steps"
        ]

    with concurrent.futures.ThreadPoolExecutor(max_workers=jobs) as pool:
        for lines in pool.map(one, sorted(gains)):
            failures += lines
    for entry in sorted(gains):
        print(f"checked under Lean: {entry}: {', '.join(gains[entry])}")
    return failures


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
                # An obligation whose whole law now closes by steps moved up.
                if level == "obligations" and law.rsplit(".", 1)[0] in new["steps"]:
                    continue
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
    parser.add_argument(
        "--lean-gains",
        type=Path,
        metavar="BASE_BASELINE",
        help="check under Lean every steps-level law the baseline records beyond BASE_BASELINE",
    )
    args = parser.parse_args(argv)
    aver = args.aver.resolve()
    if not aver.is_file():
        print(f"{aver}: no aver binary; build it with `cargo build --bin aver`", file=sys.stderr)
        return 2
    baseline = json.loads(args.baseline.read_text()) if args.baseline.exists() else {}
    if args.lean_gains is not None:
        base = json.loads(args.lean_gains.read_text()) if args.lean_gains.exists() else {}
        gains = steps_gains(base, baseline)
        if not gains:
            print("the baseline adds no law to the steps level; nothing to check under Lean")
            return 0
        failures = lean_confirms(aver, gains, args.jobs)
        if failures:
            print("laws the Aver kernel closes by steps that Lean does not:", file=sys.stderr)
            print("\n".join(failures), file=sys.stderr)
            return 1
        print(f"Lean closes all {sum(len(v) for v in gains.values())} new steps-level law(s) by steps")
        return 0
    lean = args.lean
    if lean and not args.update and not any(row.get("tactic") for row in baseline.values()):
        # Nothing to compare the tactic level with: measuring it would only
        # report every tactic-closed law as new.
        print(
            "the baseline records no tactic level yet; checking the steps level only "
            "(record it with `python3 tools/steps_ratchet.py --lean --update`)"
        )
        lean = False
    levels = LEVELS if lean else ("steps", "obligations")

    entries = [e for e in corpus(REPO_ROOT) if not args.only or e in args.only]
    current: dict = {}
    errors: list[str] = []
    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        futures = {pool.submit(measure, aver, entry, lean): entry for entry in entries}
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

    scope = {e: baseline[e] for e in baseline if not args.only or e in args.only}
    drops, gains = compare(scope, current, levels)

    if args.update:
        if drops and not args.allow_drop:
            print("refusing to record losses without --allow-drop:", file=sys.stderr)
            print("\n".join(drops), file=sys.stderr)
            return 1
        merged = recorded(baseline, current, levels, full=not args.only)
        gains_now = steps_gains(baseline, merged)
        if gains_now and shutil.which("lake"):
            failures = lean_confirms(aver, gains_now, args.jobs)
            if failures:
                print("refusing to record laws Lean does not close by steps:", file=sys.stderr)
                print("\n".join(failures), file=sys.stderr)
                return 1
        elif gains_now:
            print("no `lake` here: the proof workflow checks the new steps-level laws under Lean")
        args.baseline.write_text(json.dumps(merged, indent=1, sort_keys=True) + "\n")
        print(f"recorded {len(merged)} files in {args.baseline}")
        return 0

    if drops:
        print("laws that left a level they are recorded at in tools/steps-baseline.json:", file=sys.stderr)
        print("\n".join(drops), file=sys.stderr)
    if gains:
        update = "python3 tools/steps_ratchet.py" + (" --lean" if lean else "") + " --update"
        print(f"laws newly closed; record them in this commit with `{update}`:", file=sys.stderr)
        print("\n".join(gains), file=sys.stderr)
    return 1 if drops or gains else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
