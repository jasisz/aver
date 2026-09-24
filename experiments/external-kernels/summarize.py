#!/usr/bin/env python3
"""Fold every package's results into one Markdown summary.

usage: summarize.py RESULTS_DIR  (one subdirectory per package)
"""
import json
import pathlib
import re
import sys

KERNELS = [
    ("leanchecker --fresh (stock, on .olean)", "leanchecker-fresh", None),
    ("official kernel, from export (arena)", "official-export", "official-export-tampered"),
    ("nanoda (arena config)", "nanoda", "nanoda-tampered"),
    ("nanoda (strict axioms)", "nanoda-strict", "nanoda-strict-tampered"),
    ("sokonanoda", "sokonanoda", "sokonanoda-tampered"),
    ("mathgraph", "mathgraph", "mathgraph-tampered"),
]


def read_results(path):
    rows = {}
    if not path.exists():
        return rows
    lines = path.read_text().splitlines()[1:]
    for line in lines:
        parts = line.split("\t")
        if len(parts) == 4:
            rows[parts[0]] = {"exit": parts[1], "wall": parts[2], "rss": parts[3]}
    return rows


def verdict(row):
    if row is None:
        return "not run"
    if row["exit"] == "0":
        return "accepted"
    if row["exit"] == "124":
        return "timeout"
    return f"rejected (exit {row['exit']})"


def mib(row):
    try:
        return f"{int(row['rss']) / 1024:.0f}"
    except (TypeError, ValueError, KeyError):
        return "?"


def timing_phases(log):
    phases = {}
    if not log.exists():
        return phases
    for line in log.read_text(errors="replace").splitlines():
        match = re.match(r"aver-cert timing: ([^:]+): ([0-9.]+)s", line)
        if match:
            phases[match.group(1)] = phases.get(match.group(1), 0.0) + float(match.group(2))
    return phases


def main():
    root = pathlib.Path(sys.argv[1])
    out = ["# External kernel experiment", ""]
    kernel_rows = []
    phase_rows = []
    profile_rows = []
    facts_rows = []
    for package in sorted(p for p in root.iterdir() if p.is_dir()):
        name = package.name.removeprefix("results-")
        rows = read_results(package / "results.tsv")
        facts = {}
        if (package / "package.txt").exists():
            for line in (package / "package.txt").read_text().splitlines():
                if "=" in line:
                    key, value = line.split("=", 1)
                    facts[key] = value
        facts_rows.append((name, facts, rows))
        for label, step, tampered in KERNELS:
            row = rows.get(step)
            trow = rows.get(tampered) if tampered else None
            kernel_rows.append(
                f"| {name} | {label} | {row['wall'] if row else '-'} | {mib(row) if row else '-'} | "
                f"{verdict(row)} | {verdict(trow) if tampered else 'n/a (no .olean for a tampered export)'} |"
            )
        for step in ("check", "verify"):
            phases = timing_phases(package / "logs" / f"{step}.log")
            row = rows.get(step)
            phase_text = ", ".join(f"{k} {v:.1f}s" for k, v in phases.items()) or "-"
            phase_rows.append(
                f"| {name} | {step} | {row['wall'] if row else '-'} | {verdict(row)} | {phase_text} |"
            )
        profile = package / "profile.jsonl"
        if profile.exists():
            for line in profile.read_text().splitlines():
                try:
                    data = json.loads(line)
                except json.JSONDecodeError:
                    continue
                cats = data["categories"]
                total = sum(v for k, v in cats.items() if k != "import")
                kernel = cats.get("type checking", 0.0)
                module = pathlib.Path(data["log"]).stem
                top = ", ".join(
                    f"{k} {v:.1f}s" for k, v in sorted(cats.items(), key=lambda kv: -kv[1])[:5]
                )
                share = f"{100 * kernel / total:.0f}%" if total else "?"
                profile_rows.append(f"| {name} | {module} | {total:.1f} | {kernel:.1f} | {share} | {top} |")

    out += [
        "## Kernels on the CheckerWitness closure",
        "",
        "| package | kernel | wall s | peak MiB | original | one artifact byte flipped |",
        "|---|---|---|---|---|---|",
        *kernel_rows,
        "",
        "## Package and export facts",
        "",
    ]
    for name, facts, rows in facts_rows:
        export = rows.get("export")
        tamper = rows.get("verify-tampered-artifact")
        out.append(
            f"- **{name}**: artifact {facts.get('artifact_bytes', '?')} B; package "
            f"{facts.get('package_lean_files', '?')} Lean files, {facts.get('package_lean_bytes', '?')} B; "
            f"export {facts.get('export_bytes', '?')} B, {facts.get('export_lines', '?')} lines, "
            f"{facts.get('export_natlits', '?')} Nat literals (largest {facts.get('export_max_natlit_digits', '?')} digits); "
            f"lean4export {export['wall'] if export else '-'} s; stock `aver-cert verify` on the flipped artifact "
            f"with its hash re-pinned: {verdict(tamper)}"
        )
    out += [
        "",
        "## Verifier steps (`AVER_CERT_TIMINGS=1`)",
        "",
        "| package | command | wall s | verdict | phases |",
        "|---|---|---|---|---|",
        *phase_rows,
        "",
        "## Kernel versus elaboration (Lean profiler, heaviest modules)",
        "",
        "| package | module | profiled s (excl. import) | type checking s | kernel share | top categories |",
        "|---|---|---|---|---|---|",
        *profile_rows,
        "",
    ]
    print("\n".join(out))


if __name__ == "__main__":
    main()
