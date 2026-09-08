#!/usr/bin/env python3
"""Measure guided proof portability on identical sources with strict backend gates.

Results describe whole-file verification. A modular caller in a failing file
never receives independent credit; Dafny verification is not a Lean axiom audit.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import shutil
import signal
import subprocess
import time


ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "tests/fixtures/dafny_guidance_spike"
EXPECTED = {
    "positive": {"lean": "verified", "dafny": "verified"},
    "missing_guard": {"lean": "failed", "dafny": "failed"},
    "false_reason": {"lean": "failed", "dafny": "failed"},
    "restated_goal": {"lean": "failed", "dafny": "failed"},
    "failed_citation": {"lean": "failed", "dafny": "failed"},
    "recursive": {"lean": "verified", "dafny": "verified"},
    "recursive_false_reason": {"lean": "failed", "dafny": "failed"},
    "nonlinear_identity": {"lean": "verified", "dafny": "verified"},
    "nonlinear_positive": {"lean": "failed", "dafny": "verified"},
    "nonlinear_missing_sign": {"lean": "failed", "dafny": "failed"},
    "nonlinear_zero_factor": {"lean": "failed", "dafny": "failed"},
    "nonlinear_failed_citation": {"lean": "failed", "dafny": "failed"},
    "k5_integerorder": {"lean": "verified", "dafny": "verified"},
}
SOURCE_PATHS = {
    "k5_integerorder": ROOT / "projects/k5_fdiv/domain/integerorder.av",
}
MODULE_ROOTS: dict[str, Path] = {}

STRUCTURED = ROOT / "tests/fixtures/dafny_guidance_structured"
for name in ["btc_stackitem_slice", "adt_positive", "result_positive", "missing_guard", "recursive_false_reason", "failed_citation"]:
    case = f"structured_{name}"
    expected = "verified" if name in {"btc_stackitem_slice", "adt_positive", "result_positive"} else "failed"
    EXPECTED[case] = {"lean": expected, "dafny": expected}
    SOURCE_PATHS[case] = STRUCTURED / f"{name}.av"

IMPORT_DIV = ROOT / "tests/fixtures/dafny_guidance_import_div"
for name in ["imports_positive", "arithmetic_positive", "transitive_consumer", "imported_false", "arithmetic_false"]:
    case = f"import_div_{name}"
    expected = "failed" if name in {"imported_false", "arithmetic_false"} else "verified"
    EXPECTED[case] = {"lean": expected, "dafny": expected}
    if name in {"imports_positive", "transitive_consumer", "imported_false"}:
        MODULE_ROOTS[case] = IMPORT_DIV / name
        SOURCE_PATHS[case] = MODULE_ROOTS[case] / "main.av"
    else:
        SOURCE_PATHS[case] = IMPORT_DIV / f"{name}.av"


MUTUAL = ROOT / "tests/fixtures/dafny_guidance_mutual"
for name in ["positive", "imported", "false_reason"]:
    case = f"mutual_{name}"
    expected = "failed" if name == "false_reason" else "verified"
    EXPECTED[case] = {"lean": expected, "dafny": expected}
    if name == "imported":
        MODULE_ROOTS[case] = MUTUAL / name
        SOURCE_PATHS[case] = MODULE_ROOTS[case] / "main.av"
    else:
        SOURCE_PATHS[case] = MUTUAL / f"{name}.av"

# These exact source files measure a useful automation difference as well as
# admission: Dafny closes the positive callback/display laws; Lean's current
# generated proof attempts remain incomplete. False reasons must fail both.
STRUCTURE = ROOT / "tests/fixtures/dafny_structure"
for name, path in {
    "display_positive": "strings/positive.av",
    "callbacks_positive": "callbacks_positive.av",
    "callbacks_imported": "callbacks_imported/main.av",
    "display_false": "strings/false_reason.av",
    "callbacks_false": "callbacks_false.av",
}.items():
    case = f"structure_{name}"
    EXPECTED[case] = {"lean": "failed", "dafny": "failed" if name.endswith("_false") else "verified"}
    SOURCE_PATHS[case] = STRUCTURE / path
    if name == "callbacks_imported":
        MODULE_ROOTS[case] = STRUCTURE / "callbacks_imported"


def digest(path: Path) -> str:
    with path.open("rb") as source:
        return hashlib.file_digest(source, "sha256").hexdigest()


def source_hashes(source: Path, module_root: Path | None = None) -> dict[str, str]:
    """Fingerprint fixture imports too, including added or removed Aver files.

    Imported fixtures have an explicit module root. Hash its complete Aver tree
    so a changed transitive supplier cannot keep an unchanged entry's credit.
    Missing files are absent from the map, making deletion an input change.
    """
    paths = {source}
    if module_root is not None:
        paths.update(module_root.rglob("*.av"))
    return {
        str(path.relative_to(ROOT) if path.is_relative_to(ROOT) else path): digest(path)
        for path in sorted(paths) if path.is_file()
    }


def summary_from_log(log: str, backend: str) -> dict | None:
    summaries = []
    for line in log.splitlines():
        try:
            value = json.loads(line)
        except ValueError:
            continue
        if isinstance(value, dict) and value.get("backend") == backend:
            summaries.append(value)
    return summaries[0] if len(summaries) == 1 else None


def outcome(summary: dict | None, returncode: int, timed_out: bool = False) -> str:
    if timed_out:
        return "timeout"
    if summary is None or returncode not in (0, 1):
        return "checker_error"
    if summary.get("timeouts", 0):
        return "timeout"
    if summary.get("build_errors", 0) or summary.get("model_panicked", False):
        return "checker_error"
    if summary.get("declined", 0):
        return "declined"
    if summary.get("omitted", 0) or summary.get("axioms", 0):
        return "conditional"
    if summary.get("errors", 0) or summary.get("sorries", 0):
        return "failed"
    if returncode == 0 and summary.get("passed") is True:
        if summary.get("backend") == "lean" and summary.get("universal") is not True:
            return "conditional"
        return "verified"
    # A rejected invocation without an actual proof error is not evidence for
    # a negative control (for example, Dafny may reject a warning-only run).
    return "checker_error"


def run_process(command: list[str], timeout: float) -> tuple[str, int, bool]:
    process = subprocess.Popen(
        command, cwd=ROOT, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
        text=True, start_new_session=True,
    )
    try:
        output, _ = process.communicate(timeout=timeout)
        return output, process.returncode, False
    except subprocess.TimeoutExpired:
        # Stop the verifier descendants as well as Aver; no orphan Lean/Z3 jobs.
        try:
            os.killpg(process.pid, signal.SIGTERM)
        except ProcessLookupError:
            pass
        try:
            output, _ = process.communicate(timeout=5)
        except subprocess.TimeoutExpired:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            output, _ = process.communicate()
        return output, process.returncode, True


def claim_inventory(directory: Path, backend: str) -> dict[str, list[str]]:
    if backend == "lean":
        manifest = directory / "proof_manifest.json"
        if not manifest.is_file():
            return {"laws": [], "obligations": []}
        value = json.loads(manifest.read_text())
        return {
            kind: sorted(claim["law"] for claim in value.get(kind, []))
            for kind in ("laws", "obligations")
        }
    claims: dict[str, list[str]] = {"laws": [], "obligations": []}
    marker = re.compile(r"^\s*// aver:dafny-(law|obligation)\s+\S+\s+(\S+)\s*$")
    for source in directory.rglob("*.dfy"):
        for line in source.read_text().splitlines():
            match = marker.match(line)
            if match:
                kind = "laws" if match[1] == "law" else "obligations"
                claims[kind].append(match[2])
    return {kind: sorted(names) for kind, names in claims.items()}


def coverage(rows: list[dict]) -> dict:
    verified = {
        backend: {
            (row["source_sha256"], claim)
            for row in rows if row["backend"] == backend and row["outcome"] == "verified"
            for claim in row["claims"]["laws"]
        }
        for backend in ("lean", "dafny")
    }
    return {
        "scope": "selected spike sources; failed files grant no independent law credit",
        "both_backends": len(verified["lean"] & verified["dafny"]),
        "lean_only": len(verified["lean"] - verified["dafny"]),
        "dafny_only": len(verified["dafny"] - verified["lean"]),
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--aver", default="aver", help="Aver executable to measure")
    parser.add_argument("--output", type=Path, default=ROOT / "out/dafny-guidance-spike")
    parser.add_argument("--case", action="append", choices=EXPECTED)
    parser.add_argument("--timeout", type=float, default=180, help="Wall time per backend run")
    args = parser.parse_args()
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    binary_name = shutil.which(args.aver)
    if binary_name is None:
        parser.error(f"Aver executable not found: {args.aver}")
    binary = Path(binary_name).resolve()
    output = args.output.resolve()
    output.mkdir(parents=True, exist_ok=True)
    rows = []
    problems = []
    compiler_hash = digest(binary)
    versions = {}
    for tool in (str(binary), "lake", "dafny"):
        try:
            version, code, expired = run_process([tool, "--version"], min(args.timeout, 10))
            versions[Path(tool).name] = {
                "output": version.strip(), "returncode": code, "timed_out": expired,
            }
        except OSError as error:
            versions[Path(tool).name] = {"error": str(error)}
    for case in args.case or EXPECTED:
        source = SOURCE_PATHS.get(case, FIXTURES / f"{case}.av")
        module_root = MODULE_ROOTS.get(case)
        source_hash = digest(source)
        input_hashes = source_hashes(source, module_root)
        case_rows = []
        for backend in ("lean", "dafny"):
            destination = output / case / backend
            destination.parent.mkdir(parents=True, exist_ok=True)
            # Reusing old generated files could contaminate the evidence.
            if destination.exists():
                parser.error(f"Output already exists; choose a fresh directory: {destination}")
            command = [str(binary), "proof", str(source), "--backend", backend,
                       "--check-json", "--error-budget", "0", "--sorry-budget", "0",
                       "--declined-budget", "0", "-o", str(destination)]
            if module_root is not None:
                command.extend(["--module-root", str(module_root)])
            inputs_changed_before = (
                source_hashes(source, module_root) != input_hashes
                or digest(binary) != compiler_hash
            )
            started = time.monotonic()
            try:
                log, code, timed_out = run_process(command, args.timeout)
            except OSError as error:
                log, code, timed_out = str(error), 2, False
            (destination.parent / f"{backend}.log").write_text(log)
            summary = summary_from_log(log, backend)
            result = outcome(summary, code, timed_out)
            if (inputs_changed_before
                    or source_hashes(source, module_root) != input_hashes
                    or digest(binary) != compiler_hash):
                result = "input_changed"
            inventory = claim_inventory(destination, backend)
            generated_toolchain = None
            actual_lean_version = None
            if backend == "lean" and (destination / "lean-toolchain").is_file():
                generated_toolchain = (destination / "lean-toolchain").read_text().strip()
                try:
                    version, version_code, version_expired = run_process(
                        ["elan", "run", generated_toolchain, "lean", "--version"],
                        min(args.timeout, 10),
                    )
                    actual_lean_version = {
                        "output": version.strip(), "returncode": version_code,
                        "timed_out": version_expired,
                    }
                    if version_code != 0 or version_expired:
                        problems.append(f"{case}/lean: cannot identify generated toolchain version")
                except OSError as error:
                    actual_lean_version = {"error": str(error)}
                    problems.append(f"{case}/lean: cannot identify generated toolchain version")
            row = {
                "case": case, "backend": backend, "source_sha256": source_hash,
                "source_path": str(source.relative_to(ROOT)),
                "source_hashes_sha256": input_hashes,
                "module_root": str(module_root.relative_to(ROOT)) if module_root else None,
                "outcome": result, "expected": EXPECTED[case][backend],
                "returncode": code, "seconds": round(time.monotonic() - started, 3),
                "summary": summary, "claims": inventory,
                "generated_toolchain": generated_toolchain,
                "actual_lean_version": actual_lean_version,
            }
            rows.append(row)
            case_rows.append(row)
            if result != row["expected"]:
                problems.append(f"{case}/{backend}: expected {row['expected']}, got {result}")
            print(f"{case:20} {backend:6} {result}", flush=True)
        if all(row["outcome"] == "verified" for row in case_rows):
            if case_rows[0]["claims"] != case_rows[1]["claims"]:
                problems.append(f"{case}: verified backends reported different source claim IDs")
            for names in case_rows[0]["claims"].values():
                if len(names) != len(set(names)):
                    problems.append(f"{case}: duplicate source claim IDs")
    report = {
        "scope": "whole-file strict verification, not independent per-law credit",
        "dafny_evidence": "Dafny/Boogie/Z3 verification, not a Lean kernel axiom audit",
        "compiler_sha256": compiler_hash, "ambient_versions": versions, "results": rows,
        "verified_law_coverage": coverage(rows),
        "expectations_met": not problems, "problems": problems,
    }
    (output / "matrix.json").write_text(json.dumps(report, indent=2) + "\n")
    for problem in problems:
        print(problem)
    return int(bool(problems))


if __name__ == "__main__":
    raise SystemExit(main())
