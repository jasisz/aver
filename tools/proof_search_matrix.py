#!/usr/bin/env python3
"""Record proof outcomes under source refactoring; never equate completion with proof.

Run against two frozen Aver binaries into separate new directories to compare
search policies. Checker budgets are unchanged. An outer timeout kills the
whole invocation process group and records an unanswered case.
"""

import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import os
import re
from pathlib import Path
import signal
import subprocess
import time


ROOT = Path(__file__).resolve().parents[1]
FIXTURES = ROOT / "tests/fixtures/source_recursion"


def cases():
    source = (FIXTURES / "roundtrip.av").read_text()
    yield "roundtrip", {"main.av": source}
    yield "roundtrip_alias", {"main.av": source.replace(
        "    match value > 0", "    current = value\n    match current > 0")}
    yield "roundtrip_equation", {"main.av": source.replace(
        "read(List.reverse(digits(value, [])), 0) => value",
        "value => read(List.reverse(digits(value, [])), 0)")}
    unrelated = """fn unchanged(flag: Bool) -> Bool
    flag
verify unchanged law identity
    given flag: Bool = [false, true]
    unchanged(flag) => flag

"""
    yield "roundtrip_unrelated", {"main.av": source.replace("fn read(", unrelated + "fn read(")}
    start = source.index("fn read(")
    end = source.index("fn digits(")
    reader = "module Reader\n    exposes [read]\n\n" + source[start:end]
    consumer = (source[:start] + source[end:]).replace(
        "module Roundtrip", "module Roundtrip\n    depends [Reader]").replace(
        "read(", "Reader.read(")
    yield "roundtrip_import", {"main.av": consumer, "reader.av": reader}
    for name in ["reverse_algebra", "signed_frame"]:
        yield name, {"main.av": (FIXTURES / f"{name}.av").read_text()}


def run(binary, output, name, backend, timeout, expected_laws):
    directory = output / name
    args = [str(binary), "proof", str(directory / "main.av"), "--backend", backend,
            "--check-json", "--module-root", str(directory), "-o", str(directory / backend)]
    start = time.monotonic()
    process = subprocess.Popen(args, cwd=ROOT, stdout=subprocess.PIPE,
                               stderr=subprocess.STDOUT, start_new_session=True)
    timed_out = False
    try:
        log, _ = process.communicate(timeout=timeout)
    except subprocess.TimeoutExpired:
        timed_out = True
        os.killpg(process.pid, signal.SIGKILL)
        log, _ = process.communicate()
    text = log.decode(errors="replace")
    (directory / f"{backend}.log").write_text(text)
    summary = None
    for line in reversed(text.splitlines()):
        if line.startswith("{"):
            try:
                summary = json.loads(line)
                break
            except json.JSONDecodeError:
                pass
    if backend == "lean":
        exported = (summary or {}).get("universal_laws", 0)
    else:
        # These fixtures use ordinary laws. Count their emitted declarations
        # as well as requiring the whole-file checker gate.
        exported = sum(len(re.findall(r"(?m)^\s*// Law: ", p.read_text()))
                       for p in (directory / backend).rglob("*.dfy"))
    strict = (not timed_out and process.returncode == 0
              and bool((summary or {}).get("passed"))
              and exported == expected_laws
              and all((summary or {}).get(k, 0) == 0
                      for k in ["errors", "timeouts", "omitted", "axioms",
                                "sorries", "bounded_laws", "build_errors", "declined"]))
    row = dict(expected_laws=expected_laws, exported_laws=exported, strict_passed=strict,
               case=name, backend=backend, elapsed=round(time.monotonic() - start, 3),
               outer_timeout=timed_out, exit_code=process.returncode, summary=summary)
    print(json.dumps(row), flush=True)
    return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--aver", required=True, type=Path)
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--workers", type=int, choices=range(1, 5), default=2)
    parser.add_argument("--timeout", type=int, default=90)
    parser.add_argument("--backend", action="append", choices=["lean", "dafny"])
    args = parser.parse_args()
    binary = args.aver.resolve(strict=True)
    output = args.out.resolve()
    digest = hashlib.sha256(binary.read_bytes()).hexdigest()
    output.mkdir(parents=True, exist_ok=False)
    jobs = []
    source_hashes = {}
    for name, files in cases():
        directory = output / name
        directory.mkdir()
        source_hashes[name] = {}
        for filename, source in files.items():
            (directory / filename).write_text(source)
            source_hashes[name][filename] = hashlib.sha256(source.encode()).hexdigest()
        expected = sum(len(re.findall(r"(?m)^verify .* law ", source))
                       for source in files.values())
        jobs.extend((name, backend, expected) for backend in (args.backend or ["lean", "dafny"]))
    with ThreadPoolExecutor(max_workers=args.workers) as executor:
        futures = [executor.submit(run, binary, output, name, backend, args.timeout, expected)
                   for name, backend, expected in jobs]
        rows = [future.result() for future in futures]
    final_digest = hashlib.sha256(binary.read_bytes()).hexdigest()
    report = dict(binary=str(binary), sha256=digest, binary_unchanged=digest == final_digest,
                  sources=source_hashes, workers=args.workers, outer_timeout=args.timeout, results=rows)
    (output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    if digest != final_digest:
        raise SystemExit("Binary changed during measurement; discard this comparison")


if __name__ == "__main__":
    main()
