#!/usr/bin/env python3
"""Measure cold Lean proof runs, including every Lake invocation and proof credit.

Run frozen before/after Aver binaries with the same sources into separate new
output directories. Runs are serial; each starts without a generated Lake cache.
The installed Lean toolchain is shared. No proof budget or axiom gate is relaxed.
"""

import argparse
import hashlib
import json
import os
from pathlib import Path
import platform
import shutil
import signal
import subprocess
import sys
import time


ROOT = Path(__file__).resolve().parents[1]
CASES = {
    "single-list": ("proof-corpus/decomposed/handwritten/all_zero_sum.av", "."),
    "k5-round": ("projects/k5_fdiv/domain/round.av", "projects/k5_fdiv"),
    "k5-kernel": ("projects/k5_fdiv/domain/kernel.av", "projects/k5_fdiv"),
}


def digest(path):
    with path.open("rb") as stream:
        return hashlib.file_digest(stream, "sha256").hexdigest()


def lake_wrapper():
    """Forward the real pinned-toolchain Lake invocation, keeping its output."""
    directory = Path(os.environ["AVER_PROOF_BENCH_RUN"])
    stem = f"lake-{time.time_ns()}"
    start = time.monotonic()
    with (directory / f"{stem}.stdout").open("wb") as stdout, \
            (directory / f"{stem}.stderr").open("wb") as stderr:
        result = subprocess.run([os.environ["AVER_PROOF_BENCH_LAKE"], *sys.argv[2:]],
                                stdout=stdout, stderr=stderr)
    row = dict(args=sys.argv[2:], elapsed=time.monotonic() - start,
               exit_code=result.returncode, log=stem)
    with (directory / "lake.jsonl").open("a") as stream:
        stream.write(json.dumps(row) + "\n")
    for suffix, stream in [("stdout", sys.stdout.buffer), ("stderr", sys.stderr.buffer)]:
        with (directory / f"{stem}.{suffix}").open("rb") as log:
            shutil.copyfileobj(log, stream)
    return result.returncode


def source_hashes():
    # Include all Aver inputs, including imported modules and embedded stdlib.
    files = subprocess.check_output(
        ["git", "ls-files", "-z", "--", "*.av"], cwd=ROOT).split(b"\0")
    return {os.fsdecode(p): digest(ROOT / os.fsdecode(p)) for p in files if p}


def run(binary, output, case, repeat, timeout, env):
    directory = output / f"{case}-{repeat}"
    directory.mkdir()
    source, module_root = CASES[case]
    command = [str(binary), "proof", source, "--module-root", module_root,
               "--backend", "lean", "--check-json", "--sorry-budget", "0",
               "-o", str(directory / "proof")]
    start = time.monotonic()
    timed_out = False
    with (directory / "aver.log").open("wb") as log:
        process = subprocess.Popen(command, cwd=ROOT, stdout=log, stderr=subprocess.STDOUT,
                                   env=dict(env, AVER_PROOF_BENCH_RUN=str(directory)),
                                   start_new_session=True)
        try:
            process.wait(timeout=timeout)
        except subprocess.TimeoutExpired:
            timed_out = True
            os.killpg(process.pid, signal.SIGKILL)
            process.wait()
    elapsed = time.monotonic() - start
    summary = None
    for line in reversed((directory / "aver.log").read_text(errors="replace").splitlines()):
        if line.startswith("{"):
            try:
                summary = json.loads(line)
                break
            except json.JSONDecodeError:
                pass
    manifest_path = directory / "proof/proof_manifest.json"
    manifest = json.loads(manifest_path.read_text()) if manifest_path.exists() else None
    lake_path = directory / "lake.jsonl"
    lake = [json.loads(line) for line in lake_path.read_text().splitlines()] \
        if lake_path.exists() else []
    row = dict(case=case, repeat=repeat, elapsed=elapsed, command=command,
               exit_code=process.returncode, timed_out=timed_out, lake=lake,
               summary=summary, manifest=manifest)
    (directory / "result.json").write_text(json.dumps(row, indent=2) + "\n")
    print(f"{case} #{repeat}: {elapsed:.3f}s, exit={process.returncode}", flush=True)
    return row


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--aver", required=True, type=Path)
    parser.add_argument("--out", required=True, type=Path)
    parser.add_argument("--case", action="append", choices=CASES)
    parser.add_argument("--repeat", type=int, choices=range(1, 11), default=3)
    parser.add_argument("--timeout", type=int, default=900)
    args = parser.parse_args()
    binary = args.aver.resolve(strict=True)
    output = args.out.resolve()
    lake = shutil.which("lake")
    if lake is None:
        parser.error("lake must be available on PATH")
    if args.timeout <= 0:
        parser.error("--timeout must be positive")
    output.mkdir(parents=True, exist_ok=False)
    shim = output / "bin"
    shim.mkdir()
    # Keep the Elan proxy path (resolving its symlink to elan changes argv[0]).
    wrapper = shim / "lake"
    wrapper.write_text(f"#!{sys.executable}\nimport runpy, sys\n"
                       f"sys.argv = [{str(__file__)!r}, '--lake-wrapper', *sys.argv[1:]]\n"
                       f"runpy.run_path({str(Path(__file__).resolve())!r}, run_name='__main__')\n")
    wrapper.chmod(0o755)
    env = dict(os.environ, PATH=str(shim) + os.pathsep + os.environ.get("PATH", ""),
               AVER_PROOF_BENCH_LAKE=os.path.abspath(lake))
    # External debugging modes must not add copy/build work to the measurement.
    for key in ["AVER_SPECULATIVE_KEEP", "AVER_SPECULATIVE_LOG"]:
        env.pop(key, None)
    initial = dict(binary_sha256=digest(binary), sources=source_hashes())
    metadata = dict(binary=str(binary), platform=platform.platform(),
                    python=sys.version, lake=lake, inputs=initial)
    (output / "metadata.json").write_text(json.dumps(metadata, indent=2) + "\n")
    rows = [run(binary, output, case, repeat, args.timeout, env)
            for repeat in range(1, args.repeat + 1) for case in (args.case or CASES)]
    unchanged = initial == dict(binary_sha256=digest(binary), sources=source_hashes())
    report = dict(**metadata, inputs_unchanged=unchanged, runs=rows)
    (output / "report.json").write_text(json.dumps(report, indent=2) + "\n")
    return 0 if unchanged and all(
        row["exit_code"] == 0 and not row["timed_out"] and row["manifest"] is not None
        and (row["summary"] or {}).get("passed") is True for row in rows) else 1


if __name__ == "__main__":
    sys.exit(lake_wrapper() if sys.argv[1:2] == ["--lake-wrapper"] else main())
