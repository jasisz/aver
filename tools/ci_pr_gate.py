#!/usr/bin/env python3
"""Decide from a pull request's changed paths whether an expensive lane runs.

Only pull requests are gated. Pushes, the schedule and manual runs always
run every lane; the workflows check the event before they call this script.

The two gates lean in opposite directions on purpose:

* ``cert`` lists the paths that cannot reach the certificate (documentation,
  editor support, the CI files of other workflows, integration tests outside
  the certificate suites). The Certification lanes run when ANY changed path
  is outside that list, so a path nobody thought about runs the lanes.
* ``pack`` lists the paths the Wasmtime deployment pack is built from. The
  pack lane runs when any changed path is in that list. Every push to main
  and every release candidate still runs it, so a path missing here costs at
  most one main push before the failure shows.

An empty or unreadable list of paths always runs the lane.

Usage: ``python3 tools/ci_pr_gate.py <gate> < changed-paths.txt`` prints
``true`` or ``false``.
"""

from __future__ import annotations

import fnmatch
import re
import sys
from typing import Iterable

# Paths that cannot change the wasm-gc bytes, the certificate package the
# compiler emits, the checker-owned wall, the verifier or a certificate test.
# `*` crosses directory separators here (fnmatch semantics).
CERT_SKIP = (
    "docs/*",
    "decisions/*",
    "recordings/*",
    "editors/*",
    "bench/*",
    "benches/*",
    "fuzz/*",
    "aver-lsp/*",
    "tools/website/*",
    "tools/tests/*",
    "tools/ci_test_shard.py",
    "tools/ci_test_schedule.json",
    ".claude/*",
    ".github/workflows/ci.yml",
    ".github/workflows/proof.yml",
    ".github/workflows/fuzz.yml",
    ".github/workflows/rust-codegen.yml",
    "README.md",
    "CHANGELOG.md",
    "AGENTS.md",
    "STAN.md",
    "CLAUDE.md",
    "LICENSE",
    ".gitignore",
    ".dockerignore",
    "Dockerfile",
)

# A top-level integration test file that is not a certificate suite. Its
# fixtures and the shared `tests/support/` helpers are not covered by this
# pattern, because certificate suites read them too.
NON_CERT_TEST_FILE = re.compile(r"^tests/(?!cert_)[^/]+\.rs$")

# What `aver compile --pack wasmtime` and its test are made of: the CLI, the
# provider and host runtime, the wasm-gc and component emitters, the Rust
# provider composition, the certificate the pack carries, the runtime crates
# and the locked dependency graph the release host is built from.
PACK_TRIGGERS = (
    "Cargo.toml",
    "Cargo.lock",
    "build.rs",
    "rust-toolchain*",
    ".cargo/*",
    "aver-rt/*",
    "aver-memory/*",
    "aver-cert/*",
    "stdlib/*",
    "wit/*",
    "src/main/*",
    "src/runtime/*",
    "src/provider/*",
    "src/provider_vm_host.rs",
    "src/toolchain_source.rs",
    "src/capability.rs",
    "src/capability/*",
    "src/config.rs",
    "src/config/*",
    "src/stdlib.rs",
    "src/stdlib/*",
    "src/codegen/rust/*",
    "src/codegen/wasm_gc/*",
    "src/codegen/wasip2/*",
    "src/codegen/cert/*",
    "tests/wasmtime_pack_spec.rs",
    "tests/support/*",
    "tests/fixtures/native_provider_*",
    ".github/workflows/ci.yml",
    "tools/ci_pr_gate.py",
)


def _matches(path: str, patterns: Iterable[str]) -> bool:
    return any(fnmatch.fnmatchcase(path, pattern) for pattern in patterns)


def cert_skippable(path: str) -> bool:
    return _matches(path, CERT_SKIP) or bool(NON_CERT_TEST_FILE.match(path))


def gate(name: str, paths: Iterable[str]) -> bool:
    changed = sorted({path.strip() for path in paths if path.strip()})
    if not changed:
        return True
    if name == "cert":
        return not all(cert_skippable(path) for path in changed)
    if name == "pack":
        return any(_matches(path, PACK_TRIGGERS) for path in changed)
    raise SystemExit(f"unknown gate `{name}`")


def main(argv: list[str]) -> int:
    if len(argv) != 2:
        raise SystemExit("usage: ci_pr_gate.py <cert|pack> < changed-paths")
    print("true" if gate(argv[1], sys.stdin.read().splitlines()) else "false")
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv))
