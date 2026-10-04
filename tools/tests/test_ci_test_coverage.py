#!/usr/bin/env python3
"""The feature-gated test coverage check reads cfg gates and workflow steps."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "ci_test_coverage", REPO_ROOT / "tools" / "ci_test_coverage.py"
)
assert SPEC is not None and SPEC.loader is not None
coverage = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = coverage
SPEC.loader.exec_module(coverage)


class RequiredFeatureTests(unittest.TestCase):
    def test_file_and_item_gates_are_read(self) -> None:
        self.assertEqual(coverage.required_features('#![cfg(feature = "wasm")]\n'), {"wasm"})
        self.assertEqual(
            coverage.required_features(
                '#[cfg(unix)]\nfn a() {}\n#[cfg(all(feature = "certify", feature = "wasip2"))]\n'
            ),
            {"wasm", "wasip2"},
        )
        self.assertEqual(
            coverage.required_features('if !cfg!(feature = "wasip2") { return; }\n'),
            {"wasip2"},
        )

    def test_default_and_negated_gates_need_nothing(self) -> None:
        self.assertEqual(coverage.required_features('#![cfg(feature = "runtime")]\n'), set())
        self.assertEqual(
            coverage.required_features('#[cfg(not(feature = "wasm"))]\n'), set()
        )
        self.assertEqual(
            coverage.required_features('// #[cfg(feature = "wasm")] in prose\n'), set()
        )


class WorkflowRunTests(unittest.TestCase):
    def test_loop_members_count_and_comments_do_not(self) -> None:
        text = """
      - name: Test suites
        # alpha_spec is only mentioned here
        run: |
          for test in \\
            beta_spec \\
            gamma_spec
          do
            cargo test -p aver-lang --features wasm --test "$test"
          done
      - name: Default features
        run: cargo test --test delta_spec
"""
        runs = coverage.workflow_runs(
            text, {"alpha_spec", "beta_spec", "gamma_spec", "delta_spec"}
        )
        self.assertEqual(
            sorted(runs),
            [("beta_spec", frozenset({"wasm"})), ("gamma_spec", frozenset({"wasm"}))],
        )

    def test_repository_has_no_unrun_gated_test(self) -> None:
        self.assertEqual(coverage.gaps(), [])


if __name__ == "__main__":
    unittest.main()
