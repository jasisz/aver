#!/usr/bin/env python3
"""Keep the certification hardening lanes exhaustive in full CI and named on PRs."""

from __future__ import annotations

import json
import re
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]


def full_matrix(lanes: list[dict]) -> list[dict]:
    """The lanes of a scheduled, manual or release-candidate run."""
    return [lane for lane in lanes if not lane.get("pr_only")]


def pull_request_matrix(lanes: list[dict]) -> list[dict]:
    return [lane for lane in lanes if lane.get("smoke")]


class CertificationLaneTests(unittest.TestCase):
    def setUp(self) -> None:
        self.lanes = json.loads((REPO_ROOT / ".github/cert-lanes.json").read_text())

    def test_hardening_shards_cover_the_whole_suite_in_the_full_matrix(self) -> None:
        lanes = [
            lane for lane in full_matrix(self.lanes)
            if lane["suite"] == "cert_hardening_spec"
        ]
        self.assertGreater(len(lanes), 1, "hardening must not return to one timed-out job")
        partitions = []
        for lane in lanes:
            self.assertEqual(lane["mode"], "nextest")
            self.assertEqual(lane["filter"], "all()")
            self.assertEqual(set(lane["features"].split(",")), {"wasm", "wasip2"})
            index, count = map(int, lane["partition"].split("/"))
            self.assertEqual(count, len(lanes))
            partitions.append(index)
        self.assertEqual(sorted(partitions), list(range(1, len(lanes) + 1)))

    def test_pull_requests_run_named_hardening_cases_that_exist(self) -> None:
        source = (REPO_ROOT / "tests/cert_hardening_spec.rs").read_text()
        lanes = [
            lane for lane in pull_request_matrix(self.lanes)
            if lane["suite"] == "cert_hardening_spec"
        ]
        self.assertTrue(lanes, "pull requests must still run hardening cases")
        named: list[str] = []
        for lane in lanes:
            self.assertTrue(lane.get("pr_only"), "full hardening shards stay off PRs")
            self.assertEqual(set(lane["features"].split(",")), {"wasm", "wasip2"})
            names = re.findall(r"test\(=([A-Za-z0-9_]+)\)", lane["filter"])
            self.assertTrue(names)
            for name in names:
                self.assertRegex(source, rf"\bfn {name}\(")
            named.extend(names)
        self.assertEqual(len(named), len(set(named)), "a case is named by two lanes")
        # The clean certificate is the control every decline is measured against.
        self.assertIn("cert_hardening_accepts_the_clean_certificate", named)

    def test_extra_wasip2_tests_run_once_in_both_matrices(self) -> None:
        for label, selected in (
            ("full", full_matrix(self.lanes)),
            ("pull request", pull_request_matrix(self.lanes)),
        ):
            with self.subTest(matrix=label):
                owners = [lane for lane in selected if lane.get("wasip2_tests")]
                self.assertEqual(len(owners), 1)
                self.assertEqual(owners[0]["suite"], "cert_hardening_spec")
                self.assertIn("wasip2", owners[0]["features"].split(","))


if __name__ == "__main__":
    unittest.main()
