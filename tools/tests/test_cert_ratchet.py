#!/usr/bin/env python3
"""The certificate ratchet names every loss and asks for every gain."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location("cert_ratchet", REPO_ROOT / "tools" / "cert_ratchet.py")
assert SPEC is not None and SPEC.loader is not None
ratchet = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = ratchet
SPEC.loader.exec_module(ratchet)


def program(certified: dict, bridges=(), laws=()) -> dict:
    return {"certified": certified, "bridges": list(bridges), "laws": list(laws)}


class CompareTests(unittest.TestCase):
    def test_equal_state_passes(self) -> None:
        state = {"a.av": program({"f": "L1"}, ["f"], ["f.law"])}
        self.assertEqual(ratchet.compare(state, state), ([], []))

    def test_losses_are_named(self) -> None:
        old = {"a.av": program({"f": "L3", "g": "L1"}, ["f"], ["f.law"])}
        new = {"a.av": program({"f": "L1"})}
        drops, gains = ratchet.compare(old, new)
        self.assertEqual(gains, [])
        self.assertIn("a.av: `g` is no longer certified", drops)
        self.assertIn("a.av: `f` level L3 -> L1", drops)
        self.assertIn("a.av: bridge `f` is gone", drops)
        self.assertIn("a.av: law-claim `f.law` is gone", drops)

    def test_gains_must_be_recorded(self) -> None:
        old = {"a.av": program({"f": "L1"})}
        new = {"a.av": program({"f": "L3", "g": "L1"}), "b.av": program({})}
        drops, gains = ratchet.compare(old, new)
        self.assertEqual(drops, [])
        self.assertEqual(len(gains), 3)

    def test_summary_keeps_names_and_levels_only(self) -> None:
        manifest = {
            "certified": [{"name": "f", "level": "L3", "theorem": "t"}],
            "sourceBridges": [{"export": "f", "theorem": "b"}],
            "laws": [{"label": "M.f.law", "statement": "s"}],
        }
        self.assertEqual(
            ratchet.summarize(manifest),
            {"certified": {"f": "L3"}, "bridges": ["f"], "laws": ["M.f.law"]},
        )


if __name__ == "__main__":
    unittest.main()
