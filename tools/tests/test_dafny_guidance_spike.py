"""The portability report must distinguish proof failure from missing evidence."""

import importlib.util
from pathlib import Path
import unittest

SPEC = importlib.util.spec_from_file_location(
    "dafny_guidance_spike", Path(__file__).resolve().parents[1] / "dafny_guidance_spike.py"
)
assert SPEC is not None and SPEC.loader is not None
spike = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(spike)


class EvidenceTests(unittest.TestCase):
    def test_failed_supplier_file_never_adds_modular_caller_credit(self):
        def row(backend, status, source):
            return {"backend": backend, "outcome": status, "source_sha256": source,
                    "claims": {"laws": ["f.claim"]}}

        counts = spike.coverage([
            row("lean", "verified", "healthy"), row("dafny", "verified", "healthy"),
            row("lean", "verified", "recursive"), row("dafny", "declined", "recursive"),
            row("lean", "failed", "tainted"), row("dafny", "failed", "tainted"),
        ])
        self.assertEqual(counts["both_backends"], 1)
        self.assertEqual(counts["lean_only"], 1)
        self.assertEqual(counts["dafny_only"], 0)

    def test_a_successful_exit_cannot_hide_partial_evidence(self):
        clean = {"backend": "dafny", "passed": True, "errors": 0}
        self.assertEqual(spike.outcome(clean, 0), "verified")
        for extra, expected in [
            ({"errors": 1}, "failed"),
            ({"axioms": 1}, "conditional"),
            ({"omitted": 1}, "conditional"),
            ({"declined": 1}, "declined"),
            ({"timeouts": 1}, "timeout"),
            ({"build_errors": 1}, "checker_error"),
        ]:
            self.assertEqual(spike.outcome(clean | extra, 0), expected)

    def test_sampled_lean_success_is_not_universal_verification(self):
        partial = {"backend": "lean", "passed": True, "universal": False}
        self.assertEqual(spike.outcome(partial, 0), "conditional")
        self.assertEqual(spike.outcome(partial | {"universal": True}, 0), "verified")

    def test_missing_or_failed_harness_does_not_count_as_a_negative_proof(self):
        self.assertEqual(spike.outcome(None, 0), "checker_error")
        self.assertEqual(spike.outcome({"backend": "dafny", "errors": 1}, 2), "checker_error")
        self.assertEqual(spike.outcome(None, -15, True), "timeout")

    def test_summary_requires_one_result_from_the_selected_backend(self):
        record = '{"backend":"dafny","passed":true}'
        self.assertIsNone(spike.summary_from_log(record, "lean"))
        self.assertIsNone(spike.summary_from_log(record + "\n" + record, "dafny"))
        self.assertEqual(spike.summary_from_log("compiled\n" + record, "dafny")["passed"], True)


if __name__ == "__main__":
    unittest.main()
