"""The portability report must distinguish proof failure from missing evidence."""

import importlib.util
import json
from pathlib import Path
from tempfile import TemporaryDirectory
import unittest
from unittest.mock import patch

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
        self.assertEqual(spike.outcome(
            {"backend": "dafny", "passed": False, "errors": 0, "timeouts": 0}, 1
        ), "checker_error")

    def test_summary_requires_one_result_from_the_selected_backend(self):
        record = '{"backend":"dafny","passed":true}'
        self.assertIsNone(spike.summary_from_log(record, "lean"))
        self.assertIsNone(spike.summary_from_log(record + "\n" + record, "dafny"))
        self.assertEqual(spike.summary_from_log("compiled\n" + record, "dafny")["passed"], True)

    def test_import_fingerprint_tracks_nested_additions_edits_and_deletions(self):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "main.av"
            source.write_text("entry")
            nested = root / "domain"
            nested.mkdir()
            supplier = nested / "supplier.av"
            supplier.write_text("original supplier")
            baseline = spike.source_hashes(source, root)
            entry_hash = spike.digest(source)
            supplier.write_text("changed supplier")
            self.assertNotEqual(spike.source_hashes(source, root), baseline)
            self.assertEqual(spike.digest(source), entry_hash)
            supplier.write_text("original supplier")
            self.assertEqual(spike.source_hashes(source, root), baseline)
            extra = nested / "new.av"
            extra.write_text("new dependency")
            self.assertNotEqual(spike.source_hashes(source, root), baseline)
            extra.unlink()
            supplier.unlink()
            self.assertNotEqual(spike.source_hashes(source, root), baseline)
            source.unlink()
            self.assertEqual(spike.source_hashes(source, root), {})

    def test_standalone_fingerprint_does_not_include_unrelated_fixtures(self):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            source = root / "main.av"
            source.write_text("entry")
            baseline = spike.source_hashes(source)
            (root / "unrelated.av").write_text("unrelated fixture")
            (root / "notes.md").write_text("non-source metadata")
            self.assertEqual(spike.source_hashes(source), baseline)
            (root / "unrelated.av").unlink()
            self.assertEqual(spike.source_hashes(source, root), baseline)

    def test_runner_rejects_changed_import_even_when_restored_during_second_run(self):
        with TemporaryDirectory() as directory:
            root = Path(directory)
            fixture = root / "fixture"
            fixture.mkdir()
            source = fixture / "main.av"
            source.write_text("unchanged entry")
            supplier = fixture / "supplier.av"
            supplier.write_text("original supplier")
            binary = root / "aver"
            binary.write_text("compiler")
            output = root / "output"
            commands = []

            def run(command, timeout):
                if "proof" not in command:
                    return "test version", 0, False
                commands.append(command)
                backend = command[command.index("--backend") + 1]
                supplier.write_text("changed supplier" if backend == "lean" else "original supplier")
                return json.dumps({"backend": backend, "passed": True, "universal": True}), 0, False

            with patch.multiple(
                spike, ROOT=root, SOURCE_PATHS={"imports": source},
                MODULE_ROOTS={"imports": fixture},
                EXPECTED={"imports": {"lean": "verified", "dafny": "verified"}},
            ), patch.object(spike.shutil, "which", return_value=str(binary)), patch.object(
                spike, "run_process", side_effect=run
            ), patch.object(spike, "claim_inventory", return_value={"laws": ["f.claim"], "obligations": []}), patch(
                "sys.argv", ["spike", "--output", str(output), "--case", "imports"]
            ), patch("builtins.print"):
                self.assertEqual(spike.main(), 1)
            report = json.loads((output / "matrix.json").read_text())
            self.assertEqual([row["outcome"] for row in report["results"]], ["input_changed"] * 2)
            self.assertEqual(report["verified_law_coverage"]["both_backends"], 0)
            for row, command in zip(report["results"], commands):
                self.assertEqual(row["source_sha256"], spike.digest(source))
                self.assertEqual(row["module_root"], "fixture")
                self.assertEqual(row["source_hashes_sha256"], {
                    "fixture/main.av": spike.digest(source),
                    "fixture/supplier.av": spike.digest(supplier),
                })
                self.assertEqual(command[command.index("--module-root") + 1], str(fixture))


if __name__ == "__main__":
    unittest.main()
