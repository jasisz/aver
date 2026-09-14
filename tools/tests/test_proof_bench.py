"""The benchmark must preserve verifier failures and record incomplete runs."""

import importlib.util
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest


SCRIPT = Path(__file__).resolve().parents[1] / "proof_bench.py"
SPEC = importlib.util.spec_from_file_location("proof_bench", SCRIPT)
bench = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(bench)


class ProofBenchTests(unittest.TestCase):
    def test_lake_wrapper_preserves_arguments_output_and_failure(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            lake = directory / "fake-lake"
            lake.write_text(f"#!{sys.executable}\nimport os, sys\n"
                            "print(repr(sys.argv[1:]))\nprint(os.getcwd())\n"
                            "print('checker failed', file=sys.stderr)\nsys.exit(7)\n")
            lake.chmod(0o755)
            result = subprocess.run(
                [sys.executable, str(SCRIPT), "--lake-wrapper", "env", "lean", "a b.lean"],
                cwd=directory, capture_output=True, text=True,
                env=dict(os.environ, AVER_PROOF_BENCH_RUN=tmp,
                         AVER_PROOF_BENCH_LAKE=str(lake)))
            self.assertEqual(result.returncode, 7)
            self.assertIn("['env', 'lean', 'a b.lean']", result.stdout)
            self.assertIn(str(directory.resolve()), result.stdout)
            self.assertEqual(result.stderr, "checker failed\n")
            row = json.loads((directory / "lake.jsonl").read_text())
            self.assertEqual(row["exit_code"], 7)
            self.assertEqual(row["args"], ["env", "lean", "a b.lean"])
            self.assertGreater(row["elapsed"], 0)
            self.assertEqual((directory / (row["log"] + ".stdout")).read_text(), result.stdout)

    @unittest.skipUnless(os.name == "posix", "process group timeout requires POSIX")
    def test_timeout_is_retained_without_inventing_proof_credit(self):
        with tempfile.TemporaryDirectory() as tmp:
            directory = Path(tmp)
            binary = directory / "fake-aver"
            binary.write_text(f"#!{sys.executable}\nimport time\n"
                              "print('started', flush=True)\ntime.sleep(60)\n")
            binary.chmod(0o755)
            row = bench.run(binary, directory, "single-list", 1, 0.2, os.environ)
            self.assertTrue(row["timed_out"])
            self.assertLess(row["exit_code"], 0)
            self.assertIsNone(row["summary"])
            self.assertIsNone(row["manifest"])
            self.assertEqual(row["lake"], [])
            self.assertEqual(json.loads((directory / "single-list-1/result.json").read_text()), row)


if __name__ == "__main__":
    unittest.main()
