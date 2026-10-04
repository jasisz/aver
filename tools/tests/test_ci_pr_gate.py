#!/usr/bin/env python3
"""The pull-request path gates run a lane whenever they are unsure."""

from __future__ import annotations

import importlib.util
import sys
import unittest
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "ci_pr_gate", REPO_ROOT / "tools" / "ci_pr_gate.py"
)
assert SPEC is not None and SPEC.loader is not None
gates = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = gates
SPEC.loader.exec_module(gates)


class CertGateTests(unittest.TestCase):
    def test_documentation_and_other_suites_skip_certification(self) -> None:
        self.assertFalse(
            gates.gate(
                "cert",
                [
                    "docs/certification.md",
                    "CHANGELOG.md",
                    "tests/proof_spec.rs",
                    ".github/workflows/proof.yml",
                    "tools/website/index.html",
                ],
            )
        )

    def test_any_path_that_can_reach_the_certificate_runs_it(self) -> None:
        for path in (
            "aver-cert/assets/wall/current/Wall.lean",
            "aver-cert/src/verifier.rs",
            "src/codegen/wasm_gc/body/mod.rs",
            "src/codegen/cert/plan_from_mir.rs",
            "src/codegen/lean/mod.rs",
            "src/ir/mir/mod.rs",
            "stdlib/List.av",
            "Cargo.lock",
            "tests/cert_hardening_spec.rs",
            "tests/support/aver_cmd.rs",
            "tests/fixtures/wasip2_carrierless.av",
            "tools/certkit/fixtures/mutual.av",
            "examples/data/json.av",
            "projects/k5_fdiv/main.av",
            ".github/cert-lanes.json",
            ".github/workflows/cert.yml",
            "some/new/directory/file.rs",
        ):
            with self.subTest(path=path):
                self.assertTrue(gates.gate("cert", ["docs/x.md", path]))

    def test_an_unknown_file_list_runs_everything(self) -> None:
        self.assertTrue(gates.gate("cert", []))
        self.assertTrue(gates.gate("pack", []))


class PackGateTests(unittest.TestCase):
    def test_host_inputs_run_the_pack(self) -> None:
        for path in (
            "Cargo.lock",
            "aver-rt/src/lib.rs",
            "src/main/commands.rs",
            "src/runtime/wasm_gc/bundle.rs",
            "src/provider_vm_host.rs",
            "src/codegen/rust/composition.rs",
            "tests/wasmtime_pack_spec.rs",
            "tests/fixtures/native_provider_composed/main.av",
        ):
            with self.subTest(path=path):
                self.assertTrue(gates.gate("pack", [path]))

    def test_proof_side_changes_leave_the_pack_to_main(self) -> None:
        self.assertFalse(
            gates.gate(
                "pack",
                [
                    "src/codegen/lean/mod.rs",
                    "src/ir/proof_steps/check.rs",
                    "tests/proof_steps_spec.rs",
                    "docs/cli.md",
                ],
            )
        )


if __name__ == "__main__":
    unittest.main()
