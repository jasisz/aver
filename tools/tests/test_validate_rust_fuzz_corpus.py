from __future__ import annotations

import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "validate_rust_fuzz_corpus.py"
SPEC = importlib.util.spec_from_file_location("validate_rust_fuzz_corpus", SCRIPT)
assert SPEC is not None and SPEC.loader is not None
oracle = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = oracle
SPEC.loader.exec_module(oracle)


def encoded_project(files: list[tuple[str, str]], entry_index: int) -> bytes:
    data = bytearray(oracle.MULTIMODULE_MAGIC)
    data.append(len(files))
    for name, body in files:
        name_bytes = name.encode()
        body_bytes = body.encode()
        data.append(len(name_bytes))
        data.extend(name_bytes)
        data.extend(len(body_bytes).to_bytes(2, "little"))
        data.extend(body_bytes)
    data.append(entry_index)
    return bytes(data)


class RustFuzzCorpusOracleTests(unittest.TestCase):
    def test_decodes_dotted_modules_to_loader_paths(self) -> None:
        data = encoded_project(
            [
                ("Domain.Helper", "module Domain.Helper\n"),
                (
                    "Main",
                    "module Main\n    intent = \"entry\"\n    depends [Domain.Helper]\n",
                ),
            ],
            1,
        )
        with tempfile.TemporaryDirectory() as temp:
            decoded = oracle.decode_input(data, Path(temp))
            self.assertIsNotNone(decoded)
            assert decoded is not None
            self.assertEqual(decoded.entry, Path(temp) / "main.av")
            self.assertEqual(
                (Path(temp) / "domain" / "helper.av").read_text(),
                "module Domain.Helper\n",
            )

    def test_candidate_order_is_deterministic_and_deduplicated(self) -> None:
        with tempfile.TemporaryDirectory() as temp:
            corpus = Path(temp)
            (corpus / "z").write_bytes(b"same")
            (corpus / "a").write_bytes(b"same")
            (corpus / "b").write_bytes(b"different")
            (corpus / "multi").write_bytes(encoded_project([("Main", "module Main\n")], 0))
            first = oracle.stable_candidates([corpus], 10)
            second = oracle.stable_candidates([corpus], 10)
            self.assertEqual(first, second)
            self.assertEqual(len(first), 3)
            self.assertTrue(first[0][1].startswith(oracle.MULTIMODULE_MAGIC))


if __name__ == "__main__":
    unittest.main()
