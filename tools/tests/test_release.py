#!/usr/bin/env python3
"""Network-free regression tests for the release planner."""

from __future__ import annotations

import contextlib
import importlib.util
import io
import subprocess
import sys
import tempfile
import tomllib
import unittest
import urllib.error
from pathlib import Path
from unittest import mock


REPO_ROOT = Path(__file__).resolve().parents[2]
SPEC = importlib.util.spec_from_file_location(
    "aver_release", REPO_ROOT / "tools" / "release.py"
)
assert SPEC is not None and SPEC.loader is not None
release = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = release
SPEC.loader.exec_module(release)


def versions(*, main: str = "0.26.0") -> dict[str, str]:
    return {
        "aver-rt": "0.4.9",
        "aver-memory": "0.2.11",
        "aver-cert": "0.1.0",
        "aver-lang": main,
        "aver-lsp": "0.6.15",
    }


def registry(**published: set[str]) -> dict[str, object]:
    return {
        crate: release.RegistryState(
            release.RegistryKind.FOUND,
            frozenset(published.get(crate.replace("-", "_"), set())),
        )
        for crate in release.CRATE_ORDER
    }


class RegistryQueryTests(unittest.TestCase):
    def test_found_missing_and_unknown_are_distinct(self) -> None:
        checksum = "a" * 64
        payload = io.BytesIO(
            ('{"versions":[{"num":"0.1.0","checksum":"' + checksum + '"}]}').encode()
        )
        found = release.query_registry(
            "aver-cert", opener=lambda *_args, **_kwargs: payload
        )
        self.assertEqual(found.kind, release.RegistryKind.FOUND)
        self.assertTrue(found.has("0.1.0"))
        self.assertEqual(found.checksum("0.1.0"), checksum)

        def missing(*_args, **_kwargs):
            raise urllib.error.HTTPError("url", 404, "not found", None, None)

        self.assertEqual(
            release.query_registry("aver-cert", opener=missing).kind,
            release.RegistryKind.MISSING,
        )

        def unavailable(*_args, **_kwargs):
            raise OSError("dns unavailable")

        unknown = release.query_registry("aver-cert", opener=unavailable)
        self.assertEqual(unknown.kind, release.RegistryKind.UNKNOWN)
        self.assertIn("dns unavailable", unknown.error or "")

        malformed = release.query_registry(
            "aver-cert", opener=lambda *_args, **_kwargs: io.BytesIO(b"not json")
        )
        self.assertEqual(malformed.kind, release.RegistryKind.UNKNOWN)

        wrong_shape = release.query_registry(
            "aver-cert", opener=lambda *_args, **_kwargs: io.BytesIO(b"{}")
        )
        self.assertEqual(wrong_shape.kind, release.RegistryKind.UNKNOWN)

    def test_unknown_snapshot_fails_closed(self) -> None:
        def query(crate: str):
            if crate == "aver-cert":
                return release.RegistryState(
                    release.RegistryKind.UNKNOWN, error="timeout"
                )
            return release.RegistryState(release.RegistryKind.FOUND)

        with self.assertRaisesRegex(release.ReleaseError, "aver-cert: timeout"):
            release.require_registry_snapshot(query=query)


class VersionPlanningTests(unittest.TestCase):
    def test_first_public_cert_keeps_010_and_changed_memory_bumps(self) -> None:
        state = registry(
            aver_rt={"0.4.9"},
            aver_memory={"0.2.11"},
            aver_lang={"0.26.0"},
            aver_lsp={"0.6.15"},
        )
        state["aver-cert"] = release.RegistryState(release.RegistryKind.MISSING)

        target = release.compute_new_versions(
            versions(),
            "0.27.0",
            state,
            changed=lambda crate, _version, _baseline: crate == "aver-memory",
        )

        self.assertEqual(target["aver-cert"], "0.1.0")
        self.assertEqual(target["aver-memory"], "0.2.12")
        self.assertEqual(target["aver-lang"], "0.27.0")
        self.assertEqual(target["aver-lsp"], "0.6.16")

    def test_saved_plan_repairs_partial_main_before_lsp_write(self) -> None:
        base = versions()
        target = versions(main="0.27.0")
        target["aver-memory"] = "0.2.12"
        target["aver-lsp"] = "0.6.16"
        partial = dict(base)
        partial["aver-memory"] = "0.2.12"
        partial["aver-lang"] = "0.27.0"
        state = registry(
            aver_rt={"0.4.9"},
            aver_memory={"0.2.11"},
            aver_lsp={"0.6.15"},
        )
        plan = release.create_release_plan(
            "0.27.0",
            base,
            target,
            state,
        )
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "plan.json"
            release.save_release_plan(plan, path)
            loaded = release.load_release_plan("0.27.0", partial, path)

        self.assertIsNotNone(loaded)
        assert loaded is not None
        self.assertEqual(loaded.target_versions["aver-lsp"], "0.6.16")
        self.assertEqual(loaded.publish_states["aver-lang"], "pending")

    def test_manifest_resume_without_saved_plan_is_rejected(self) -> None:
        with self.assertRaisesRegex(release.ReleaseError, "no saved release plan"):
            release.compute_new_versions(
                versions(main="0.27.0"),
                "0.27.0",
                registry(),
                changed=lambda *_args: False,
            )

    def test_requested_main_collision_never_auto_bumps(self) -> None:
        state = registry(aver_lang={"0.26.0", "0.27.0"})
        with self.assertRaisesRegex(release.ReleaseError, "already on crates.io"):
            release.compute_new_versions(
                versions(),
                "0.27.0",
                state,
                changed=lambda *_args: False,
            )

    def test_subcrate_collision_is_an_error(self) -> None:
        state = registry(
            aver_memory={"0.2.11", "0.2.12"},
            aver_lang={"0.26.0"},
        )
        with self.assertRaisesRegex(release.ReleaseError, "aver-memory 0.2.12"):
            release.compute_new_versions(
                versions(),
                "0.27.0",
                state,
                changed=lambda crate, *_args: crate == "aver-memory",
            )

    def test_published_first_cert_uses_027_tag_as_future_baseline(self) -> None:
        state = registry(aver_cert={"0.1.0"}, aver_lang={"0.26.0"})
        calls: list[tuple[str, str, str | None]] = []

        def changed(crate: str, version: str, baseline: str | None) -> bool:
            calls.append((crate, version, baseline))
            return crate == "aver-cert"

        target = release.compute_new_versions(
            versions(),
            "0.27.1",
            state,
            changed=changed,
            ref_exists=lambda ref: ref == "v0.27.0",
        )
        self.assertEqual(target["aver-cert"], "0.1.1")
        self.assertIn(("aver-cert", "0.1.0", "v0.27.0"), calls)

    def test_first_cert_without_its_published_baseline_tag_fails(self) -> None:
        state = registry(aver_cert={"0.1.0"}, aver_lang={"0.26.0"})
        with self.assertRaisesRegex(release.ReleaseError, "baseline tag v0.27.0"):
            release.compute_new_versions(
                versions(),
                "0.27.1",
                state,
                changed=lambda *_args: False,
                ref_exists=lambda _ref: False,
            )

    def test_publish_plan_preserves_dependency_order(self) -> None:
        state = registry(aver_rt={"0.4.9"})
        steps = release.plan_publish(versions(main="0.27.0"), state)
        self.assertEqual([step.crate for step in steps], release.CRATE_ORDER)
        self.assertFalse(steps[0].should_publish)
        self.assertTrue(steps[-1].should_publish)

    def test_sealed_plan_rejects_changed_inputs_on_resume(self) -> None:
        current = versions(main="0.27.0")
        plan = release.ReleasePlan(
            requested_main="0.27.0",
            base_versions=versions(),
            target_versions=current,
            planning_fingerprints={
                crate: f"planning-{crate}" for crate in release.CRATE_ORDER
            },
            fingerprints={},
            publish_states={crate: "pending" for crate in release.CRATE_ORDER},
        )
        fingerprints = {crate: f"first-{crate}" for crate in release.CRATE_ORDER}
        with (
            mock.patch.object(
                release, "read_crate_version", side_effect=lambda crate: current[crate]
            ),
            mock.patch.object(release, "validate_exact_pins"),
            mock.patch.object(
                release,
                "publish_inputs_fingerprint",
                side_effect=lambda crate: fingerprints[crate],
            ),
            mock.patch.object(release, "save_release_plan"),
        ):
            release.seal_release_plan(plan)
            fingerprints["aver-cert"] = "changed"
            with self.assertRaisesRegex(release.ReleaseError, "aver-cert"):
                release.seal_release_plan(plan)

    def test_unsealed_resume_bumps_a_newly_changed_existing_crate(self) -> None:
        base = versions()
        target = versions(main="0.27.0")
        target["aver-lsp"] = "0.6.16"
        state = registry(
            aver_rt={"0.4.9"},
            aver_memory={"0.2.11"},
            aver_lang={"0.26.0"},
            aver_lsp={"0.6.15"},
        )
        state["aver-cert"] = release.RegistryState(release.RegistryKind.MISSING)
        fingerprints = {crate: f"before-{crate}" for crate in release.CRATE_ORDER}
        plan = release.create_release_plan(
            "0.27.0",
            base,
            target,
            state,
            fingerprint=lambda crate: fingerprints[crate],
        )
        fingerprints["aver-rt"] = "after-aver-rt"

        with mock.patch.object(release, "save_release_plan") as saver:
            release.refresh_unsealed_release_plan(
                plan,
                state,
                fingerprint=lambda crate: fingerprints[crate],
                persist=False,
            )

        saver.assert_not_called()
        self.assertEqual(plan.target_versions["aver-rt"], "0.4.10")
        self.assertEqual(plan.target_versions["aver-memory"], "0.2.12")
        self.assertEqual(plan.publish_states["aver-rt"], "pending")

    def test_first_seal_rejects_changes_after_the_last_replan(self) -> None:
        current = versions(main="0.27.0")
        plan = release.ReleasePlan(
            requested_main="0.27.0",
            base_versions=versions(),
            target_versions=current,
            planning_fingerprints={
                crate: f"before-{crate}" for crate in release.CRATE_ORDER
            },
            fingerprints={},
            publish_states={
                crate: "existing" if crate == "aver-rt" else "pending"
                for crate in release.CRATE_ORDER
            },
        )

        with (
            mock.patch.object(
                release, "read_crate_version", side_effect=lambda crate: current[crate]
            ),
            mock.patch.object(release, "validate_exact_pins"),
            mock.patch.object(
                release,
                "planning_inputs_fingerprint",
                side_effect=lambda crate: (
                    "changed-aver-rt" if crate == "aver-rt" else f"before-{crate}"
                ),
            ),
        ):
            with self.assertRaisesRegex(release.ReleaseError, "rerun to replan"):
                release.seal_release_plan(plan)

    def test_pending_target_appearing_in_registry_is_not_treated_as_ours(self) -> None:
        target = versions(main="0.27.0")
        initial = registry(aver_rt={"0.4.9"})
        live = registry(
            aver_rt={"0.4.9"},
            aver_memory={"0.2.11"},
            aver_cert={"0.1.0"},
            aver_lang={"0.27.0"},
            aver_lsp={"0.6.15"},
        )
        plan = release.create_release_plan("0.27.0", versions(), target, initial)
        plan.release_commit = "a" * 40
        with (
            mock.patch.object(release, "require_registry_snapshot", return_value=live),
            mock.patch.object(release, "release_cargo_env", return_value={}),
            mock.patch.object(release, "require_clean_release_tree"),
        ):
            with self.assertRaisesRegex(release.ReleaseError, "outside this saved"):
                release.publish(target, initial, plan, dry_run=False)


class ManifestTests(unittest.TestCase):
    def test_exact_pin_update_changes_only_the_named_package(self) -> None:
        manifest = """\
[package]
name = "demo"
version = "1.0.0"

[dependencies]
aver-cert = { path = "cert", version = "=0.1.0", features = ["plans"] }
other = { version = "=0.1.0" }
aver = { path = "..", version = "=0.26.0", package = "aver-lang" }
"""
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "Cargo.toml"
            path.write_text(manifest)
            release.set_dep_pin(path, "aver-cert", "0.1.1")
            release.set_dep_pin(path, "aver-lang", "0.27.0")
            parsed = tomllib.loads(path.read_text())

        self.assertEqual(parsed["dependencies"]["aver-cert"]["version"], "=0.1.1")
        self.assertEqual(parsed["dependencies"]["aver"]["version"], "=0.27.0")
        self.assertEqual(parsed["dependencies"]["other"]["version"], "=0.1.0")

    def test_non_exact_or_missing_dependency_pin_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "Cargo.toml"
            path.write_text(
                '[package]\nname = "demo"\nversion = "1.0.0"\n'
                '[dependencies]\naver-cert = { version = "0.1.0" }\n'
            )
            with self.assertRaisesRegex(release.ReleaseError, "exact version pin"):
                release.set_dep_pin(path, "aver-cert", "0.1.1")
            with self.assertRaisesRegex(release.ReleaseError, "found 0"):
                release.set_dep_pin(path, "aver-lang", "0.27.0")

    def test_manifest_is_a_publish_input_but_lockfile_is_not(self) -> None:
        self.assertTrue(release.is_publish_input("aver-cert/Cargo.toml"))
        self.assertFalse(release.is_publish_input("aver-cert/Cargo.lock"))

    def test_publish_fingerprint_includes_the_executable_bit(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            crate_dir = root / "aver-cert"
            crate_dir.mkdir()
            script = crate_dir / "script"
            script.write_text("same bytes\n")
            manifests = dict(release.VERSION_FILES)
            manifests["aver-cert"] = crate_dir / "Cargo.toml"
            listed = subprocess.CompletedProcess(
                ["git", "ls-files"], 0, stdout=b"aver-cert/script\0", stderr=b""
            )
            with (
                mock.patch.object(release, "REPO_ROOT", root),
                mock.patch.object(release, "VERSION_FILES", manifests),
                mock.patch.object(release.subprocess, "run", return_value=listed),
            ):
                plain = release.publish_inputs_fingerprint(
                    "aver-cert", include_workspace_lock=False
                )
                script.chmod(0o755)
                executable = release.publish_inputs_fingerprint(
                    "aver-cert", include_workspace_lock=False
                )

        self.assertNotEqual(plain, executable)

    def test_producer_feature_wiring_is_exact(self) -> None:
        root = tomllib.loads((REPO_ROOT / "Cargo.toml").read_text())
        cert = tomllib.loads((REPO_ROOT / "aver-cert" / "Cargo.toml").read_text())
        dep = root["dependencies"]["aver-cert"]

        self.assertIn("dep:aver-cert", root["features"]["wasm-compile"])
        self.assertEqual(dep["version"], f"={cert['package']['version']}")
        self.assertFalse(dep["default-features"])
        # The base dependency carries only the plan surface; certificate
        # production is opted into through the root `certify` feature, which
        # also pulls the wasm-gc compile pipeline it certifies so `certify`
        # is a complete, compilable feature combination on its own.
        self.assertEqual(dep["features"], ["plans"])
        self.assertEqual(
            root["features"]["certify"], ["wasm-compile", "aver-cert/producer"]
        )
        self.assertEqual(cert["features"]["producer"], ["engine"])
        # The whole ladder from the slim plan surface up to the producer is
        # pinned exactly: a new edge anywhere in aver-cert's feature graph must
        # show up here before it can widen what any downstream build compiles.
        self.assertEqual(cert["features"]["plans"], ["dep:wasm-encoder"])
        self.assertEqual(
            cert["features"]["engine"], ["plans", "dep:sha2", "dep:wasmparser"]
        )

        def feature_closure(feature: str) -> set[str]:
            seen: set[str] = set()
            stack = [feature]
            while stack:
                name = stack.pop()
                if name in seen:
                    continue
                seen.add(name)
                stack.extend(root["features"].get(name, []))
            return seen

        def forwarded_cert_features(feature: str) -> set[str]:
            # Dependency-feature edges like `aver-cert/producer` (and the weak
            # form `aver-cert?/…`) are the only way a root feature can enable
            # more of aver-cert than the base dependency's `plans` surface.
            return {
                name
                for name in feature_closure(feature)
                if name.startswith("aver-cert/") or name.startswith("aver-cert?/")
            }

        # Publishing builds with `--features wasm`, so the crates.io binary
        # must keep the certificate producer — and nothing else beyond it.
        self.assertEqual(forwarded_cert_features("wasm"), {"aver-cert/producer"})
        # Browser/component builds must stay on the plan surface only: no
        # aver-cert feature at all may be forwarded (not producer, and not a
        # sibling like engine or verify slipping in around it).
        self.assertEqual(forwarded_cert_features("playground"), set())
        self.assertEqual(forwarded_cert_features("wasip2"), set())

    def test_saved_targets_repair_a_main_written_before_lsp(self) -> None:
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            paths = {crate: root / f"{crate}.toml" for crate in release.CRATE_ORDER}
            paths["aver-rt"].write_text(
                '[package]\nname = "aver-rt"\nversion = "0.4.9"\n'
            )
            paths["aver-memory"].write_text(
                '[package]\nname = "aver-memory"\nversion = "0.2.11"\n'
                "[dependencies]\n"
                'aver-rt = { path = "rt", version = "=0.4.9" }\n'
            )
            paths["aver-cert"].write_text(
                '[package]\nname = "aver-cert"\nversion = "0.1.0"\n'
            )
            paths["aver-lang"].write_text(
                '[package]\nname = "aver-lang"\nversion = "0.26.0"\n'
                "[dependencies]\n"
                'aver-cert = { path = "cert", version = "=0.1.0" }\n'
                'aver-memory = { path = "memory", version = "=0.2.11" }\n'
                'aver-rt = { path = "rt", version = "=0.4.9" }\n'
            )
            paths["aver-lsp"].write_text(
                '[package]\nname = "aver-lsp"\nversion = "0.6.15"\n'
                "[dependencies]\n"
                'aver = { path = "..", version = "=0.26.0", '
                'package = "aver-lang" }\n'
            )

            release.bump_toml_version(paths["aver-memory"], "0.2.11", "0.2.12")
            release.bump_toml_version(paths["aver-lang"], "0.26.0", "0.27.0")
            partial = versions(main="0.27.0")
            partial["aver-memory"] = "0.2.12"
            target = dict(partial)
            target["aver-lsp"] = "0.6.16"

            with (
                mock.patch.object(release, "VERSION_FILES", paths),
                contextlib.redirect_stdout(io.StringIO()),
            ):
                release.bump_all_versions(partial, target, dry_run=False)
                lsp_version = release.read_crate_version("aver-lsp")
                _key, lsp_pin = release.read_exact_dep_pin(
                    paths["aver-lsp"], "aver-lang"
                )

        self.assertEqual(lsp_version, "0.6.16")
        self.assertEqual(lsp_pin, "0.27.0")


class PublishIntegrityTests(unittest.TestCase):
    def test_failed_publish_keeps_ambiguous_publishing_state(self) -> None:
        target = versions(main="0.27.0")
        initial = registry(
            aver_rt={"0.4.9"},
            aver_memory={"0.2.11"},
            aver_lang={"0.27.0"},
            aver_lsp={"0.6.15"},
        )
        initial["aver-cert"] = release.RegistryState(release.RegistryKind.MISSING)
        plan = release.create_release_plan(
            "0.27.0",
            versions(),
            target,
            initial,
            fingerprint=lambda crate: f"planning-{crate}",
        )
        plan.fingerprints = {crate: f"sealed-{crate}" for crate in release.CRATE_ORDER}
        plan.release_commit = "a" * 40
        checksum = "b" * 64

        with (
            mock.patch.object(
                release, "require_registry_snapshot", return_value=initial
            ),
            mock.patch.object(release, "release_cargo_env", return_value={}),
            mock.patch.object(release, "require_clean_release_tree"),
            mock.patch.object(release, "wait_for_package_resolution"),
            mock.patch.object(
                release, "package_archive_checksum", return_value=checksum
            ),
            mock.patch.object(release, "save_release_plan"),
            mock.patch.object(
                release,
                "run",
                side_effect=subprocess.CalledProcessError(1, ["cargo", "publish"]),
            ) as runner,
        ):
            with self.assertRaises(subprocess.CalledProcessError):
                release.publish(target, initial, plan, dry_run=False)

        self.assertEqual(plan.publish_states["aver-cert"], "publishing")
        self.assertEqual(plan.package_checksums["aver-cert"], checksum)
        publish_cmd = runner.call_args.args[0]
        self.assertIn("--registry", publish_cmd)
        self.assertIn("crates-io", publish_cmd)
        self.assertNotIn("--allow-dirty", publish_cmd)

    def test_resume_accepts_only_the_staged_package_checksum(self) -> None:
        target = versions(main="0.27.0")
        initial = registry(
            aver_rt={"0.4.9"},
            aver_memory={"0.2.11"},
            aver_lang={"0.27.0"},
            aver_lsp={"0.6.15"},
        )
        initial["aver-cert"] = release.RegistryState(release.RegistryKind.MISSING)
        checksum = "c" * 64
        live = dict(initial)
        live["aver-cert"] = release.RegistryState(
            release.RegistryKind.FOUND,
            frozenset({"0.1.0"}),
            checksums={"0.1.0": checksum},
        )
        plan = release.create_release_plan(
            "0.27.0",
            versions(),
            target,
            initial,
            fingerprint=lambda crate: f"planning-{crate}",
        )
        plan.fingerprints = {crate: f"sealed-{crate}" for crate in release.CRATE_ORDER}
        plan.publish_states["aver-cert"] = "publishing"
        plan.package_checksums["aver-cert"] = checksum
        plan.release_commit = "a" * 40

        with (
            mock.patch.object(release, "require_registry_snapshot", return_value=live),
            mock.patch.object(release, "release_cargo_env", return_value={}),
            mock.patch.object(release, "require_clean_release_tree"),
            mock.patch.object(release, "save_release_plan"),
        ):
            release.publish(target, initial, plan, dry_run=False)
        self.assertEqual(plan.publish_states["aver-cert"], "published")

        plan.publish_states["aver-cert"] = "publishing"
        live["aver-cert"] = release.RegistryState(
            release.RegistryKind.FOUND,
            frozenset({"0.1.0"}),
            checksums={"0.1.0": "d" * 64},
        )
        with (
            mock.patch.object(release, "require_registry_snapshot", return_value=live),
            mock.patch.object(release, "release_cargo_env", return_value={}),
            mock.patch.object(release, "require_clean_release_tree"),
        ):
            with self.assertRaisesRegex(release.ReleaseError, "checksum mismatch"):
                release.publish(target, initial, plan, dry_run=False)


class RetryTests(unittest.TestCase):
    def test_retry_succeeds_without_real_sleep(self) -> None:
        answers = iter([(False, "missing"), (False, "lagging"), (True, "ready")])
        sleeps: list[float] = []
        release.retry_probe(
            "demo",
            lambda: next(answers),
            delays=(0, 1, 2),
            sleeper=sleeps.append,
        )
        self.assertEqual(sleeps, [1, 2])

    def test_retry_timeout_is_a_release_error(self) -> None:
        with self.assertRaisesRegex(release.ReleaseError, "last failure"):
            release.retry_probe(
                "demo",
                lambda: (False, "last failure"),
                delays=(0, 0),
                sleeper=lambda _delay: None,
            )

    def test_aver_lang_resolution_probe_enables_wasm(self) -> None:
        commands: list[list[str]] = []

        def runner(cmd, **_kwargs):
            commands.append(cmd)
            return subprocess.CompletedProcess(cmd, 0, stdout="", stderr="")

        release.wait_for_package_resolution(
            "aver-lang", runner=runner, sleeper=lambda _delay: None
        )
        self.assertIn("--features", commands[0])
        self.assertIn("wasm", commands[0])
        self.assertIn("--registry", commands[0])
        self.assertIn("crates-io", commands[0])
        self.assertNotIn("--allow-dirty", commands[0])


class FinalizationTests(unittest.TestCase):
    def test_real_release_requires_main_but_dry_run_does_not(self) -> None:
        with mock.patch.object(release, "run") as runner:
            release.require_release_branch(True)
        runner.assert_not_called()

        branch = subprocess.CompletedProcess(
            ["git", "branch"], 0, stdout="feature\n", stderr=""
        )
        with mock.patch.object(release, "run", return_value=branch):
            with self.assertRaisesRegex(release.ReleaseError, "must run from main"):
                release.require_release_branch(False)

    def test_tagging_requires_a_sealed_plan(self) -> None:
        with self.assertRaisesRegex(release.ReleaseError, "sealed release plan"):
            release.git_commit_tag_push("0.27.0", False, None)

        plan = release.ReleasePlan(
            requested_main="0.27.0",
            base_versions=versions(),
            target_versions=versions(main="0.27.0"),
            planning_fingerprints={
                crate: f"planning-{crate}" for crate in release.CRATE_ORDER
            },
            fingerprints={crate: "hash" for crate in release.CRATE_ORDER},
            publish_states={crate: "pending" for crate in release.CRATE_ORDER},
        )
        with self.assertRaisesRegex(release.ReleaseError, "before every crate"):
            release.git_commit_tag_push("0.27.0", False, plan)

    def test_tagging_rechecks_fingerprints_after_publish(self) -> None:
        plan = release.ReleasePlan(
            requested_main="0.27.0",
            base_versions=versions(),
            target_versions=versions(main="0.27.0"),
            planning_fingerprints={
                crate: f"planning-{crate}" for crate in release.CRATE_ORDER
            },
            fingerprints={crate: "hash" for crate in release.CRATE_ORDER},
            publish_states={crate: "published" for crate in release.CRATE_ORDER},
            release_commit="a" * 40,
        )
        with (
            mock.patch.object(
                release,
                "seal_release_plan",
                side_effect=release.ReleaseError("publish inputs changed"),
            ),
            mock.patch.object(release, "run") as runner,
        ):
            with self.assertRaisesRegex(release.ReleaseError, "inputs changed"):
                release.git_commit_tag_push("0.27.0", False, plan)
        runner.assert_not_called()


if __name__ == "__main__":
    unittest.main()
