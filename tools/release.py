#!/usr/bin/env python3
"""
Aver release script.

Usage:
    python3 tools/release.py 0.9.7
    python3 tools/release.py 0.9.7 --dry-run
    python3 tools/release.py 0.9.7 --skip-publish
    python3 tools/release.py 0.9.7 --skip-playground
"""

from __future__ import annotations

import argparse
import datetime
import hashlib
import json
import os
import re
import shutil
import subprocess
import sys
import time
import tomllib
import urllib.error
import urllib.request
from dataclasses import dataclass, field
from enum import Enum
from pathlib import Path
from typing import Callable, Mapping


REPO_ROOT = Path(__file__).resolve().parents[1]

# Cargo.toml files and the version keys to bump in each.
# (file, [(key_pattern, replacement_template)])
CRATE_ORDER = ["aver-rt", "aver-memory", "aver-cert", "aver-lang", "aver-lsp"]

VERSION_FILES = {
    "aver-rt": REPO_ROOT / "aver-rt" / "Cargo.toml",
    "aver-memory": REPO_ROOT / "aver-memory" / "Cargo.toml",
    "aver-cert": REPO_ROOT / "aver-cert" / "Cargo.toml",
    "aver-lang": REPO_ROOT / "Cargo.toml",
    "aver-lsp": REPO_ROOT / "aver-lsp" / "Cargo.toml",
}

RELEASE_PLAN_PATH = REPO_ROOT / "target" / ".aver-release-plan.json"
RELEASE_PLAN_SCHEMA = 2


class ReleaseError(RuntimeError):
    """A release invariant failed before an irreversible step."""


class RegistryKind(Enum):
    FOUND = "found"
    MISSING = "missing"
    UNKNOWN = "unknown"


@dataclass(frozen=True)
class RegistryState:
    kind: RegistryKind
    versions: frozenset[str] = frozenset()
    error: str | None = None
    checksums: Mapping[str, str] = field(default_factory=dict)

    def has(self, version: str) -> bool:
        return version in self.versions

    def checksum(self, version: str) -> str | None:
        return self.checksums.get(version)


@dataclass(frozen=True)
class FirstPublicRelease:
    version: str
    release_tag: str


@dataclass(frozen=True)
class PublishStep:
    crate: str
    version: str
    should_publish: bool


@dataclass
class ReleasePlan:
    requested_main: str
    base_versions: dict[str, str]
    target_versions: dict[str, str]
    planning_fingerprints: dict[str, str]
    fingerprints: dict[str, str]
    publish_states: dict[str, str]
    package_checksums: dict[str, str] = field(default_factory=dict)
    release_commit: str | None = None


def run(
    cmd: list[str],
    cwd: Path = REPO_ROOT,
    check: bool = True,
    capture: bool = False,
    env: Mapping[str, str] | None = None,
) -> subprocess.CompletedProcess:
    if capture:
        return subprocess.run(
            cmd,
            cwd=cwd,
            text=True,
            capture_output=True,
            check=check,
            env=env,
        )
    return subprocess.run(cmd, cwd=cwd, check=check, env=env)


def ci_verified_head() -> tuple[bool, str]:
    """Whether CI already ran the test suite on exactly this commit.

    Only the full `cargo test` run below is skippable this way, and only when
    every workflow that gates `main` reported success for this exact SHA. The
    other release checks stay unconditional: they cover paths CI does not run
    at all, which is how three wasm-gc handler-mode bugs survived from 0.16 to
    0.17.2. Any doubt resolves to running the tests — a slow release is
    cheaper than an unverified one.

    Expect this to decline during a run of merges: the workflows cancel each
    other in progress, so a commit that was overtaken carries `cancelled`
    rather than `success` and the tests run. Releasing from a settled `main`
    is what makes it fire.
    """
    required = {"CI", "Proof", "Certification"}
    try:
        head = run(
            ["git", "rev-parse", "HEAD"], capture=True, check=True
        ).stdout.strip()
        upstream = run(
            ["git", "rev-parse", "origin/main"], capture=True, check=True
        ).stdout.strip()
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False, "cannot read git refs"
    if head != upstream:
        return False, "HEAD is not origin/main"
    try:
        raw = run(
            [
                "gh",
                "run",
                "list",
                "--commit",
                head,
                "--limit",
                "50",
                "--json",
                "name,conclusion,status",
            ],
            capture=True,
            check=True,
        ).stdout
        runs = json.loads(raw)
    except (subprocess.CalledProcessError, FileNotFoundError, ValueError):
        return False, "cannot read CI results for this commit"
    if any(r.get("status") != "completed" for r in runs):
        return False, "CI is still running on this commit"
    if any(r.get("conclusion") == "failure" for r in runs):
        return False, "CI failed on this commit"
    succeeded = {r["name"] for r in runs if r.get("conclusion") == "success"}
    missing = required - succeeded
    if missing:
        return False, f"no successful run of {', '.join(sorted(missing))}"
    return True, f"CI green on {head[:8]}"


def current_version() -> str:
    text = (REPO_ROOT / "Cargo.toml").read_text()
    m = re.search(r'^version\s*=\s*"([^"]+)"', text, re.MULTILINE)
    if not m:
        raise SystemExit("Cannot find version in Cargo.toml")
    return m.group(1)


def bump_toml_version(path: Path, old: str, new: str) -> None:
    """Replace version = "old" with version = "new" in a Cargo.toml."""
    if old == new:
        return
    text = path.read_text()
    pattern = rf'^version\s*=\s*"{re.escape(old)}"\s*$'
    updated, count = re.subn(
        pattern,
        f'version = "{new}"',
        text,
        count=1,
        flags=re.MULTILINE,
    )
    if count != 1:
        raise ReleaseError(
            f"expected one package version {old!r} in {path}, found {count}"
        )
    path.write_text(updated)


def read_exact_dep_pin(path: Path, package_name: str) -> tuple[str, str]:
    """Return `(dependency key, version)` for one exact internal pin."""
    text = path.read_text()
    dependencies = tomllib.loads(text).get("dependencies", {})
    matches: list[tuple[str, Mapping[str, object]]] = []
    for key, spec in dependencies.items():
        if not isinstance(spec, dict):
            continue
        if key == package_name or spec.get("package") == package_name:
            matches.append((key, spec))

    if len(matches) != 1:
        raise ReleaseError(
            f"expected one dependency for package {package_name!r} in {path}, "
            f"found {len(matches)}"
        )

    key, spec = matches[0]
    current = spec.get("version")
    if not isinstance(current, str) or not current.startswith("="):
        raise ReleaseError(
            f"dependency {key!r} in {path} must use an exact version pin"
        )
    return key, current[1:]


def set_dep_pin(path: Path, package_name: str, version: str) -> None:
    """Set exactly one internal dependency's exact pin.

    The dependency key may differ from the package name (`aver` is the key for
    package `aver-lang` in aver-lsp), so use parsed TOML to identify it and a
    line-bounded replacement to preserve the manifest's formatting.
    """
    text = path.read_text()
    key, current_version = read_exact_dep_pin(path, package_name)
    current = f"={current_version}"

    pattern = re.compile(
        rf"^(?P<prefix>{re.escape(key)}\s*=\s*\{{[^\n]*?\bversion\s*=\s*)"
        rf'"{re.escape(current)}"(?P<suffix>[^\n]*\}}\s*)$',
        re.MULTILINE,
    )
    updated, count = pattern.subn(
        lambda m: f'{m.group("prefix")}"={version}"{m.group("suffix")}',
        text,
        count=1,
    )
    if count != 1:
        raise ReleaseError(
            f"could not update the exact pin for dependency {key!r} in {path}"
        )

    parsed = tomllib.loads(updated)
    rewritten = parsed["dependencies"][key]["version"]
    if rewritten != f"={version}":
        raise ReleaseError(f"dependency {key!r} in {path} was not pinned to ={version}")
    if updated != text:
        path.write_text(updated)


def bump_patch(version: str) -> str:
    """Increment patch version: 0.4.1 -> 0.4.2"""
    parts = version.split(".")
    parts[-1] = str(int(parts[-1]) + 1)
    return ".".join(parts)


# Cascade cases where a downstream MUST bump because it carries an exact pin.
# Skipping aver-memory can block `cargo publish aver-lang` on resolution; skipping
# aver-lsp would leave crates.io serving an old exact aver-lang pin even though
# the working manifest was rewritten for this release.
# `aver-lang` resolves both `aver-rt` and `aver-memory`. If `aver-rt`
# bumps but `aver-memory` stays at its already-published version, that
# version still pins the old `aver-rt` exactly — cargo can't satisfy
# both.
PUBLISH_BLOCKERS = {
    "aver-rt": ["aver-memory"],
    "aver-lang": ["aver-lsp"],
}

# `aver-cert` has its own public version line. Its first published package is
# 0.1.0 even when it ships alongside aver-lang 0.27; subsequent source changes
# patch-bump it independently.
FIRST_PUBLIC_RELEASES = {
    "aver-cert": FirstPublicRelease("0.1.0", "v0.27.0"),
}


def git_ref_exists(ref: str) -> bool:
    result = subprocess.run(
        ["git", "rev-parse", "--verify", "--quiet", ref],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
    )
    return result.returncode == 0


def is_publish_input(path: str) -> bool:
    return not path.endswith("Cargo.lock")


def crate_publish_inputs_changed(
    crate: str,
    version: str,
    baseline_ref: str | None = None,
) -> bool:
    """True if publishable crate inputs changed since their release baseline.

    More robust than diffing `last_tag..HEAD`: a crate can change in an earlier
    release window without ever being version-bumped (and thus never published),
    so the published version is stale even though nothing changed since the most
    recent tag. We baseline against "where this crate's version was last set"
    instead, which tracks the last time it was actually (re)published.

    Committed, staged, unstaged, and untracked source files are considered.
    Cargo.lock is generated workspace state, but Cargo.toml is a publish input:
    feature, dependency, and include changes must trigger a crate release.
    """
    toml = VERSION_FILES[crate]
    crate_dir = toml.parent
    if baseline_ref is None:
        baseline_ref = subprocess.run(
            [
                "git",
                "log",
                "-n",
                "1",
                "--format=%H",
                "-S",
                f'version = "{version}"',
                "--",
                str(toml),
            ],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
        ).stdout.strip()
    if not baseline_ref:
        return True  # cannot establish a baseline -> bump to be safe
    committed = (
        subprocess.run(
            [
                "git",
                "diff",
                "--name-only",
                f"{baseline_ref}..HEAD",
                "--",
                str(crate_dir),
            ],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
        )
        .stdout.strip()
        .splitlines()
    )
    working = (
        subprocess.run(
            ["git", "diff", "--name-only", "HEAD", "--", str(crate_dir)],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
        )
        .stdout.strip()
        .splitlines()
    )
    untracked = (
        subprocess.run(
            ["git", "ls-files", "--others", "--exclude-standard", "--", str(crate_dir)],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
        )
        .stdout.strip()
        .splitlines()
    )
    changed = set(committed + working + untracked)
    return any(is_publish_input(path) for path in changed)


def query_registry(
    crate: str,
    *,
    opener: Callable[..., object] | None = None,
) -> RegistryState:
    """Return crates.io state without conflating absence with query failure."""
    if opener is None:
        opener = urllib.request.urlopen
    url = f"https://crates.io/api/v1/crates/{crate}"
    req = urllib.request.Request(
        url,
        headers={"User-Agent": "aver-release-script (https://github.com/jasisz/aver)"},
    )
    try:
        with opener(req, timeout=20) as resp:  # type: ignore[attr-defined]
            data = json.load(resp)
        entries = data.get("versions") if isinstance(data, dict) else None
        if not isinstance(entries, list):
            raise ValueError("crates.io response has no versions list")
        checksums: dict[str, str] = {}
        versions: set[str] = set()
        for entry in entries:
            if not isinstance(entry, dict):
                raise ValueError("crates.io response contains an invalid version")
            version = entry.get("num")
            checksum = entry.get("checksum")
            if not isinstance(version, str):
                raise ValueError("crates.io response contains a version without num")
            versions.add(version)
            if checksum is not None and not isinstance(checksum, str):
                raise ValueError(
                    "crates.io response contains a non-string package checksum"
                )
            if checksum is not None:
                checksums[version] = checksum
        return RegistryState(
            RegistryKind.FOUND,
            frozenset(versions),
            checksums=checksums,
        )
    except urllib.error.HTTPError as exc:
        exc.close()
        if exc.code == 404:
            return RegistryState(RegistryKind.MISSING)
        return RegistryState(
            RegistryKind.UNKNOWN, error=f"HTTP {exc.code}: {exc.reason}"
        )
    except Exception as exc:  # noqa: BLE001 — converted to explicit UNKNOWN
        return RegistryState(RegistryKind.UNKNOWN, error=str(exc))


def require_registry_snapshot(
    *,
    query: Callable[[str], RegistryState] = query_registry,
) -> dict[str, RegistryState]:
    snapshot = {crate: query(crate) for crate in CRATE_ORDER}
    failures = [
        f"{crate}: {state.error or 'unknown registry error'}"
        for crate, state in snapshot.items()
        if state.kind == RegistryKind.UNKNOWN
    ]
    if failures:
        raise ReleaseError(
            "cannot establish crates.io state; refusing to plan a release:\n  "
            + "\n  ".join(failures)
        )
    return snapshot


def _cascade(new: dict[str, str], old_versions: dict[str, str]) -> bool:
    """One pass of the publish-blocker cascade. Returns whether anything bumped."""
    changed = False
    for upstream, blockers in PUBLISH_BLOCKERS.items():
        if new[upstream] == old_versions[upstream]:
            continue
        for ds in blockers:
            if new[ds] == old_versions[ds]:
                new[ds] = bump_patch(old_versions[ds])
                changed = True
    return changed


def compute_new_versions(
    old_versions: dict[str, str],
    new_main: str,
    registry: Mapping[str, RegistryState],
    *,
    changed: Callable[[str, str, str | None], bool] = crate_publish_inputs_changed,
    ref_exists: Callable[[str], bool] = git_ref_exists,
) -> dict[str, str]:
    """Compute new versions for every crate.

    Resume is handled only through a saved, fingerprinted ReleasePlan. Planning
    from manifests that may have been only partly rewritten is unsafe. A fresh
    run never silently changes the requested main version or bumps past an
    occupied subcrate version.
    """
    if old_versions["aver-lang"] == new_main:
        raise ReleaseError(
            f"aver-lang already has requested version {new_main}, but no saved "
            "release plan was loaded; refusing to guess partial-release state"
        )

    if registry["aver-lang"].has(new_main):
        raise ReleaseError(
            f"requested aver-lang {new_main} is already on crates.io; "
            "refusing to invent a different release version"
        )

    new = dict(old_versions)
    new["aver-lang"] = new_main

    # Pass 1: bump any non-main crate whose publish inputs changed since its
    # release baseline. An unpublished first version keeps its declared number.
    for crate in CRATE_ORDER:
        if crate == "aver-lang":
            continue
        old = old_versions[crate]
        first = FIRST_PUBLIC_RELEASES.get(crate)
        baseline: str | None = None
        if first is not None and old == first.version:
            if not registry[crate].has(old):
                continue
            if not ref_exists(first.release_tag):
                raise ReleaseError(
                    f"{crate} {old} exists on crates.io but baseline tag "
                    f"{first.release_tag} is missing; resume the original release"
                )
            baseline = first.release_tag
        if changed(crate, old, baseline):
            new[crate] = bump_patch(old_versions[crate])

    # Pass 2: forced cascade for publish-blockers (fixpoint).
    for _ in range(len(CRATE_ORDER)):
        if not _cascade(new, old_versions):
            break

    # Pass 3: occupied targets are collisions, never an invitation to mutate the
    # planned version behind CHANGELOG/tag/GitHub release metadata.
    for crate in CRATE_ORDER:
        if new[crate] != old_versions[crate] and registry[crate].has(new[crate]):
            raise ReleaseError(
                f"planned {crate} {new[crate]} already exists on crates.io; "
                "choose the version explicitly and rerun"
            )

    return new


def plan_publish(
    target_versions: Mapping[str, str],
    registry: Mapping[str, RegistryState],
) -> list[PublishStep]:
    """Plan each crate from exact target presence, preserving dependency order."""
    unknown = [
        crate for crate in CRATE_ORDER if registry[crate].kind == RegistryKind.UNKNOWN
    ]
    if unknown:
        raise ReleaseError("registry state is unknown for: " + ", ".join(unknown))
    return [
        PublishStep(
            crate,
            target_versions[crate],
            not registry[crate].has(target_versions[crate]),
        )
        for crate in CRATE_ORDER
    ]


def create_release_plan(
    requested_main: str,
    base_versions: Mapping[str, str],
    target_versions: Mapping[str, str],
    registry: Mapping[str, RegistryState],
    *,
    fingerprint: Callable[[str], str] | None = None,
) -> ReleasePlan:
    if fingerprint is None:
        fingerprint = planning_inputs_fingerprint
    return ReleasePlan(
        requested_main=requested_main,
        base_versions=dict(base_versions),
        target_versions=dict(target_versions),
        planning_fingerprints={crate: fingerprint(crate) for crate in CRATE_ORDER},
        fingerprints={},
        publish_states={
            crate: (
                "existing" if registry[crate].has(target_versions[crate]) else "pending"
            )
            for crate in CRATE_ORDER
        },
    )


def save_release_plan(plan: ReleasePlan, path: Path | None = None) -> None:
    if path is None:
        path = RELEASE_PLAN_PATH
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = {
        "schema": RELEASE_PLAN_SCHEMA,
        "requested_main": plan.requested_main,
        "base_versions": plan.base_versions,
        "target_versions": plan.target_versions,
        "planning_fingerprints": plan.planning_fingerprints,
        "fingerprints": plan.fingerprints,
        "publish_states": plan.publish_states,
        "package_checksums": plan.package_checksums,
        "release_commit": plan.release_commit,
    }
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(json.dumps(payload, indent=2, sort_keys=True) + "\n")
    os.replace(temporary, path)


def load_release_plan(
    requested_main: str,
    current_versions: Mapping[str, str],
    path: Path | None = None,
) -> ReleasePlan | None:
    if path is None:
        path = RELEASE_PLAN_PATH
    if not path.exists():
        return None
    try:
        payload = json.loads(path.read_text())
    except (OSError, json.JSONDecodeError) as error:
        raise ReleaseError(f"cannot read saved release plan {path}: {error}") from error

    if payload.get("schema") != RELEASE_PLAN_SCHEMA:
        raise ReleaseError(f"unsupported saved release plan schema in {path}")
    if payload.get("requested_main") != requested_main:
        raise ReleaseError(
            f"saved release plan targets {payload.get('requested_main')}, "
            f"not requested {requested_main}; remove {path} only after auditing it"
        )

    expected_keys = set(CRATE_ORDER)
    dictionaries = {
        name: payload.get(name)
        for name in (
            "base_versions",
            "target_versions",
            "planning_fingerprints",
            "fingerprints",
            "publish_states",
            "package_checksums",
        )
    }
    for name in ("base_versions", "target_versions", "publish_states"):
        value = dictionaries[name]
        if not isinstance(value, dict) or set(value) != expected_keys:
            raise ReleaseError(f"saved release plan has invalid {name}")
    planning_fingerprints = dictionaries["planning_fingerprints"]
    if (
        not isinstance(planning_fingerprints, dict)
        or set(planning_fingerprints) != expected_keys
    ):
        raise ReleaseError("saved release plan has invalid planning_fingerprints")
    fingerprints = dictionaries["fingerprints"]
    if not isinstance(fingerprints, dict) or not set(fingerprints).issubset(
        expected_keys
    ):
        raise ReleaseError("saved release plan has invalid fingerprints")
    package_checksums = dictionaries["package_checksums"]
    if not isinstance(package_checksums, dict) or not set(package_checksums).issubset(
        expected_keys
    ):
        raise ReleaseError("saved release plan has invalid package_checksums")

    states = dictionaries["publish_states"]
    allowed_states = {"existing", "pending", "publishing", "published"}
    if any(
        not isinstance(state, str) or state not in allowed_states
        for state in states.values()
    ):
        raise ReleaseError("saved release plan has an invalid publish state")

    base_versions = dictionaries["base_versions"]
    target_versions = dictionaries["target_versions"]
    if any(
        not isinstance(value, str)
        for versions in (base_versions, target_versions)
        for value in versions.values()
    ) or any(
        not isinstance(value, str)
        for values in (planning_fingerprints, fingerprints, package_checksums)
        for value in values.values()
    ):
        raise ReleaseError("saved release plan contains a non-string value")
    if any(
        re.fullmatch(r"[0-9a-f]{64}", checksum) is None
        for checksum in package_checksums.values()
    ):
        raise ReleaseError("saved release plan contains an invalid package checksum")
    if target_versions["aver-lang"] != requested_main:
        raise ReleaseError(
            "saved release plan does not target the requested aver-lang version"
        )
    for crate in CRATE_ORDER:
        current = current_versions[crate]
        if current not in {base_versions[crate], target_versions[crate]}:
            raise ReleaseError(
                f"local {crate} version {current} matches neither saved base "
                f"{base_versions[crate]} nor target {target_versions[crate]}"
            )

    release_commit = payload.get("release_commit")
    if release_commit is not None and (
        not isinstance(release_commit, str)
        or re.fullmatch(r"[0-9a-f]{40,64}", release_commit) is None
    ):
        raise ReleaseError("saved release plan has an invalid release_commit")

    return ReleasePlan(
        requested_main=requested_main,
        base_versions=dict(base_versions),
        target_versions=dict(target_versions),
        planning_fingerprints=dict(planning_fingerprints),
        fingerprints=dict(fingerprints),
        publish_states=dict(states),
        package_checksums=dict(package_checksums),
        release_commit=release_commit,
    )


def clear_release_plan(path: Path | None = None) -> None:
    if path is None:
        path = RELEASE_PLAN_PATH
    path.unlink(missing_ok=True)


def publish_inputs_fingerprint(
    crate: str, *, include_workspace_lock: bool = True
) -> str:
    """Hash conservative local inputs for resume safety.

    For aver-lang this deliberately covers the whole repository; false-positive
    refusal is preferable to tagging source that differs from an uploaded crate.
    Build output and the saved plan live under ignored `target/` and are absent.
    The workspace Cargo.lock is included for every crate because Cargo packages
    a normalized lockfile into the uploaded archive, including for subcrates.
    """
    crate_dir = VERSION_FILES[crate].parent
    pathspec = "." if crate_dir == REPO_ROOT else str(crate_dir.relative_to(REPO_ROOT))
    result = subprocess.run(
        [
            "git",
            "ls-files",
            "-z",
            "--cached",
            "--others",
            "--exclude-standard",
            "--",
            pathspec,
        ],
        cwd=REPO_ROOT,
        capture_output=True,
        check=True,
    )
    paths = {path.decode() for path in result.stdout.split(b"\0") if path}
    if include_workspace_lock and (REPO_ROOT / "Cargo.lock").is_file():
        paths.add("Cargo.lock")
    digest = hashlib.sha256()
    for relative in sorted(paths):
        path = REPO_ROOT / relative
        if not path.is_file():
            continue
        digest.update(relative.encode())
        digest.update(b"\0")
        digest.update(b"x" if path.stat().st_mode & 0o111 else b"-")
        digest.update(b"\0")
        digest.update(path.read_bytes())
        digest.update(b"\0")
    return digest.hexdigest()


def planning_inputs_fingerprint(crate: str) -> str:
    return publish_inputs_fingerprint(crate, include_workspace_lock=False)


def refresh_unsealed_release_plan(
    plan: ReleasePlan,
    registry: Mapping[str, RegistryState],
    *,
    fingerprint: Callable[[str], str] | None = None,
    persist: bool = True,
) -> None:
    """Recompute targets if source changed before the first irreversible step.

    Version/pin rewrites from an interrupted run are expected, so only crates
    that were previously unchanged and already existed on crates.io need a new
    bump when their own publish inputs changed. Crates already targeted for a
    publish keep that target; an unpublished first release can absorb fixes at
    its still-unoccupied version.
    """
    if plan.fingerprints or plan.release_commit is not None:
        return
    if any(
        state in {"publishing", "published"} for state in plan.publish_states.values()
    ):
        raise ReleaseError(
            "unsealed release plan contains an irreversible publish state"
        )
    if plan.package_checksums:
        raise ReleaseError(
            "unsealed release plan unexpectedly contains package checksums"
        )
    if fingerprint is None:
        fingerprint = planning_inputs_fingerprint
    current_fingerprints = {crate: fingerprint(crate) for crate in CRATE_ORDER}

    def changed(crate: str, _version: str, _baseline: str | None) -> bool:
        return (
            plan.target_versions[crate] != plan.base_versions[crate]
            or current_fingerprints[crate] != plan.planning_fingerprints[crate]
        )

    targets = compute_new_versions(
        plan.base_versions,
        plan.requested_main,
        registry,
        changed=changed,
    )
    plan.target_versions = targets
    plan.planning_fingerprints = current_fingerprints
    plan.publish_states = {
        crate: "existing" if registry[crate].has(targets[crate]) else "pending"
        for crate in CRATE_ORDER
    }
    if persist:
        save_release_plan(plan)


def validate_exact_pins(
    target_versions: Mapping[str, str],
    *,
    dry_run: bool,
) -> None:
    pins = [
        (VERSION_FILES["aver-lang"], "aver-cert", target_versions["aver-cert"]),
        (VERSION_FILES["aver-lang"], "aver-rt", target_versions["aver-rt"]),
        (
            VERSION_FILES["aver-lang"],
            "aver-memory",
            target_versions["aver-memory"],
        ),
        (VERSION_FILES["aver-memory"], "aver-rt", target_versions["aver-rt"]),
        (VERSION_FILES["aver-lsp"], "aver-lang", target_versions["aver-lang"]),
    ]
    for path, package, expected in pins:
        _key, current = read_exact_dep_pin(path, package)
        if current == expected:
            continue
        if dry_run:
            shown = path.relative_to(REPO_ROOT)
            print(f"  [dry-run] {shown}: {package} pin {current} -> {expected}")
            continue
        raise ReleaseError(f"{path} pins {package} to ={current}, expected ={expected}")


def seal_release_plan(plan: ReleasePlan) -> None:
    for crate in CRATE_ORDER:
        current = read_crate_version(crate)
        expected = plan.target_versions[crate]
        if current != expected:
            raise ReleaseError(
                f"cannot seal release plan: {crate} is {current}, expected {expected}"
            )
    validate_exact_pins(plan.target_versions, dry_run=False)
    if not plan.fingerprints:
        changed_unchanged_crates = [
            crate
            for crate in CRATE_ORDER
            if plan.target_versions[crate] == plan.base_versions[crate]
            and plan.publish_states[crate] == "existing"
            and planning_inputs_fingerprint(crate) != plan.planning_fingerprints[crate]
        ]
        if changed_unchanged_crates:
            raise ReleaseError(
                "publish inputs changed after planning for unchanged crates: "
                + ", ".join(changed_unchanged_crates)
                + "; rerun to replan their versions"
            )
    current_fingerprints = {
        crate: publish_inputs_fingerprint(crate) for crate in CRATE_ORDER
    }
    if plan.fingerprints:
        changed = [
            crate
            for crate in CRATE_ORDER
            if plan.fingerprints.get(crate) != current_fingerprints[crate]
        ]
        if changed:
            raise ReleaseError(
                "publish inputs changed since the first release attempt for: "
                + ", ".join(changed)
            )
        return
    plan.fingerprints = current_fingerprints
    save_release_plan(plan)


def bump_all_versions(
    old_versions: dict[str, str], new_versions: dict[str, str], dry_run: bool
) -> None:
    """Bump all crate versions."""
    for crate in CRATE_ORDER:
        path = VERSION_FILES[crate]
        old = old_versions[crate]
        new = new_versions[crate]
        if old == new:
            print(f"  {crate}: {old} (unchanged)")
            continue
        print(
            f"  {crate}: {old} -> {new}"
            if not dry_run
            else f"  [dry-run] {crate}: {old} -> {new}"
        )
        if not dry_run:
            bump_toml_version(path, old, new)

    if not dry_run:
        # Cross-references (dep pins). Set the desired value directly so a rerun
        # repairs a partially updated manifest instead of relying on a global
        # old-version replacement.
        main_toml = VERSION_FILES["aver-lang"]
        set_dep_pin(main_toml, "aver-cert", new_versions["aver-cert"])
        set_dep_pin(main_toml, "aver-rt", new_versions["aver-rt"])
        set_dep_pin(main_toml, "aver-memory", new_versions["aver-memory"])
        mem_toml = VERSION_FILES["aver-memory"]
        set_dep_pin(mem_toml, "aver-rt", new_versions["aver-rt"])
        lsp_toml = VERSION_FILES["aver-lsp"]
        set_dep_pin(lsp_toml, "aver-lang", new_versions["aver-lang"])
    validate_exact_pins(new_versions, dry_run=dry_run)


def read_crate_version(crate: str) -> str:
    path = VERSION_FILES[crate]
    text = path.read_text()
    m = re.search(r'^version\s*=\s*"([^"]+)"', text, re.MULTILINE)
    if not m:
        raise SystemExit(f"Cannot find version in {path}")
    return m.group(1)


def bump_website_version(new_version: str, dry_run: bool) -> None:
    """Bump the version shown on the landing page.

    The editorial homepage carries the full release version in its project
    record. Keep the legacy hero badge as a fallback so the release script
    remains usable while the new homepage is being promoted.
    """
    path = REPO_ROOT / "tools" / "website" / "index.html"
    text = path.read_text()

    project_record_pattern = (
        r"(<dt>\s*Version\s*</dt>\s*<dd>\s*)"
        r"v?\d+(?:\.\d+)+"
        r"(\s*/\s*experimental\s*</dd>)"
    )
    new_text, count = re.subn(
        project_record_pattern,
        lambda m: f"{m.group(1)}{new_version}{m.group(2)}",
        text,
        flags=re.IGNORECASE,
    )
    label = f"website project record -> {new_version}"

    if count == 0:
        major_minor = ".".join(new_version.split(".")[:2])
        short = f"v{major_minor}"
        legacy_badge_pattern = (
            r"(MIT licensed &middot; Written in Rust &middot; )"
            r"v\d+(?:\.\d+)+( &middot;)"
        )
        new_text, count = re.subn(
            legacy_badge_pattern,
            lambda m: f"{m.group(1)}{short}{m.group(2)}",
            text,
        )
        label = f"website hero badge -> {short}"

    if count == 0:
        print("  WARN: website version marker not found, skipping")
        return
    if count > 1:
        raise ReleaseError(
            f"expected one website version marker in {path}, found {count}"
        )

    if dry_run:
        print(f"  [dry-run] {label}")
    else:
        path.write_text(new_text)
        print(f"  Updated {label}")


def regenerate_self_host(dry_run: bool) -> None:
    print("Regenerating self-host...")
    if dry_run:
        print(
            "  [dry-run] would run: aver compile self_hosted/main.av --target rust ..."
        )
        return

    aver_bin = REPO_ROOT / "target" / "debug" / "aver"
    if not aver_bin.exists():
        print("  Building aver first...")
        run(["cargo", "build"])

    run(
        [
            str(aver_bin),
            "compile",
            "self_hosted/main.av",
            "--target",
            "rust",
            "--output",
            "self_hosted/out",
            "--module-root",
            "self_hosted",
            "--with-self-host-support",
            "--guest-entry",
            "runGuestCliProgram",
            "--with-replay",
            "--policy",
            "runtime",
        ]
    )

    # Copy generated src to src/self_host/
    src_dest = REPO_ROOT / "src" / "self_host"
    src_source = REPO_ROOT / "self_hosted" / "out" / "src"
    if src_dest.exists():
        shutil.rmtree(src_dest)
    shutil.copytree(src_source, src_dest)

    # Remove verify.rs (test-only, causes compile errors as [[bin]] in aver-lang)
    verify_rs = src_dest / "verify.rs"
    if verify_rs.exists():
        verify_rs.unlink()

    # Patch main.rs: add clippy::all allow, remove verify module
    main_rs = src_dest / "main.rs"
    text = main_rs.read_text()
    text = text.replace(
        "#![allow(",
        "#![allow(clippy::all, ",
    )
    text = text.replace("\n#[cfg(test)]\nmod verify;\n", "\n")
    main_rs.write_text(text)

    # Format generated code, to a fixed point.
    #
    # One `cargo fmt` is not enough on this input: rustfmt needs a second pass
    # on `domain/eval/core/mod.rs`, where a `return` of a long path call sits
    # right at the width limit and the first pass leaves it unwrapped. Stopping
    # after one pass left a tree that `cargo fmt --all -- --check` rejects, so
    # the regen step handed CI a format failure. Loop until a pass changes
    # nothing, which is the state `--check` is asking about.
    for attempt in range(1, 6):
        run(["cargo", "fmt"])
        if run(["cargo", "fmt", "--all", "--", "--check"], check=False).returncode == 0:
            break
        if attempt == 5:
            raise SystemExit(
                "cargo fmt did not reach a fixed point on the regenerated "
                "self-host after 5 passes"
            )

    print(f"  Copied {sum(1 for _ in src_dest.rglob('*.rs'))} files to src/self_host/")


def regenerate_playground(dry_run: bool) -> None:
    print("Regenerating playground WASM artifacts...")
    if dry_run:
        print("  [dry-run] would run: python3 tools/website/rebuild_playground.py")
        return

    # `rebuild_playground.py` restores the wasm-capable aver bin
    # itself after `wasm-pack` invalidates cargo's feature cache
    # (the script's `main` re-runs `cargo build --release --bin aver
    # --features wasm` between `build_compiler` and `build_wasm`).
    # Pre-build once so the wasm-pack invocation has a warm cache
    # to invalidate — saves a few minutes on cold runs.
    run(["cargo", "build", "--release", "--bin", "aver", "--features", "wasm"])
    run(
        [
            sys.executable,
            str(REPO_ROOT / "tools" / "website" / "rebuild_playground.py"),
        ]
    )


def verify(dry_run: bool) -> None:
    print("Running verification...", flush=True)
    if dry_run:
        print(
            "  [dry-run] would run: release unit tests, package, fmt, clippy, test, bench scenarios, edge compile"
        )
        return

    print("  verify: release tooling unit tests", flush=True)
    run([sys.executable, "tools/tests/test_release.py"])
    print("  verify: cargo package -p aver-cert --all-features", flush=True)
    run(["cargo", "package", "-p", "aver-cert", "--all-features", "--allow-dirty"])
    print("  verify: cargo fmt", flush=True)
    run(["cargo", "fmt"])
    # Skip generated self-host code in clippy (same as CI)
    print("  verify: cargo clippy --workspace (no aver-lang)", flush=True)
    run(
        [
            "cargo",
            "clippy",
            "--workspace",
            "--all-targets",
            "--exclude",
            "aver-lang",
            "--",
            "-D",
            "warnings",
        ]
    )
    print("  verify: cargo clippy -p aver-lang --features wasm,wasip2", flush=True)
    run(
        [
            "cargo",
            "clippy",
            "-p",
            "aver-lang",
            "--lib",
            "--bin",
            "aver",
            "--features",
            "wasm,wasip2",
            "--",
            "-D",
            "warnings",
        ]
    )
    # The certificate integration suites exercise the real subprocess boundary:
    # `aver cert ...` must find the standalone sibling executable. Building
    # aver-lang does not build dependency binaries, so make that executable an
    # explicit release-gate input even from a completely clean target directory.
    print("  verify: cargo build -p aver-cert --bin aver-cert", flush=True)
    run(["cargo", "build", "-p", "aver-cert", "--bin", "aver-cert"])
    # `wasip2` is a separate feature; with `wasm` alone every
    # `#![cfg(feature = "wasip2")]` test file reports 0 tests, so the
    # wasip2 codegen / component-model surface was never gated by the
    # release check. Enable both (they share `wasm-compile`) so the
    # wasip2_* suites actually run.
    skip_tests, why = ci_verified_head()
    if skip_tests:
        print(
            f"  verify: cargo test --features wasm,wasip2 — SKIPPED ({why});"
            " every other release check below still runs",
            flush=True,
        )
    else:
        print(f"  verify: cargo test --features wasm,wasip2 ({why})", flush=True)
        run(["cargo", "test", "--features", "wasm,wasip2"])

    # Bench smoke. Runs every scenario in `bench/scenarios/` end-to-end on
    # the VM target — catches pipeline / VM regressions that the unit tests
    # miss (e.g. a real program that compiles fine but crashes in the
    # bytecode dispatch). The release script doesn't gate on numbers (the
    # CI gate is 0.15.2 work) but the run must succeed; any scenario that
    # errors out blocks the release.
    run(["cargo", "build", "--release", "--bin", "aver", "--features", "wasm"])
    aver_bin = REPO_ROOT / "target" / "release" / "aver"
    run([str(aver_bin), "bench", str(REPO_ROOT / "bench" / "scenarios"), "--json"])

    # `--target wasm-gc --handler X` (and `--preset cloudflare`) smoke.
    # The 0.17.2 release surfaced three compounding wasm-gc handler-mode
    # bugs that lived from 0.16 onward because the path was never gated
    # on a release. Compiling `tools/edge/app.av` with `--preset
    # cloudflare` exercises every codepath the live edge demo touches —
    # handler synthesis, builtin record dedup, data-count snapshot,
    # caller_fn collector — and `wasm-tools validate` proves the bytes
    # pass an external validator (not just our internal one).
    print("  edge: aver compile tools/edge/app.av --preset cloudflare …")
    edge_out = REPO_ROOT / "target" / "release-edge-smoke"
    if edge_out.exists():
        shutil.rmtree(edge_out)
    edge_out.mkdir(parents=True, exist_ok=True)
    run(
        [
            str(aver_bin),
            "compile",
            str(REPO_ROOT / "tools" / "edge" / "app.av"),
            "--preset",
            "cloudflare",
            "--handler",
            "handler",
            "--module-root",
            str(REPO_ROOT / "tools" / "edge"),
            "-o",
            str(edge_out),
        ]
    )
    # External validation — wasmtime's wasm-tools is the same validator
    # the workerd / wasmtime CLI use; if this passes the bytes will
    # instantiate cleanly anywhere wasm-gc + tail-calls are supported.
    if shutil.which("wasm-tools") is not None:
        run(["wasm-tools", "validate", "--features", "all", str(edge_out / "app.wasm")])
    else:
        print("  edge: wasm-tools not on PATH — skipping external validation")
    shutil.rmtree(edge_out)


def retry_probe(
    label: str,
    probe: Callable[[], tuple[bool, str]],
    *,
    delays: tuple[int, ...] = (0, 1, 2, 4, 8, 15, 30, 30, 30, 30),
    sleeper: Callable[[float], None] = time.sleep,
) -> None:
    """Retry a registry/index readiness probe without hiding its last error."""
    detail = "not ready"
    for delay in delays:
        if delay:
            print(f"  waiting {delay}s for {label}...", flush=True)
            sleeper(delay)
        ready, detail = probe()
        if ready:
            return
    raise ReleaseError(f"timed out waiting for {label}: {detail}")


def wait_for_registry_version(
    crate: str,
    version: str,
    *,
    query: Callable[[str], RegistryState] = query_registry,
    sleeper: Callable[[float], None] = time.sleep,
) -> RegistryState:
    latest = RegistryState(RegistryKind.UNKNOWN, error="not queried")

    def probe() -> tuple[bool, str]:
        nonlocal latest
        latest = query(crate)
        if latest.has(version):
            return True, "available"
        return False, latest.error or f"{version} is not visible yet"

    retry_probe(
        f"{crate} {version} in crates.io",
        probe,
        sleeper=sleeper,
    )
    return latest


def wait_for_package_resolution(
    crate: str,
    *,
    runner: Callable[..., subprocess.CompletedProcess] = run,
    sleeper: Callable[[float], None] = time.sleep,
    env: Mapping[str, str] | None = None,
    cwd: Path = REPO_ROOT,
) -> None:
    cmd = [
        "cargo",
        "package",
        "-p",
        crate,
        "--locked",
        "--no-verify",
        "--registry",
        "crates-io",
    ]
    if crate == "aver-lang":
        cmd.extend(["--features", "wasm"])

    def probe() -> tuple[bool, str]:
        result = runner(cmd, cwd=cwd, check=False, capture=True, env=env)
        if result.returncode == 0:
            return True, "package resolves"
        detail = (result.stderr or result.stdout or "cargo package failed").strip()
        return False, detail[-1000:]

    retry_probe(
        f"registry dependencies for {crate}",
        probe,
        sleeper=sleeper,
    )


def release_cargo_env(release_commit: str) -> dict[str, str]:
    timestamp = run(
        ["git", "show", "-s", "--format=%ct", release_commit], capture=True
    ).stdout.strip()
    if not timestamp.isdigit():
        raise ReleaseError(
            f"cannot determine SOURCE_DATE_EPOCH for release commit {release_commit}"
        )
    return {
        **os.environ,
        "SOURCE_DATE_EPOCH": timestamp,
        "CARGO_TARGET_DIR": str(REPO_ROOT / "target"),
    }


def require_clean_release_tree(plan: ReleasePlan) -> None:
    if plan.release_commit is None:
        raise ReleaseError("release tree has not been committed")
    head = run(["git", "rev-parse", "HEAD"], capture=True).stdout.strip()
    if head != plan.release_commit:
        raise ReleaseError(
            f"release HEAD changed: expected {plan.release_commit}, got {head}"
        )
    if run(["git", "status", "--porcelain"], capture=True).stdout.strip():
        raise ReleaseError("release tree is dirty; refusing to package or publish")
    seal_release_plan(plan)


def package_archive_checksum(crate: str, version: str) -> str:
    archive = REPO_ROOT / "target" / "package" / f"{crate}-{version}.crate"
    if not archive.is_file():
        raise ReleaseError(f"cargo package did not create {archive}")
    return hashlib.sha256(archive.read_bytes()).hexdigest()


def require_matching_package_checksum(
    crate: str,
    version: str,
    expected: str | None,
    registry: RegistryState,
) -> None:
    if expected is None:
        raise ReleaseError(
            f"saved release plan has no package checksum for {crate} {version}"
        )
    actual = registry.checksum(version)
    if actual is None:
        raise ReleaseError(
            f"crates.io exposed {crate} {version} without a package checksum"
        )
    if actual != expected:
        raise ReleaseError(
            f"crates.io checksum mismatch for {crate} {version}: "
            f"expected {expected}, got {actual}"
        )


def publish(
    new_versions: dict[str, str],
    initial_registry: Mapping[str, RegistryState],
    plan: ReleasePlan | None,
    dry_run: bool,
) -> None:
    print("Publishing to crates.io...")
    if dry_run:
        for step in plan_publish(new_versions, initial_registry):
            if step.should_publish:
                features = " --features wasm" if step.crate == "aver-lang" else ""
                print(
                    f"  [dry-run] cargo publish -p {step.crate} "
                    f"--locked --registry crates-io{features}"
                )
            else:
                print(f"  {step.crate}: skipped ({step.version} already published)")
        return

    if plan is None:
        raise ReleaseError("real publish requires a saved release plan")
    if plan.release_commit is None:
        raise ReleaseError("real publish requires a committed release tree")
    cargo_env = release_cargo_env(plan.release_commit)
    require_clean_release_tree(plan)

    # Verification can take long enough for registry state to change. Refresh
    # before the first upload. A target that appeared meanwhile is accepted only
    # if this saved plan had already entered publishing/published for that crate.
    live = require_registry_snapshot()
    collisions = [
        f"{crate} {new_versions[crate]}"
        for crate in CRATE_ORDER
        if not initial_registry[crate].has(new_versions[crate])
        and live[crate].has(new_versions[crate])
        and plan.publish_states[crate] not in {"publishing", "published"}
    ]
    if collisions:
        raise ReleaseError(
            "planned versions appeared in crates.io outside this saved release "
            "attempt: " + ", ".join(collisions)
        )

    for step in plan_publish(new_versions, live):
        state = plan.publish_states[step.crate]
        if not step.should_publish:
            if state == "pending":
                raise ReleaseError(
                    f"{step.crate} {step.version} appeared in crates.io before "
                    "this saved plan began publishing it"
                )
            if state in {"publishing", "published"}:
                require_matching_package_checksum(
                    step.crate,
                    step.version,
                    plan.package_checksums.get(step.crate),
                    live[step.crate],
                )
            if state == "publishing":
                plan.publish_states[step.crate] = "published"
                save_release_plan(plan)
            print(f"  {step.crate}: skipped ({step.version} already published)")
            continue

        if state in {"existing", "published"}:
            raise ReleaseError(
                f"saved plan says {step.crate} {step.version} is {state}, but "
                "crates.io does not expose it"
            )

        # This probes Cargo's registry/index view, not only the crates.io API.
        # It closes the propagation race for exact-pinned upstream packages.
        require_clean_release_tree(plan)
        wait_for_package_resolution(step.crate, env=cargo_env)
        require_clean_release_tree(plan)
        checksum = package_archive_checksum(step.crate, step.version)
        planned_checksum = plan.package_checksums.get(step.crate)
        if planned_checksum is not None and planned_checksum != checksum:
            raise ReleaseError(
                f"local package checksum changed for {step.crate} {step.version}: "
                f"expected {planned_checksum}, got {checksum}"
            )
        plan.package_checksums[step.crate] = checksum
        plan.publish_states[step.crate] = "publishing"
        save_release_plan(plan)
        print(f"  Publishing {step.crate} {step.version}...")
        publish_cmd = [
            "cargo",
            "publish",
            "-p",
            step.crate,
            "--locked",
            "--registry",
            "crates-io",
        ]
        if step.crate == "aver-lang":
            publish_cmd.extend(["--features", "wasm"])
        run(
            publish_cmd,
            env=cargo_env,
        )
        require_clean_release_tree(plan)
        live[step.crate] = wait_for_registry_version(step.crate, step.version)
        require_matching_package_checksum(
            step.crate,
            step.version,
            checksum,
            live[step.crate],
        )
        plan.publish_states[step.crate] = "published"
        save_release_plan(plan)


def stamp_changelog_release_date(new_version: str, dry_run: bool) -> None:
    """Replace `## {new_version} "{codename}" (unreleased)` (or the no-
    codename variant) with `## {new_version} "{codename}" — YYYY-MM-DD`.

    Idempotent: if the header already carries a date instead of
    `(unreleased)`, this is a no-op so re-running the release script
    after a partial failure doesn't double-stamp. The codename (if any)
    is preserved verbatim — the regex captures whatever sits between
    the version and `(unreleased)`.
    """
    path = REPO_ROOT / "CHANGELOG.md"
    text = path.read_text()
    today = datetime.date.today().isoformat()
    pattern = rf'^(## {re.escape(new_version)}(?: "[^"]+")?) \(unreleased\)$'
    new_text, n = re.subn(pattern, rf"\1 — {today}", text, count=1, flags=re.MULTILINE)
    if n == 0:
        stamped = re.search(
            rf'^## {re.escape(new_version)}(?: "[^"]+")? — \d{{4}}-\d{{2}}-\d{{2}}$',
            text,
            re.MULTILINE,
        )
        if stamped:
            print(f"  CHANGELOG: `## {new_version}` is already stamped")
            return
        raise ReleaseError(
            f"CHANGELOG has no releasable `## {new_version} ... (unreleased)` header"
        )
    if dry_run:
        print(f"  [dry-run] would stamp CHANGELOG `## {new_version}` with {today}")
        return
    path.write_text(new_text)
    print(f"  CHANGELOG: stamped `## {new_version}` with {today}")


def codename_for(version: str, changelog: str) -> str | None:
    """Find the codename declared on this version's CHANGELOG header.

    Only thematic releases (X.Y.0) carry a codename — patches stay plain
    so the release-list signal stays tight: codename = milestone, no
    codename = bugfix in that era.
    """
    m = re.search(rf'^## {re.escape(version)} "([^"]+)"', changelog, re.MULTILINE)
    return m.group(1) if m else None


def git_commit_release(
    version: str,
    dry_run: bool,
    plan: ReleasePlan | None = None,
) -> None:
    msg = f"Release {version}"
    if dry_run:
        print(f"  [dry-run] git add + commit: {msg}")
        return
    if plan is None or not plan.fingerprints:
        raise ReleaseError("refusing to commit without a sealed release plan")

    status = run(["git", "status", "--porcelain"], capture=True).stdout.strip()
    if status:
        run(["git", "add", "-A"])
        run(["git", "commit", "-m", msg])
    else:
        print("  no release changes to commit; checking saved release commit")

    head = run(["git", "rev-parse", "HEAD"], capture=True).stdout.strip()
    if plan.release_commit is None:
        plan.release_commit = head
        save_release_plan(plan)
    elif plan.release_commit != head:
        raise ReleaseError(
            f"saved release commit is {plan.release_commit}, not current HEAD {head}"
        )
    if run(["git", "status", "--porcelain"], capture=True).stdout.strip():
        raise ReleaseError("release tree is dirty immediately after its commit")
    seal_release_plan(plan)


def git_commit_tag_push(
    version: str,
    dry_run: bool,
    plan: ReleasePlan | None = None,
    *,
    require_published: bool = True,
) -> None:
    tag = f"v{version}"
    changelog = (REPO_ROOT / "CHANGELOG.md").read_text()
    codename = codename_for(version, changelog)
    title = f'Aver {version} "{codename}"' if codename else f"Aver {version}"

    if dry_run:
        print(f"  [dry-run] git tag {tag}")
        print(f"  [dry-run] git push origin main + {tag}")
        print(f"  [dry-run] gh release create {tag} --title {title!r}")
        return

    if plan is None or not plan.fingerprints:
        raise ReleaseError("refusing to tag without a sealed release plan")
    if require_published:
        incomplete = [
            crate
            for crate, state in plan.publish_states.items()
            if state not in {"existing", "published"}
        ]
        if incomplete:
            raise ReleaseError(
                "refusing to tag before every crate is resolved: "
                + ", ".join(incomplete)
            )
    seal_release_plan(plan)
    head = run(["git", "rev-parse", "HEAD"], capture=True).stdout.strip()
    if plan.release_commit != head:
        raise ReleaseError(
            f"release commit changed before tagging: expected {plan.release_commit}, "
            f"got {head}"
        )
    if run(["git", "status", "--porcelain"], capture=True).stdout.strip():
        raise ReleaseError("release tree changed after package publication")

    tag_commit = run(
        ["git", "rev-list", "-n", "1", tag],
        check=False,
        capture=True,
    ).stdout.strip()
    if tag_commit:
        if tag_commit != head:
            raise ReleaseError(
                f"tag {tag} already points to {tag_commit}, not current HEAD {head}"
            )
        print(f"  tag {tag} already points to HEAD; skipping tag")
    else:
        run(["git", "tag", tag])
    run(["git", "push", "origin", "main"])
    run(["git", "push", "origin", tag])

    # Extract release notes for this version's section
    pattern = rf"## {re.escape(version)}.*?\n(.*?)(?=\n## |\Z)"
    m = re.search(pattern, changelog, re.DOTALL)
    notes = m.group(1).strip() if m else f"Release {version}"

    existing = run(
        ["gh", "release", "view", tag, "--json", "tagName"],
        check=False,
        capture=True,
    )
    if existing.returncode == 0:
        print(f"  GitHub release {tag} already exists; skipping create")
        return
    error = (existing.stderr or "").lower()
    if "not found" not in error and "http 404" not in error:
        raise ReleaseError(
            f"could not determine whether GitHub release {tag} exists: "
            f"{existing.stderr.strip()}"
        )
    run(["gh", "release", "create", tag, "--title", title, "--notes", notes])


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Release a new version of Aver")
    parser.add_argument("version", help="New version number (e.g. 0.9.7)")
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="Show what would be done without executing",
    )
    parser.add_argument(
        "--skip-publish", action="store_true", help="Skip crates.io publish"
    )
    parser.add_argument(
        "--skip-playground", action="store_true", help="Skip playground WASM rebuild"
    )
    parser.add_argument(
        "--skip-self-host", action="store_true", help="Skip self-host regeneration"
    )
    parser.add_argument(
        "--deploy-edge",
        action="store_true",
        help="After publish, rebuild tools/edge/dist with --preset cloudflare, "
        "wrangler deploy, and curl-smoke /, /api, /fractal. Requires "
        "wrangler on PATH (or npx + node>=22) and an authenticated "
        "Cloudflare account.",
    )
    return parser.parse_args()


def require_release_branch(dry_run: bool) -> None:
    if dry_run:
        return
    branch = run(["git", "branch", "--show-current"], capture=True).stdout.strip()
    if branch != "main":
        raise ReleaseError(
            f"real releases must run from main, not {branch or 'detached HEAD'}"
        )


def main() -> int:
    args = parse_args()
    new_version = args.version
    dry_run = args.dry_run
    require_release_branch(dry_run)

    if dry_run:
        print(f"=== DRY RUN: release {new_version} ===\n")
    else:
        print(f"=== Releasing {new_version} ===\n")

    # 0. Editor grammars must match editors/keywords.json (sublime + vscode +
    #    playground highlight.js). Drift here means a new keyword shipped to
    #    one place and not the others — block the release.
    print("Checking editor grammar sync...")
    if dry_run:
        print("  [dry-run] would run: python3 editors/sync.py --check")
    else:
        run([sys.executable, "editors/sync.py", "--check"])
    print()

    # 1. Read current versions
    old_versions = {crate: read_crate_version(crate) for crate in CRATE_ORDER}
    print("Current versions:")
    for crate, ver in old_versions.items():
        print(f"  {crate}: {ver}")
    print()

    # 2. Snapshot crates.io before any mutation, then compute a fixed plan.
    # Unknown registry state is a hard stop; absence and query failure are not
    # interchangeable when publish is irreversible.
    print("Reading crates.io state...")
    registry = require_registry_snapshot()
    plan = load_release_plan(new_version, old_versions)
    if plan is not None:
        refresh_unsealed_release_plan(plan, registry, persist=not dry_run)
        new_versions = plan.target_versions
        print(f"  resuming saved release plan from {RELEASE_PLAN_PATH}")
    else:
        new_versions = compute_new_versions(old_versions, new_version, registry)
        if not dry_run:
            plan = create_release_plan(
                new_version,
                old_versions,
                new_versions,
                registry,
            )
            save_release_plan(plan)
            print(f"  saved release plan to {RELEASE_PLAN_PATH}")
    print("\nTarget versions:")
    for crate, ver in new_versions.items():
        changed = " (changed)" if ver != old_versions[crate] else ""
        print(f"  {crate}: {ver}{changed}")
    print("\nBumping versions...")
    bump_all_versions(old_versions, new_versions, dry_run)
    print()

    # 2.5 Bump landing-page version badge
    print("Bumping website badge...")
    bump_website_version(new_version, dry_run)
    print()

    # 3. Regenerate self-host
    if not args.skip_self_host:
        regenerate_self_host(dry_run)
        print()

    # 4. Regenerate playground
    if not args.skip_playground:
        regenerate_playground(dry_run)
        print()

    # 5. Verify
    verify(dry_run)
    print()

    # 5.5 Stamp the CHANGELOG header for this release with today's date.
    #     Done after verify so a failed verification doesn't dirty the
    #     working tree with a half-finished release commit.
    print("Stamping CHANGELOG release date...")
    stamp_changelog_release_date(new_version, dry_run)
    print()

    if not dry_run:
        if plan is None:
            raise ReleaseError("real release lost its saved plan")
        print("Sealing release input fingerprints...")
        seal_release_plan(plan)
        print()

    # 6. Commit the exact tree Cargo will package. This makes
    # `.cargo_vcs_info.json` point at the eventual release tag instead of a
    # stale dirty parent commit.
    print("Committing release tree...")
    git_commit_release(new_version, dry_run, plan)
    print()

    # 7. Publish
    if not args.skip_publish:
        publish(new_versions, registry, plan, dry_run)
        print()

    # 8. Tag, push, GitHub release. Even `--skip-publish` may only finalize if
    # every target already exists; pending crates keep the saved plan resumable.
    git_commit_tag_push(
        new_version,
        dry_run,
        plan,
        require_published=True,
    )
    print()

    # 9. Optional: rebuild + deploy `tools/edge` (Cloudflare Workers
    #    Mandelbrot demo). Live deploy of a public endpoint, so opt-in.
    if args.deploy_edge:
        deploy_edge(dry_run)
        print()

    if not dry_run:
        if plan is None or any(
            state not in {"existing", "published"}
            for state in plan.publish_states.values()
        ):
            raise ReleaseError("refusing to clear an incomplete release plan")
        clear_release_plan()

    print(f"{'[dry-run] ' if dry_run else ''}Done! Released {new_version}")
    return 0


def deploy_edge(dry_run: bool) -> None:
    """Rebuild `tools/edge/dist/` from `tools/edge/app.av` with
    `--preset cloudflare`, `wrangler deploy`, then smoke-test the
    landing / `/api` / `/fractal` endpoints. Mismatched ABI between
    the wasm-gc emit and the worker.js stubs would silently break a
    deploy on prior releases — this step turns that into a
    fail-fast gate."""
    import urllib.request

    print("Deploying tools/edge to Cloudflare...")
    if dry_run:
        print(
            "  [dry-run] would run: aver compile --preset cloudflare, wrangler deploy, curl smoke"
        )
        return

    aver_bin = REPO_ROOT / "target" / "release" / "aver"
    edge_dir = REPO_ROOT / "tools" / "edge"
    dist_dir = edge_dir / "dist"

    print("  rebuild dist/ from app.av...")
    run(
        [
            str(aver_bin),
            "compile",
            str(edge_dir / "app.av"),
            "--preset",
            "cloudflare",
            "--handler",
            "handler",
            "--module-root",
            str(edge_dir),
            "-o",
            str(dist_dir),
        ]
    )

    print("  wrangler deploy...")
    # wrangler 4.x needs Node >=22; if the default node is too old, prefer
    # `~/.nvm/versions/node/v22.*/bin` if present. Fall back to PATH `wrangler`.
    env_path = os.environ.get("PATH", "")
    nvm_dir = Path.home() / ".nvm" / "versions" / "node"
    if nvm_dir.exists():
        v22 = sorted([p for p in nvm_dir.iterdir() if p.name.startswith("v22.")])
        if v22:
            env_path = f"{v22[-1] / 'bin'}:{env_path}"
    deploy_env = {**os.environ, "PATH": env_path}
    subprocess.run(
        ["npx", "wrangler@latest", "deploy"],
        cwd=dist_dir,
        env=deploy_env,
        check=True,
    )

    # Smoke. Endpoint URL comes from wrangler's deploy output, but for the
    # canonical demo it's the production hostname workers.dev assigns.
    # Hard-coded here matches what the worker.js / wrangler.toml shipped
    # in `tools/edge/dist/` declare.
    base = "https://aver-edge-gc-demo.jasisz.workers.dev"
    for path in ("/", "/api", "/fractal"):
        url = base + path
        with urllib.request.urlopen(url, timeout=10) as resp:
            if resp.status != 200:
                raise SystemExit(
                    f"edge smoke: {url} returned HTTP {resp.status} (expected 200)"
                )
            print(
                f"  {url} HTTP {resp.status} ({resp.headers.get('content-length', '?')} bytes)"
            )


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except ReleaseError as error:
        print(f"release blocked: {error}", file=sys.stderr)
        raise SystemExit(1) from None
