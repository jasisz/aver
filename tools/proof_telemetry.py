#!/usr/bin/env python3
"""Proof telemetry report generator.

Stdlib-only.  The tool separates verifier credit from no-lake emission facts:

* committed proof_manifest.json history is the only source for historical tier
  credit;
* `aver compile --emit-ir-after=law_lower` is the fast source for today's pinned
  ProofStrategy values;
* optional `aver proof` runs are transpile-only and only read generated
  `-- aver:law-class` markers.  The tool never passes `--check` and never runs
  lake.
"""

from __future__ import annotations

import argparse
import collections
import dataclasses
import datetime as _dt
import json
import os
from pathlib import Path
import re
import shutil
import subprocess
import sys
import tempfile
import time
from typing import Iterable


LAW_IR_RE = re.compile(
    r"^- (?P<fn>.+?)::(?P<law>[^\s]+) "
    r"\((?P<strategy>.*), (?P<quantifiers>\d+) quantifier\(s\), "
    r"(?P<premises>\d+) premise\(s\)\)$"
)
LAW_CLASS_RE = re.compile(
    r"-- aver:law-class\s+(?P<theorem>\S+)\s+(?P<class>\S+)"
    r"(?:\s+(?P<law>\S+))?"
)
MODULE_RE = re.compile(r"^\s*module\s+([A-Za-z0-9_.]+)\b")
STRATEGY_RE = re.compile(r"^([A-Za-z0-9_]+)")
FIGURE_RE = re.compile(r"figure:\s*([A-Za-z0-9_]+)")
OP_RE = re.compile(r"op:\s*([A-Za-z0-9_]+)")
HAND_FOR_RE = re.compile(r"\bfor\s+`?([A-Za-z0-9_.]+)`?", re.IGNORECASE)
VERIFY_LAW_RE = re.compile(
    r"verify\s+([A-Za-z0-9_]+)\s+law\s+([A-Za-z0-9_]+)",
    re.IGNORECASE,
)


@dataclasses.dataclass(frozen=True)
class SourceFile:
    path: Path
    module: str


@dataclasses.dataclass
class StrategyRow:
    law_key: str
    display_law: str
    module: str
    source_file: str
    fn: str
    law: str
    strategy: str
    mechanism: str
    raw_strategy: str
    source: str
    quantifiers: int
    premises: int


@dataclasses.dataclass
class ClassTag:
    law_key: str
    display_law: str
    module: str
    class_name: str
    theorem: str
    source: str


@dataclasses.dataclass
class ManifestRecord:
    commit: str
    commit_date: str
    timestamp: int
    path: str
    module: str
    law: str
    tier: str
    theorem: str
    backend: str
    credit: str | None
    strategy: str | None


@dataclasses.dataclass
class HandSidecar:
    law: str
    path: str
    kind: str


def run(
    args: list[str],
    repo: Path,
    timeout: int = 60,
    check: bool = False,
) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        args,
        cwd=str(repo),
        text=True,
        capture_output=True,
        timeout=timeout,
    )
    if check and result.returncode != 0:
        joined = " ".join(args)
        raise RuntimeError(
            f"{joined} failed with exit {result.returncode}\n"
            f"stdout:\n{result.stdout}\nstderr:\n{result.stderr}"
        )
    return result


def git(repo: Path, args: list[str], timeout: int = 60) -> str:
    return run(["git", *args], repo, timeout=timeout, check=True).stdout


def git_ls_files(repo: Path, pathspecs: list[str] | None = None) -> list[str]:
    args = ["ls-files"]
    if pathspecs:
        args.append("--")
        args.extend(pathspecs)
    return [line for line in git(repo, args).splitlines() if line]


def short_commit(repo: Path) -> str:
    return git(repo, ["rev-parse", "--short=12", "HEAD"]).strip()


def current_branch(repo: Path) -> str:
    return git(repo, ["branch", "--show-current"]).strip()


def module_from_source(path: Path) -> str:
    try:
        with path.open("r", encoding="utf-8") as handle:
            for _ in range(80):
                line = handle.readline()
                if not line:
                    break
                match = MODULE_RE.match(line)
                if match:
                    return match.group(1)
    except OSError:
        pass
    return path.stem[:1].upper() + path.stem[1:]


def module_from_lean_path(path: Path, root: Path | None = None) -> str:
    rel = path
    if root is not None:
        try:
            rel = path.relative_to(root)
        except ValueError:
            rel = path
    parts = list(rel.with_suffix("").parts)
    if not parts:
        return path.stem
    if parts[-1] == "AverCommon":
        return "AverCommon"
    return ".".join(parts)


def default_sources(repo: Path) -> list[SourceFile]:
    rels = git_ls_files(
        repo,
        [
            "examples/formal/*.av",
            "projects/k5_fdiv/domain/*.av",
            "projects/k5_fdiv/main.av",
            "projects/k5_fdiv/leafprobe.av",
        ],
    )
    paths = [repo / p for p in sorted(rels)]
    seen: set[Path] = set()
    out: list[SourceFile] = []
    for path in paths:
        if path in seen:
            continue
        seen.add(path)
        out.append(SourceFile(path=path, module=module_from_source(path)))
    return out


def expand_sources(repo: Path, values: list[str]) -> list[SourceFile]:
    if not values:
        return default_sources(repo)
    tracked = set(git_ls_files(repo))
    rels: list[str] = []
    for value in values:
        p = Path(value)
        if p.is_absolute():
            try:
                value_rel = p.resolve().relative_to(repo).as_posix()
            except ValueError:
                continue
        else:
            value_rel = p.as_posix().strip("/")
        if value_rel in tracked and value_rel.endswith(".av"):
            rels.append(value_rel)
            continue
        prefix = value_rel.rstrip("/") + "/"
        rels.extend(
            path
            for path in tracked
            if path.startswith(prefix)
            and path.endswith(".av")
            and "/" not in path[len(prefix) :]
        )
    out = []
    seen: set[Path] = set()
    for rel_path in sorted(rels):
        full = repo / rel_path
        if full in seen:
            continue
        seen.add(full)
        out.append(SourceFile(path=full, module=module_from_source(full)))
    return out


def rel(repo: Path, path: Path) -> str:
    try:
        return str(path.relative_to(repo))
    except ValueError:
        return str(path)


def module_root_args(repo: Path, source: Path) -> list[str]:
    try:
        source.relative_to(repo / "projects/k5_fdiv")
    except ValueError:
        return []
    return ["--module-root", "projects/k5_fdiv"]


def base_strategy(raw: str) -> str:
    match = STRATEGY_RE.match(raw)
    return match.group(1) if match else raw.strip()


def mechanism_name(raw: str) -> str:
    base = base_strategy(raw)
    if base == "FloorDivWindow":
        match = FIGURE_RE.search(raw)
        if match:
            return f"{base}.{match.group(1)}"
    if base in {"Commutative", "Associative", "IdentityElement", "AntiCommutative"}:
        match = OP_RE.search(raw)
        if match:
            return f"{base}.{match.group(1)}"
    return base


def module_source_index(repo: Path) -> dict[str, str]:
    out: dict[str, str] = {}
    for rel_path in sorted(git_ls_files(repo, ["*.av"])):
        module = module_from_source(repo / rel_path)
        out.setdefault(module, rel_path)
    return out


def normalize_fn_label(
    source_module: str,
    fn_label: str,
    known_modules: set[str],
) -> tuple[str, str]:
    if "." not in fn_label:
        return f"{source_module}.{fn_label}", source_module

    prefix, leaf = fn_label.rsplit(".", 1)
    if prefix in known_modules:
        return fn_label, prefix

    longest_known = sorted(
        (module for module in known_modules if fn_label.startswith(f"{module}.")),
        key=len,
        reverse=True,
    )
    if longest_known:
        return fn_label, longest_known[0]

    if source_module == prefix or source_module.endswith(f".{prefix}"):
        return f"{source_module}.{leaf}", source_module

    suffix_matches = [
        module
        for module in known_modules
        if module == prefix or module.endswith(f".{prefix}")
    ]
    if len(suffix_matches) == 1:
        module = suffix_matches[0]
        return f"{module}.{leaf}", module

    module = prefix
    return fn_label, module


def qualify_law(
    source_module: str,
    fn_label: str,
    law_name: str,
    known_modules: set[str],
) -> tuple[str, str, str]:
    fn_key, module = normalize_fn_label(source_module, fn_label, known_modules)
    return f"{fn_key}.{law_name}", module, fn_key


def assert_unique_strategy_keys(rows: Iterable[StrategyRow]) -> None:
    seen: dict[tuple[str, str, str], StrategyRow] = {}
    duplicates: list[str] = []
    for row in rows:
        key = (row.source_file, row.fn, row.law)
        prior = seen.get(key)
        if prior is not None:
            duplicates.append(
                f"{key}: {prior.raw_strategy} from {prior.source} vs "
                f"{row.raw_strategy} from {row.source}"
            )
        else:
            seen[key] = row
    if duplicates:
        raise AssertionError(
            "duplicate proof telemetry rows for (source_file, fn, law):\n"
            + "\n".join(duplicates[:20])
        )


def collect_current_strategies(
    repo: Path,
    aver_bin: str,
    sources: list[SourceFile],
    timeout: int,
) -> tuple[list[StrategyRow], list[str], list[str]]:
    module_sources = module_source_index(repo)
    known_modules = set(module_sources)
    rows_by_identity: dict[tuple[str, str, str], StrategyRow] = {}
    conflicts: list[str] = []
    failures: list[str] = []
    for source in sources:
        source_rel = rel(repo, source.path)
        args = [
            aver_bin,
            "compile",
            source_rel,
            "--emit-ir-after=law_lower",
            *module_root_args(repo, source.path),
        ]
        result = run(args, repo, timeout=timeout)
        if result.returncode != 0:
            msg = (result.stderr or result.stdout).strip().splitlines()
            failures.append(f"{source_rel}: exit {result.returncode}: {msg[:2]}")
            continue
        for line in result.stdout.splitlines():
            match = LAW_IR_RE.match(line.strip())
            if not match:
                continue
            law_name = match.group("law")
            law_key, module, fn_key = qualify_law(
                source.module, match.group("fn"), law_name, known_modules
            )
            source_file = module_sources.get(module, source_rel)
            raw = match.group("strategy").strip()
            row = StrategyRow(
                law_key=law_key,
                display_law=law_key,
                module=module,
                source_file=source_file,
                fn=fn_key,
                law=law_name,
                strategy=base_strategy(raw),
                mechanism=mechanism_name(raw),
                raw_strategy=raw,
                source=source_rel,
                quantifiers=int(match.group("quantifiers")),
                premises=int(match.group("premises")),
            )
            identity = (row.source_file, row.fn, row.law)
            prior = rows_by_identity.get(identity)
            if prior is not None and prior.raw_strategy != row.raw_strategy:
                conflicts.append(
                    f"{law_key}: {prior.raw_strategy} from {prior.source} vs "
                    f"{row.raw_strategy} from {row.source}"
                )
                continue
            rows_by_identity.setdefault(identity, row)
    rows = sorted(rows_by_identity.values(), key=lambda r: (r.source_file, r.fn, r.law))
    assert_unique_strategy_keys(rows)
    return rows, failures, conflicts


def parse_class_tags_in_dir(root: Path, source_label: str) -> list[ClassTag]:
    tags: dict[str, ClassTag] = {}
    for lean_path in sorted(root.rglob("*.lean")):
        try:
            text = lean_path.read_text(encoding="utf-8")
        except OSError:
            continue
        module = module_from_lean_path(lean_path, root)
        if module == "AverCommon":
            continue
        for line in text.splitlines():
            match = LAW_CLASS_RE.search(line)
            if not match:
                continue
            law = match.group("law") or match.group("theorem")
            law_key = law if law.startswith(f"{module}.") else f"{module}.{law}"
            tags[law_key] = ClassTag(
                law_key=law_key,
                display_law=law_key,
                module=module,
                class_name=match.group("class"),
                theorem=match.group("theorem"),
                source=source_label,
            )
    return list(tags.values())


def collect_committed_lean_tags(repo: Path) -> list[ClassTag]:
    root_tags: dict[str, ClassTag] = {}
    for rel_path in sorted(git_ls_files(repo, ["*.lean"])):
        path = repo / rel_path
        for tag in parse_class_tags_in_file(repo, path):
            root_tags.setdefault(tag.law_key, tag)
    return sorted(root_tags.values(), key=lambda t: t.law_key)


def parse_class_tags_in_file(repo: Path, path: Path) -> list[ClassTag]:
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return []
    tags: list[ClassTag] = []
    try:
        module_path = path.relative_to(repo)
    except ValueError:
        module_path = path
    module = module_from_lean_path(module_path)
    source_label = rel(repo, path)
    for line in text.splitlines():
        match = LAW_CLASS_RE.search(line)
        if not match:
            continue
        law = match.group("law") or match.group("theorem")
        law_key = law if law.startswith(f"{module}.") else f"{module}.{law}"
        tags.append(
            ClassTag(
                law_key=law_key,
                display_law=law_key,
                module=module,
                class_name=match.group("class"),
                theorem=match.group("theorem"),
                source=source_label,
            )
        )
    return tags


def collect_transpiled_lean_tags(
    repo: Path,
    aver_bin: str,
    sources: list[SourceFile],
    timeout: int,
) -> tuple[list[ClassTag], list[str], float]:
    tags_by_law: dict[str, ClassTag] = {}
    failures: list[str] = []
    start = time.monotonic()
    with tempfile.TemporaryDirectory(prefix="aver-proof-telemetry-") as tmp:
        tmp_root = Path(tmp)
        for source in sources:
            source_rel = rel(repo, source.path)
            out_dir = tmp_root / re.sub(r"[^A-Za-z0-9_.-]+", "_", source_rel)
            args = [
                aver_bin,
                "proof",
                source_rel,
                "-o",
                str(out_dir),
                *module_root_args(repo, source.path),
            ]
            result = run(args, repo, timeout=timeout)
            if result.returncode != 0:
                msg = (result.stderr or result.stdout).strip().splitlines()
                failures.append(f"{source_rel}: exit {result.returncode}: {msg[:2]}")
                continue
            for tag in parse_class_tags_in_dir(out_dir, f"local transpile: {source_rel}"):
                tags_by_law.setdefault(tag.law_key, tag)
    return sorted(tags_by_law.values(), key=lambda t: t.law_key), failures, time.monotonic() - start


def collect_manifest_history(repo: Path) -> tuple[list[ManifestRecord], int, int, list[str]]:
    log = run(
        [
            "git",
            "log",
            "--all",
            "--format=COMMIT\t%H\t%ct\t%cs",
            "--name-only",
            "--",
            ":(glob)**/proof_manifest.json",
            "proof_manifest.json",
        ],
        repo,
        timeout=60,
    )
    if log.returncode != 0:
        return [], 0, 0, [log.stderr.strip() or log.stdout.strip()]

    records: list[ManifestRecord] = []
    errors: list[str] = []
    commit = ""
    timestamp = 0
    date = ""
    commits_seen: set[str] = set()
    manifest_paths: set[str] = set()
    for raw_line in log.stdout.splitlines():
        line = raw_line.strip()
        if not line:
            continue
        if line.startswith("COMMIT\t"):
            _, commit, ts_s, date = line.split("\t", 3)
            timestamp = int(ts_s)
            commits_seen.add(commit)
            continue
        if not line.endswith("proof_manifest.json"):
            continue
        manifest_paths.add(line)
        show = run(["git", "show", f"{commit}:{line}"], repo, timeout=30)
        if show.returncode != 0:
            errors.append(f"{commit[:12]}:{line}: {show.stderr.strip()}")
            continue
        try:
            payload = json.loads(show.stdout)
        except json.JSONDecodeError as exc:
            errors.append(f"{commit[:12]}:{line}: JSON parse failed: {exc}")
            continue
        for item in payload.get("laws", []):
            if not isinstance(item, dict):
                continue
            law = str(item.get("law", ""))
            if not law:
                continue
            strategy = item.get("strategy")
            if strategy is not None:
                strategy = str(strategy)
            credit = item.get("credit")
            if credit is not None:
                credit = str(credit)
            records.append(
                ManifestRecord(
                    commit=commit,
                    commit_date=date,
                    timestamp=timestamp,
                    path=line,
                    module=manifest_module(line),
                    law=law,
                    tier=str(item.get("tier", "")),
                    theorem=str(item.get("theorem", "")),
                    backend=str(item.get("backend", payload.get("backend", ""))),
                    credit=credit,
                    strategy=strategy,
                )
            )
    return records, len(commits_seen), len(manifest_paths), errors


def manifest_module(path: str) -> str:
    parts = Path(path).parts
    if len(parts) <= 1:
        return "<root>"
    parent = Path(*parts[:-1])
    name = parent.name
    if name in {"out", "lean", "proofs", "proof"} and len(parent.parts) > 1:
        return parent.parent.as_posix()
    return parent.as_posix()


def collect_head_manifest_count(repo: Path) -> int:
    out = git(repo, ["ls-tree", "-r", "--name-only", "HEAD"])
    return sum(1 for line in out.splitlines() if line.endswith("proof_manifest.json"))


def collect_hand_sidecars(repo: Path) -> list[HandSidecar]:
    sidecars: dict[str, HandSidecar] = {}
    for rel_path in sorted(git_ls_files(repo, ["*.lean", "*.dfy"])):
        path = repo / rel_path
        try:
            head = "\n".join(path.read_text(encoding="utf-8").splitlines()[:40])
        except OSError:
            continue
        lower = head.lower()
        if "hand proof" not in lower and "manual companion proof" not in lower:
            continue
        verify_match = VERIFY_LAW_RE.search(head)
        if verify_match:
            law = f"{verify_match.group(1)}.{verify_match.group(2)}"
        else:
            match = HAND_FOR_RE.search(head)
            if match:
                law = match.group(1).strip(".")
            else:
                law = path.stem.replace("__", ".")
        kind = "hand-credit" if "credit `hand`" in lower or "hand proof" in lower else "manual sidecar"
        sidecars[rel(repo, path)] = HandSidecar(law=law, path=rel(repo, path), kind=kind)
    return list(sidecars.values())


def class_to_tier(class_name: str) -> str:
    if class_name == "bounded-domain":
        return "bounded"
    return class_name


def trigger_counts(rows: Iterable[StrategyRow]) -> tuple[collections.Counter[str], dict[str, list[StrategyRow]]]:
    grouped: dict[str, list[StrategyRow]] = collections.defaultdict(list)
    for row in rows:
        grouped[row.strategy].append(row)
    counts = collections.Counter({strategy: len(items) for strategy, items in grouped.items()})
    return counts, grouped


def mechanism_counts(rows: Iterable[StrategyRow]) -> collections.Counter[str]:
    return collections.Counter(row.mechanism for row in rows)


def tier_counts_by_module(records: Iterable[ManifestRecord]) -> dict[tuple[str, str, str], collections.Counter[str]]:
    out: dict[tuple[str, str, str], collections.Counter[str]] = {}
    for rec in records:
        key = (rec.commit_date, rec.commit[:12], rec.module)
        out.setdefault(key, collections.Counter())[rec.tier] += 1
    return out


def current_class_counts(tags: Iterable[ClassTag]) -> dict[str, collections.Counter[str]]:
    out: dict[str, collections.Counter[str]] = {}
    for tag in tags:
        out.setdefault(tag.module, collections.Counter())[class_to_tier(tag.class_name)] += 1
    return out


def historical_reuse_curve(records: Iterable[ManifestRecord]) -> list[tuple[str, str, str, int]]:
    attributed = [
        rec
        for rec in records
        if rec.tier == "universal" and rec.strategy and rec.credit != "hand"
    ]
    attributed.sort(key=lambda r: (r.timestamp, r.commit, r.path, r.law))
    seen: dict[str, set[str]] = collections.defaultdict(set)
    curve: list[tuple[str, str, str, int]] = []
    last_point: tuple[str, str] | None = None
    for rec in attributed:
        seen[rec.strategy].add(f"{rec.path}:{rec.law}")
        point = (rec.commit_date, rec.commit[:12])
        if point != last_point:
            last_point = point
            for strategy, laws in sorted(seen.items()):
                curve.append((point[0], point[1], strategy, len(laws)))
    return curve


def md_table(headers: list[str], rows: list[list[object]]) -> list[str]:
    lines = []
    lines.append("| " + " | ".join(headers) + " |")
    lines.append("| " + " | ".join(["---"] * len(headers)) + " |")
    if not rows:
        lines.append("| " + " | ".join(["(none)"] + [""] * (len(headers) - 1)) + " |")
        return lines
    for row in rows:
        cells = []
        for item in row:
            text = str(item)
            text = text.replace("|", "\\|").replace("\n", "<br>")
            cells.append(text)
        lines.append("| " + " | ".join(cells) + " |")
    return lines


def render_report(
    *,
    repo: Path,
    elapsed: float,
    branch: str,
    commit: str,
    head_manifest_count: int,
    manifest_records: list[ManifestRecord],
    manifest_commits: int,
    manifest_paths: int,
    manifest_errors: list[str],
    strategy_rows: list[StrategyRow],
    strategy_failures: list[str],
    strategy_conflicts: list[str],
    committed_tags: list[ClassTag],
    transpiled_tags: list[ClassTag],
    transpile_failures: list[str],
    transpile_elapsed: float,
    hand_sidecars: list[HandSidecar],
    lean_tag_sources: list[str],
    no_current: bool,
    current_source_count: int,
) -> str:
    now = _dt.date.today().isoformat()
    all_tags: dict[str, ClassTag] = {tag.law_key: tag for tag in committed_tags}
    for tag in transpiled_tags:
        all_tags[tag.law_key] = tag
    tags = sorted(all_tags.values(), key=lambda t: t.law_key)
    attributed_manifest = [r for r in manifest_records if r.strategy]
    concrete_rows = [r for r in strategy_rows if r.strategy not in {"BackendDispatch", "Sorry"}]
    trigger_counter, trigger_grouped = trigger_counts(strategy_rows)
    mech_counter = mechanism_counts(strategy_rows)
    class_counts = current_class_counts(tags)
    manifest_tiers = tier_counts_by_module(manifest_records)
    reuse_curve = historical_reuse_curve(manifest_records)

    lines: list[str] = []
    lines.append("# Proof Telemetry Report")
    lines.append("")
    lines.append(f"Date: {now}")
    lines.append(f"Repo: `{repo}`")
    lines.append(f"Branch: `{branch}`")
    lines.append(f"Input commit: `{commit}`")
    lines.append(f"Tool runtime: {elapsed:.2f}s")
    lines.append("")
    lines.append("## How To Run")
    lines.append("Run from the repository root after building the local `aver` binary:")
    lines.append("")
    lines.append("```bash")
    lines.append("RUSTC_WRAPPER= cargo build --bin aver")
    lines.append("python3 tools/proof_telemetry.py --aver-bin target/debug/aver --output /tmp/proof-telemetry.md")
    lines.append("```")
    lines.append("")
    lines.append(
        "Add `--lean-tags-for <path>` to collect optional no-lake Lean class tags. "
        "The committed report uses `examples/formal` and `projects/k5_fdiv/domain/round.av`; "
        "full K5 Lean tag export is intentionally not part of the default fast path."
    )
    lines.append("")
    lines.append("Fast smoke check:")
    lines.append("")
    lines.append("```bash")
    lines.append("python3 tools/proof_telemetry.py --no-current >/tmp/proof-telemetry-smoke.md")
    lines.append("python3 -m py_compile tools/proof_telemetry.py")
    lines.append("```")
    lines.append("")
    lines.append("## Summary")
    lines.extend(
        md_table(
            ["metric", "value"],
            [
                ["HEAD proof_manifest.json files", head_manifest_count],
                ["manifest-history commits with files", manifest_commits],
                ["manifest-history paths", manifest_paths],
                ["manifest law records", len(manifest_records)],
                ["manifest records with strategy field", len(attributed_manifest)],
                ["current source files requested", "skipped" if no_current else current_source_count],
                ["current source files with law rows", len({r.source for r in strategy_rows})],
                ["current distinct law strategy rows", len(strategy_rows)],
                ["current concrete strategy pins", len(concrete_rows)],
                ["current BackendDispatch pins", trigger_counter.get("BackendDispatch", 0)],
                ["current Lean class tags collected", len(tags)],
                ["hand/manual Lean sidecars", len(hand_sidecars)],
            ],
        )
    )
    lines.append("")
    lines.append("## Data Recoverability")
    if manifest_records:
        lines.append(
            "Committed manifests are present, but current manifest records do not "
            "necessarily carry strategy attribution unless the `strategy` field is present."
        )
    else:
        lines.append(
            "No committed `proof_manifest.json` files were found in `git log --all` for this clone."
        )
    lines.append(
        "The current manifest schema observable in the repo records `law`, `backend`, "
        "`tier`, `axioms`, `theorem`, optional `open_goal`, and optional `credit`; "
        "it does not record `ProofStrategy` today."
    )
    if strategy_rows:
        lines.append(
            "Today's strategy attribution is recoverable without lake through "
            "`aver compile <file> --emit-ir-after=law_lower`."
        )
    if lean_tag_sources:
        lines.append(
            "Today's Lean class tags were collected by transpiling only, via "
            "`aver proof <file> -o <tmp>` without `--check`; no `lake` command was run."
        )
        lines.append("Lean tag requested roots: " + ", ".join(f"`{s}`" for s in lean_tag_sources))
        lines.append(f"Lean tag transpile runtime: {transpile_elapsed:.2f}s")
    else:
        lines.append(
            "No local Lean class transpile roots were requested; only committed `.lean` files were scanned."
        )
    lines.append("")
    lines.append("## Reuse Curve From Manifest History")
    if reuse_curve:
        rows = [[d, c, s, n] for d, c, s, n in reuse_curve[-80:]]
        lines.extend(md_table(["date", "commit", "strategy", "distinct universal laws"], rows))
    else:
        lines.append(
            "No strategy-attributed reuse curve is recoverable from committed manifest history."
        )
        lines.extend(
            md_table(
                ["committed manifest records", "records with strategy", "curve rows"],
                [[len(manifest_records), len(attributed_manifest), 0]],
            )
        )
    lines.append("")
    lines.append("## Today's Trigger Count Ranking")
    lines.append(
        "This is a no-lake classifier ranking: it counts distinct laws pinned to each "
        "`ProofStrategy` today. It is not a kernel-credit table."
    )
    trigger_rows = []
    for strategy, count in sorted(trigger_counter.items(), key=lambda kv: (-kv[1], kv[0])):
        examples = ", ".join(r.display_law for r in sorted(trigger_grouped[strategy], key=lambda r: r.law_key)[:3])
        trigger_rows.append([strategy, count, examples])
    lines.extend(md_table(["strategy", "distinct laws", "examples"], trigger_rows))
    lines.append("")
    lines.append("## Trigger Count 1 Rows")
    one_rows = []
    for strategy, items in sorted(trigger_grouped.items()):
        if len(items) == 1:
            item = sorted(items, key=lambda r: r.law_key)[0]
            one_rows.append([strategy, item.display_law, item.source])
    lines.extend(md_table(["strategy", "law", "source"], one_rows))
    lines.append("")
    lines.append("## Mechanism Detail")
    mech_rows = [
        [mech, count]
        for mech, count in sorted(mech_counter.items(), key=lambda kv: (-kv[1], kv[0]))
    ]
    lines.extend(md_table(["mechanism", "distinct laws"], mech_rows))
    lines.append("")
    lines.append("## Current Emitted Class Counts")
    lines.append(
        "These counts come from `-- aver:law-class` markers in committed Lean plus requested "
        "local no-lake proof export. They are statement classes, not `lake` credit."
    )
    class_rows = []
    for module, counter in sorted(class_counts.items()):
        class_rows.append(
            [
                module,
                counter.get("universal", 0),
                counter.get("bounded", 0),
                sum(counter.values()),
            ]
        )
    lines.extend(md_table(["module", "universal", "bounded", "total tags"], class_rows))
    lines.append("")
    lines.append("## Tier Evolution Per Module")
    if manifest_tiers:
        rows = []
        for (date, commit_s, module), counter in sorted(manifest_tiers.items())[-120:]:
            rows.append(
                [
                    date,
                    commit_s,
                    module,
                    counter.get("universal", 0),
                    counter.get("bounded", 0),
                    counter.get("sampled", 0),
                    counter.get("failed", 0),
                ]
            )
        lines.extend(md_table(["date", "commit", "module", "universal", "bounded", "sampled", "failed"], rows))
    else:
        lines.append(
            "No per-module tier evolution is recoverable because no committed manifest history was found."
        )
        lines.extend(md_table(["module-history rows", "universal", "bounded"], [[0, 0, 0]]))
    lines.append("")
    lines.append("## Hand-Credit And Manual Laws")
    hand_rows = [[s.law, s.kind, s.path] for s in sorted(hand_sidecars, key=lambda s: s.path)]
    lines.extend(md_table(["law", "kind", "path"], hand_rows))
    lines.append("")
    lines.append("## Source Coverage")
    by_source = collections.Counter(r.source for r in strategy_rows)
    source_rows = [[source, count] for source, count in sorted(by_source.items())]
    lines.extend(md_table(["source", "strategy rows"], source_rows))
    lines.append("")
    lines.append("## Check And Test")
    lines.append("Commands used for this report:")
    lines.append("")
    lines.append("```bash")
    lines.append("RUSTC_WRAPPER= cargo build --bin aver")
    lines.append("python3 tools/proof_telemetry.py --no-current >/tmp/proof-telemetry-smoke.md")
    cmd = ["python3", "tools/proof_telemetry.py", "--aver-bin", "target/debug/aver"]
    for src in lean_tag_sources:
        cmd.extend(["--lean-tags-for", src])
    cmd.extend(["--output", "docs/proof-telemetry.md"])
    lines.append(" ".join(cmd))
    lines.append("python3 -m py_compile tools/proof_telemetry.py")
    lines.append("```")
    lines.append("")
    lines.append("The tool never invokes `lake`; local Lean export is transpile-only.")
    lines.append("")
    lines.append("## Deviations And Gaps")
    deviations = []
    if not manifest_records:
        deviations.append(
            "The brief expected committed `proof_manifest.json` history. This clone has 0 such files across `git log --all`, so historical reuse and tier-evolution tables cannot be populated honestly."
        )
    if manifest_records and not attributed_manifest:
        deviations.append(
            "Manifest records lack per-law strategy attribution, so historical per-strategy reuse cannot be computed without a future manifest `strategy` field or committed generated strategy tags."
        )
    if transpile_failures:
        deviations.append(f"Lean tag transpile failures: {len(transpile_failures)}")
    if strategy_failures:
        deviations.append(f"Strategy extraction failures: {len(strategy_failures)}")
    if strategy_conflicts:
        deviations.append(f"Strategy conflicts after de-duplication: {len(strategy_conflicts)}")
    if not deviations:
        deviations.append("No deviations in the recoverable data paths.")
    for item in deviations:
        lines.append(f"- {item}")
    if manifest_errors or strategy_failures or strategy_conflicts or transpile_failures:
        lines.append("")
        lines.append("## Diagnostics")
        for label, items in [
            ("manifest errors", manifest_errors),
            ("strategy failures", strategy_failures),
            ("strategy conflicts", strategy_conflicts),
            ("lean tag failures", transpile_failures),
        ]:
            if not items:
                continue
            lines.append(f"### {label}")
            for item in items[:30]:
                lines.append(f"- {item}")
    lines.append("")
    return "\n".join(lines)


def parse_args(argv: list[str]) -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--repo", default=".", help="Repository root")
    parser.add_argument(
        "--aver-bin",
        default=None,
        help="Aver binary for current no-lake telemetry (default: target/debug/aver if present, else aver)",
    )
    parser.add_argument(
        "--source",
        action="append",
        default=[],
        help="Source file or directory for current strategy extraction. Defaults to examples/formal and projects/k5_fdiv.",
    )
    parser.add_argument(
        "--lean-tags-for",
        action="append",
        default=[],
        help="Source file or directory to transpile with `aver proof` and scan for law-class tags. Optional because full K5 export is slow.",
    )
    parser.add_argument("--no-current", action="store_true", help="Skip current strategy extraction")
    parser.add_argument("--timeout", type=int, default=60, help="Per-command timeout in seconds")
    parser.add_argument("--output", help="Write Markdown report to this path")
    return parser.parse_args(argv)


def resolve_aver_bin(repo: Path, value: str | None) -> str:
    if value:
        return value
    local = repo / "target/debug/aver"
    if local.exists():
        return str(local)
    found = shutil.which("aver")
    return found or "aver"


def main(argv: list[str]) -> int:
    args = parse_args(argv)
    repo = Path(args.repo).resolve()
    start = time.monotonic()
    aver_bin = resolve_aver_bin(repo, args.aver_bin)

    branch = current_branch(repo)
    commit = short_commit(repo)
    head_manifest_count = collect_head_manifest_count(repo)
    manifest_records, manifest_commits, manifest_paths, manifest_errors = collect_manifest_history(repo)

    strategy_rows: list[StrategyRow] = []
    strategy_failures: list[str] = []
    strategy_conflicts: list[str] = []
    current_source_count = 0
    if not args.no_current:
        sources = expand_sources(repo, args.source)
        current_source_count = len(sources)
        strategy_rows, strategy_failures, strategy_conflicts = collect_current_strategies(
            repo, aver_bin, sources, args.timeout
        )

    committed_tags = collect_committed_lean_tags(repo)
    lean_tag_sources = args.lean_tags_for
    transpiled_tags: list[ClassTag] = []
    transpile_failures: list[str] = []
    transpile_elapsed = 0.0
    if lean_tag_sources:
        tag_sources = expand_sources(repo, lean_tag_sources)
        transpiled_tags, transpile_failures, transpile_elapsed = collect_transpiled_lean_tags(
            repo, aver_bin, tag_sources, args.timeout
        )

    hand_sidecars = collect_hand_sidecars(repo)
    elapsed = time.monotonic() - start
    report = render_report(
        repo=repo,
        elapsed=elapsed,
        branch=branch,
        commit=commit,
        head_manifest_count=head_manifest_count,
        manifest_records=manifest_records,
        manifest_commits=manifest_commits,
        manifest_paths=manifest_paths,
        manifest_errors=manifest_errors,
        strategy_rows=strategy_rows,
        strategy_failures=strategy_failures,
        strategy_conflicts=strategy_conflicts,
        committed_tags=committed_tags,
        transpiled_tags=transpiled_tags,
        transpile_failures=transpile_failures,
        transpile_elapsed=transpile_elapsed,
        hand_sidecars=hand_sidecars,
        lean_tag_sources=lean_tag_sources,
        no_current=args.no_current,
        current_source_count=current_source_count,
    )
    if args.output:
        out = (repo / args.output).resolve() if not os.path.isabs(args.output) else Path(args.output)
        out.parent.mkdir(parents=True, exist_ok=True)
        out.write_text(report, encoding="utf-8")
    else:
        print(report)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
