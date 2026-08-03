#!/usr/bin/env python3
"""THE canonical certificate-coverage counter for this repository.

Every coverage number quoted anywhere should come from this tool, together
with the rule that produced it. Historically, ad-hoc sweeps produced four
different totals for "how many functions certify" because each chose its own
corpus, denominator and dedup rule. This file fixes all three, once.

CORPUS (fixed — this list IS the definition):
  * ``examples/**/*.av``
  * ``projects/**/*.av``
  * excluded, intentionally-broken diagnostic fixtures (they do not compile,
    by design — they exist to demo compiler errors):
      - ``examples/diagnostics/test_errors.av``
      - ``examples/diagnostics/lint_demo.av``
  * deliberately NOT part of the corpus: ``tools/certkit/fixtures/*.av``.
    Those are synthetic probes for the certifier's own test suite, built to
    exercise specific certificate families (``cert_goals.av`` alone holds 25
    by-construction-certifiable functions). Counting them inflates "corpus
    coverage" with functions nobody wrote as programs.
  * known non-compiling on the wasm-gc backend (attempted every run, listed
    separately, NOT an error): every ``projects/k5_fdiv/**`` unit. The K5
    corpus uses fully-qualified record constructors
    (``Domain.Rational.Fraction(...)``) that multi-module flattening does not
    resolve yet; the day the backend accepts them, their functions join both
    the numerator and the denominator automatically. Function-free modules
    (``projects/*/domain/types.av``) are counted as zero-function units.

UNITS. Every corpus file is one compile unit. Each unit is compiled with the
real certifying pipeline — ``aver compile <file> --module-root <root>
--target wasm-gc --certify`` — where <root> is the nearest ancestor directory
(walking up from the file, stopping at ``examples/`` or ``projects/<name>/``)
from which the file's whole transitive ``depends [...]`` closure resolves.
Multi-module entrypoints (e.g. ``examples/games/rogue/main.av``,
``projects/payment_ops/main.av``) are therefore compiled as projects: their
dependency modules are loaded, flattened and re-included under module-prefixed
names (``Types_tileGlyph``). Verdicts are read from the emitted
``cert/cert-manifest.json`` (``certified`` and ``declaredUncertified``;
runtime-glue exports ``__*``, ``_start`` and ``memory`` are not functions and
are ignored — the remainder equals the manifest's ``source_level_only`` list,
which carries the honest per-function decline reasons).

COUNTING RULES (stated in the output as well):
  * HEADLINE — certified functions deduplicated by definition site
    (file that defines the function + function name). A definition site
    counts as certified when its OWN unit certifies it under its plain name.
    Module-prefixed re-inclusions of dependency modules inside entrypoint
    units are never counted here — they are the same source function again.
  * PER-UNIT SUM — the sum of ``certified`` lists over all units. This
    double-counts every dependency function once per entrypoint that includes
    it; it is reported because it answers "how many certified names do the
    emitted certificates carry", never as the coverage number.
  * DENOMINATOR — total corpus functions by the same definition-site rule:
    the sum over units of their own (un-prefixed) certified + declined
    functions.
  * RE-INCLUSION-ONLY — definition sites certified under a prefix inside
    some entrypoint but not in their own unit (flattened context can change
    a verdict). Reported separately so the headline stays honest.

"CERTIFIED" IS NOT "CLOSES". ``--certify`` emits a claim; it never runs the
Lean toolchain. A certificate only becomes real coverage when the checker
accepts it (``aver cert check`` builds the certificate project and
kernel-checks it). The default run measures certified claims only, which is
why it is fast. ``--closure`` additionally runs ``aver cert check`` per unit
(minutes per unit — not the default) and reports how many units actually
close.

Binaries are always built from this repository (``cargo build --bin aver
--features wasm``; plus ``-p aver-cert --bin aver-cert`` for ``--closure``).
A globally installed ``aver`` is never used.

Baseline workflow: ``--save tools/coverage-baseline.json`` on a green main;
later runs use ``--baseline tools/coverage-baseline.json`` to diff. Losing a
previously-certified definition site is flagged and exits non-zero.

Exit codes: 0 clean; 1 regression vs baseline or a corpus unit failing to
compile; 2 harness failure.
"""

from __future__ import annotations

import argparse
import collections
import concurrent.futures
import dataclasses
import datetime
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import tempfile

REPO = Path(__file__).resolve().parent.parent
CORPUS_GLOBS = ("examples/**/*.av", "projects/**/*.av")
EXCLUDED = (
    "examples/diagnostics/test_errors.av",
    "examples/diagnostics/lint_demo.av",
)
# Units known not to compile on the wasm-gc backend (see module docstring).
# They stay in the corpus and are attempted every run; a failure matching a
# prefix here is reported under its own heading instead of flagging the run.
KNOWN_NONCOMPILING = ("projects/k5_fdiv/",)
GLUE_EXPORTS = ("_start", "memory")  # plus anything starting with "__"
DEPENDS_RE = re.compile(r"\bdepends\s*\[([^\]]*)\]")

# Decline reasons are the classifier's exact strings
# (aver-cert/src/engine/classify_entry.rs); bucket on their stable prefixes.
REASON_BUCKETS = (
    (re.compile(r"^calls the host capability"), "effects: calls a host capability"),
    (re.compile(r"no Int carrier host helpers"), "module has no Int carrier"),
    (re.compile(r"^parameter \d+ is "), "parameter shape outside the fragment"),
    (re.compile(r"^returns nothing"), "returns nothing"),
    (re.compile(r"^returns \d+ values"), "multiple results"),
    (re.compile(r"^returns "), "result shape outside the fragment"),
    (re.compile(r"^body uses loops/branches"), "control flow outside the fragment"),
    (re.compile(r"^body uses (the )?wasm instruction"), "instruction outside the fragment"),
    (re.compile(r"^calls other user functions"), "cross-function call, no template"),
    (re.compile(r"^takes no parameters"), "zero arity"),
    (re.compile(r"^takes \d+ parameters"), "arity above 2"),
    (re.compile(r"^body does not match a certified template"), "no template matched"),
    (re.compile(r"composition|carrier struct type not found"), "composition closure declined"),
    (re.compile(r"^producer fragment plan rejected"), "plan rejected by the producer"),
    (re.compile(r"no byte-matching artifact plan"), "classified but no byte-matching plan"),
    (re.compile(r"^module export is outside"), "export outside obligations"),
)


def bucket_reason(reason: str) -> str:
    for pattern, label in REASON_BUCKETS:
        if pattern.search(reason):
            return label
    return "other"


def parse_depends(path: Path) -> list[str]:
    try:
        text = path.read_text(encoding="utf-8")
    except OSError:
        return []
    for raw in text.splitlines():
        line = raw.split("//", 1)[0]
        m = DEPENDS_RE.search(line)
        if m:
            return [d.strip() for d in m.group(1).split(",") if d.strip()]
    return []


def find_module_file(name: str, root: Path) -> Path | None:
    """Mirror of src/source.rs::find_module_file (lowercased path, exact fallback)."""
    parts = [p for p in name.split(".") if p]
    if not parts:
        return None
    lower = root.joinpath(*[p.lower() for p in parts[:-1]], parts[-1].lower() + ".av")
    if lower.exists():
        return lower
    exact = root.joinpath(*parts[:-1], parts[-1] + ".av")
    if exact.exists():
        return exact
    return None


def resolve_closure(entry: Path, root: Path) -> dict[str, Path] | None:
    """Transitive depends closure {dotted module name -> file}, or None if any
    dependency does not resolve from this root."""
    closure: dict[str, Path] = {}
    stack = parse_depends(entry)
    while stack:
        name = stack.pop()
        if name in closure:
            continue
        dep = find_module_file(name, root)
        if dep is None:
            return None
        closure[name] = dep
        stack.extend(parse_depends(dep))
    return closure


def corpus_boundary(rel: Path) -> Path:
    """Topmost directory a unit's module root may be: examples/ for examples,
    the project directory for projects/<name>/..."""
    if rel.parts[0] == "projects" and len(rel.parts) > 2:
        return REPO / rel.parts[0] / rel.parts[1]
    return REPO / rel.parts[0]


def pick_module_root(file: Path) -> tuple[Path, dict[str, Path] | None]:
    """Nearest ancestor (file dir first, corpus boundary last) resolving the
    whole depends closure. Falls back to the file's directory (a unit whose
    deps do not resolve fails to compile and is reported as such)."""
    boundary = corpus_boundary(file.relative_to(REPO))
    candidates = []
    d = file.parent
    while True:
        candidates.append(d)
        if d == boundary or d == REPO:
            break
        d = d.parent
    for root in candidates:
        closure = resolve_closure(file, root)
        if closure is not None:
            return root, closure
    return file.parent, None


@dataclasses.dataclass
class Unit:
    file: Path            # absolute
    rel: str              # repo-relative path
    root: Path            # module root (absolute)
    closure: dict[str, Path] | None  # dotted name -> file

    # sweep results
    status: str = "pending"           # ok | compile-failed
    error: str = ""
    certified: list[dict] = dataclasses.field(default_factory=list)   # manifest entries
    declined: list[dict] = dataclasses.field(default_factory=list)    # name+reason
    closes: str | None = None         # closes | vacuous | fails (with --closure)

    @property
    def unit_id(self) -> str:
        return self.rel[: -len(".av")].replace("/", "-")

    def prefix_map(self) -> list[tuple[str, str]]:
        """[(prefix_with_underscore, dep rel path)] sorted longest-first."""
        if not self.closure:
            return []
        pairs = [
            (dotted.replace(".", "_") + "_", str(dep.relative_to(REPO)))
            for dotted, dep in self.closure.items()
        ]
        return sorted(pairs, key=lambda p: -len(p[0]))

    def defsite(self, name: str) -> tuple[str, str]:
        """(defining file rel path, plain function name) for a manifest name."""
        for prefix, dep_rel in self.prefix_map():
            if name.startswith(prefix):
                return dep_rel, name[len(prefix):]
        return self.rel, name


def discover_corpus() -> list[Unit]:
    files = []
    for glob in CORPUS_GLOBS:
        files.extend(REPO.glob(glob))
    excluded = {REPO / e for e in EXCLUDED}
    units = []
    for f in sorted(set(files)):
        if f in excluded:
            continue
        root, closure = pick_module_root(f)
        units.append(Unit(file=f, rel=str(f.relative_to(REPO)), root=root, closure=closure))
    return units


def target_dir() -> Path:
    """Cargo's build directory: ``CARGO_TARGET_DIR`` when set, else ``target/``."""
    override = os.environ.get("CARGO_TARGET_DIR")
    return Path(override).expanduser().resolve() if override else REPO / "target"


def build_binaries(closure: bool) -> None:
    cmds = [["cargo", "build", "--bin", "aver", "--features", "wasm"]]
    if closure:
        cmds.append(["cargo", "build", "-p", "aver-cert", "--bin", "aver-cert"])
    for cmd in cmds:
        proc = subprocess.run(cmd, cwd=REPO, capture_output=True, text=True)
        if proc.returncode != 0:
            sys.stderr.write(proc.stderr)
            sys.exit(2)


def cert_env() -> dict[str, str]:
    env = dict(os.environ)
    tmp = Path(tempfile.gettempdir())
    env.setdefault("AVER_CERT_PRELUDE_CACHE", str(tmp / "aver-cert-prelude-store"))
    env.setdefault("AVER_CERT_DATA_CACHE", str(tmp / "aver-cert-data-store"))
    return env


def sweep_unit(unit: Unit, aver: Path, workdir: Path, closure: bool) -> Unit:
    out = workdir / unit.unit_id
    cmd = [
        str(aver), "compile", str(unit.file),
        "--module-root", str(unit.root),
        "--target", "wasm-gc", "--certify",
        "-o", str(out),
    ]
    try:
        proc = subprocess.run(cmd, cwd=REPO, env=cert_env(),
                              capture_output=True, text=True, timeout=300)
    except subprocess.TimeoutExpired:
        unit.status, unit.error = "compile-failed", "timeout (300s)"
        return unit
    manifest_path = out / "cert" / "cert-manifest.json"
    if proc.returncode != 0 or not manifest_path.exists():
        blob = proc.stderr.strip() or proc.stdout.strip() or "unknown"
        unit.error = blob.splitlines()[-1][:200]
        if "module has no fn definitions" in blob:
            unit.status = "no-functions"   # type-only module: nothing to measure
        elif unit.rel.startswith(KNOWN_NONCOMPILING):
            unit.status = "known-noncompiling"
        else:
            unit.status = "compile-failed"
        return unit
    manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
    unit.certified = manifest.get("certified", [])
    declared = manifest.get("declaredUncertified", [])
    unit.declined = [
        d for d in declared
        if not d["name"].startswith("__") and d["name"] not in GLUE_EXPORTS
    ]
    unit.status = "ok"
    if closure:
        wasm = out / f"{unit.file.stem}.wasm"
        check = [str(aver), "cert", "check", str(wasm), str(out / "cert")]
        try:
            proc = subprocess.run(check, cwd=REPO, env=cert_env(),
                                  capture_output=True, text=True, timeout=3600)
        except subprocess.TimeoutExpired:
            unit.closes = "fails"
            return unit
        blob = proc.stdout + proc.stderr
        if proc.returncode == 0:
            unit.closes = "closes"
        elif "NO CHECKED EXPORTS" in blob:
            unit.closes = "vacuous"   # closure built, zero certified exports
        else:
            unit.closes = "fails"
    return unit


def aggregate(units: list[Unit]) -> dict:
    certified_sites: dict[str, str] = {}      # "file::name" -> class (own-unit basis)
    declined_sites: dict[str, str] = {}       # "file::name" -> reason bucket
    declined_reasons: dict[str, str] = {}     # "file::name" -> raw reason
    union_certified: dict[str, str] = {}      # any-unit basis (incl. re-inclusions)
    per_unit_sum = 0
    families_sum: collections.Counter = collections.Counter()

    for unit in units:
        if unit.status != "ok":
            continue
        per_unit_sum += len(unit.certified)
        for entry in unit.certified:
            site_file, plain = unit.defsite(entry["name"])
            key = f"{site_file}::{plain}"
            families_sum[entry["class"]] += 1
            union_certified.setdefault(key, entry["class"])
            if site_file == unit.rel:
                certified_sites[key] = entry["class"]
        for entry in unit.declined:
            site_file, plain = unit.defsite(entry["name"])
            if site_file == unit.rel:
                key = f"{site_file}::{plain}"
                declined_sites[key] = bucket_reason(entry["reason"])
                declined_reasons[key] = entry["reason"]

    reincl_only = {
        k: v for k, v in union_certified.items()
        if k not in certified_sites
    }
    families = collections.Counter(certified_sites.values())
    reasons = collections.Counter(declined_sites.values())
    return {
        "certified_sites": certified_sites,
        "declined_sites": declined_sites,
        "declined_reasons": declined_reasons,
        "reincl_only": reincl_only,
        "per_unit_sum": per_unit_sum,
        "families": families,
        "families_per_unit_sum": families_sum,
        "reasons": reasons,
    }


def repo_commit() -> str:
    try:
        return subprocess.run(["git", "rev-parse", "--short", "HEAD"], cwd=REPO,
                              capture_output=True, text=True, timeout=10).stdout.strip()
    except OSError:
        return "unknown"


def to_json(units: list[Unit], agg: dict, closure: bool) -> dict:
    failed = [u for u in units if u.status == "compile-failed"]
    known = [u for u in units if u.status == "known-noncompiling"]
    doc = {
        "schema_version": 1,
        "tool": "tools/coverage.py",
        "generated": datetime.date.today().isoformat(),
        "repo_commit": repo_commit(),
        "corpus": {
            "globs": list(CORPUS_GLOBS),
            "excluded": list(EXCLUDED),
            "units": len(units),
        },
        "rules": {
            "headline": "certified functions deduplicated by definition site "
                        "(defining file + name); module-prefixed re-inclusions of "
                        "dependency modules never count",
            "denominator": "all corpus functions by the same definition-site rule "
                           "(certified + declined, own unit, un-prefixed)",
            "per_unit_sum": "sum of manifest certified lists; double-counts "
                            "dependency functions once per including entrypoint",
            "certified_vs_closes": "certified = producer emitted a claim (no Lean "
                                   "run); closes = `aver cert check` accepts it. "
                                   "Only --closure measures the latter.",
        },
        "headline": {
            "certified_by_definition_site": len(agg["certified_sites"]),
            "total_functions_by_definition_site":
                len(agg["certified_sites"]) + len(agg["declined_sites"]),
            "certified_per_unit_sum": agg["per_unit_sum"],
            "reinclusion_only_certified": len(agg["reincl_only"]),
            "units_failing_to_compile": len(failed),
            "units_known_noncompiling": len(known),
        },
        "families": dict(agg["families"].most_common()),
        "families_per_unit_sum": dict(agg["families_per_unit_sum"].most_common()),
        "decline_reasons": dict(agg["reasons"].most_common()),
        "certified": dict(sorted(agg["certified_sites"].items())),
        "declined": dict(sorted(agg["declined_sites"].items())),
        "reinclusion_only": dict(sorted(agg["reincl_only"].items())),
        "units": [
            {
                "unit": u.unit_id,
                "path": u.rel,
                "module_root": str(u.root.relative_to(REPO)) or ".",
                "status": u.status,
                "error": u.error or None,
                "certified_own": sum(1 for c in u.certified if u.defsite(c["name"])[0] == u.rel),
                "certified_total": len(u.certified),
                "functions_own": sum(1 for c in u.certified if u.defsite(c["name"])[0] == u.rel)
                                 + sum(1 for d in u.declined if u.defsite(d["name"])[0] == u.rel),
                "closes": u.closes,
            }
            for u in units
        ],
    }
    if closure:
        counter = collections.Counter(u.closes for u in units if u.status == "ok")
        doc["closure"] = {
            "closes_with_checked_exports": counter.get("closes", 0),
            "closes_vacuously_zero_exports": counter.get("vacuous", 0),
            "does_not_close": counter.get("fails", 0),
            "note": "a certificate that never closes is not real coverage",
        }
    return doc


def print_report(doc: dict) -> None:
    h = doc["headline"]
    total = h["total_functions_by_definition_site"]
    cert = h["certified_by_definition_site"]
    pct = 100.0 * cert / total if total else 0.0
    print(f"# Certificate coverage — {doc['generated']} @ {doc['repo_commit']}")
    print(f"corpus: {' + '.join(doc['corpus']['globs'])}, "
          f"{doc['corpus']['units']} units "
          f"(excluded broken diagnostics: {', '.join(doc['corpus']['excluded'])})")
    print()
    print(f"HEADLINE  {cert} / {total} functions certified ({pct:.1f}%)")
    print(f"  rule: {doc['rules']['headline']}")
    print(f"  denominator rule: {doc['rules']['denominator']}")
    print(f"per-unit sum: {h['certified_per_unit_sum']} certified names across all "
          f"emitted certificates")
    print(f"  rule: {doc['rules']['per_unit_sum']}")
    if h["reinclusion_only_certified"]:
        print(f"re-inclusion-only certified (flattened context, not in own unit): "
              f"{h['reinclusion_only_certified']} — {sorted(doc['reinclusion_only'])}")
    if h["units_failing_to_compile"]:
        bad = [u["path"] for u in doc["units"] if u["status"] == "compile-failed"]
        print(f"!! {h['units_failing_to_compile']} corpus unit(s) FAILED TO COMPILE "
              f"(their functions are missing from the denominator): {bad}")
    if h["units_known_noncompiling"]:
        print(f"known non-compiling on wasm-gc (see docstring; functions absent "
              f"from the denominator until the backend accepts them): "
              f"{h['units_known_noncompiling']} unit(s) under "
              f"{', '.join(KNOWN_NONCOMPILING)}")
    print()
    print("NOTE: 'certified' means the producer emitted a claim. It does NOT mean")
    print("the certificate closes under `aver cert check` — run --closure for that.")
    print()
    print("## Family histogram (definition-site basis; per-unit sum in parentheses)")
    sums = doc["families_per_unit_sum"]
    for fam, n in doc["families"].items():
        print(f"  {n:4d}  ({sums.get(fam, 0):4d})  {fam}")
    print()
    print("## Decline reasons (definition-site basis)")
    for reason, n in doc["decline_reasons"].items():
        print(f"  {n:4d}  {reason}")
    print()
    if "closure" in doc:
        c = doc["closure"]
        print("## Closure (`aver cert check` per unit)")
        print(f"  closes with checked exports: {c['closes_with_checked_exports']}")
        print(f"  closes vacuously (0 certified exports): {c['closes_vacuously_zero_exports']}")
        print(f"  DOES NOT CLOSE: {c['does_not_close']}")
        print(f"  {c['note']}")
        print()
    print("## Per-unit table (own = defined in the file itself; total incl. re-inclusions)")
    for u in doc["units"]:
        closes = f"  closes={u['closes']}" if u["closes"] else ""
        if u["status"] == "no-functions":
            print(f"    -           {u['path']}  (type-only module, no functions)")
        elif u["status"] == "known-noncompiling":
            print(f"  KNOWN-NONCOMPILING {u['path']}  ({u['error']})")
        elif u["status"] != "ok":
            print(f"  FAILED {u['path']}  ({u['error']})")
        elif u["certified_total"]:
            print(f"  {u['certified_own']:3d}/{u['functions_own']:<3d} own "
                  f"{u['certified_total']:3d} total  {u['path']}"
                  f"  [root {u['module_root']}]{closes}")
        else:
            print(f"    0/{u['functions_own']:<3d} own   0 total  {u['path']}{closes}")


def diff_baseline(doc: dict, baseline_path: Path) -> int:
    base = json.loads(baseline_path.read_text(encoding="utf-8"))
    old = base.get("certified", {})
    new = doc.get("certified", {})
    gained = sorted(set(new) - set(old))
    lost = sorted(set(old) - set(new))
    print()
    print(f"## Diff vs baseline {baseline_path} "
          f"({base.get('generated')} @ {base.get('repo_commit')})")
    print(f"  certified: {len(old)} -> {len(new)}")
    ob = base.get("headline", {}).get("total_functions_by_definition_site")
    print(f"  denominator: {ob} -> "
          f"{doc['headline']['total_functions_by_definition_site']}")
    for k in gained:
        print(f"  + GAINED {k} ({new[k]})")
    for k in lost:
        print(f"  - LOST   {k} ({old[k]})  <-- REGRESSION")
    if not gained and not lost:
        print("  no change in certified definition sites")
    return 1 if lost else 0


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--jobs", type=int, default=8, help="parallel compile workers (default 8)")
    ap.add_argument("--json", action="store_true", help="print the JSON document instead of text")
    ap.add_argument("--save", type=Path, help="also write the JSON document to this path")
    ap.add_argument("--baseline", type=Path, help="diff against a saved run; lost functions exit 1")
    ap.add_argument("--closure", action="store_true",
                    help="additionally run `aver cert check` per unit (SLOW: minutes per unit)")
    ap.add_argument("--no-build", action="store_true",
                    help="skip cargo build (reuse the existing debug binary)")
    ap.add_argument("--workdir", type=Path, help="keep sweep artifacts here (default: temp dir)")
    args = ap.parse_args()

    aver = target_dir() / "debug" / "aver"
    if not args.no_build:
        build_binaries(args.closure)
    if not aver.exists():
        sys.stderr.write(f"missing {aver}; run without --no-build\n")
        return 2

    units = discover_corpus()
    if args.workdir:
        workdir = args.workdir
        workdir.mkdir(parents=True, exist_ok=True)
    else:
        workdir = Path(tempfile.mkdtemp(prefix="aver-coverage-"))

    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as pool:
        list(pool.map(lambda u: sweep_unit(u, aver, workdir, args.closure), units))

    agg = aggregate(units)
    doc = to_json(units, agg, args.closure)

    if args.save:
        args.save.write_text(json.dumps(doc, indent=1) + "\n", encoding="utf-8")
    if args.json:
        print(json.dumps(doc, indent=1))
    else:
        print_report(doc)

    rc = 0
    if args.baseline:
        rc = diff_baseline(doc, args.baseline)
    if doc["headline"]["units_failing_to_compile"]:
        rc = max(rc, 1)   # unexpected failures only; known-noncompiling stays green
    return rc


if __name__ == "__main__":
    sys.exit(main())
