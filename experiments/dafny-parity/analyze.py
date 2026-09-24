#!/usr/bin/env python3
"""Per-law Lean vs Dafny verdicts from the raw outputs run_target.sh keeps.

usage: analyze.py <results-dir> <out.json> <out.md>

<results-dir> holds one directory per <target>-<backend>. For each target the
law inventory is the source scan of the entry's module closure; each law then
gets one status per backend:

  universal   proved for every input, no trust escape
  bounded     proved only over the law's finite given domain
  sorry       stated, but closed by `sorry` (Lean) / `assume {:axiom}` (Dafny)
  failed      the checker rejected it (error other than a time limit)
  timeout     the checker ran out of time / heartbeats on it
  declined    the exporter refused to state it, with a reason
  not_exported  no theorem / lemma for it in the export
  no_run      the backend run itself did not produce a verdict
"""
import csv, io, json, os, re, sys, tarfile
from collections import Counter, defaultdict

LAW_RE = re.compile(r"^verify\s+([A-Za-z_][\w.]*)\s+law\s+([A-Za-z_]\w*)", re.M)


def read(path, default=""):
    try:
        with open(path, encoding="utf-8", errors="replace") as f:
            return f.read()
    except OSError:
        return default


def tar_texts(path, suffix):
    out = {}
    if not os.path.exists(path):
        return out
    with tarfile.open(path) as t:
        for m in t.getmembers():
            if m.isfile() and m.name.endswith(suffix):
                f = t.extractfile(m)
                if f:
                    out[os.path.normpath(m.name)] = f.read().decode("utf-8", "replace")
    return out


# ---------------------------------------------------------------- inventory

def module_closure(sources, entry_rel):
    """Map qualified module name -> source text, following depends from the entry."""
    by_path = {k.lower(): v for k, v in sources.items()}
    entry_text = sources.get(os.path.normpath(entry_rel), "")
    mods = {"<entry>": entry_text}
    todo = [entry_text]
    while todo:
        text = todo.pop()
        m = re.search(r"^\s+depends\s*\[([^\]]*)\]", text, re.M)
        if not m:
            continue
        for dep in [d.strip() for d in m.group(1).split(",") if d.strip()]:
            if dep in mods:
                continue
            p = "/".join(dep.split(".")).lower() + ".av"
            if p in by_path:
                mods[dep] = by_path[p]
                todo.append(by_path[p])
    return mods


def inventory(sources, entry_rel):
    laws = []
    for mod, text in module_closure(sources, entry_rel).items():
        name = mod
        if mod == "<entry>":
            mm = re.search(r"^module\s+(\S+)", text, re.M)
            name = mm.group(1) if mm else "Entry"
        for fn, law in LAW_RE.findall(text):
            laws.append({"module": name, "fn": fn, "law": law})
    return laws


def key_of(module, fn, law):
    return (module.split(".")[-1].lower(), fn, law)


# ---------------------------------------------------------------- Lean

def lean_side(d):
    res = {"ran": False, "laws": {}, "declined": {}, "notes": []}
    st = read(os.path.join(d, "lean.status"))
    res["status"] = st.strip().replace("\n", " ")
    js = read(os.path.join(d, "lean.json")).strip()
    j = None
    if js:
        try:
            j = json.loads(js[js.find("{"):])
        except Exception as e:  # noqa: BLE001
            res["notes"].append(f"lean.json unparsable: {e}")
    res["summary"] = {k: j.get(k) for k in ("passed", "sorries", "build_errors", "universal_laws",
                                           "bounded_laws", "declined", "model_panicked")} if j else None
    man = read(os.path.join(d, "lean.manifest.json"))
    if man:
        m = json.loads(man)
        res["ran"] = True
        for l in m.get("laws", []):
            parts = l["law"].split(".")
            res["laws"][key_of(".".join(parts[:-2]) or "<entry>", parts[-2], parts[-1])] = {
                "status": {"universal": "universal", "bounded": "bounded", "sampled": "bounded",
                           "failed": "sorry"}.get(l["tier"], l["tier"]),
                "tier": l["tier"], "axioms": l.get("axioms", [])}
        for dc in m.get("declined", []):
            res["declined"][dc["claim"]] = dc.get("reason", "")
    if j:
        for dc in j.get("declined_claims", []) or []:
            res["declined"].setdefault(dc["claim"], dc.get("reason", ""))
    # Per-theorem fallback from the replayed lake log.
    lake = read(os.path.join(d, "lake.log"))
    res["lake_tail"] = lake[-3000:]
    lean_files = tar_texts(os.path.join(d, "lean.export.tgz"), ".lean")
    thm_ranges = defaultdict(list)  # file -> [(start, name)]
    for path, text in lean_files.items():
        for i, line in enumerate(text.splitlines(), 1):
            mm = re.match(r"\s*(?:private\s+)?theorem\s+(\S+)", line)
            if mm:
                thm_ranges[path].append((i, mm.group(1)))
    res["theorems"] = {name for v in thm_ranges.values() for _, name in v}
    issues = defaultdict(list)
    for mm in re.finditer(r"(?m)^(error|warning): (?:\S*/)?([^\s:]+\.lean):(\d+):\d+: (.*)$", lake):
        kind, f, line, msg = mm.group(1), mm.group(2), int(mm.group(3)), mm.group(4)
        cands = [p for p in thm_ranges if p.endswith(f)]
        for p in cands:
            owner = None
            for start, name in thm_ranges[p]:
                if start <= line:
                    owner = name
            if owner:
                issues[owner].append((kind, msg[:200]))
    res["theorem_issues"] = dict(issues)
    return res


def lean_status(law, lean):
    k = key_of(law["module"], law["fn"], law["law"])
    rec = lean["laws"].get(k)
    if rec is None and law["module"] != "<entry>":
        rec = lean["laws"].get(key_of("<entry>", law["fn"], law["law"]))
    thm = f'{law["fn"]}_law_{law["law"]}'
    iss = [i for n, v in lean["theorem_issues"].items() if n.split(".")[-1] == thm for i in v]
    detail = "; ".join(f"{a}: {b}" for a, b in iss[:3])
    if rec:
        return rec["status"], detail or ",".join(rec["axioms"])
    for claim, reason in lean["declined"].items():
        if claim.endswith(f'{law["fn"]}.{law["law"]}'):
            return "declined", reason[:300]
    errs = [m for kind, m in iss if kind == "error"]
    if errs:
        return ("timeout" if any("heartbeat" in e or "timeout" in e for e in errs) else "failed"), detail
    if any("sorry" in m for _, m in iss):
        return "sorry", detail
    if not lean["ran"]:
        return "no_run", lean.get("status", "")
    present = any(n.split(".")[-1] == thm for n in lean["theorems"])
    return ("not_exported", "") if not present else ("failed", "theorem present, not in manifest")


# ---------------------------------------------------------------- Dafny

def classify_dafny_error(msg):
    m = msg.lower()
    if "timed out" in m:
        return "timeout"
    if "out of resource" in m:
        return "out_of_resource"
    if "postcondition" in m:
        return "postcondition"
    if "assertion" in m:
        return "assertion"
    if "precondition" in m:
        return "precondition"
    if "decreases" in m or "termination" in m:
        return "termination"
    if "resolution" in m or "type error" in m or "unresolved" in m or "not found" in m:
        return "resolution"
    return "other"


def guided_label(name):
    mm = re.match(r"averGuided_([0-9a-f]+)$", name)
    if not mm:
        return None
    try:
        return bytes.fromhex(mm.group(1)).decode("utf-8", "replace")
    except ValueError:
        return None


def dafny_side(d):
    res = {"ran": False, "notes": []}
    res["status"] = read(os.path.join(d, "dafny.status")).strip().replace("\n", " ")
    log = read(os.path.join(d, "dafny.log"))
    res["log_tail"] = log[-3000:]
    fin = re.search(r"Dafny program verifier finished with (\d+) verified, (\d+) errors?(?:, (\d+) time outs?)?", log)
    res["finished"] = fin.group(0) if fin else None
    res["ran"] = bool(fin)
    files = tar_texts(os.path.join(d, "dafny.export.tgz"), ".dfy")
    lemmas = []            # dicts: file, name, start, end, body
    comments = []          # (file, label, text)
    for path, text in files.items():
        lines = text.splitlines()
        decl = []
        for i, line in enumerate(lines, 1):
            mm = re.match(r"\s*(lemma|method|function|predicate|datatype|type|module|const)\b(?:\s*\{[^}]*\})*\s*([A-Za-z_][\w']*)", line)
            if mm and not line.strip().startswith("//"):
                decl.append((i, mm.group(1), mm.group(2)))
            cm = re.match(r"\s*// Law (\S+?)(?::| is not exported)\s*(.*)$", line)
            if cm:
                comments.append((path, cm.group(1), line.strip()))
        for n, (start, kind, name) in enumerate(decl):
            end = decl[n + 1][0] - 1 if n + 1 < len(decl) else len(lines)
            if kind in ("lemma", "method"):
                body = "\n".join(lines[start - 1:end])
                lemmas.append({"file": path, "name": name, "kind": kind, "start": start, "end": end,
                               "axiom": "assume {:axiom}" in body,
                               "bounded": bool(re.search(r"_sample_\d+\(\)", body)) and kind == "lemma"})
    res["lemmas"] = lemmas
    res["comments"] = comments
    # Errors from stdout, mapped to the enclosing lemma by file and line.
    errs = defaultdict(list)
    for mm in re.finditer(r"(?m)^(\S+\.dfy)\((\d+),(\d+)\): Error: (.*)$", log):
        f, line, msg = os.path.normpath(mm.group(1)), int(mm.group(2)), mm.group(4)
        owner = None
        for lm in lemmas:
            if (lm["file"] == f or lm["file"].endswith(f) or f.endswith(lm["file"])) and lm["start"] <= line <= lm["end"]:
                owner = lm
        k = (owner["file"], owner["name"]) if owner else ("?", f"{f}:{line}")
        errs[k].append(msg[:240])
    for mm in re.finditer(r"Verification of '([^']+)' timed out", log):
        nm = mm.group(1).split(".")[-1]
        for lm in lemmas:
            if lm["name"] == nm:
                errs[(lm["file"], lm["name"])].append("timed out")
    res["errors"] = {f"{a}::{b}": v for (a, b), v in errs.items()}
    res["_errs"] = errs
    # Per-implementation outcomes from the CSV log, where present.
    outcomes = {}
    csvt = read(os.path.join(d, "dafny.verification.csv"))
    if csvt:
        for row in csv.DictReader(io.StringIO(csvt)):
            nm = (row.get("TestResult.DisplayName") or "").split(" (")[0].split(".")[-1]
            oc = row.get("TestResult.Outcome") or ""
            if nm:
                prev = outcomes.get(nm)
                outcomes[nm] = oc if prev in (None, "Passed") else prev
    res["outcomes"] = outcomes
    if not fin:
        res["notes"].append("no 'finished with' line: dafny rejected the program before verification")
    return res


def dafny_status(law, dafny):
    fn, name = law["fn"], law["law"]
    label = f"{fn}.{name}"
    mod_last = law["module"].split(".")[-1].lower()

    def in_module(path):
        base = os.path.basename(path)[:-4].lower()
        return law["module"] == "<entry>" or base == mod_last or base.endswith(mod_last)

    for path, lab, text in dafny["comments"]:
        if lab.rstrip(":") == label and in_module(path):
            reason = text.split(":", 1)[1].strip() if ":" in text else text
            if "is not exported" in text:
                return "declined", reason[:300]
            return "not_exported", reason[:300]
    lemma_names = {f"{fn}_{name}", f"{fn}__{name}", f"{fn}_{name}_"}
    main = [lm for lm in dafny["lemmas"] if lm["kind"] == "lemma" and in_module(lm["file"]) and
            (lm["name"] in lemma_names or guided_label(lm["name"]) in (label,) or
             (guided_label(lm["name"]) or "").endswith("." + label))]
    helpers = [lm for lm in dafny["lemmas"] if lm["kind"] == "lemma" and in_module(lm["file"]) and lm not in main and
               (lm["name"].startswith(f"{fn}_{name}__") or
                (guided_label(lm["name"]) or "").startswith(label + ".") or
                (guided_label(lm["name"]) or "").startswith(label + "#"))]
    if not main:
        return ("no_run", dafny["status"]) if not dafny["files_present"] else ("not_exported", "no lemma found")
    group = main + helpers
    if not dafny["ran"]:
        return "no_run", "; ".join(dafny["notes"])[:300]
    msgs = []
    for lm in group:
        msgs += [(lm["name"], m) for m in dafny["_errs"].get((lm["file"], lm["name"]), [])]
        oc = dafny["outcomes"].get(lm["name"])
        if oc and oc != "Passed" and not dafny["_errs"].get((lm["file"], lm["name"])):
            msgs.append((lm["name"], f"outcome {oc}"))
    if msgs:
        classes = sorted({classify_dafny_error(m) for _, m in msgs})
        st = "timeout" if classes == ["timeout"] or "out_of_resource" in classes and len(classes) == 1 else "failed"
        return st, (",".join(classes) + " | " + "; ".join(f"{a}: {b}" for a, b in msgs[:3]))[:400]
    if any(lm["axiom"] for lm in group):
        return "sorry", "assume {:axiom} in " + ",".join(lm["name"] for lm in group if lm["axiom"])
    if any(lm["bounded"] for lm in main):
        return "bounded", "per-sample dispatch"
    return "universal", ""


# ---------------------------------------------------------------- report

def main():
    root, out_json, out_md = sys.argv[1:4]
    targets = defaultdict(dict)
    for dn in sorted(os.listdir(root)):
        p = os.path.join(root, dn)
        if not os.path.isdir(p):
            continue
        tgt, _, backend = dn.rpartition("-")
        targets[tgt][backend] = p
    report = {"targets": {}}
    md = ["# Dafny / Lean per-law parity", ""]
    summary_rows = []
    all_rows = []
    for tgt, dirs in sorted(targets.items()):
        any_dir = next(iter(dirs.values()))
        label, mroot, entry = (read(os.path.join(any_dir, "target.txt")).split() + ["", "", ""])[:3]
        sources = tar_texts(os.path.join(any_dir, "sources.tgz"), ".av")
        entry_rel = os.path.relpath(entry, mroot) if entry else ""
        laws = inventory(sources, entry_rel)
        lean = lean_side(dirs["lean"]) if "lean" in dirs else None
        dafny = dafny_side(dirs["dafny"]) if "dafny" in dirs else None
        if dafny is not None:
            dafny["files_present"] = bool(dafny["lemmas"] or dafny["comments"])
        rows = []
        for law in laws:
            ls, ld = lean_status(law, lean) if lean else ("no_run", "")
            ds, dd = dafny_status(law, dafny) if dafny else ("no_run", "")
            mod = law["module"]
            rows.append({"law": f'{mod}.{law["fn"]}.{law["law"]}', "lean": ls, "lean_detail": ld,
                         "dafny": ds, "dafny_detail": dd})
        c = Counter()
        for r in rows:
            lu, du = r["lean"] == "universal", r["dafny"] == "universal"
            c["total"] += 1
            c["lean_universal"] += lu
            c["dafny_universal"] += du
            c["both"] += lu and du
            c["lean_only"] += lu and not du
            c["dafny_only"] += du and not lu
            c["neither"] += not lu and not du
        report["targets"][tgt] = {
            "entry": entry, "module_root": mroot, "counts": dict(c), "laws": rows,
            "lean_status": lean and lean["status"], "lean_summary": lean and lean.get("summary"),
            "dafny_status": dafny and dafny["status"], "dafny_finished": dafny and dafny["finished"],
            "lean_counts": dict(Counter(r["lean"] for r in rows)),
            "dafny_counts": dict(Counter(r["dafny"] for r in rows)),
            "dafny_unmapped_errors": {k: v for k, v in (dafny or {}).get("errors", {}).items() if k.startswith("?::")},
            "btc": read(os.path.join(any_dir, "btc.txt")).strip(),
            "notes": (lean or {}).get("notes", []) + (dafny or {}).get("notes", []),
            "lean_lake_tail": (lean or {}).get("lake_tail", "")[-1500:],
            "dafny_log_tail": (dafny or {}).get("log_tail", "")[-1500:],
        }
        summary_rows.append((tgt, c))
        all_rows += [(tgt, r) for r in rows]
    md += ["| target | laws | Lean universal | Dafny universal | both | Lean only | Dafny only | neither |",
           "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for tgt, c in summary_rows:
        md.append(f'| {tgt} | {c["total"]} | {c["lean_universal"]} | {c["dafny_universal"]} | {c["both"]} | '
                  f'{c["lean_only"]} | {c["dafny_only"]} | {c["neither"]} |')
    md += ["", "## Status counts", "", "| target | Lean | Dafny |", "|---|---|---|"]
    for tgt, t in report["targets"].items():
        md.append(f'| {tgt} | {t["lean_counts"]} | {t["dafny_counts"]} |')
    md += ["", "## Dafny-only (Z3 closes, Lean does not)", "", "| target | law | Lean | Lean detail |", "|---|---|---|---|"]
    for tgt, r in all_rows:
        if r["dafny"] == "universal" and r["lean"] != "universal":
            md.append(f'| {tgt} | `{r["law"]}` | {r["lean"]} | {r["lean_detail"][:120]} |')
    md += ["", "## Neither universal", "", "| target | law | Lean | Dafny | Dafny detail |", "|---|---|---|---|---|"]
    for tgt, r in all_rows:
        if r["dafny"] != "universal" and r["lean"] != "universal":
            md.append(f'| {tgt} | `{r["law"]}` | {r["lean"]} | {r["dafny"]} | {r["dafny_detail"][:160].replace("|", "/")} |')
    md += ["", "## Lean-only", "", "| target | law | Dafny | Dafny detail |", "|---|---|---|---|"]
    for tgt, r in all_rows:
        if r["lean"] == "universal" and r["dafny"] != "universal":
            md.append(f'| {tgt} | `{r["law"]}` | {r["dafny"]} | {r["dafny_detail"][:160].replace("|", "/")} |')
    md += ["", "## Runs", ""]
    for tgt, t in report["targets"].items():
        md.append(f'- **{tgt}** lean: `{t["lean_status"]}` {t["lean_summary"]}; dafny: `{t["dafny_status"]}` {t["dafny_finished"]} {t["btc"]}')
    with open(out_json, "w") as f:
        json.dump(report, f, indent=1, default=str)
    with open(out_md, "w") as f:
        f.write("\n".join(md) + "\n")


if __name__ == "__main__":
    main()
