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

def lean_side(d, p="lean", lake_name="lake.log"):
    res = {"ran": False, "laws": {}, "declined": {}, "notes": []}
    st = read(os.path.join(d, f"{p}.status"))
    res["status"] = st.strip().replace("\n", " ")
    js = read(os.path.join(d, f"{p}.json")).strip()
    j = None
    if js:
        try:
            j = json.loads(js[js.find("{"):])
        except Exception as e:  # noqa: BLE001
            res["notes"].append(f"lean.json unparsable: {e}")
    res["summary"] = {k: j.get(k) for k in ("passed", "sorries", "build_errors", "universal_laws",
                                           "bounded_laws", "declined", "model_panicked")} if j else None
    man = read(os.path.join(d, f"{p}.manifest.json"))
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
    lake = read(os.path.join(d, lake_name))
    res["lake_tail"] = lake[-3000:]
    lean_files = tar_texts(os.path.join(d, f"{p}.export.tgz"), ".lean")
    thm_ranges = defaultdict(list)  # file -> [(start, name)]
    for path, text in lean_files.items():
        for i, line in enumerate(text.splitlines(), 1):
            mm = re.match(r"\s*(?:private\s+)?theorem\s+(\S+)", line)
            if mm:
                thm_ranges[path].append((i, mm.group(1)))
    res["theorems"] = {name for v in thm_ranges.values() for _, name in v}
    res["classes"] = {}
    for text in lean_files.values():
        for mm in re.finditer(r"-- aver:law-class (\S+) (\S+) (\S+)", text):
            res["classes"][mm.group(1)] = mm.group(2)
    res["build_failed"] = "lake_exit=0" not in st
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


def lean_merged(law, lean):
    """First pass, unless an isolation pass ran; stripped laws keep pass one."""
    iso = lean.get("isolated")
    label_tail = f'{law["fn"]}.{law["law"]}'
    if iso is None or any(s.endswith(label_tail) and s.split(".")[-3:-2] in ([], [law["module"].split(".")[-1]])
                          for s in lean["stripped"]):
        st, det = lean_status(law, lean)
        if iso is not None and st in ("universal", "bounded", "bounded-domain"):
            st, det = "failed", "broke the whole Lean build (hard error); " + det
        return st, det
    st, det = lean_status(law, iso)
    return st, ("isolation pass: " + det) if det else "isolation pass"


def lean_status(law, lean):
    k = key_of(law["module"], law["fn"], law["law"])
    rec = lean["laws"].get(k)
    if rec is None and law["module"] != "<entry>":
        rec = lean["laws"].get(key_of("<entry>", law["fn"], law["law"]))
    thm = f'{law["fn"]}_law_{law["law"]}'

    def bare(n):
        return n.split(".")[-1].replace("'", "").replace("«", "").replace("»", "")
    iss = [i for n, v in lean["theorem_issues"].items() if bare(n) == thm for i in v]
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
    present = any(bare(n) == thm for n in lean["theorems"])
    if present and lean["build_failed"] and not lean["laws"]:
        cls = next((c for n, c in lean["classes"].items() if bare(n) == thm), "?")
        # The build failed elsewhere, so no axiom audit ran; this theorem itself
        # elaborated without error or sorry.
        return {"universal": "universal", "bounded": "bounded"}.get(cls, cls), "unaudited: build failed elsewhere"
    if not lean["ran"]:
        return "no_run", lean.get("status", "")
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


DECL_RE = re.compile(r"^\s*(?:ghost\s+)?(lemma|method|function|predicate|datatype|type|const)\b(?:\s*\{[^}]*\})*\s*([A-Za-z_][\w']*)")
REF_RE = re.compile(r"(?:\b(Aver_\w+|AverCommon)\.)?\b([A-Za-z_][\w']*)")


def dafny_side(d):
    res = {"ran": False, "notes": []}
    res["status"] = read(os.path.join(d, "dafny.status")).strip().replace("\n", " ")
    log = read(os.path.join(d, "dafny.log"))
    res["log_tail"] = log[-3000:]
    fin = re.search(r"Dafny program verifier finished with (\d+) verified, (\d+) errors?(?:, (\d+) time outs?)?", log)
    res["finished"] = fin.group(0) if fin else None
    res["ran"] = bool(fin)
    files = tar_texts(os.path.join(d, "dafny.export.tgz"), ".dfy")
    decls = []
    comments = []
    opened = defaultdict(set)
    for path, text in files.items():
        lines = text.splitlines()
        module = None
        found = []
        for i, line in enumerate(lines, 1):
            s = line.strip()
            if s.startswith("//"):
                cm = re.match(r"// Law (\S+?)(?::| is not exported)\s*(.*)$", s)
                if cm:
                    comments.append((path, cm.group(1), s))
                continue
            mm = re.match(r"module\s+(\S+)\s*\{", s)
            if mm and module is None:
                module = mm.group(1)
                continue
            om = re.match(r"import opened (\S+)", s)
            if om and module:
                opened[module].add(om.group(1))
            mm = DECL_RE.match(line)
            if mm:
                found.append((i, mm.group(1), mm.group(2)))
        module = module or "_default"
        for n, (start, kind, name) in enumerate(found):
            end = found[n + 1][0] - 1 if n + 1 < len(found) else len(lines)
            body = "\n".join(lines[start - 1:end])
            decls.append({"file": path, "module": module, "name": name, "kind": kind, "start": start,
                          "end": end, "body": body, "axiom": "assume {:axiom}" in body,
                          "opaque": kind == "function" and "{:axiom}" in lines[start - 1],
                          "bounded": kind == "lemma" and bool(re.search(r"_sample_\d+\(", body))})
    res["decls"] = decls
    res["comments"] = comments
    index = {(x["module"], x["name"]): x for x in decls}
    by_file = defaultdict(list)
    for x in decls:
        by_file[x["file"]].append(x)
    errs = defaultdict(list)
    unmapped = []
    for mm in re.finditer(r"(?m)^(\S+\.dfy)\((\d+),(\d+)\): Error: (.*)$", log):
        f, line, msg = os.path.normpath(mm.group(1)), int(mm.group(2)), mm.group(4)
        owner = None
        for p, lst in by_file.items():
            if p == f or p.endswith("/" + f) or f.endswith("/" + p) or os.path.basename(p) == os.path.basename(f) and p.endswith(f):
                for x in lst:
                    if x["start"] <= line <= x["end"]:
                        owner = x
        if owner:
            errs[(owner["module"], owner["name"])].append(msg[:240])
        else:
            unmapped.append(f"{f}:{line}: {msg[:200]}")
    res["unmapped_errors"] = unmapped
    outcomes = {}
    txt = read(os.path.join(d, "dafny.verification.txt"))
    for mm in re.finditer(r"Results for (\S+) \((correctness|well-formedness)\)\s*\n\s*Overall outcome: (\w+)", txt):
        nm, oc = mm.group(1).split(".")[-1], mm.group(3)
        if outcomes.get(nm, "Correct") == "Correct":
            outcomes[nm] = oc
    res["outcomes"] = outcomes

    def refs(x):
        out = set()
        code = re.sub(r"//[^\n]*", "", x["body"])
        for q, nm in REF_RE.findall(code):
            if nm == x["name"] and not q:
                continue
            cands = [(q, nm)] if q else [(x["module"], nm)] + [(o, nm) for o in opened.get(x["module"], ())]
            for c in cands:
                if c in index:
                    out.add(c)
                    break
        return out

    ref_cache = {}

    def cone(key):
        seen, todo = set(), [key]
        while todo:
            k = todo.pop()
            if k in seen:
                continue
            seen.add(k)
            if k not in ref_cache:
                ref_cache[k] = refs(index[k])
            todo += [r for r in ref_cache[k] if index[r]["kind"] in ("lemma", "function", "predicate", "const")]
        return seen

    res["cone"] = cone
    res["index"] = index
    res["_errs"] = errs
    res["errors"] = {f"{a}.{b}": v for (a, b), v in errs.items()}
    if not fin:
        res["notes"].append("no 'finished with' line: dafny rejected the program before verification")
    return res


def dafny_status(law, dafny):
    fn, name = law["fn"], law["law"]
    label = f"{fn}.{name}"
    mod_last = law["module"].split(".")[-1].lower()

    def in_module(x):
        m = x["module"].lower()
        return law["module"] == "<entry>" or m.endswith("_" + mod_last) or m == mod_last or \
            os.path.basename(x["file"])[:-4].lower() == mod_last

    for path, lab, text in dafny["comments"]:
        base = os.path.basename(path)[:-4].lower()
        if lab.rstrip(":") == label and (base == mod_last or law["module"] == "<entry>"):
            reason = text.split(":", 1)[1].strip() if ":" in text else text
            if "is not exported" in text:
                return "declined", reason[:300]
            return "not_exported", reason[:300]
    names = {f"{fn}_{name}"}
    lemmas = [x for x in dafny["decls"] if x["kind"] == "lemma" and in_module(x)]
    main = [x for x in lemmas if x["name"] in names or guided_label(x["name"]) == label]
    helpers = [x for x in lemmas if x not in main and any(x["name"].startswith(m["name"] + "_") for m in main)]
    if not main:
        if not dafny["decls"]:
            return "no_run", "no export"
        return "not_exported", "no lemma found"
    if not dafny["ran"]:
        return "no_run", "whole file rejected before verification (export gap): " + \
            "; ".join(dafny["notes"])[:200]
    group = main + helpers
    own = []
    for x in group:
        k = (x["module"], x["name"])
        own += [(x["name"], m) for m in dafny["_errs"].get(k, [])]
        oc = dafny["outcomes"].get(x["name"])
        if oc and oc != "Correct" and not dafny["_errs"].get(k):
            own.append((x["name"], f"outcome {oc}"))
    if own:
        classes = sorted({classify_dafny_error(m) for _, m in own})
        st = "timeout" if set(classes) <= {"timeout", "out_of_resource"} else "failed"
        return st, (",".join(classes) + " | " + "; ".join(f"{a}: {b}" for a, b in own[:3]))[:400]
    full = set()
    for x in group:
        full |= dafny["cone"]((x["module"], x["name"]))
    bad = [k for k in full if dafny["_errs"].get(k) and dafny["index"][k]["kind"] != "method"]
    if bad:
        why = "; ".join(f"{k[0]}.{k[1]}: {classify_dafny_error(dafny['_errs'][k][0])}" for k in sorted(bad)[:4])
        return "failed_supplier", why[:400]
    ax = [k for k in full if dafny["index"][k]["axiom"]]
    if ax:
        return "sorry", "assume {:axiom} in " + ",".join(k[1] for k in ax)[:200]
    if any(x["bounded"] for x in main):
        return "bounded", "per-sample dispatch"
    opq = [k[1] for k in full if dafny["index"][k]["opaque"]]
    return "universal", ("opaque fns in cone: " + ",".join(opq[:5])) if opq else ""


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
        if lean is not None and os.path.exists(os.path.join(dirs["lean"], "lean2.status")):
            lean["isolated"] = lean_side(dirs["lean"], "lean2", "lake2.log")
            lean["stripped"] = [x for x in read(os.path.join(dirs["lean"], "lean.stripped")).split() if x]
        dafny = dafny_side(dirs["dafny"]) if "dafny" in dirs else None
        rows = []
        for law in laws:
            ls, ld = lean_merged(law, lean) if lean else ("no_run", "")
            if ls == "bounded-domain":
                ls = "bounded"
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
            "lean_status": lean and lean["status"],
            "lean_stripped": (lean or {}).get("stripped", []),
            "lean_isolated_status": (lean or {}).get("isolated", {}).get("status") if lean and lean.get("isolated") else None, "lean_summary": lean and lean.get("summary"),
            "dafny_status": dafny and dafny["status"], "dafny_finished": dafny and dafny["finished"],
            "lean_counts": dict(Counter(r["lean"] for r in rows)),
            "dafny_counts": dict(Counter(r["dafny"] for r in rows)),
            "dafny_unmapped_errors": (dafny or {}).get("unmapped_errors", []),
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
