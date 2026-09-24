#!/usr/bin/env python3
"""Prove each law's exported statement in a standalone Lean file.

    run_attempts.py <models-dir> <laws-dir> <Kit.lean> <out-dir> [main-verdicts-dir]

<models-dir>/<target>/ is a stripped, built `aver proof` export (strip_proofs.py
then `lake build`). Each laws/<name>.lean names its target, module and theorem
in a header, and holds proof variants written against the placeholder
`STATEMENT`. For every law this script

  1. reads the exported theorem statement from the module (for a
     bounded-domain law, the sampled-domain premises `x = v1 ∨ x = v2 ...` are
     dropped, which gives the universal `∀ givens, when = true -> claim`);
  2. writes one file: `import <module>`, the kit, the module's `open`s, then
     every variant with STATEMENT replaced, inside the module's namespace, and
     `#print axioms` for each variant;
  3. compiles it with `lake env lean` in the export directory and records,
     per variant, errors and axioms. A variant counts as closed only with no
     error in its range and axioms within propext / Classical.choice /
     Quot.sound (so no sorry, no native_decide).

`core_*` variants must not mention the kit; `kit_*` variants may.
"""
import json
import pathlib
import re
import subprocess
import sys
import time

STD_AXIOMS = {"propext", "Classical.choice", "Quot.sound"}
MSG = re.compile(r"^(?P<file>[^\s:][^:]*):(?P<line>\d+):(?P<col>\d+): (?P<kind>error|warning|info): ?(?P<msg>.*)$")
AXIOMS = re.compile(r"^'(?P<name>[^']+)' (?:depends on axioms: \[(?P<ax>[^\]]*)\]|does not depend on any axioms)")
DECL = re.compile(r"^(?:set_option .* in\s*)?(?:private )?theorem (\w+)")


def header(path):
    info = {}
    for line in path.read_text().split("\n"):
        m = re.match(r"^-- (\w[\w ]*): (.*)$", line)
        if m:
            info[m.group(1).strip()] = m.group(2).strip()
        elif line.strip() and not line.startswith("--"):
            break
    return info


def split_top(s, sep):
    parts, depth, cur, i = [], 0, "", 0
    opens, closes = "([{⟨", ")]}⟩"
    while i < len(s):
        ch = s[i]
        if ch in opens:
            depth += 1
        elif ch in closes:
            depth -= 1
        if depth == 0 and s.startswith(sep, i):
            parts.append(cur)
            cur = ""
            i += len(sep)
            continue
        cur += ch
        i += 1
    parts.append(cur)
    return parts


def strip_domain(stmt):
    """Drop `x = a ∨ x = b ...` premises over bound variables."""
    m = re.match(r"∀ ((?:\([^()]*\)\s*)+),\s*", stmt)
    if not m:
        return stmt, []
    names = re.findall(r"\((\S+) :", m.group(1))
    parts = split_top(stmt[m.end():], " -> ")
    kept, dropped = [], []
    for p in parts[:-1]:
        alts = [a.strip() for a in split_top(p, " ∨ ")]
        var = next((n for n in names if all(a.startswith(n + " = ") for a in alts)), None)
        (dropped if var else kept).append(p)
    return stmt[: m.end()] + " -> ".join(kept + [parts[-1]]), dropped


def exported(model, module, theorem):
    text = (model / (module.replace(".", "/") + ".lean")).read_text()
    lines = text.split("\n")
    ns_line = next(i for i, l in enumerate(lines) if l.startswith("namespace "))
    ns = lines[ns_line].split()[1]
    opens = [l for l in lines[:ns_line] if l.startswith("open ")]
    opts = [l for l in lines[:ns_line] if l.startswith("set_option ")]
    cls = re.search(rf"^-- aver:law-class {re.escape(theorem)}(?:_part1)? (\S+)", text, re.M)
    stmt = None
    for name in (theorem, theorem + "_part1"):
        m = re.search(rf"^theorem {re.escape(name)} : (.*?):=\s*by\b", text, re.M | re.S)
        if m:
            stmt = m.group(1).strip()
            break
    return ns, opens, opts, (cls.group(1) if cls else "?"), stmt


def parse_messages(output, genname):
    msgs, cur = [], None
    for line in output.split("\n"):
        m = MSG.match(line)
        if m:
            cur = {"line": int(m.group("line")), "kind": m.group("kind"), "msg": m.group("msg"),
                   "file": m.group("file")}
            msgs.append(cur)
        elif cur is not None:
            cur["msg"] += "\n" + line
    return [m for m in msgs if m["file"].endswith(genname)]


def run_law(tpl, models, kit_body, outdir):
    info = header(tpl)
    model = models / info["target"]
    rec = {"file": tpl.name, **info}
    if not model.exists():
        rec["error"] = f"no model for target {info['target']}"
        return rec
    try:
        ns, opens, opts, cls, stmt = exported(model, info["module"], info["theorem"])
    except (FileNotFoundError, StopIteration) as e:
        rec["error"] = f"module not found: {e}"
        return rec
    rec["law_class"] = cls
    if stmt is None:
        rec["error"] = "theorem not found in the export"
        return rec
    universal, dropped = strip_domain(stmt)
    rec["exported_statement"] = stmt
    rec["statement"] = universal
    rec["dropped_premises"] = dropped
    body = tpl.read_text().replace("STATEMENT", universal)
    variants = [m.group(1) for m in re.finditer(r"^theorem ((?:core|kit)_\w+)", body, re.M)]
    for v in variants:
        seg = re.search(rf"^theorem {v} .*?(?=^\S|\Z)", body, re.M | re.S).group(0)
        if v.startswith("core_") and "AverKit" in seg:
            rec.setdefault("violations", []).append(f"{v} cites the kit")
    pre = [f"import {info['module']}", kit_body, *opts, "set_option maxHeartbeats 400000", *opens,
           f"namespace {ns}", ""]
    post = ["", f"end {ns}", ""] + [f"#print axioms {ns}.{v}" for v in variants]
    text = "\n".join(pre) + body + "\n".join(post) + "\n"
    gen = outdir / "gen" / tpl.name
    gen.parent.mkdir(parents=True, exist_ok=True)
    gen.write_text(text)
    lines = text.split("\n")
    starts = [(i + 1, DECL.match(l).group(1)) for i, l in enumerate(lines) if DECL.match(l)]
    t0 = time.time()
    try:
        p = subprocess.run(["lake", "env", "lean", str(gen.resolve())], cwd=model, capture_output=True,
                           text=True, timeout=1800)
        output, code = p.stdout + p.stderr, p.returncode
    except subprocess.TimeoutExpired as e:
        output, code = f"{e.stdout or ''}{e.stderr or ''}", "timeout"
    rec["seconds"] = round(time.time() - t0, 1)
    rec["exit"] = code
    (outdir / "logs").mkdir(exist_ok=True)
    (outdir / "logs" / (tpl.stem + ".log")).write_text(output)
    msgs = parse_messages(output, gen.name)

    def owner(line):
        name = None
        for start, n in starts:
            if start <= line:
                name = n
        return name

    axioms = {}
    for line in output.split("\n"):
        m = AXIOMS.match(line.strip())
        if m:
            axioms[m.group("name").split(".")[-1]] = [a.strip() for a in (m.group("ax") or "").split(",") if a.strip()]
    res = {}
    for v in variants:
        errs = [m["msg"][:600] for m in msgs if m["kind"] == "error" and owner(m["line"]) == v]
        ax = axioms.get(v)
        closed = not errs and ax is not None and set(ax) <= STD_AXIOMS
        res[v] = {"closed": closed, "errors": errs, "axioms": ax}
    rec["helper_errors"] = [f"{owner(m['line'])}: {m['msg'][:400]}" for m in msgs
                            if m["kind"] == "error" and owner(m["line"]) and not owner(m["line"]).startswith(("core_", "kit_"))]
    rec["preamble_errors"] = [m["msg"][:400] for m in msgs if m["kind"] == "error" and owner(m["line"]) is None]
    rec["variants"] = res
    core = [v for v in variants if v.startswith("core_") and res[v]["closed"]]
    kit = [v for v in variants if v.startswith("kit_") and res[v]["closed"]]
    rec["verdict"] = "core" if core else ("kit" if kit else "open")
    rec["closed_by"] = core or kit
    return rec


def kit_check(models, kit_path, outdir):
    kit = kit_path.read_text()
    names = re.findall(r"^theorem (\w+)", kit, re.M)
    model = next(p for p in sorted(models.iterdir()) if (p / "AverCommon.lean").exists())
    gen = outdir / "gen" / "KitCheck.lean"
    gen.parent.mkdir(parents=True, exist_ok=True)
    gen.write_text(kit + "\n" + "\n".join(f"#print axioms AverKit.{n}" for n in names) + "\n")
    p = subprocess.run(["lake", "env", "lean", str(gen.resolve())], cwd=model, capture_output=True, text=True,
                       timeout=1800)
    out = p.stdout + p.stderr
    (outdir / "logs").mkdir(exist_ok=True)
    (outdir / "logs" / "KitCheck.log").write_text(out)
    axioms = {}
    for line in out.split("\n"):
        m = AXIOMS.match(line.strip())
        if m:
            axioms[m.group("name").split(".")[-1]] = [a.strip() for a in (m.group("ax") or "").split(",") if a.strip()]
    errors = [m["msg"][:600] for m in parse_messages(out, gen.name) if m["kind"] == "error"]
    code_lines = [l for l in kit.split("\n") if l.strip() and not l.strip().startswith("--")]
    return {"exit": p.returncode, "errors": errors, "lemmas": {n: axioms.get(n) for n in names},
            "all_proven": p.returncode == 0 and not errors and all(axioms.get(n) is not None and set(axioms[n]) <= STD_AXIOMS for n in names),
            "lines_total": len(kit.split("\n")), "lines_nonblank_noncomment": len(code_lines)}


def main_verdicts(vdir):
    out = {}
    if not vdir or not vdir.exists():
        return out
    for f in vdir.rglob("proof_manifest.json"):
        try:
            for law in json.loads(f.read_text()).get("laws", []):
                out[law["theorem"].split(".")[-1]] = law.get("tier")
        except (ValueError, KeyError):
            pass
    return out


def main():
    models, laws, kit_path, outdir = map(pathlib.Path, sys.argv[1:5])
    vdir = pathlib.Path(sys.argv[5]) if len(sys.argv) > 5 else None
    outdir.mkdir(parents=True, exist_ok=True)
    kit_text = kit_path.read_text()
    kit_body = "\n".join(l for l in kit_text.split("\n") if not l.startswith("import "))
    kit = kit_check(models, kit_path, outdir)
    verdicts = main_verdicts(vdir)
    records = []
    for tpl in sorted(laws.glob("*.lean")):
        rec = run_law(tpl, models, kit_body, outdir)
        for name in (rec.get("theorem"), (rec.get("theorem") or "") + "_part1"):
            if name in verdicts:
                rec["main_tier"] = verdicts[name]
                break
        records.append(rec)
        print(f"{tpl.name}: {rec.get('verdict', rec.get('error'))} {rec.get('closed_by', '')}", flush=True)
    (outdir / "results.json").write_text(json.dumps({"kit": kit, "laws": records}, indent=2, ensure_ascii=False))
    md = ["## Kit", "", f"all proven: **{kit['all_proven']}**, {kit['lines_total']} lines "
          f"({kit['lines_nonblank_noncomment']} non-blank, non-comment)", ""]
    md += [f"- `{n}`: {', '.join(a) if a is not None else 'NOT CHECKED'}" for n, a in kit["lemmas"].items()]
    if kit["errors"]:
        md += ["", "```", *[e for e in kit["errors"]], "```"]
    md += ["", "## Laws", "", "| law | class | main tier | verdict | closed by | variants |", "|---|---|---|---|---|---|"]
    for r in records:
        vs = r.get("variants", {})
        cell = "<br>".join(f"{v}: {'ok' if x['closed'] else ('error' if x['errors'] else 'axioms ' + str(x['axioms']))}"
                           for v, x in vs.items())
        md.append(f"| {r.get('law', r['file'])} | {r.get('law_class', '')} | {r.get('main_tier', '')} | "
                  f"**{r.get('verdict', r.get('error'))}** | {', '.join(r.get('closed_by', []))} | {cell} |")
    md += ["", "## Errors (first per variant)", ""]
    for r in records:
        for v, x in r.get("variants", {}).items():
            if x["errors"]:
                md += [f"<details><summary>{r['file']} / {v}</summary>", "", "```", x["errors"][0][:1200], "```",
                       "</details>", ""]
        for e in r.get("helper_errors", []) + r.get("preamble_errors", []):
            md += [f"<details><summary>{r['file']} / helper or preamble</summary>", "", "```", e[:1200], "```",
                   "</details>", ""]
    (outdir / "results.md").write_text("\n".join(md) + "\n")


if __name__ == "__main__":
    main()
