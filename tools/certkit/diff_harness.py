#!/usr/bin/env python3
"""Three-way differential harness for the certificate semantics.

For every user function of every fixture, generate seeded pseudo-random inputs
and run each input through:
  (a) the Aver VM             — `aver run`
  (b) the real wasm engine    — `aver run --wasm-gc`
  (c) the Lean interpreter    — `CertPrelude.wFuncN` on boxed inputs

(a)/(b) are driven by appending a `main` to the fixture that prints `f(input)`
for a list of inputs; (c) by generating a self-contained `Main.lean` that boxes
each input per the representation relation, runs `wFuncN`, and prints the
decoded result. Runtime helpers (box / Int.add / Int.sub) are supplied to the
Lean side as EXECUTABLE reference faces of the named contracts.

Outputs a per-opcode coverage matrix, case counts, and a divergence list. Any
divergence prints a full repro and exits nonzero. stdlib only.
"""
import json
import os
import random
import re
import struct
import pathlib
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import wat        # noqa: E402
import extract    # noqa: E402

ROOT = os.path.abspath(os.path.join(HERE, "..", ".."))
AVER = os.path.join(ROOT, "target", "debug", "aver")
PRELUDE_DIR = os.path.join(HERE, "prelude")
FIXTURES = os.path.join(HERE, "fixtures")
WORK = "/tmp/certkit_harness"

# The 39 measured user-code opcode mnemonics (see runtime_surface + census).
ALL_OPCODES = [
    "array.new_data", "array.new_fixed", "call", "else", "end",
    "f64.add", "f64.const", "f64.div", "f64.eq", "f64.ge", "f64.gt", "f64.le",
    "f64.lt", "f64.mul", "f64.sub", "i32.and", "i32.const", "i32.eq",
    "i32.gt_s", "i32.le_s", "i32.lt_s", "i64.const", "i64.eq", "i64.eqz",
    "i64.ge_s", "i64.gt_s", "i64.le_s", "i64.lt_s", "if", "local.get",
    "local.set", "ref.cast", "ref.is_null", "ref.null", "ref.test", "return",
    "return_call", "struct.get", "struct.new",
]

# Opcodes that MUST stay differentially tested across all three engines.
# Pinned from the 2026-07-05 baseline run (33/39). If a fixture regression
# drops one of these out of cross-engine coverage, the harness FAILS instead
# of silently relabeling it as interpreter-guarded.
EXPECTED_CROSS_ENGINE = sorted(set(ALL_OPCODES) - {
    "array.new_fixed", "i32.and", "i32.eq", "i64.eqz", "ref.null", "return",
})

SUPPORTED_IN = {"int", "float", "bool", "list_int", "adt"}
SUPPORTED_OUT = {"int", "float", "bool", "string"}


# ---- runtime-helper identification --------------------------------------

def _norm_body(lines):
    """Normalise a helper body so it matches across modules: mask every numeric
    immediate (callee indices, struct/array type indices, local indices and
    constants all shift between modules while the helper's opcode structure is
    fixed per release). add / sub / mul remain distinguishable by structure."""
    return "\n".join(re.sub(r"\d+", "#", ln) for ln in lines)


# operation name -> (aver expr, lean host-ref constructor, arity)
HELPER_REF = {
    "add": ("a + b", "addRef {C}", 2), "sub": ("a - b", "subRef {C}", 2),
    "mul": ("a * b", None, 2),
    "le": ("a <= b", "leRef", 2), "lt": ("a < b", "ltRef", 2),
    "ge": ("a >= b", "geRef", 2), "gt": ("a > b", "gtRef", 2),
    "eq": ("a == b", "eqRef", 2),
}


def fingerprint_ops():
    """Compile `op = a OP b` micro-modules and record each runtime helper's
    normalised body -> operation name, so any module's helper call indices can
    be resolved back to a named contract with an executable reference face."""
    fps = {}
    for name, (expr, _ref, _ar) in HELPER_REF.items():
        rty = "Bool" if name in ("le", "lt", "ge", "gt", "eq") else "Int"
        src = (f"module Fp\n    intent = \"fp\"\n    exposes [op]\n\n"
               f"fn op(a: Int, b: Int) -> {rty}\n    ? \"x\"\n    {expr}\n")
        path = os.path.join(WORK, f"fp_{name}.av")
        with open(path, "w") as fh:
            fh.write(src)
        w = wat.wat_of(compile_wasm(path))
        bodies = wat.function_bodies(w)
        opidx = wat.user_functions(w)["op"]
        calls = [int(l.split()[1]) for l in bodies[opidx]["lines"] if l.startswith("call ")]
        assert len(calls) == 1, f"expected one helper call in {name}, got {calls}"
        fps[_norm_body(bodies[calls[0]]["lines"])] = name
    return fps


# ---- compile helpers -----------------------------------------------------

def compile_wasm(av_path):
    subprocess.run([AVER, "compile", av_path, "--target", "wasm-gc", "-o", WORK],
                   check=True, capture_output=True, text=True)
    base = os.path.splitext(os.path.basename(av_path))[0]
    return os.path.join(WORK, base + ".wasm")


# ---- ADT parsing from Aver source ---------------------------------------

_TYPE_HDR = re.compile(r"^type\s+(\w+)\s*$")
_VARIANT = re.compile(r"^\s+(\w+)(?:\(([^)]*)\))?\s*$")
_AVER_KIND = {"Int": "int", "Float": "float", "Bool": "bool", "String": "string"}


def parse_types(src):
    """Aver `type` decls -> {TypeName: [(Variant, [field_kind,...]), ...]}."""
    types, cur = {}, None
    for line in src.splitlines():
        m = _TYPE_HDR.match(line)
        if m:
            cur = m.group(1)
            types[cur] = []
            continue
        if cur is not None:
            v = _VARIANT.match(line)
            if v and not line.lstrip().startswith(("?", "fn ", "verify", "intent")):
                fields = []
                if v.group(2):
                    fields = [_AVER_KIND.get(t.strip(), "unknown")
                              for t in v.group(2).split(",")]
                types[cur].append((v.group(1), fields))
            elif line.strip() and not line.startswith((" ", "\t")):
                cur = None
    return {k: vs for k, vs in types.items() if vs}


def _kind_to_valtype(kind, ctx):
    return {"int": f"refnull{ctx['carrier']}", "float": "f64", "bool": "i32",
            "string": f"refnull{ctx['string']}"}.get(kind, "?")


def map_variants_to_types(adt_types, wtypes, ctx):
    """variant (by field-kind sequence) -> wasm struct type index."""
    reserved = {ctx["carrier"], ctx["string"], ctx["cons"]}
    struct_idx = []
    for i, t in enumerate(wtypes):
        if t and t[0] == "struct" and i not in reserved:
            fields = [f.replace(" ", "").replace("(mut", "").replace(")", "") for f in t[1]]
            struct_idx.append((i, fields))
    out = {}
    for tyname, variants in adt_types.items():
        for vname, fkinds in variants:
            want = [_kind_to_valtype(k, ctx).replace(" ", "") for k in fkinds]
            matches = [i for i, fields in struct_idx
                       if _shape_eq(fields, want)]
            assert len(matches) == 1, \
                f"ambiguous/unmatched struct for {tyname}.{vname} want={want} got={matches}"
            out[(tyname, vname)] = matches[0]
    return out


def _shape_eq(fields, want):
    if len(fields) != len(want):
        return False
    for f, w in zip(fields, want):
        fn = f.replace("refnull", "refnull")
        if w == "f64" and "f64" in fn:
            continue
        if w.startswith("refnull") and w[7:] in re.findall(r"\d+", fn):
            continue
        if w == "i32" and fn == "i32":
            continue
        if w == "f64" and fn == "f64":
            continue
        return False
    return True


# ---- input generation ----------------------------------------------------

def gen_value(kind, rng, ctx, adt_types, variant_map):
    """Return (aver_expr, lean_term) for one input of the given kind."""
    if kind == "int":
        n = rng.choice([0, 1, -1, rng.randint(-50, 80)])
        return str(n) if n >= 0 else f"({n})", f"(carrierSmall {ctx['carrier']} ({n}))"
    if kind == "float":
        x = rng.randint(-400, 400) / 8.0            # exact binary fraction
        bits = struct.unpack("<Q", struct.pack("<d", x))[0]
        return _aver_float(x), f"(WVal.f64v (0x{bits:016x} : UInt64))"
    if kind == "bool":
        b = rng.random() < 0.5
        return ("true" if b else "false"), f"(WVal.i32v {1 if b else 0})"
    if kind == "list_int":
        n = rng.randint(0, 5)
        elems = [rng.randint(-20, 40) for _ in range(n)]
        aver = "[" + ", ".join(str(e) for e in elems) + "]"
        lean = "WVal.null"
        for e in reversed(elems):
            lean = f"(WVal.structv {ctx['cons']} [(carrierSmall {ctx['carrier']} ({e})), {lean}])"
        return aver, lean
    if kind == "adt":
        tyname = next(iter(adt_types))
        vname, fkinds = rng.choice(adt_types[tyname])
        favers, fleans = [], []
        for fk in fkinds:
            a, l = gen_value(fk, rng, ctx, adt_types, variant_map)
            favers.append(a)
            fleans.append(l)
        if fkinds:
            aver = f"{tyname}.{vname}(" + ", ".join(favers) + ")"
        else:
            aver = f"{tyname}.{vname}"
        tyidx = variant_map[(tyname, vname)]
        lean = f"(WVal.structv {tyidx} [" + ", ".join(fleans) + "])"
        return aver, lean
    raise ValueError(f"cannot generate kind {kind}")


def _aver_float(x):
    s = repr(x)
    if "." not in s and "e" not in s and "E" not in s:
        s += ".0"
    return s if x >= 0 else f"({s})"


# ---- Lean side -----------------------------------------------------------

DECODE = {
    "int": "carrierToInt", "bool": "asBoolS", "float": "asF64S", "string": "asStr",
}


def gen_main_lean(module, lean_arms, host_arms, cases):
    """Self-contained Main.lean: extracted code table + host + case prints."""
    prints = []
    for (fname, idx, args_lean, out_kind) in cases:
        args = "[" + ", ".join(args_lean) + "]"
        dec = DECODE[out_kind]
        prints.append(
            f'  IO.println ("R\\t" ++ fmt ((wFuncN {module}Code {module}Host 4000 {idx} {args}).bind {dec}))')
    arms = "\n".join(lean_arms)
    harms = "\n".join(host_arms)
    body = "\n".join(prints)
    return f"""import CertPrelude
open CertPrelude

def {module}Code : CodeTbl := fun fn =>
  match fn with
{arms}
  | _ => none

def {module}Host : HostTbl := fun fn =>
  match fn with
{harms}
  | _ => none

def asBoolS : WVal → Option String
  | WVal.i32v 0 => some "false" | WVal.i32v 1 => some "true" | _ => none
def asF64S : WVal → Option String
  | WVal.f64v b => some ("B:" ++ toString b.toNat)  -- raw IEEE-754 bits (Lean toString Float is lossy)
  | _ => none
def asStr : WVal → Option String
  | WVal.arr _ es =>
      (es.mapM (fun v => match v with
        | WVal.i32v n => some (Char.ofNat n.toNat) | _ => none)).map String.ofList
  | _ => none
def carrierToIntS : WVal → Option String := fun v => (carrierToInt v).map toString

def fmt : Option String → String
  | some s => s
  | none => "<BOT>"

def main : IO Unit := do
{body}
"""


# Note: `carrierToInt` returns `Option Int`; we wrap it so every decoder has type
# `WVal → Option String`.  Patch the int decoder name:
DECODE["int"] = "carrierToIntS"


# ---- driver (Aver) -------------------------------------------------------

def gen_driver_av(src, cases):
    """Fixture source + a `main` that prints each call result, one per line."""
    lines = []
    for (fname, _idx, args_aver, _out) in cases:
        call = f"{fname}(" + ", ".join(args_aver) + ")"
        lines.append(f'    Console.print("{{{call}}}")')
    main = "\nfn main() -> Unit\n    ! [Console.print]\n" + "\n".join(lines) + "\n"
    return src + main


def run_capture(cmd):
    r = subprocess.run(cmd, capture_output=True, text=True)
    if r.returncode != 0:
        return None, r.stderr
    return r.stdout.splitlines(), None


# ---- comparison ----------------------------------------------------------

def _f64_bits(s):
    """Canonical IEEE-754 bits for a float column: `B:<nat>` from Lean is the
    exact bit pattern; VM/wasm print a round-trip decimal we re-pack. This makes
    the comparison bit-exact and immune to formatter differences."""
    s = s.strip()
    if s.startswith("B:"):
        return int(s[2:])
    return struct.unpack("<Q", struct.pack("<d", float(s)))[0]


def values_equal(kind, a, b):
    if a is None or b is None:
        return a == b
    if kind == "float":
        try:
            return _f64_bits(a) == _f64_bits(b)
        except ValueError:
            return a == b
    return a == b


# ---- per-fixture run -----------------------------------------------------

def run_fixture(av_path, fingerprints, exports_box_name, n_cases, rng):
    name = os.path.splitext(os.path.basename(av_path))[0]
    src = open(av_path).read()
    module = "".join(p.capitalize() for p in name.split("_"))
    wasm = compile_wasm(av_path)
    meta, lean_arms = extract.extract(wasm)
    w = wat.wat_of(wasm)
    wtypes = wat.parse_types(w)
    bodies = wat.function_bodies(w)
    exports = wat.parse_exports(w)
    ctx = {"carrier": meta["carrier"], "string": meta["string"], "cons": meta["cons"]}
    adt_types = parse_types(src)
    variant_map = map_variants_to_types(adt_types, wtypes, ctx) if adt_types else {}

    userfns = {f["idx"]: f for f in meta["functions"]}

    # Resolve runtime-helper call slots -> Lean host arms.
    box_idx = None
    for ename, (kind, idx) in exports.items():
        if ename == exports_box_name:
            box_idx = idx
    host_arms, unresolved = {}, {}
    for f in meta["functions"]:
        for c in f["calls"]:
            t = c["idx"]
            if t in userfns:
                continue
            if t == box_idx:
                host_arms[t] = f"  | {t} => some (1, boxRef {ctx['carrier']})"
            else:
                op = fingerprints.get(_norm_body(bodies.get(t, {}).get("lines", [])))
                ref = HELPER_REF.get(op, (None, None, None))[1] if op else None
                if ref is not None:
                    ref = ref.replace("{C}", str(ctx["carrier"]))
                    host_arms[t] = f"  | {t} => some ({HELPER_REF[op][2]}, {ref})"
                elif op == "mul":
                    unresolved[t] = "Int.mul (no reference face in Stage A)"
                else:
                    unresolved[t] = f"unidentified helper (export={c['export']})"

    # Choose testable functions.
    testable, skipped = [], []
    for f in meta["functions"]:
        pk = [p["kind"] for p in f["params"]]
        rk = [r["kind"] for r in f["results"]]
        if len(rk) != 1 or rk[0] not in SUPPORTED_OUT or any(k not in SUPPORTED_IN for k in pk):
            skipped.append((f["name"], f"sig {pk}->{rk}"))
            continue
        if any(c["idx"] in unresolved for c in f["calls"]):
            bad = [unresolved[c["idx"]] for c in f["calls"] if c["idx"] in unresolved]
            skipped.append((f["name"], f"unresolved helper: {bad[0]}"))
            continue
        if "adt" in pk and not adt_types:
            skipped.append((f["name"], "adt param but no type decl parsed"))
            continue
        testable.append(f)

    # Generate cases.
    cases_av, cases_lean = [], []
    for f in testable:
        for _ in range(n_cases):
            avers, leans = [], []
            for p in f["params"]:
                a, l = gen_value(p["kind"], rng, ctx, adt_types, variant_map)
                avers.append(a)
                leans.append(l)
            cases_av.append((f["name"], f["idx"], avers, f["results"][0]["kind"]))
            cases_lean.append((f["name"], f["idx"], leans, f["results"][0]["kind"]))

    # Run three ways, in chunks: a single `main` / `do` block with hundreds of
    # statements overflows the Aver compiler's (and lean --run's) stack, so we
    # split the cases into small driver files and concatenate the outputs.
    host_arm_list = list(host_arms.values()) or ["  | 0 => none"]
    main_lean = os.path.join(PRELUDE_DIR, "Main.lean")
    vm_out, wg_out, lean_out = [], [], []
    vm_err = wg_err = lean_err = None
    CHUNK = 60
    for s in range(0, len(cases_av), CHUNK):
        cav, clean = cases_av[s:s + CHUNK], cases_lean[s:s + CHUNK]
        drv = os.path.join(WORK, f"drv_{name}.av")
        with open(drv, "w") as fh:
            fh.write(gen_driver_av(src, cav))
        vo, ve = run_capture([AVER, "run", drv])
        wo, we = run_capture([AVER, "run", "--wasm-gc", drv])
        with open(main_lean, "w") as fh:
            fh.write(gen_main_lean(module, lean_arms, host_arm_list, clean))
        lr = subprocess.run(["lake", "env", "lean", "--run", "Main.lean"],
                            cwd=PRELUDE_DIR, capture_output=True, text=True)
        lo = ([ln[2:] for ln in lr.stdout.splitlines() if ln.startswith("R\t")]
              if lr.returncode == 0 else None)
        if vo is None:
            vm_out, vm_err = None, ve
        if wo is None:
            wg_out, wg_err = None, we
        if lo is None:
            lean_out, lean_err = None, lr.stderr
        if None in (vo, wo, lo):
            break
        vm_out += vo
        wg_out += wo
        lean_out += lo
    if os.path.exists(main_lean):
        os.remove(main_lean)

    return {
        "name": name, "module": module, "testable": [f["name"] for f in testable],
        "skipped": skipped, "unresolved": unresolved,
        "cases": cases_av, "vm": vm_out, "vm_err": vm_err,
        "wg": wg_out, "wg_err": wg_err, "lean": lean_out,
        "lean_err": lean_err,
        "opcodes_tested": _opcodes_of(bodies, [f["idx"] for f in testable]),
    }


def _opcodes_of(bodies, idxs):
    ops = set()
    for i in idxs:
        for ln in bodies.get(i, {}).get("lines", []):
            if ln.split():
                ops.add(ln.split()[0])
    return ops


# ---- main ----------------------------------------------------------------

def main():
    os.makedirs(WORK, exist_ok=True)
    if not os.path.exists(AVER):
        print(f"FATAL: {AVER} missing; build with `cargo build --bin aver --features wasm`")
        sys.exit(2)
    # Ensure the prelude is built (Main.lean imports the compiled olean).
    subprocess.run(["lake", "build"], cwd=PRELUDE_DIR, check=True, capture_output=True)

    n_cases = int(os.environ.get("CERTKIT_N", "60"))
    rng = random.Random(20260705)
    fingerprints = fingerprint_ops()

    fixtures = sorted(f for f in os.listdir(FIXTURES) if f.endswith(".av"))
    results, divergences, total_cases = [], [], 0
    covered = set()

    for fx in fixtures:
        r = run_fixture(os.path.join(FIXTURES, fx), fingerprints,
                        "__rt_aint_from_i64", n_cases, rng)
        results.append(r)
        # Backend availability sanity.
        for backend in ("vm", "wg", "lean"):
            if r[backend] is None:
                divergences.append(f"[{r['name']}] backend {backend} FAILED: "
                                   f"{(r.get(backend + '_err') or '')[:400]}")
        if None in (r["vm"], r["wg"], r["lean"]):
            continue
        n = len(r["cases"])
        if not (len(r["vm"]) == len(r["wg"]) == len(r["lean"]) == n):
            divergences.append(
                f"[{r['name']}] line-count mismatch vm={len(r['vm'])} "
                f"wg={len(r['wg'])} lean={len(r['lean'])} cases={n}")
            continue
        for i, (fname, idx, args, kind) in enumerate(r["cases"]):
            vm, wg, ln = r["vm"][i], r["wg"][i], r["lean"][i]
            ok_vw = values_equal(kind, vm, wg)
            ok_vl = values_equal(kind, vm, ln)
            if not (ok_vw and ok_vl):
                divergences.append(
                    f"[{r['name']}] {fname}({', '.join(args)}) :: "
                    f"VM={vm!r} WASM={wg!r} LEAN={ln!r} (kind={kind})")
            total_cases += 1
        covered |= r["opcodes_tested"]

    # ---- report ----
    print("=" * 72)
    print("CERTKIT THREE-WAY DIFFERENTIAL — VM vs wasm-gc vs Lean interpreter")
    print("=" * 72)
    print(f"fixtures: {len(fixtures)}   cases compared: {total_cases}   "
          f"divergences: {len(divergences)}")
    print()
    print("Per-fixture:")
    for r in results:
        nt = len(r["testable"])
        print(f"  {r['name']:16} testable={nt:2} "
              f"cases={len(r['cases']) if r['cases'] else 0:4} "
              f"skipped={len(r['skipped'])}")
        for sname, why in r["skipped"]:
            print(f"      - skip {sname}: {why}")

    print()
    print("Per-opcode coverage (cross-engine = in a differentially-tested function):")
    xeng = covered & set(ALL_OPCODES)
    guarded = set(ALL_OPCODES) - xeng
    # A non-cross-engine opcode only counts as covered when the prelude sanity
    # file really contains a guard for it: guards are introduced by comment
    # lines naming the opcode(s), e.g. `-- ref.null (+ ref.is_null)`.
    sanity_src = (pathlib.Path(__file__).parent / "prelude"
                  / "CertPreludeSanity.lean").read_text()
    guard_comment_ops = set()
    for line in sanity_src.splitlines():
        line = line.strip()
        if line.startswith("--"):
            for op in ALL_OPCODES:
                if op in line:
                    guard_comment_ops.add(op)
    dropped = sorted(set(EXPECTED_CROSS_ENGINE) - xeng)
    unguarded = sorted(op for op in guarded if op not in guard_comment_ops)
    missing = set(dropped) | set(unguarded)
    for op in ALL_OPCODES:
        tag = "cross-engine" if op in xeng else "interp-guard"
        print(f"  {'MISS' if op in missing else 'OK '} {op:16} {tag}")
    print(f"\n  cross-engine opcodes : {len(xeng)}/{len(ALL_OPCODES)}")
    print(f"  interp-guard opcodes : {len(guarded)}/{len(ALL_OPCODES)}  "
          f"(each verified to have a CertPreludeSanity guard)")
    if dropped:
        print(f"\nRESULT: FAIL — opcodes dropped out of cross-engine coverage: "
              f"{', '.join(dropped)} (a fixture regression?)")
        sys.exit(1)
    if unguarded:
        print(f"\nRESULT: FAIL — interp-guard opcodes without a sanity guard: "
              f"{', '.join(unguarded)}")
        sys.exit(1)

    print()
    if divergences:
        print(f"DIVERGENCES ({len(divergences)}):")
        for d in divergences[:50]:
            print("  " + d)
        print("\nRESULT: FAIL")
        sys.exit(1)
    if total_cases < 1000:
        print(f"RESULT: FAIL — only {total_cases} cases compared (<1000). "
              f"Raise CERTKIT_N.")
        sys.exit(1)
    print("RESULT: PASS — zero divergences, all 39 opcodes exercised "
          "(cross-engine + interpreter guards).")


if __name__ == "__main__":
    main()
