//! Stage B artifact-certificate emitter: `aver compile --target wasm-gc --certify`.
//!
//! Emits, next to `<name>.wasm`, a self-contained Lean `cert/` project that
//! `lake build`s green with kernel-clean theorems for the user functions that
//! fall into the two measured classes:
//!
//! * straight-line `Int -> Int` add-a-constant (the `addTwo` kill-fast shape),
//! * single-argument self-recursion of the `sumTo` shape
//!   (`match n <= 0 { true -> 0; false -> n + f(n - 1) }`).
//!
//! Everything else is FAIL-CLOSED: listed in `cert-manifest.json` as
//! `source-level-only` with a reason. No weaker theorem is ever emitted.
//!
//! The certified-function bodies are read back from the module bytes the
//! compiler just emitted (the same bytes whose sha256 the certificate pins),
//! matched against the two structural templates, and re-rendered as
//! `CertPrelude.WInstr` data. A function whose real emitted body does not match
//! a template is declined — so the `WInstr` data in `Module.lean` is exactly
//! the shape present in the hashed bytes. Checker-side re-derivation of the
//! body from the bytes (avercheck) is Stage C and deliberately not built here.

use sha2::{Digest, Sha256};
use std::path::Path;

/// The Stage-A semantics prelude, single source of truth, embedded so the
/// emitter is self-contained.
const CERT_PRELUDE: &str = include_str!("../../../tools/certkit/prelude/CertPrelude.lean");
const LEAN_TOOLCHAIN: &str = include_str!("../../../tools/certkit/prelude/lean-toolchain");

/// The audited statement schema, single source of truth, embedded so both the
/// emitter and the `aver cert verify` checker pin the exact same bytes. The
/// consumer trusts the certificate by checking the final theorem NAME, the
/// manifest LITERAL, and the hash of THIS file plus the prelude — never Lean
/// proof syntax. Fixed content (no per-build parts) so its sha256 is known to
/// the checker at compile time.
pub const CERT_SCHEMA: &str = include_str!("Schema.lean");

/// Emitted-fragment profile and runtime ABI identifiers recorded in the
/// manifest. Stable strings the checker echoes; bumped when the certified
/// fragment or the runtime import surface changes.
pub const PROFILE_ID: &str = "AverUserProfile/v0";
pub const RUNTIME_ABI: &str = "aver-wasm-gc/0";
/// Certification level of a v0 artifact certificate: conditional on the named
/// runtime contracts (see the consult level naming L0/L1/L2/L3).
pub const CERT_LEVEL: &str = "L1";
/// The one approved final-theorem statement line. `aver cert verify` confirms
/// this exact line is present in `Final.lean` (name + `Holds manifest`), which
/// is what pins the statement without matching arbitrary Lean syntax.
pub const FINAL_THEOREM: &str = "AverCert.Final.cert";
pub const FINAL_STATEMENT_LINE: &str =
    "theorem AverCert.Final.cert : AverCert.Schema.Holds manifest";

/// sha256 of a byte slice, lowercase hex.
pub fn sha256_hex(bytes: &[u8]) -> String {
    let mut h = Sha256::new();
    h.update(bytes);
    hex(&h.finalize())
}

/// The content hashes of the audited schema and semantics prelude as embedded
/// in THIS binary — the checker's anchor: a cert whose on-disk `Schema.lean` /
/// `CertPrelude.lean` do not hash to these is not the audited version.
pub fn audited_schema_sha() -> String {
    sha256_hex(CERT_SCHEMA.as_bytes())
}
pub fn audited_prelude_sha() -> String {
    sha256_hex(CERT_PRELUDE.as_bytes())
}

/// A user function recovered from the emitted module.
struct UserFn {
    name: String,
    wasm_idx: u32,
    arity: usize,
    nlocals: usize,
    ops: Vec<Op>,
    /// call targets in body order, for reason reporting.
    calls: Vec<u32>,
    has_loop_or_branch: bool,
}

/// The minimal opcode surface the two templates need. Anything else is `Other`
/// (which forces a decline) — a certified body never contains an `Other`.
#[derive(Clone, PartialEq)]
enum Op {
    LocalGet(u32),
    LocalSet(u32),
    I64Const(i64),
    I32Const(i32),
    StructGet(u32, u32),
    RefIsNull,
    I64LeS,
    I32LtS,
    If,
    Else,
    End,
    Call(u32),
    Other,
}

/// A certified function and the template holes extracted from its body.
enum Cert {
    /// `fn(x: Int) -> Int = x + k`; box=`box_idx`, add=`add_idx`.
    StraightLine {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        k: i64,
        box_idx: u32,
        add_idx: u32,
    },
    /// sumTo-shape self-recursion; box/add/sub host helpers.
    Recursive {
        name: String,
        self_idx: u32,
        nlocals: usize,
        carrier: u32,
        box_idx: u32,
        add_idx: u32,
        sub_idx: u32,
    },
}

impl Cert {
    fn name(&self) -> &str {
        match self {
            Cert::StraightLine { name, .. } | Cert::Recursive { name, .. } => name,
        }
    }
    fn self_idx(&self) -> u32 {
        match self {
            Cert::StraightLine { self_idx, .. } | Cert::Recursive { self_idx, .. } => *self_idx,
        }
    }
    fn carrier(&self) -> u32 {
        match self {
            Cert::StraightLine { carrier, .. } | Cert::Recursive { carrier, .. } => *carrier,
        }
    }
    /// The Lean expression for the model this export simulates.
    fn model_expr(&self) -> String {
        match self {
            Cert::StraightLine { k, .. } => format!("fun n => n + ({k})"),
            Cert::Recursive { name, .. } => name.clone(),
        }
    }
    /// The Lean expression for the 2-arg host builder in `Obligation` shape
    /// (`add → sub → HostTbl`); straight-line ignores `sub`.
    fn host_expr(&self) -> String {
        match self {
            Cert::StraightLine { name, .. } => format!("fun add _ => CertModule.{name}Host add"),
            Cert::Recursive { name, .. } => format!("CertModule.{name}Host"),
        }
    }
}

/// Classification of every user function in the module.
pub struct Analysis {
    certs: Vec<Cert>,
    declined: Vec<(String, String)>,
    carrier: Option<u32>,
    contracts: Vec<String>,
}

impl Analysis {
    pub fn certified_names(&self) -> Vec<String> {
        self.certs.iter().map(|c| c.name().to_string()).collect()
    }
    pub fn declined(&self) -> &[(String, String)] {
        &self.declined
    }
}

/// Disassemble the emitted module and classify each user function.
pub fn analyze(wasm_bytes: &[u8]) -> Result<Analysis, String> {
    let (user_fns, box_idx, user_idx_set, carrier) = disassemble(wasm_bytes)?;

    let mut certs = Vec::new();
    let mut declined = Vec::new();
    for f in &user_fns {
        match classify(f, box_idx, carrier, &user_idx_set) {
            Ok(c) => certs.push(c),
            Err(reason) => declined.push((f.name.clone(), reason)),
        }
    }

    // Named runtime contracts actually consumed by the certified functions.
    let mut contracts = Vec::new();
    let mut has_box = false;
    let mut has_add = false;
    let mut has_sub = false;
    for c in &certs {
        match c {
            Cert::StraightLine { .. } => {
                has_box = true;
                has_add = true;
            }
            Cert::Recursive { .. } => {
                has_box = true;
                has_add = true;
                has_sub = true;
            }
        }
    }
    if has_box {
        contracts.push("__rt_aint_from_i64 (box i64 -> carrier)".to_string());
    }
    if has_add {
        contracts.push(
            "Int.add (carrier add = exact integer addition on represented values)".to_string(),
        );
    }
    if has_sub {
        contracts.push(
            "Int.sub (carrier sub = exact integer subtraction on represented values)".to_string(),
        );
    }

    Ok(Analysis {
        certs,
        declined,
        carrier,
        contracts,
    })
}

// ---- disassembly ---------------------------------------------------------

type DisasmResult = (
    Vec<UserFn>,
    u32,
    std::collections::HashSet<u32>,
    Option<u32>,
);

fn disassemble(wasm_bytes: &[u8]) -> Result<DisasmResult, String> {
    use wasmparser::{CompositeInnerType, Operator, Parser, Payload, StorageType, ValType};

    let mut num_imported_funcs: u32 = 0;
    // defined-function index -> declared type index
    let mut func_type_idx: Vec<u32> = Vec::new();
    // type index -> arity (param count) for func types
    let mut type_arity: std::collections::HashMap<u32, usize> = std::collections::HashMap::new();
    // export name -> func index
    let mut exports: Vec<(String, u32)> = Vec::new();
    // per defined-function code entry: (nlocals, ops, calls, has_loop_or_branch)
    let mut code_entries: Vec<(usize, Vec<Op>, Vec<u32>, bool)> = Vec::new();
    let mut carrier: Option<u32> = None;
    let mut next_type_idx: u32 = 0;

    for payload in Parser::new(0).parse_all(wasm_bytes) {
        let payload = payload.map_err(|e| format!("wasm parse: {e}"))?;
        match payload {
            Payload::TypeSection(reader) => {
                for rg in reader {
                    let rg = rg.map_err(|e| format!("type read: {e}"))?;
                    for sub in rg.into_types() {
                        let idx = next_type_idx;
                        next_type_idx += 1;
                        match &sub.composite_type.inner {
                            CompositeInnerType::Func(ft) => {
                                type_arity.insert(idx, ft.params().len());
                            }
                            // Int carrier: 3 fields, {i64, ref, i32}.
                            CompositeInnerType::Struct(st)
                                if carrier.is_none()
                                    && st.fields.len() == 3
                                    && matches!(
                                        st.fields[0].element_type,
                                        StorageType::Val(ValType::I64)
                                    )
                                    && matches!(
                                        st.fields[2].element_type,
                                        StorageType::Val(ValType::I32)
                                    ) =>
                            {
                                carrier = Some(idx);
                            }
                            _ => {}
                        }
                    }
                }
            }
            Payload::ImportSection(reader) => {
                // Compact import encoding groups imports; iterate each group.
                for group in reader {
                    let group = group.map_err(|e| format!("import read: {e}"))?;
                    for imp in group {
                        let (_, imp) = imp.map_err(|e| format!("import read: {e}"))?;
                        if let wasmparser::TypeRef::Func(_) = imp.ty {
                            num_imported_funcs += 1;
                        }
                    }
                }
            }
            Payload::FunctionSection(reader) => {
                for t in reader {
                    func_type_idx.push(t.map_err(|e| format!("func read: {e}"))?);
                }
            }
            Payload::ExportSection(reader) => {
                for ex in reader {
                    let ex = ex.map_err(|e| format!("export read: {e}"))?;
                    if ex.kind == wasmparser::ExternalKind::Func {
                        exports.push((ex.name.to_string(), ex.index));
                    }
                }
            }
            Payload::CodeSectionEntry(body) => {
                let mut nlocals = 0usize;
                let mut lr = body
                    .get_locals_reader()
                    .map_err(|e| format!("locals reader: {e}"))?;
                for _ in 0..lr.get_count() {
                    let (n, _ty) = lr.read().map_err(|e| format!("locals read: {e}"))?;
                    nlocals += n as usize;
                }
                let mut ops = Vec::new();
                let mut calls = Vec::new();
                let mut has_loop_or_branch = false;
                let mut opr = body
                    .get_operators_reader()
                    .map_err(|e| format!("ops reader: {e}"))?;
                while !opr.eof() {
                    let op = opr.read().map_err(|e| format!("op read: {e}"))?;
                    let mapped = match op {
                        Operator::LocalGet { local_index } => Op::LocalGet(local_index),
                        Operator::LocalSet { local_index } => Op::LocalSet(local_index),
                        Operator::I64Const { value } => Op::I64Const(value),
                        Operator::I32Const { value } => Op::I32Const(value),
                        Operator::StructGet {
                            struct_type_index,
                            field_index,
                        } => Op::StructGet(struct_type_index, field_index),
                        Operator::RefIsNull => Op::RefIsNull,
                        Operator::I64LeS => Op::I64LeS,
                        Operator::I32LtS => Op::I32LtS,
                        Operator::If { .. } => Op::If,
                        Operator::Else => Op::Else,
                        Operator::End => Op::End,
                        Operator::Call { function_index } => {
                            calls.push(function_index);
                            Op::Call(function_index)
                        }
                        Operator::ReturnCall { function_index } => {
                            calls.push(function_index);
                            Op::Other
                        }
                        Operator::Loop { .. }
                        | Operator::Block { .. }
                        | Operator::Br { .. }
                        | Operator::BrIf { .. }
                        | Operator::BrTable { .. } => {
                            has_loop_or_branch = true;
                            Op::Other
                        }
                        _ => Op::Other,
                    };
                    ops.push(mapped);
                }
                code_entries.push((nlocals, ops, calls, has_loop_or_branch));
            }
            _ => {}
        }
    }

    // Runtime helper names never certified as code.
    let is_runtime = |name: &str| {
        name.starts_with("__rt_")
            || name.starts_with("__caller")
            || name == "_start"
            || name == "memory"
    };

    let box_idx = exports
        .iter()
        .find(|(n, _)| n == "__rt_aint_from_i64")
        .map(|(_, i)| *i)
        .ok_or_else(|| "module has no __rt_aint_from_i64 box helper".to_string())?;

    // user export name -> wasm func index
    let mut user_exports: Vec<(String, u32)> = exports
        .iter()
        .filter(|(n, _)| !is_runtime(n))
        .cloned()
        .collect();
    user_exports.sort_by_key(|(_, i)| *i);

    let user_idx_set: std::collections::HashSet<u32> =
        user_exports.iter().map(|(_, i)| *i).collect();

    let mut user_fns = Vec::new();
    for (name, wasm_idx) in user_exports {
        let Some(def_idx) = wasm_idx.checked_sub(num_imported_funcs) else {
            continue;
        };
        let Some((nlocals, ops, calls, has_loop_or_branch)) =
            code_entries.get(def_idx as usize).cloned()
        else {
            continue;
        };
        let arity = func_type_idx
            .get(def_idx as usize)
            .and_then(|ti| type_arity.get(ti))
            .copied()
            .unwrap_or(0);
        user_fns.push(UserFn {
            name,
            wasm_idx,
            arity,
            nlocals,
            ops,
            calls,
            has_loop_or_branch,
        });
    }

    Ok((user_fns, box_idx, user_idx_set, carrier))
}

// ---- classification ------------------------------------------------------

fn classify(
    f: &UserFn,
    box_idx: u32,
    carrier: Option<u32>,
    user_idx_set: &std::collections::HashSet<u32>,
) -> Result<Cert, String> {
    // Strip a trailing `End` (function end) for the straight-line match.
    let ops: &[Op] = match f.ops.last() {
        Some(Op::End) => &f.ops[..f.ops.len() - 1],
        _ => &f.ops,
    };

    // Straight-line add-constant: [localGet 0, i64Const k, call box, call add].
    if let [Op::LocalGet(0), Op::I64Const(k), Op::Call(b), Op::Call(a)] = ops
        && *b == box_idx
        && *a != f.wasm_idx
        && !user_idx_set.contains(a)
        && f.arity == 1
    {
        let carrier =
            carrier.ok_or_else(|| "carrier struct type not found in module".to_string())?;
        return Ok(Cert::StraightLine {
            name: f.name.clone(),
            self_idx: f.wasm_idx,
            nlocals: f.nlocals,
            carrier,
            k: *k,
            box_idx,
            add_idx: *a,
        });
    }

    // sumTo-shape self-recursion.
    if let Some(cert) = match_recursive(f, box_idx) {
        return Ok(cert);
    }

    // ---- decline with an honest reason -----------------------------------
    if f.arity != 1 {
        return Err(format!(
            "unsupported signature ({} params); Stage-B templates cover single-argument Int functions",
            f.arity
        ));
    }
    if f.has_loop_or_branch {
        return Err(
            "body uses loops/branches outside the certified straight-line/recursive fragment"
                .to_string(),
        );
    }
    let calls_other_user = f
        .calls
        .iter()
        .any(|c| *c != f.wasm_idx && user_idx_set.contains(c));
    if calls_other_user {
        return Err(
            "calls other user functions (cross-function / mutual recursion), outside Stage-B scope"
                .to_string(),
        );
    }
    if f.ops.iter().any(|o| matches!(o, Op::Other)) {
        return Err(
            "body uses opcodes outside the certified fragment (strings / ADTs / effects / tail calls)"
                .to_string(),
        );
    }
    Err("body does not match a certified template (straight-line add-constant or single-argument self-recursion)".to_string())
}

/// Match the exact sumTo operator template, extracting `carrier` (from the
/// `struct.get` type index), `sub`, `add`, and confirming `self`/`box`.
fn match_recursive(f: &UserFn, box_idx: u32) -> Option<Cert> {
    use Op::*;
    if f.arity != 1 {
        return None;
    }
    let ops = &f.ops;
    let carrier = match ops.get(3) {
        Some(StructGet(c, 1)) => *c,
        _ => return None,
    };
    let l = match (ops.first(), ops.get(1)) {
        (Some(LocalGet(0)), Some(LocalSet(l))) => *l,
        _ => return None,
    };
    let expected_prefix = [
        LocalGet(0),
        LocalSet(l),
        LocalGet(l),
        StructGet(carrier, 1),
        RefIsNull,
        If,
        LocalGet(l),
        StructGet(carrier, 0),
        I64Const(0),
        I64LeS,
        Else,
        LocalGet(l),
        StructGet(carrier, 2),
        I32Const(0),
        I32LtS,
        End,
        If,
        I64Const(0),
        Call(box_idx),
        Else,
    ];
    if ops.len() < expected_prefix.len() {
        return None;
    }
    if ops[..expected_prefix.len()] != expected_prefix[..] {
        return None;
    }
    // recursion tail: localGet 0, localGet 0, i64Const 1, call box, call SUB,
    //                 call SELF, call ADD, End, End
    let tail = &ops[expected_prefix.len()..];
    let (b2, sub_idx, self_call, add_idx) = match tail {
        [
            LocalGet(0),
            LocalGet(0),
            I64Const(1),
            Call(b2),
            Call(sub),
            Call(sc),
            Call(add),
            End,
            End,
        ] => (*b2, *sub, *sc, *add),
        _ => return None,
    };
    if b2 != box_idx || self_call != f.wasm_idx {
        return None;
    }
    Some(Cert::Recursive {
        name: f.name.clone(),
        self_idx: f.wasm_idx,
        nlocals: f.nlocals,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
    })
}

// ---- model evaluation (anti-vacuity guard values) ------------------------

fn eval_sumto(n: i64) -> i64 {
    if n <= 0 { 0 } else { n + eval_sumto(n - 1) }
}

// ---- rendering -----------------------------------------------------------

/// Write the full `cert/` project. `model_files` are the (path, content) pairs
/// from the reused `aver proof` Lean emission (AverCommon + model modules).
pub fn write_project(
    out_dir: &Path,
    wasm_name: &str,
    wasm_bytes: &[u8],
    analysis: &Analysis,
    model_files: &[(String, String)],
) -> Result<(), String> {
    let cert_dir = out_dir.join("cert");
    std::fs::create_dir_all(&cert_dir).map_err(|e| format!("create cert dir: {e}"))?;

    // Copy in the semantics prelude + toolchain (single source of truth).
    write(&cert_dir, "CertPrelude.lean", CERT_PRELUDE)?;
    write(&cert_dir, "lean-toolchain", LEAN_TOOLCHAIN)?;

    // Copy the model files (AverCommon + <Module>.lean) verbatim.
    let mut model_roots: Vec<String> = Vec::new();
    for (path, content) in model_files {
        if path == "lakefile.lean" || path == "lean-toolchain" {
            continue;
        }
        write(&cert_dir, path, content)?;
        if let Some(stem) = path.strip_suffix(".lean") {
            model_roots.push(stem.to_string());
        }
    }

    let sha = {
        let mut h = Sha256::new();
        h.update(wasm_bytes);
        hex(&h.finalize())
    };

    write(&cert_dir, "Contracts.lean", &render_contracts(analysis))?;
    write(
        &cert_dir,
        "Module.lean",
        &render_module(analysis, wasm_name, &sha),
    )?;
    // Audited statement schema (fixed) + generated manifest literal + the one
    // final theorem that composes the per-export obligations.
    write(&cert_dir, "Schema.lean", CERT_SCHEMA)?;
    write(
        &cert_dir,
        "Manifest.lean",
        &render_manifest_lean(analysis, &model_roots, &sha),
    )?;
    write(
        &cert_dir,
        "Certificate.lean",
        &render_certificate(analysis, &model_roots),
    )?;
    write(&cert_dir, "Final.lean", &render_final(analysis))?;
    write(&cert_dir, "lakefile.lean", &render_lakefile(&model_roots))?;

    // Content hashes the checker re-verifies: the audited schema and the
    // semantics prelude. Pinning these plus the final theorem name and the
    // manifest literal is the whole trust story.
    let schema_sha = sha256_hex(CERT_SCHEMA.as_bytes());
    let prelude_sha = sha256_hex(CERT_PRELUDE.as_bytes());
    std::fs::write(
        cert_dir.join("cert-manifest.json"),
        render_manifest(analysis, wasm_name, &sha, &schema_sha, &prelude_sha),
    )
    .map_err(|e| format!("write manifest: {e}"))?;
    Ok(())
}

fn write(dir: &Path, name: &str, content: &str) -> Result<(), String> {
    std::fs::write(dir.join(name), content).map_err(|e| format!("write {name}: {e}"))
}

fn hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{b:02x}"));
    }
    s
}

fn render_contracts(analysis: &Analysis) -> String {
    let mut s = String::new();
    s.push_str(
        "/-\n  Named runtime-layer contracts consumed by the certificates in this project.\n\n\
         Each is threaded as an explicit HYPOTHESIS of the certificate theorems (the\n\
         `hadd` / `hAdd` / `hSub` / `boxRef` faces in `Certificate.lean`), never as a\n\
         Lean `axiom`, so `#print axioms` on every certificate theorem stays on the\n\
         core whitelist `[propext, Classical.choice, Quot.sound]`. The obligations\n\
         below are the \"prove once per toolchain release\" runtime layer; the\n\
         machine-readable list is `cert-manifest.json`.\n\n",
    );
    if analysis.contracts.is_empty() {
        s.push_str("  (no user function was certified — no runtime contracts consumed)\n");
    } else {
        for c in &analysis.contracts {
            s.push_str(&format!("  * {c}\n"));
        }
    }
    s.push_str("-/\n");
    s
}

fn render_module(analysis: &Analysis, wasm_name: &str, sha: &str) -> String {
    let mut s = String::new();
    s.push_str(&format!(
        "-- Emitted user-function bodies as `CertPrelude.WInstr` data, plus the\n\
         -- sha256 of the final `{wasm_name}.wasm` bytes (pinned).\n\
         import CertPrelude\n\nnamespace CertModule\nopen CertPrelude\n\n",
    ));
    s.push_str(&format!(
        "/-- sha256 of the certified `{wasm_name}.wasm` module bytes. -/\n\
         def wasmSha256 : String := \"{sha}\"\n\n",
    ));
    for c in &analysis.certs {
        s.push_str(&render_code_def(c));
        s.push('\n');
        s.push_str(&render_host_def(c));
        s.push('\n');
    }
    s.push_str("end CertModule\n");
    s
}

/// The runtime host-contract wiring for a certified body, as data in
/// `CertModule` so both the certificate proofs and the manifest reference the
/// one definition.
fn render_host_def(c: &Cert) -> String {
    match c {
        Cert::StraightLine {
            name,
            carrier,
            box_idx,
            add_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + add contracts). -/\n\
             def {name}Host (add : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  else none\n",
        ),
        Cert::Recursive {
            name,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
            ..
        } => format!(
            "/-- Runtime host wiring for `{name}` (box + add + sub contracts). -/\n\
             def {name}Host (add sub : List WVal → Option WVal) : HostTbl := fun fn =>\n  \
             if fn = {box_idx} then some (1, boxRef {carrier})\n  \
             else if fn = {add_idx} then some (2, add)\n  \
             else if fn = {sub_idx} then some (2, sub)\n  else none\n",
        ),
    }
}

fn render_code_def(c: &Cert) -> String {
    match c {
        Cert::StraightLine {
            name,
            self_idx,
            nlocals,
            k,
            box_idx,
            add_idx,
            ..
        } => format!(
            "/-- Verbatim emitted body of `{name}` (straight-line add-constant). -/\n\
             def {name}Code : CodeTbl := fun fn =>\n  \
             if fn = {self_idx} then some ⟨1, {nlocals}, \
             [.localGet 0, .i64Const ({k}), .call {box_idx}, .call {add_idx}]⟩ else none\n",
        ),
        Cert::Recursive {
            name,
            self_idx,
            nlocals,
            carrier,
            box_idx,
            add_idx,
            sub_idx,
        } => format!(
            "/-- Verbatim emitted body of `{name}` (self-recursive). -/\n\
             def {name}Code : CodeTbl := fun fn =>\n  \
             if fn = {self_idx} then some ⟨1, {nlocals},\n    \
             [ .localGet 0, .localSet 1,\n      \
             .localGet 1, .structGet {carrier} 1, .refIsNull,\n      \
             .ifElse [.localGet 1, .structGet {carrier} 0, .i64Const 0, .i64LeS]\n              \
             [.localGet 1, .structGet {carrier} 2, .i32Const 0, .i32LtS],\n      \
             .ifElse [.i64Const 0, .call {box_idx}]\n              \
             [.localGet 0, .localGet 0, .i64Const 1, .call {box_idx}, .call {sub_idx}, \
             .call {self_idx}, .call {add_idx}] ]⟩\n  else none\n",
        ),
    }
}

fn render_certificate(analysis: &Analysis, model_roots: &[String]) -> String {
    let mut s = String::new();
    s.push_str("import CertPrelude\nimport Module\nimport Schema\nimport Manifest\n");
    for r in model_roots {
        s.push_str(&format!("import {r}\n"));
    }
    s.push_str(
        "\nset_option linter.unusedSimpArgs false\n\
         set_option linter.unusedVariables false\n\
         set_option maxRecDepth 1000000\n\n\
         namespace CertProofs\nopen CertPrelude CertModule AverCert\n\n",
    );
    for c in &analysis.certs {
        match c {
            Cert::StraightLine { .. } => s.push_str(&render_straightline_cert(c)),
            Cert::Recursive { .. } => s.push_str(&render_recursive_cert(c)),
        }
        s.push('\n');
    }
    s.push_str("end CertProofs\n");
    s
}

fn render_straightline_cert(c: &Cert) -> String {
    let Cert::StraightLine {
        name,
        self_idx,
        carrier,
        k,
        box_idx,
        add_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    let g1 = k + 3;
    let g2 = k - 5;
    let _ = (box_idx, add_idx);
    format!(
        r#"/-! ### {name} — straight-line certificate (carrier type {carrier}) -/

/-- The VERBATIM emitted body of `{name}` maps any representation of `n` to a
    representation of `n + {k}`, for ALL `n : ℤ`, under the named runtime
    contract `hadd` (carrier add = exact integer addition on represented values). -/
theorem {name}_wasm_certified
    (S : ReprSpec {carrier})
    (add : List WVal → Option WVal)
    (hadd : ∀ a b va vb, S.Repr a va → S.Repr b vb →
          ∃ w, add [va, vb] = some w ∧ S.Repr (a + b) w) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w, wFuncN {name}Code ({name}Host add) 1 {self_idx} [v] = some w ∧ S.Repr (n + {k}) w := by
  intro n v hv
  obtain ⟨w, hw, hrepr⟩ := hadd n {k} v (carrierSmall {carrier} {k}) hv (S.smallIntro {k})
  refine ⟨w, ?_, hrepr⟩
  simp only [wFuncN, {name}Code, {name}Host, boxRef, carrierSmall, initLocals,
    wRunF, popArgs, List.getElem?_cons_zero, List.length, List.take, List.drop,
    List.reverse, List.replicate, if_true, reduceIte]
  simp only [carrierSmall] at hw
  simp [hw]

#print axioms {name}_wasm_certified

/-- Consumer-facing composition: whatever the bytes return represents the
    model value `n + {k}` (faithfulness law ∘ simulation). -/
theorem {name}_wasm_faithful
    (S : ReprSpec {carrier})
    (add : List WVal → Option WVal)
    (hadd : ∀ a b va vb, S.Repr a va → S.Repr b vb →
          ∃ w, add [va, vb] = some w ∧ S.Repr (a + b) w) :
    ∀ (n : Int) (v : WVal), S.Repr n v →
      ∃ w m, wFuncN {name}Code ({name}Host add) 1 {self_idx} [v] = some w ∧ S.Repr m w ∧ m = n + {k} :=
  fun n v hv =>
    let ⟨w, hrun, hrepr⟩ := {name}_wasm_certified S add hadd n v hv
    ⟨w, n + {k}, hrun, hrepr, rfl⟩

#print axioms {name}_wasm_faithful

-- anti-vacuity: the emitted body actually RUNS on concrete inputs.
def {name}HostRef : HostTbl := {name}Host (addRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt)
      = some ({g1}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 4 {self_idx} [carrierSmall {carrier} (-5)]).bind carrierToInt)
      = some ({g2}) := by native_decide

/-- Schema-shaped simulation obligation for `{name}` (composed by the single
    final theorem). Partial correctness over any fuel and representation. -/
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub hadd hsub fuel n v w hv hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  cases fuel with
  | zero => simp only [wFuncN, reduceCtorEq] at hrun
  | succ f =>
    rcases hc : add [v, carrierSmall {carrier} ({k})] with _ | r
    · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun
    · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, popArgs, initLocals, hc] at hrun
      subst hrun
      exact hadd n ({k}) v (carrierSmall {carrier} ({k})) r hv (S.smallIntro ({k})) hc
"#
    )
}

fn render_recursive_cert(c: &Cert) -> String {
    let Cert::Recursive {
        name,
        self_idx,
        carrier,
        box_idx,
        add_idx,
        sub_idx,
        ..
    } = c
    else {
        unreachable!()
    };
    let g3 = eval_sumto(3);
    let _ = (box_idx, add_idx, sub_idx);
    format!(
        r#"/-! ### {name} — self-recursive certificate (carrier type {carrier}) -/

-- model-side fuel bridge (the cap-induction pattern at R = 1).
theorem {name}_fuel_irrel :
    ∀ (t k1 k2 : Nat) (n : Int), n.natAbs < t → n.natAbs < k1 → n.natAbs < k2 →
      {name}__fuel k1 n = {name}__fuel k2 n := by
  intro t
  induction t with
  | zero => intro k1 k2 n ht _ _; omega
  | succ t ih =>
      intro k1 k2 n ht h1 h2
      cases k1 with
      | zero => omega
      | succ m1 =>
      cases k2 with
      | zero => omega
      | succ m2 =>
      by_cases hn : n ≤ 0
      · simp [{name}__fuel, hn]
      · have hrec := ih m1 m2 (n - 1) (by omega) (by omega) (by omega)
        simp only [{name}__fuel]
        rw [if_neg hn, if_neg hn, hrec]

theorem {name}_fuel_stable (k : Nat) (n : Int) (h : n.natAbs < k) :
    {name}__fuel k n = {name} n :=
  {name}_fuel_irrel (n.natAbs + k + 1) k (n.natAbs + 1) n (by omega) h (by omega)

theorem {name}_step (n : Int) (hn : ¬ n ≤ 0) : {name} n = n + {name} (n - 1) := by
  have h0 : {name} n = {name}__fuel (n.natAbs + 1) n := rfl
  rw [h0]
  simp only [{name}__fuel]
  rw [if_neg hn, {name}_fuel_stable n.natAbs (n - 1) (by omega)]

theorem {name}_base (n : Int) (hn : n ≤ 0) : {name} n = 0 := by
  have h0 : {name} n = {name}__fuel (n.natAbs + 1) n := rfl
  rw [h0]; simp [{name}__fuel, hn]

/-- THE CERTIFICATE THEOREM: partial correctness of the VERBATIM emitted
    recursive body against the generated model, for ALL n : ℤ. -/
theorem {name}_wasm_certified
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {carrier} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {carrier} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {carrier} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {carrier} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {carrier} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [v] = some w →
      Repr ({name} n) w := by
  intro fuel
  induction fuel with
  | zero =>
      intro n v w hv hrun
      simp [wFuncN] at hrun
  | succ fuel ih =>
      intro n v w hv hrun
      rcases hcar n v hv with ⟨s, sg, rfl⟩ | ⟨s, lty, les, sg, rfl⟩
      · have hs := hsmall_elim n s sg hv
        subst hs
        by_cases hle : s ≤ (0 : Int)
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rw [{name}_base s hle, ← hrun]
          exact hsmall_intro 0
        · simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hle] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .null, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (s - 1) vd :=
              hSub s 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({name} (s - 1)) vr := ih (s - 1) vd vr hrd hrec
              rcases hadd : add [.structv {carrier} [.i64v s, .null, .i32v sg], vr] with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step s hle, ← hrun]
                exact hAdd s ({name} (s - 1)) _ _ wa hv hrr hadd
      · obtain ⟨hsign, hne⟩ := hbig n s lty les sg hv
        by_cases hlt : sg < (0 : Int)
        · have hn0 : n ≤ 0 := by have := hsign.mp hlt; omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rw [{name}_base n hn0, ← hrun]
          exact hsmall_intro 0
        · have hn0 : ¬ n ≤ 0 := by
            intro hle
            have : ¬ n < 0 := fun h => hlt (hsign.mpr h)
            omega
          simp [wFuncN, wRunF, {name}Code, {name}Host, boxRef, b32,
            popArgs, initLocals, hlt] at hrun
          rcases hsub : sub [.structv {carrier} [.i64v s, .arr lty les, .i32v sg], carrierSmall {carrier} 1] with _ | vd
          · simp [hsub] at hrun
          · simp only [hsub] at hrun
            have hrd : Repr (n - 1) vd :=
              hSub n 1 _ _ vd hv (hsmall_intro 1) hsub
            rcases hrec : wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [vd] with _ | vr
            · simp [hrec] at hrun
            · simp only [hrec] at hrun
              have hrr : Repr ({name} (n - 1)) vr := ih (n - 1) vd vr hrd hrec
              rcases hadd : add [.structv {carrier} [.i64v s, .arr lty les, .i32v sg], vr] with _ | wa
              · simp [hadd] at hrun
              · simp only [hadd, Option.some.injEq] at hrun
                rw [{name}_step n hn0, ← hrun]
                exact hAdd n ({name} (n - 1)) _ _ wa hv hrr hadd

#print axioms {name}_wasm_certified

/-- Consumer-facing composition: whatever the bytes return represents the model
    value `{name} n` (faithfulness law ∘ simulation). -/
theorem {name}_wasm_faithful
    (Repr : Int → WVal → Prop)
    (hcar : ∀ n v, Repr n v →
      (∃ s sg, v = .structv {carrier} [.i64v s, .null, .i32v sg]) ∨
      (∃ s lty les sg, v = .structv {carrier} [.i64v s, .arr lty les, .i32v sg]))
    (hsmall_intro : ∀ k : Int, Repr k (carrierSmall {carrier} k))
    (hsmall_elim : ∀ n s sg, Repr n (.structv {carrier} [.i64v s, .null, .i32v sg]) → s = n)
    (hbig : ∀ n s lty les sg,
      Repr n (.structv {carrier} [.i64v s, .arr lty les, .i32v sg]) → ((sg < 0) ↔ (n < 0)) ∧ n ≠ 0)
    (add sub : List WVal → Option WVal)
    (hAdd : ∀ a b va vb w, Repr a va → Repr b vb → add [va, vb] = some w → Repr (a + b) w)
    (hSub : ∀ a b va vb w, Repr a va → Repr b vb → sub [va, vb] = some w → Repr (a - b) w) :
    ∀ (fuel : Nat) (n : Int) (v w : WVal), Repr n v →
      wFuncN {name}Code ({name}Host add sub) fuel {self_idx} [v] = some w →
      ∃ m : Int, Repr m w ∧ m = {name} n :=
  fun fuel n v w hv hrun =>
    ⟨{name} n,
     {name}_wasm_certified Repr hcar hsmall_intro hsmall_elim hbig add sub hAdd hSub fuel n v w hv hrun,
     rfl⟩

#print axioms {name}_wasm_faithful

-- anti-vacuity: the emitted body actually RUNS on concrete inputs.
def {name}HostRef : HostTbl := {name}Host (addRef {carrier}) (subRef {carrier})
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 3]).bind carrierToInt)
      = some ({g3}) := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} 0]).bind carrierToInt)
      = some 0 := by native_decide
example :
    ((wFuncN {name}Code {name}HostRef 20 {self_idx} [carrierSmall {carrier} (-4)]).bind carrierToInt)
      = some 0 := by native_decide

/-- Schema-shaped simulation obligation for `{name}` (composed by the single
    final theorem): the emitted recursive body simulates the model `{name}`. -/
theorem {name}_simulates : AverCert.Schema.Obligation.holds {name}Ob := by
  intro S add sub hadd hsub fuel n v w hv hrun
  simp only [{name}Ob, AverCert.Schema.Obligation.holds] at hrun ⊢
  exact {name}_wasm_certified S.Repr S.car S.smallIntro S.smallElim S.bigElim
    add sub hadd hsub fuel n v w hv hrun
"#
    )
}

/// The generated manifest literal, mirroring `cert-manifest.json`: the subject
/// metadata plus one `Obligation` per certified export. This is the LITERAL the
/// consumer pins.
fn render_manifest_lean(analysis: &Analysis, model_roots: &[String], sha: &str) -> String {
    let mut s = String::new();
    s.push_str("import Schema\nimport Module\n");
    for r in model_roots {
        s.push_str(&format!("import {r}\n"));
    }
    s.push_str(
        "\nset_option linter.unusedVariables false\n\n\
         namespace AverCert\nopen AverCert.Schema CertPrelude\n\n",
    );
    // One obligation def per certified export.
    for c in &analysis.certs {
        let name = c.name();
        s.push_str(&format!(
            "abbrev {name}Ob : Schema.Obligation :=\n  \
             {{ export_ := \"{name}\", policy := .simulatesModel, carrier := {carrier},\n    \
             code := CertModule.{name}Code, host := {host}, self := {self_idx}, model := {model} }}\n\n",
            carrier = c.carrier(),
            host = c.host_expr(),
            self_idx = c.self_idx(),
            model = c.model_expr(),
        ));
    }
    // Subject + manifest.
    let exports = analysis
        .certs
        .iter()
        .map(|c| format!("\"{}\"", c.name()))
        .collect::<Vec<_>>()
        .join(", ");
    let contracts = analysis
        .contracts
        .iter()
        .map(|c| lean_str(c))
        .collect::<Vec<_>>()
        .join(", ");
    let obligations = analysis
        .certs
        .iter()
        .map(|c| format!("{}Ob", c.name()))
        .collect::<Vec<_>>()
        .join(", ");
    s.push_str(&format!(
        "def manifest : Schema.Manifest :=\n  \
         {{ subject :=\n      \
         {{ artifactHash := \"{sha}\",\n        \
         profile := \"{PROFILE_ID}\",\n        \
         abi := \"{RUNTIME_ABI}\",\n        \
         exports := [{exports}],\n        \
         contracts := [{contracts}] }},\n    \
         obligations := [{obligations}] }}\n\n\
         end AverCert\n",
    ));
    s
}

/// The single final theorem: `AverCert.Final.cert : Holds manifest`, proved by
/// composing the per-export `_simulates` obligations. No other final theorem is
/// emitted; the checker pins this exact statement line.
fn render_final(analysis: &Analysis) -> String {
    let mut s = String::new();
    s.push_str(
        "import Certificate\nimport Manifest\nimport Schema\n\n\
         set_option maxRecDepth 1000000\n\
         set_option linter.unusedSimpArgs false\n\n\
         open AverCert AverCert.Schema\n\n",
    );
    s.push_str(
        "/-- THE single artifact certificate: the pinned module hash is this module's\n\
        hash, and every certified export simulates its model under the named runtime\n\
        contracts. Proof composes the per-export obligations; nothing else. -/\n",
    );
    s.push_str(&format!("{FINAL_STATEMENT_LINE} := by\n"));
    if analysis.certs.is_empty() {
        s.push_str(
            "  refine ⟨rfl, ?_⟩\n  \
             intro o ho\n  \
             simp only [manifest, List.mem_nil_iff, List.not_mem_nil] at ho\n",
        );
    } else {
        s.push_str("  refine ⟨rfl, ?_⟩\n  intro o ho\n");
        s.push_str(
            "  simp only [manifest, List.mem_cons, List.mem_singleton, List.mem_nil_iff,\n    \
             List.not_mem_nil, or_false] at ho\n",
        );
        // `rcases` with one `rfl` per obligation, split on the disjunction.
        let pattern = std::iter::repeat_n("rfl", analysis.certs.len())
            .collect::<Vec<_>>()
            .join(" | ");
        s.push_str(&format!("  rcases ho with {pattern}\n"));
        // Every resulting goal is closed by exactly one export's obligation.
        let arms = analysis
            .certs
            .iter()
            .map(|c| format!("exact ⟨rfl, CertProofs.{}_simulates⟩", c.name()))
            .collect::<Vec<_>>()
            .join("\n    | ");
        s.push_str(&format!("  all_goals\n    first\n    | {arms}\n"));
    }
    s.push_str(&format!("\n#print axioms {FINAL_THEOREM}\n"));
    s
}

fn render_lakefile(model_roots: &[String]) -> String {
    let mut roots = vec!["`CertPrelude".to_string(), "`Contracts".to_string()];
    for r in model_roots {
        roots.push(format!("`{r}"));
    }
    roots.push("`Module".to_string());
    roots.push("`Schema".to_string());
    roots.push("`Manifest".to_string());
    roots.push("`Certificate".to_string());
    roots.push("`Final".to_string());
    format!(
        "import Lake\nopen Lake DSL\n\npackage «avercert» where\n  version := v!\"0.1.0\"\n\n\
         @[default_target]\nlean_lib «AverCert» where\n  srcDir := \".\"\n  roots := #[{}]\n",
        roots.join(", ")
    )
}

fn render_manifest(
    analysis: &Analysis,
    wasm_name: &str,
    sha: &str,
    schema_sha: &str,
    prelude_sha: &str,
) -> String {
    let mut s = String::new();
    s.push_str("{\n");
    s.push_str("  \"schema_version\": 1,\n");
    s.push_str(&format!("  \"wasm\": \"{wasm_name}.wasm\",\n"));
    s.push_str(&format!("  \"wasm_sha256\": \"{sha}\",\n"));
    s.push_str(&format!("  \"level\": \"{CERT_LEVEL}\",\n"));
    s.push_str(&format!("  \"profile\": \"{PROFILE_ID}\",\n"));
    s.push_str(&format!("  \"abi\": \"{RUNTIME_ABI}\",\n"));
    s.push_str(&format!("  \"final_theorem\": \"{FINAL_THEOREM}\",\n"));
    s.push_str(&format!("  \"schema_sha256\": \"{schema_sha}\",\n"));
    s.push_str(&format!("  \"prelude_sha256\": \"{prelude_sha}\",\n"));
    if let Some(c) = analysis.carrier {
        s.push_str(&format!("  \"carrier_type_index\": {c},\n"));
    } else {
        s.push_str("  \"carrier_type_index\": null,\n");
    }
    s.push_str("  \"runtime_contracts\": [");
    for (i, c) in analysis.contracts.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!("\n    {}", json_str(c)));
    }
    if !analysis.contracts.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    s.push_str("  \"certified\": [");
    for (i, c) in analysis.certs.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        let kind = match c {
            Cert::StraightLine { .. } => "straight-line",
            Cert::Recursive { .. } => "self-recursive",
        };
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"class\": \"{}\", \"policy\": \"simulatesModel\", \
             \"level\": \"{}\", \"theorem\": \"CertProofs.{}_wasm_certified\"}}",
            json_str(c.name()),
            kind,
            CERT_LEVEL,
            c.name()
        ));
    }
    if !analysis.certs.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("],\n");
    s.push_str("  \"source_level_only\": [");
    for (i, (name, reason)) in analysis.declined.iter().enumerate() {
        if i > 0 {
            s.push(',');
        }
        s.push_str(&format!(
            "\n    {{\"name\": {}, \"reason\": {}}}",
            json_str(name),
            json_str(reason)
        ));
    }
    if !analysis.declined.is_empty() {
        s.push_str("\n  ");
    }
    s.push_str("]\n}\n");
    s
}

/// A Lean string literal (escapes `"` and `\`); contract descriptions never
/// contain control characters.
fn lean_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}

fn json_str(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 2);
    out.push('"');
    for ch in s.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            _ => out.push(ch),
        }
    }
    out.push('"');
    out
}
