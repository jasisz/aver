// Included from engine/mod.rs (engine feature) — see the include! list there.

// ---- plan-equals-source bridges (schema 9) ------------------------------------
//
// Every certified export's obligation is stated over its plan. A bridge is the
// kernel-checked identification of that plan with the transpiled SOURCE
// function the model modules and the law-claims speak about. The statement is
// rendered from structure by `crate::bridge_statement` (the checker renders it
// again and pins it); this file decides which exports get one, derives the
// encoders from the plan's types and the emitted Lean model, and writes the
// proofs.
//
// The proofs follow the wall's two engines (`GrammarBridge.lean`):
//
// * per bridged function, ONE step lemma (`Step`): its plan body, with every
//   call answered by the callees' source images, returns its own source image.
//   The script unfolds the source function once (its equation lemma), expands
//   the argument shapes the decoder recognises, and evaluates symbolically;
// * per export, `exact_of_step` (a call closure without recursion) or
//   `bridge_of_step` (any closure) assembles the step lemmas of its closure.
//
// Nothing here is trusted. A name that does not exist fails the build (and so
// the package, which is why every name comes from the emitted Lean text); a
// proof that does not close falls to `sorry`, which the checker's per-bridge
// axiom audit turns into a not-credited bridge — never a failed package.

use crate::bridge_statement::{
    BridgeKind, MAX_BRIDGE_STATEMENT_LEN, ROOT_PREFIX, SourceEncoder, binder_names,
    is_plain_dotted_name, param_binders, pinned_from_expanded, render_bridge_statement,
    render_bridge_statement_expanded, statement_is_root_qualified,
    statement_is_single_plain_line, tuple_components,
};

/// The Lean source model of the certified module, as the compiler emitted it
/// (the `aver proof` model files and the law-claims its emitter recorded).
#[derive(Debug, Clone, Default)]
pub struct SourceModel {
    /// Model files `(relative path, content)`, lakefile and toolchain excluded.
    pub files: Vec<(String, String)>,
    /// The Lean namespace of the entry module's definitions.
    pub entry_namespace: String,
    /// Every dependency module as `(Aver module path, Lean namespace)`. A
    /// dependency function's wasm name is its Aver path flattened with `_`.
    pub dependency_namespaces: Vec<(String, String)>,
    pub law_claims: Vec<LawClaim>,
    /// Why there is no usable model at all (the emission panicked); every
    /// bridge and law-claim is then declined with this reason.
    pub failure: Option<String>,
    /// Which certified exports are offered a bridge.
    pub bridge_scope: BridgeScope,
}

/// Which certified exports the producer tries to bridge to their source.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum BridgeScope {
    /// Only the law cone: the functions some law-claim of the package
    /// mentions, and the functions their plans call, transitively. These are
    /// exactly the bridges a law-claim can cite.
    #[default]
    LawCone,
    /// Every certified export (`aver compile --certify --examples`), including
    /// the functions only `verify` examples reach.
    Every,
}

/// Why a certified export outside the law cone has no bridge.
pub const OUTSIDE_LAW_CONE_REASON: &str =
    "no law-claim reaches it (only verify examples or nothing do); pass --examples to bridge it";

impl SourceModel {
    pub fn failed(reason: String) -> Self {
        Self {
            failure: Some(reason),
            ..Self::default()
        }
    }
}

/// One declared plan-equals-source bridge, as the manifest transports it:
/// STRUCTURE, never statement text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceBridge {
    pub export: String,
    pub theorem: String,
    pub corollary: String,
    /// Fully qualified Lean name of the source function (no `_root_.`).
    pub model: String,
    pub kind: BridgeKind,
    pub params: Vec<SourceEncoder>,
    pub result: SourceEncoder,
}

/// Lean namespace every bridge theorem, corollary and helper lives in.
pub const BRIDGE_NAMESPACE: &str = "AverCert.Bridge";
/// The suffix the corollary name carries over the export name.
pub const BRIDGE_COROLLARY_SUFFIX: &str = "_certified";

impl SourceBridge {
    pub fn theorem_name(export: &str) -> String {
        format!("{BRIDGE_NAMESPACE}.{export}")
    }

    pub fn corollary_name(export: &str) -> String {
        format!("{BRIDGE_NAMESPACE}.{export}{BRIDGE_COROLLARY_SUFFIX}")
    }

    /// The pinned statement (what the checker renders and pins).
    pub fn statement(&self) -> String {
        render_bridge_statement(
            &self.export,
            &self.model,
            self.kind,
            &self.params,
            &self.result,
        )
    }

    /// The producer's own proof target, restated as [`Self::statement`] by the
    /// `_certified` corollary.
    pub fn expanded_statement(&self) -> String {
        render_bridge_statement_expanded(
            &self.export,
            &self.model,
            self.kind,
            &self.params,
            &self.result,
        )
    }

    /// The manifest JSON entry.
    pub fn to_json(&self) -> String {
        format!(
            "{{\"export\": {}, \"theorem\": {}, \"corollary\": {}, \"model\": {}, \"kind\": {}, \"params\": [{}], \"result\": {}}}",
            json_str(&self.export),
            json_str(&self.theorem),
            json_str(&self.corollary),
            json_str(&self.model),
            json_str(self.kind.tag()),
            self.params
                .iter()
                .map(SourceEncoder::to_json)
                .collect::<Vec<_>>()
                .join(", "),
            self.result.to_json()
        )
    }
}

/// Mirror of the checker's `validate_source_bridge_candidate`: a bridge the
/// checker would hard-reject fails candidate parsing for the WHOLE package
/// before Lean runs, so such a bridge is simply not declared.
fn bridge_survives_checker_gates(bridge: &SourceBridge) -> bool {
    if !crate::bridge_statement::is_plain_export_name(&bridge.export)
        || !is_plain_dotted_name(&bridge.model)
        || bridge.theorem != SourceBridge::theorem_name(&bridge.export)
        || bridge.corollary != SourceBridge::corollary_name(&bridge.export)
    {
        return false;
    }
    if !bridge
        .params
        .iter()
        .chain(std::iter::once(&bridge.result))
        .all(SourceEncoder::is_well_formed)
    {
        return false;
    }
    let statement = bridge.statement();
    statement_is_single_plain_line(&statement, MAX_BRIDGE_STATEMENT_LEN)
        && statement_is_root_qualified(&statement)
}

// ---- reading the emitted Lean model ------------------------------------------

/// A parsed Lean type expression of a model signature.
#[derive(Debug, Clone, PartialEq, Eq)]
enum LTy {
    /// A head applied to arguments (`Int`, `Option Int`, `Except String Int`).
    App(String, Vec<LTy>),
    /// `A × B × C`.
    Prod(Vec<LTy>),
}

fn parse_lean_ty(text: &str) -> Option<LTy> {
    let mut tokens = Vec::new();
    let mut chars = text.chars().peekable();
    while let Some(&c) = chars.peek() {
        if c.is_whitespace() {
            chars.next();
        } else if c == '(' || c == ')' || c == '×' {
            tokens.push(c.to_string());
            chars.next();
        } else if c.is_alphanumeric() || c == '_' || c == '.' || c == '\'' {
            let mut ident = String::new();
            while let Some(&d) = chars.peek() {
                if d.is_alphanumeric() || d == '_' || d == '.' || d == '\'' {
                    ident.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
            tokens.push(ident);
        } else {
            return None;
        }
    }
    let mut at = 0;
    let ty = parse_prod(&tokens, &mut at)?;
    (at == tokens.len()).then_some(ty)
}

fn parse_prod(tokens: &[String], at: &mut usize) -> Option<LTy> {
    let mut parts = vec![parse_app(tokens, at)?];
    while tokens.get(*at).map(String::as_str) == Some("×") {
        *at += 1;
        parts.push(parse_app(tokens, at)?);
    }
    if parts.len() == 1 {
        parts.pop()
    } else {
        // `×` nests to the right; flatten a right-nested product.
        let mut flat = Vec::new();
        let last = parts.pop()?;
        flat.extend(parts);
        match last {
            LTy::Prod(rest) => flat.extend(rest),
            other => flat.push(other),
        }
        Some(LTy::Prod(flat))
    }
}

fn parse_app(tokens: &[String], at: &mut usize) -> Option<LTy> {
    let head = match parse_atom(tokens, at)? {
        LTy::App(head, args) if args.is_empty() => head,
        other => return Some(other),
    };
    let mut args = Vec::new();
    while let Some(token) = tokens.get(*at) {
        if token == ")" || token == "×" {
            break;
        }
        args.push(parse_atom(tokens, at)?);
    }
    Some(LTy::App(head, args))
}

fn parse_atom(tokens: &[String], at: &mut usize) -> Option<LTy> {
    let token = tokens.get(*at)?;
    *at += 1;
    if token == "(" {
        let inner = parse_prod(tokens, at)?;
        if tokens.get(*at).map(String::as_str) != Some(")") {
            return None;
        }
        *at += 1;
        Some(inner)
    } else if token == ")" || token == "×" {
        None
    } else {
        Some(LTy::App(token.clone(), Vec::new()))
    }
}

/// One `def` of the model: its namespace, parameter types and result type,
/// as written, and its body text (the lines up to the next blank line).
#[derive(Debug, Clone)]
struct LeanDef {
    qualified: String,
    namespace: String,
    module: String,
    params: Vec<String>,
    ret: String,
    body: String,
}

#[derive(Debug, Clone)]
struct LeanStructure {
    namespace: String,
    fields: Vec<(String, String)>,
}

#[derive(Debug, Clone)]
struct LeanInductive {
    namespace: String,
    ctors: Vec<(String, Vec<String>)>,
}

/// What the producer reads off the emitted model files: single-line `def`
/// signatures, `structure` fields and `inductive` constructors, by fully
/// qualified name. `partial` defs are opaque to Lean and are left out.
#[derive(Debug, Default)]
struct ModelInfo {
    defs: BTreeMap<String, LeanDef>,
    structures: BTreeMap<String, LeanStructure>,
    inductives: BTreeMap<String, LeanInductive>,
    /// Flat wasm name → qualified defs that flatten to it.
    by_flat: BTreeMap<String, Vec<String>>,
}

fn qualify(namespace: &str, name: &str) -> String {
    if namespace.is_empty() {
        name.to_string()
    } else {
        format!("{namespace}.{name}")
    }
}

/// `(p : T) (q : U) : R :=` — the parameter types and the result type.
fn parse_def_tail(tail: &str) -> Option<(Vec<String>, String)> {
    let before = tail.trim().strip_suffix(":=")?.trim_end();
    let mut params = Vec::new();
    let mut rest = before;
    loop {
        let trimmed = rest.trim_start();
        if let Some(inner_start) = trimmed.strip_prefix('(') {
            let mut depth = 1usize;
            let mut end = None;
            for (at, ch) in inner_start.char_indices() {
                match ch {
                    '(' => depth += 1,
                    ')' => {
                        depth -= 1;
                        if depth == 0 {
                            end = Some(at);
                            break;
                        }
                    }
                    _ => {}
                }
            }
            let end = end?;
            let binder = &inner_start[..end];
            let (_, ty) = binder.split_once(" : ")?;
            params.push(ty.trim().to_string());
            rest = &inner_start[end + 1..];
        } else {
            let ret = trimmed.strip_prefix(':')?.trim();
            if ret.is_empty() {
                return None;
            }
            return Some((params, ret.to_string()));
        }
    }
}

impl ModelInfo {
    fn from_model(model: &SourceModel) -> Self {
        let mut info = Self::default();
        for (path, content) in &model.files {
            if let Ok(root) = crate::lean_gate::lean_module_root(path) {
                info.parse(content, &format!("{MODEL_PACKAGE_DIR}.{root}"));
            }
        }
        // Flat wasm names: the entry module's functions keep their bare name,
        // a dependency module's are its Aver path flattened with `_`.
        let mut flats: Vec<(String, String)> = Vec::new();
        for (qualified, def) in &info.defs {
            let bare = qualified
                .rsplit('.')
                .next()
                .unwrap_or(qualified)
                .trim_end_matches('\'')
                .to_string();
            if def.namespace == model.entry_namespace {
                flats.push((bare.clone(), qualified.clone()));
            }
            for (aver_path, lean_ns) in &model.dependency_namespaces {
                if def.namespace == *lean_ns {
                    flats.push((
                        format!("{}_{bare}", aver_path.replace('.', "_")),
                        qualified.clone(),
                    ));
                }
            }
        }
        for (flat, qualified) in flats {
            info.by_flat.entry(flat).or_default().push(qualified);
        }
        info
    }

    fn parse(&mut self, content: &str, module: &str) {
        let mut namespaces: Vec<String> = Vec::new();
        let lines: Vec<&str> = content.lines().collect();
        let mut i = 0;
        while i < lines.len() {
            let line = lines[i];
            let trimmed = line.trim_start();
            let ns = namespaces.join(".");
            if let Some(name) = line.strip_prefix("namespace ") {
                namespaces.push(name.trim().to_string());
            } else if let Some(name) = line.strip_prefix("end ") {
                if namespaces.last().map(String::as_str) == Some(name.trim()) {
                    namespaces.pop();
                }
            } else if let Some(rest) = line.strip_prefix("structure ") {
                if let Some(name) = rest.strip_suffix(" where") {
                    let mut fields = Vec::new();
                    let mut j = i + 1;
                    while j < lines.len() && lines[j].starts_with("  ") {
                        let field = lines[j].trim();
                        if let Some((fname, fty)) = field.split_once(" : ")
                            && is_plain_dotted_name(fname)
                            && !fname.contains('.')
                        {
                            fields.push((fname.to_string(), fty.trim().to_string()));
                        }
                        j += 1;
                    }
                    self.structures.insert(
                        qualify(&ns, name.trim()),
                        LeanStructure {
                            namespace: ns.clone(),
                            fields,
                        },
                    );
                    i = j;
                    continue;
                }
            } else if let Some(rest) = line.strip_prefix("inductive ") {
                if let Some(name) = rest.strip_suffix(" where") {
                    let mut ctors = Vec::new();
                    let mut j = i + 1;
                    while j < lines.len() && lines[j].starts_with("  | ") {
                        let ctor = lines[j].trim_start().trim_start_matches("| ").trim();
                        let cname = ctor.split_whitespace().next().unwrap_or("").to_string();
                        let fields = parse_def_tail(&format!(
                            "{} : Unit :=",
                            &ctor[cname.len()..]
                        ))
                        .map(|(params, _)| params);
                        if let Some(fields) = fields {
                            ctors.push((cname, fields));
                        } else {
                            ctors.clear();
                            break;
                        }
                        j += 1;
                    }
                    if !ctors.is_empty() {
                        self.inductives.insert(
                            qualify(&ns, name.trim()),
                            LeanInductive {
                                namespace: ns.clone(),
                                ctors,
                            },
                        );
                    }
                    i = j;
                    continue;
                }
            } else if let Some(rest) = trimmed.strip_prefix("def ") {
                let name = rest.split_whitespace().next().unwrap_or("");
                if !name.is_empty()
                    && is_plain_dotted_name(name)
                    && let Some((params, ret)) = parse_def_tail(&rest[name.len()..])
                {
                    let qualified = qualify(&ns, name);
                    let body = lines[i + 1..]
                        .iter()
                        .take_while(|l| !l.trim().is_empty())
                        .copied()
                        .collect::<Vec<_>>()
                        .join("\n");
                    self.defs.insert(
                        qualified.clone(),
                        LeanDef {
                            qualified,
                            namespace: ns.clone(),
                            module: module.to_string(),
                            params,
                            ret,
                            body,
                        },
                    );
                }
            }
            i += 1;
        }
    }

    /// The model def a wasm function name denotes, when exactly one does.
    fn def_for(&self, flat: &str) -> Result<&LeanDef, String> {
        match self.by_flat.get(flat).map(Vec::as_slice) {
            Some([one]) => Ok(&self.defs[one]),
            Some([]) | None => Err("the Lean source model has no definition for this function".into()),
            Some(_) => Err("several Lean source definitions flatten to this function's name".into()),
        }
    }

    /// The nullary definitions `def`'s body mentions, fully qualified. The
    /// optimized plan carries such a constant as its value, so a step proof
    /// unfolds it on the source side.
    fn inlined_constants(&self, def: &LeanDef) -> Vec<String> {
        let mut out = Vec::new();
        for token in crate::bridge_statement::statement_tokens(&def.body) {
            let found = self.resolve(&self.defs, &def.namespace, token);
            if let Some((qualified, target)) = found
                && target.params.is_empty()
                && qualified != def.qualified
                && !out.contains(&qualified)
            {
                out.push(qualified);
            }
        }
        out.sort();
        out
    }

    /// Resolve a type name as written inside namespace `ns` the way Lean
    /// does: the innermost enclosing namespace first.
    fn resolve<'a, T>(&self, map: &'a BTreeMap<String, T>, ns: &str, written: &str) -> Option<(String, &'a T)> {
        let mut scope: Vec<&str> = if ns.is_empty() { Vec::new() } else { ns.split('.').collect() };
        loop {
            let candidate = if scope.is_empty() {
                written.to_string()
            } else {
                format!("{}.{written}", scope.join("."))
            };
            if let Some(found) = map.get(&candidate) {
                return Some((candidate, found));
            }
            scope.pop()?;
        }
    }

    /// Derive the encoder of a source value whose plan type is `pty` and
    /// whose Lean type is written `lty` inside namespace `ns`. Every step
    /// cross-checks the byte-pinned layout (the type table) against the
    /// emitted Lean declaration; a disagreement declines.
    fn encoder(
        &self,
        pty: &PlanTy,
        lty: &LTy,
        ns: &str,
        tt: &PlanTypeTable,
        stack: &mut Vec<String>,
    ) -> Result<SourceEncoder, String> {
        let head = |name: &str| matches!(lty, LTy::App(h, a) if h == name && a.is_empty());
        let app1 = |name: &str| match lty {
            LTy::App(h, a) if h == name && a.len() == 1 => Some(&a[0]),
            _ => None,
        };
        match pty {
            PlanTy::Int if head("Int") => Ok(SourceEncoder::Int),
            PlanTy::Bool if head("Bool") => Ok(SourceEncoder::Bool),
            PlanTy::Float if head("Float") => Ok(SourceEncoder::Float),
            PlanTy::Str if head("String") => Ok(SourceEncoder::Str),
            PlanTy::Option(t) => match app1("Option") {
                Some(inner) => Ok(SourceEncoder::Option(Box::new(
                    self.encoder(t, inner, ns, tt, stack)?,
                ))),
                None => Err(format!("plan type Option has Lean type `{lty:?}`")),
            },
            PlanTy::Result(t, e) => match lty {
                LTy::App(h, a) if h == "Except" && a.len() == 2 => Ok(SourceEncoder::Result {
                    ok: Box::new(self.encoder(t, &a[1], ns, tt, stack)?),
                    err: Box::new(self.encoder(e, &a[0], ns, tt, stack)?),
                }),
                _ => Err("plan type Result has no matching `Except` Lean type".into()),
            },
            PlanTy::List(t) => match app1("List") {
                Some(inner) => Ok(SourceEncoder::List(Box::new(
                    self.encoder(t, inner, ns, tt, stack)?,
                ))),
                None => Err("plan type List has no matching Lean `List`".into()),
            },
            PlanTy::Vec(t) => match app1("Array") {
                Some(inner) => Ok(SourceEncoder::Vector(Box::new(
                    self.encoder(t, inner, ns, tt, stack)?,
                ))),
                None => Err("plan type Vector has no matching Lean `Array`".into()),
            },
            PlanTy::Record(tid) => {
                let decl = tt
                    .records
                    .iter()
                    .find(|r| r.tid == *tid)
                    .ok_or("the plan cites a record the type table does not declare")?;
                match lty {
                    LTy::Prod(elems) => {
                        if elems.len() != decl.fields.len() {
                            return Err("tuple arity differs from the byte-pinned layout".into());
                        }
                        let elems = decl
                            .fields
                            .iter()
                            .zip(elems)
                            .map(|(p, l)| self.encoder(p, l, ns, tt, stack))
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(SourceEncoder::Tuple { tid: *tid, elems })
                    }
                    LTy::App(name, args) if args.is_empty() => {
                        let (qualified, info) = self
                            .resolve(&self.structures, ns, name)
                            .ok_or_else(|| format!("`{name}` is not a Lean structure of the model"))?;
                        if stack.contains(&qualified) {
                            return Err(format!("`{qualified}` is a recursive type"));
                        }
                        if info.fields.len() != decl.fields.len() {
                            return Err(format!(
                                "`{qualified}` has {} fields, the byte-pinned layout {}",
                                info.fields.len(),
                                decl.fields.len()
                            ));
                        }
                        stack.push(qualified.clone());
                        let mut fields = Vec::new();
                        for ((fname, fty), pfield) in info.fields.iter().zip(&decl.fields) {
                            let lfield = parse_lean_ty(fty)
                                .ok_or_else(|| format!("unreadable field type `{fty}`"))?;
                            fields.push((
                                format!("{ROOT_PREFIX}{qualified}.{fname}"),
                                self.encoder(pfield, &lfield, &info.namespace, tt, stack)?,
                            ));
                        }
                        stack.pop();
                        Ok(SourceEncoder::Record {
                            tid: *tid,
                            lean_type: format!("{ROOT_PREFIX}{qualified}"),
                            fields,
                        })
                    }
                    _ => Err("plan record type has no matching Lean structure".into()),
                }
            }
            PlanTy::Sum(tid) => {
                let decl = tt
                    .sums
                    .iter()
                    .find(|s| s.tid == *tid)
                    .ok_or("the plan cites a sum the type table does not declare")?;
                let LTy::App(name, args) = lty else {
                    return Err("plan sum type has no matching Lean inductive".into());
                };
                if !args.is_empty() {
                    return Err("plan sum type has no matching Lean inductive".into());
                }
                let (qualified, info) = self
                    .resolve(&self.inductives, ns, name)
                    .ok_or_else(|| format!("`{name}` is not a Lean inductive of the model"))?;
                if stack.contains(&qualified) {
                    return Err(format!("`{qualified}` is a recursive type"));
                }
                if info.ctors.len() != decl.ctors.len() {
                    return Err(format!(
                        "`{qualified}` has {} constructors, the byte-pinned layout {}",
                        info.ctors.len(),
                        decl.ctors.len()
                    ));
                }
                stack.push(qualified.clone());
                let mut ctors = Vec::new();
                for ((cname, ftys), (_, pfields)) in info.ctors.iter().zip(&decl.ctors) {
                    if ftys.len() != pfields.len() {
                        return Err(format!("`{qualified}.{cname}` field count differs from the layout"));
                    }
                    let mut fields = Vec::new();
                    for (fty, pfield) in ftys.iter().zip(pfields) {
                        let lfield = parse_lean_ty(fty)
                            .ok_or_else(|| format!("unreadable field type `{fty}`"))?;
                        fields.push(self.encoder(pfield, &lfield, &info.namespace, tt, stack)?);
                    }
                    ctors.push((format!("{ROOT_PREFIX}{qualified}.{cname}"), fields));
                }
                stack.pop();
                Ok(SourceEncoder::Sum {
                    tid: *tid,
                    lean_type: format!("{ROOT_PREFIX}{qualified}"),
                    ctors,
                })
            }
            _ => Err(format!("plan type {pty:?} has no source encoding for Lean `{lty:?}`")),
        }
    }
}

// ---- decoders: the argument shapes a step lemma expands ---------------------

/// One argument shape a decoder recognises: its pattern binders, the `SVal`
/// pattern over them, and the source value it denotes.
#[derive(Debug, Clone)]
struct Alt {
    /// Atomic values the shape binds as a whole `SVal` and decodes:
    /// `(SVal variable, source variable, decoder)`, the decoder being the
    /// wall's `decodeStr` for a String or a `decList…` of the step lemmas for
    /// a List.
    binds: Vec<Bind>,
    pattern: String,
    source: String,
}

/// A value a shape decodes whole: `(SVal variable, source variable,
/// decoder)`.
type Bind = (String, String, String);

/// Most argument shapes one function's decoder may expand into.
const MAX_ALTS: usize = 64;

fn product(parts: Vec<Vec<Alt>>) -> Result<Vec<Vec<Alt>>, String> {
    let mut out: Vec<Vec<Alt>> = vec![Vec::new()];
    for part in parts {
        let mut next = Vec::new();
        for prefix in &out {
            for alt in &part {
                let mut row = prefix.clone();
                row.push(alt.clone());
                next.push(row);
                if next.len() > MAX_ALTS {
                    return Err(format!("more than {MAX_ALTS} argument shapes to expand"));
                }
            }
        }
        out = next;
    }
    Ok(out)
}

fn join_row(row: &[Alt]) -> (Vec<Bind>, Vec<String>, Vec<String>) {
    let mut binders = Vec::new();
    let mut patterns = Vec::new();
    let mut sources = Vec::new();
    for alt in row {
        binders.extend(alt.binds.iter().cloned());
        patterns.push(alt.pattern.clone());
        sources.push(alt.source.clone());
    }
    (binders, patterns, sources)
}

/// The step lemmas' own decoder of a List argument of Ints, Bools or Strings.
fn list_decoder(elem: &SourceEncoder) -> Option<&'static str> {
    match elem {
        SourceEncoder::Int => Some("decListInt"),
        SourceEncoder::Bool => Some("decListBool"),
        SourceEncoder::Str => Some("decListString"),
        _ => None,
    }
}

/// The element decoders of the List arguments whose elements are records,
/// tuples, sums, options, results or Lists: one `decElem_k` per distinct
/// element encoder, decoded one cons cell at a time by the generic `decList`
/// of `BridgeSupport`. The element decoder is derived from the element's
/// encoder, which is derived from the plan's element TYPE, never from a name:
/// a List argument whose plan type is another element type gets another
/// encoder, so another decoder and another statement.
#[derive(Debug, Default, Clone)]
struct ElemDecoders {
    /// In dependency order: a decoder's nested List elements come first.
    elems: Vec<ElemDecoder>,
    /// The decoders the shapes being derived name, at any depth.
    touched: BTreeSet<usize>,
}

#[derive(Debug, Clone)]
struct ElemDecoder {
    enc: SourceEncoder,
    alts: Vec<Alt>,
}

impl ElemDecoders {
    /// The decoder term of a List of `elem`, registering its element decoder.
    fn list_term(&mut self, elem: &SourceEncoder) -> Result<String, String> {
        if let Some(own) = list_decoder(elem) {
            return Ok(format!("AverCert.Bridge.{own}"));
        }
        let k = match self.elems.iter().position(|d| d.enc == *elem) {
            Some(k) => k,
            None => {
                let mut fresh = 0;
                let alts = alts(elem, &mut fresh, self)?;
                self.elems.push(ElemDecoder {
                    enc: elem.clone(),
                    alts,
                });
                self.elems.len() - 1
            }
        };
        self.touched.insert(k);
        Ok(format!(
            "(AverCert.Bridge.decList {} AverCert.Bridge.decElem_{k})",
            elem.grammar_ty()
        ))
    }
}

fn alts(enc: &SourceEncoder, fresh: &mut usize, decoders: &mut ElemDecoders) -> Result<Vec<Alt>, String> {
    const SVAL: &str = "_root_.AverCert.Grammar.SVal";
    let leaf = |ctor: &str, fresh: &mut usize| {
        let name = format!("t{fresh}");
        *fresh += 1;
        vec![Alt {
            binds: Vec::new(),
            pattern: format!("{SVAL}.{ctor} {name}"),
            source: name,
        }]
    };
    match enc {
        SourceEncoder::Int => Ok(leaf("i", fresh)),
        SourceEncoder::Bool => Ok(leaf("b", fresh)),
        // A String is matched as a whole value and decoded by the wall's
        // `decodeStr` (the inverse of its injective byte encoding).
        SourceEncoder::Str => {
            let v = format!("v{fresh}");
            let t = format!("t{fresh}");
            *fresh += 1;
            Ok(vec![Alt {
                binds: vec![(v.clone(), t.clone(), "AverCert.GrammarBridge.decodeStr".into())],
                pattern: v,
                source: t,
            }])
        }
        // A List is matched as a whole value and decoded one cons cell at a
        // time: by the step lemmas' `decList…` for Ints, Bools or Strings, by
        // the generic `decList` over the element's own decoder otherwise. A
        // step splits it into its empty and cons shapes (`…_split`).
        SourceEncoder::List(elem) => {
            let decoder = decoders.list_term(elem).map_err(|_| {
                format!(
                    "a `list` argument of `{}` elements has no decoder in this version",
                    elem.kind()
                )
            })?;
            let v = format!("v{fresh}");
            let t = format!("t{fresh}");
            *fresh += 1;
            Ok(vec![Alt {
                binds: vec![(v.clone(), t.clone(), decoder)],
                pattern: v,
                source: t,
            }])
        }
        SourceEncoder::Float | SourceEncoder::Vector(_) => {
            Err(format!("a `{}` argument has no decoder in this version", enc.kind()))
        }
        SourceEncoder::Record {
            tid,
            lean_type,
            fields,
        } => {
            let parts = fields
                .iter()
                .map(|(_, f)| alts(f, fresh, decoders))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(product(parts)?
                .into_iter()
                .map(|row| {
                    let (binds, patterns, sources) = join_row(&row);
                    Alt {
                        binds,
                        pattern: format!("{SVAL}.record {tid} [{}]", patterns.join(", ")),
                        source: format!("(⟨{}⟩ : {lean_type})", sources.join(", ")),
                    }
                })
                .collect())
        }
        SourceEncoder::Tuple { tid, elems } => {
            let parts = elems
                .iter()
                .map(|f| alts(f, fresh, decoders))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(product(parts)?
                .into_iter()
                .map(|row| {
                    let (binds, patterns, sources) = join_row(&row);
                    Alt {
                        binds,
                        pattern: format!("{SVAL}.record {tid} [{}]", patterns.join(", ")),
                        source: format!("({})", sources.join(", ")),
                    }
                })
                .collect())
        }
        SourceEncoder::Sum { tid, ctors, .. } => {
            let mut out = Vec::new();
            for (index, (ctor, fields)) in ctors.iter().enumerate() {
                let parts = fields
                    .iter()
                    .map(|f| alts(f, fresh, decoders))
                    .collect::<Result<Vec<_>, _>>()?;
                for row in product(parts)? {
                    let (binds, patterns, sources) = join_row(&row);
                    let source = if sources.is_empty() {
                        ctor.clone()
                    } else {
                        format!("({ctor} {})", sources.join(" "))
                    };
                    out.push(Alt {
                        binds,
                        pattern: format!("{SVAL}.variant {tid} {index} [{}]", patterns.join(", ")),
                        source,
                    });
                }
                if out.len() > MAX_ALTS {
                    return Err(format!("more than {MAX_ALTS} argument shapes to expand"));
                }
            }
            Ok(out)
        }
        SourceEncoder::Option(elem) => {
            let ty = elem.grammar_ty();
            let mut out = vec![Alt {
                binds: Vec::new(),
                pattern: format!("{SVAL}.none {ty}"),
                source: "_root_.Option.none".into(),
            }];
            for alt in alts(elem, fresh, decoders)? {
                out.push(Alt {
                    binds: alt.binds,
                    pattern: format!("{SVAL}.some {ty} ({})", alt.pattern),
                    source: format!("(_root_.Option.some {})", alt.source),
                });
            }
            Ok(out)
        }
        SourceEncoder::Result { ok, err } => {
            let (t, e) = (ok.grammar_ty(), err.grammar_ty());
            let mut out = Vec::new();
            for alt in alts(ok, fresh, decoders)? {
                out.push(Alt {
                    binds: alt.binds,
                    pattern: format!("{SVAL}.ok {t} {e} ({})", alt.pattern),
                    source: format!("(_root_.Except.ok {})", alt.source),
                });
            }
            for alt in alts(err, fresh, decoders)? {
                out.push(Alt {
                    binds: alt.binds,
                    pattern: format!("{SVAL}.err {t} {e} ({})", alt.pattern),
                    source: format!("(_root_.Except.error {})", alt.source),
                });
            }
            Ok(out)
        }
    }
}

/// The `rcases` pattern that splits a parameter into the constructors its
/// encoding matches on (a sum, option or result, at any depth inside records
/// and tuples), or `None` when the encoding reduces without a split.
fn rcases_pattern(enc: &SourceEncoder) -> Option<String> {
    let sub = |e: &SourceEncoder| rcases_pattern(e).unwrap_or_else(|| "_".to_string());
    match enc {
        SourceEncoder::Record { fields, .. } => fields
            .iter()
            .any(|(_, f)| rcases_pattern(f).is_some())
            .then(|| {
                format!(
                    "⟨{}⟩",
                    fields.iter().map(|(_, f)| sub(f)).collect::<Vec<_>>().join(", ")
                )
            }),
        SourceEncoder::Tuple { elems, .. } => elems.iter().any(|f| rcases_pattern(f).is_some()).then(|| {
            format!("⟨{}⟩", elems.iter().map(sub).collect::<Vec<_>>().join(", "))
        }),
        SourceEncoder::Sum { ctors, .. } => Some(format!(
            "({})",
            ctors
                .iter()
                .map(|(_, fields)| format!(
                    "⟨{}⟩",
                    fields.iter().map(sub).collect::<Vec<_>>().join(", ")
                ))
                .collect::<Vec<_>>()
                .join(" | ")
        )),
        SourceEncoder::Option(e) => Some(format!("(⟨⟩ | {})", sub(e))),
        SourceEncoder::Result { ok, err } => Some(format!("({} | {})", sub(err), sub(ok))),
        _ => None,
    }
}

/// Most List fields of one List element a step splits into their empty and
/// cons shapes (each split doubles the step's goals).
const MAX_ELEM_LIST_SPLITS: usize = 2;

/// The `rcases` pattern a step takes the head of a decoded List apart with:
/// [`rcases_pattern`]'s constructor splits, and the List fields of records
/// and tuples split into their empty and cons shapes too (a plan matching on
/// a field of the head meets a cons cell or an empty List, as it does for a
/// List argument), at most `budget` of them.
fn elem_split_pattern(enc: &SourceEncoder, budget: &mut usize) -> Option<String> {
    let sub = |e: &SourceEncoder, budget: &mut usize| {
        elem_split_pattern(e, budget).unwrap_or_else(|| "_".to_string())
    };
    match enc {
        SourceEncoder::List(_) if *budget > 0 => {
            *budget -= 1;
            Some("(_ | ⟨_, _⟩)".to_string())
        }
        SourceEncoder::Record { fields, .. } => {
            let parts: Vec<String> = fields.iter().map(|(_, f)| sub(f, budget)).collect();
            parts.iter().any(|p| p != "_").then(|| format!("⟨{}⟩", parts.join(", ")))
        }
        SourceEncoder::Tuple { elems, .. } => {
            let parts: Vec<String> = elems.iter().map(|f| sub(f, budget)).collect();
            parts.iter().any(|p| p != "_").then(|| format!("⟨{}⟩", parts.join(", ")))
        }
        SourceEncoder::Sum { ctors, .. } => Some(format!(
            "({})",
            ctors
                .iter()
                .map(|(_, fields)| format!(
                    "⟨{}⟩",
                    fields.iter().map(|f| sub(f, budget)).collect::<Vec<_>>().join(", ")
                ))
                .collect::<Vec<_>>()
                .join(" | ")
        )),
        SourceEncoder::Option(e) => Some(format!("(⟨⟩ | {})", sub(e, budget))),
        SourceEncoder::Result { ok, err } => {
            let e = sub(err, budget);
            Some(format!("({e} | {})", sub(ok, budget)))
        }
        _ => None,
    }
}

// ---- planning ---------------------------------------------------------------

/// Everything the renderer needs for one bridged function (exported or an
/// internal callee).
#[derive(Debug, Clone)]
struct BridgedFn {
    func_idx: u32,
    model: String,
    /// The actual model file, not inferred from the source namespace.
    model_root: String,
    /// A proof-local copy of the body in the same plan grammar. The thin
    /// step binding must identify it with `Plans.fn{func_idx}.body`.
    body: String,
    params: Vec<SourceEncoder>,
    result: SourceEncoder,
    /// Direct callees, deduplicated.
    callees: Vec<u32>,
    /// Whether the model definition is a fuel wrapper (`f x = f__fuel
    /// (natAbs x + 1) x`, the transpiler's shape for mutual recursion): the
    /// step unfolds both, and callers unfold the wrapper.
    fuel: bool,
    /// The decoder's argument shapes.
    shapes: Vec<DecoderShape>,
    /// The String literals its plan mentions (literal nodes and patterns).
    literals: BTreeSet<Vec<u8>>,
    /// Whether its plan calls itself (self recursion).
    recursive: bool,
    /// Nullary source definitions its model definition mentions, fully
    /// qualified: the plan carries them inlined as their values.
    constants: Vec<String>,
    /// The element decoders (`decElem_k`) its argument shapes name.
    elem_decoders: BTreeSet<usize>,
    /// Whether its plan calls a List helper (`len`, `reverse`, `concat`,
    /// `take`, `drop`, `contains`), whose results the step evaluates over
    /// encoded Lists.
    list_helpers: bool,
}

/// One decoder argument shape: the atomic values it decodes, its argument
/// patterns, and the source arguments it denotes.
type DecoderShape = (Vec<Bind>, Vec<String>, Vec<String>);

/// The outcome of bridge planning.
struct BridgePlan {
    fns: BTreeMap<u32, BridgedFn>,
    /// Exported bridges in certified-export order.
    bridges: Vec<(SourceBridge, u32)>,
    declined: Vec<(String, String)>,
    /// Depth of each function whose call closure has no recursion.
    depth: BTreeMap<u32, u32>,
    /// The String literals of the bridged plans, whose bytes the step proofs
    /// rewrite `strBytes "…"` to.
    literals: BTreeSet<Vec<u8>>,
    /// Some bridged plan calls `Result.withDefault`, so the models spell it
    /// `Except.withDefault` (and the model prelude defines it).
    with_default: bool,
    /// Per planned function: its position in `Plans.fnPlans` and its entry
    /// exactly as `Plans.lean` spells it.
    entries: BTreeMap<u32, (usize, String)>,
    /// The piece declarations `Plans.types` is written in, which the typing
    /// `simp` of an export theorem must unfold with it (the string segments
    /// excepted: typing never reads them).
    type_pieces: Vec<String>,
    /// The element decoders of the bridged functions' List arguments, and
    /// the model modules that declare their element types.
    elems: ElemDecoders,
    elem_roots: BTreeSet<String>,
}

/// The largest plan a bridge is attempted for. A step proof unfolds the
/// whole body and its callees' images at once, and its heartbeat cap does not
/// bound every tactic it runs: on k5's 247-node `divideBang` family one step
/// ran for minutes and the package's bridge build past any phase limit.
const MAX_BRIDGE_PLAN_NODES: usize = 100;

/// The number of nodes of a plan body (patterns not counted).
fn plan_nodes(e: &PlanExpr) -> usize {
    1 + match e {
        PlanExpr::Literal(_) | PlanExpr::Local(_) => 0,
        PlanExpr::Let(_, v, b) => plan_nodes(v) + plan_nodes(b),
        PlanExpr::Call(_, args)
        | PlanExpr::TailCall(_, args)
        | PlanExpr::RecordCreate(_, args)
        | PlanExpr::Construct(_, _, args)
        | PlanExpr::Interp(args)
        | PlanExpr::List(_, args) => args.iter().map(plan_nodes).sum(),
        PlanExpr::BinOp(_, l, r) => plan_nodes(l) + plan_nodes(r),
        PlanExpr::Neg(x) | PlanExpr::Project(_, _, x) | PlanExpr::Try(x, _) | PlanExpr::Scope(x) => {
            plan_nodes(x)
        }
        PlanExpr::If(c, t, el) => plan_nodes(c) + plan_nodes(t) + plan_nodes(el),
        PlanExpr::Match(s, arms) => plan_nodes(s) + arms.iter().map(|(_, b)| plan_nodes(b)).sum::<usize>(),
    }
}

/// Every String literal a plan mentions (literal nodes and literal
/// patterns), as bytes.
fn string_literals(e: &PlanExpr, out: &mut BTreeSet<Vec<u8>>) {
    match e {
        PlanExpr::Literal(PlanLit::Str(bytes)) => {
            out.insert(bytes.clone());
        }
        PlanExpr::Literal(_) | PlanExpr::Local(_) => {}
        PlanExpr::List(_, items) => items.iter().for_each(|a| string_literals(a, out)),
        PlanExpr::Let(_, v, b) => {
            string_literals(v, out);
            string_literals(b, out);
        }
        PlanExpr::Call(_, args)
        | PlanExpr::TailCall(_, args)
        | PlanExpr::RecordCreate(_, args)
        | PlanExpr::Construct(_, _, args)
        | PlanExpr::Interp(args) => args.iter().for_each(|a| string_literals(a, out)),
        PlanExpr::BinOp(_, l, r) => {
            string_literals(l, out);
            string_literals(r, out);
        }
        PlanExpr::Neg(x)
        | PlanExpr::Project(_, _, x)
        | PlanExpr::Try(x, _)
        | PlanExpr::Scope(x) => string_literals(x, out),
        PlanExpr::If(c, t, el) => {
            string_literals(c, out);
            string_literals(t, out);
            string_literals(el, out);
        }
        PlanExpr::Match(subject, arms) => {
            string_literals(subject, out);
            for (pat, body) in arms {
                if let PlanPat::LitStr(bytes) = pat {
                    out.insert(bytes.clone());
                }
                string_literals(body, out);
            }
        }
    }
}

/// A Lean string literal denoting exactly `bytes`, or `None` when they are
/// not UTF-8.
fn lean_string_literal(bytes: &[u8]) -> Option<String> {
    let text = std::str::from_utf8(bytes).ok()?;
    let mut out = String::from("\"");
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 || c as u32 == 0x7f => {
                out.push_str(&format!("\\x{:02x}", c as u32))
            }
            c => out.push(c),
        }
    }
    out.push('"');
    Some(out)
}

fn direct_callees(body: &PlanExpr) -> Vec<u32> {
    let mut targets = Vec::new();
    call_targets(body, &mut targets);
    targets.sort_unstable();
    targets.dedup();
    targets
}

fn closure_of(start: u32, fns: &BTreeMap<u32, BridgedFn>) -> Vec<u32> {
    let mut seen = BTreeSet::new();
    let mut work = vec![start];
    while let Some(f) = work.pop() {
        if seen.insert(f)
            && let Some(b) = fns.get(&f)
        {
            work.extend(b.callees.iter().copied());
        }
    }
    seen.into_iter().collect()
}

/// The plan builtins that call a List helper.
const LIST_HELPER_BUILTINS: [&str; 6] =
    [".listLen", ".listReverse", ".listConcat", ".listTake", ".listDrop", ".listContains"];

/// A function's decoder shapes, and the element decoders they name.
fn decoder_shapes(
    params: &[SourceEncoder],
    elems: &mut ElemDecoders,
) -> Result<(Vec<DecoderShape>, BTreeSet<usize>), String> {
    elems.touched.clear();
    let mut fresh = 0;
    let parts = params
        .iter()
        .map(|p| alts(p, &mut fresh, elems))
        .collect::<Result<Vec<_>, _>>()?;
    let shapes = product(parts)?.iter().map(|row| join_row(row)).collect();
    Ok((shapes, std::mem::take(&mut elems.touched)))
}

/// The planned functions a bridge is attempted for under
/// [`BridgeScope::LawCone`]: every function a law statement mentions by its
/// `_root_.` name (the only spelling a law cites a bridge by), closed over
/// the calls of the plans, since a bridge needs the bridges of its callees.
/// `None` under [`BridgeScope::Every`].
fn law_cone(
    analysis: &Analysis,
    model: &SourceModel,
    info: &ModelInfo,
    law_claims: &[LawClaim],
) -> Option<BTreeSet<u32>> {
    if model.bridge_scope == BridgeScope::Every {
        return None;
    }
    let mut mentioned: BTreeSet<&str> = BTreeSet::new();
    for claim in law_claims {
        for token in crate::bridge_statement::statement_tokens(&claim.statement) {
            if let Some(named) = token.strip_prefix(ROOT_PREFIX)
                && let Some((key, _)) = info.defs.get_key_value(named)
            {
                mentioned.insert(key.as_str());
            }
        }
    }
    let mut work: Vec<u32> = analysis
        .entries
        .iter()
        .filter(|e| {
            let flat = analysis.fn_names.get(&e.func_idx).unwrap_or(&e.name);
            info.def_for(flat)
                .is_ok_and(|def| mentioned.contains(def.qualified.as_str()))
        })
        .map(|e| e.func_idx)
        .collect();
    let bodies: BTreeMap<u32, &PlanExpr> = analysis
        .entries
        .iter()
        .map(|e| (e.func_idx, &e.plan.body))
        .collect();
    let mut cone = BTreeSet::new();
    while let Some(f) = work.pop() {
        if cone.insert(f)
            && let Some(body) = bodies.get(&f)
        {
            work.extend(direct_callees(body));
        }
    }
    Some(cone)
}

fn plan_bridges(analysis: &Analysis, model: &SourceModel, law_claims: &[LawClaim]) -> BridgePlan {
    let mut declined: Vec<(String, String)> = Vec::new();
    let mut plan = BridgePlan {
        fns: BTreeMap::new(),
        bridges: Vec::new(),
        declined: Vec::new(),
        depth: BTreeMap::new(),
        literals: BTreeSet::new(),
        with_default: false,
        elems: ElemDecoders::default(),
        elem_roots: BTreeSet::new(),
        type_pieces: analysis
            .types
            .lean_piece_names("types")
            .into_iter()
            .filter(|piece| !piece.starts_with("types_strSegs_"))
            .map(|piece| format!("AverCert.Plans.{piece}"))
            .collect(),
        entries: analysis
            .entries
            .iter()
            .enumerate()
            .map(|(index, e)| {
                let entry = format!(
                    "⟨{}, {}, {}, {}, AverCert.Plans.{}⟩",
                    lean_str(&e.name),
                    e.exported,
                    e.func_idx,
                    e.group,
                    plan_def_name(e.func_idx)
                );
                (e.func_idx, (index, entry))
            })
            .collect(),
    };
    if let Some(reason) = &model.failure {
        for c in &analysis.certified {
            declined.push((c.name.clone(), reason.clone()));
        }
        plan.declined = declined;
        return plan;
    }
    let info = ModelInfo::from_model(model);
    let cone = law_cone(analysis, model, &info, law_claims);
    // Candidate functions: every planned function in scope whose source
    // definition and encoders resolve.
    let mut reasons: BTreeMap<u32, String> = BTreeMap::new();
    for e in &analysis.entries {
        if cone.as_ref().is_some_and(|cone| !cone.contains(&e.func_idx)) {
            reasons.insert(e.func_idx, OUTSIDE_LAW_CONE_REASON.to_string());
            continue;
        }
        let flat = analysis
            .fn_names
            .get(&e.func_idx)
            .cloned()
            .unwrap_or_else(|| e.name.clone());
        let derived = (|| -> Result<BridgedFn, String> {
            // The source model has no `Bytes` encoding and the step lemmas no
            // `Bytes` builtin, so such a bridge could only fall to `sorry`.
            if e.plan.body.lean().contains("(.builtin .bytes") {
                return Err("the plan calls a Bytes builtin, which the bridge proofs do not model yet"
                    .to_string());
            }
            // An `Int` interpolation part is the helper's decimal bytes, which
            // the source model and the step lemmas do not spell yet.
            if e.plan.body.lean().contains("(.builtin .strFromInt)") {
                return Err("the plan interpolates an Int, which the bridge proofs do not model yet"
                    .to_string());
            }
            // An early return (`?`) is the source model's `Result` propagation,
            // which the one-step proofs do not unfold yet.
            if e.plan.body.lean().contains("(.try_ ") {
                return Err("the plan returns early through `?`, which the bridge proofs do not \
                            model yet"
                    .to_string());
            }
            if plan_nodes(&e.plan.body) > MAX_BRIDGE_PLAN_NODES {
                return Err(format!(
                    "the plan has more than {MAX_BRIDGE_PLAN_NODES} nodes, beyond what the \
                     one-step bridge proof unfolds in bounded time"
                ));
            }
            let body = e.plan.body.lean();
            let list_helpers = LIST_HELPER_BUILTINS
                .iter()
                .any(|b| body.contains(&format!("(.builtin {b})")));
            let def = info.def_for(&flat)?;
            if def.params.len() != e.plan.params.len() {
                return Err(format!(
                    "source arity {} differs from the plan's {}",
                    def.params.len(),
                    e.plan.params.len()
                ));
            }
            let mut params = Vec::new();
            for (pty, written) in e.plan.params.iter().zip(&def.params) {
                let lty = parse_lean_ty(written)
                    .ok_or_else(|| format!("unreadable Lean parameter type `{written}`"))?;
                params.push(info.encoder(pty, &lty, &def.namespace, &analysis.types, &mut Vec::new())?);
            }
            let lret = parse_lean_ty(&def.ret)
                .ok_or_else(|| format!("unreadable Lean result type `{}`", def.ret))?;
            let result = info.encoder(&e.plan.ret, &lret, &def.namespace, &analysis.types, &mut Vec::new())?;
            // Validated here; the shapes are derived again, over the one
            // element-decoder table of the surviving functions, below.
            let (shapes, elem_decoders) = decoder_shapes(&params, &mut ElemDecoders::default())?;
            let mut literals = BTreeSet::new();
            string_literals(&e.plan.body, &mut literals);
            let callees = direct_callees(&e.plan.body);
            Ok(BridgedFn {
                func_idx: e.func_idx,
                model: def.qualified.clone(),
                model_root: def.module.clone(),
                body,
                params,
                result,
                recursive: callees.contains(&e.func_idx),
                callees,
                fuel: info.defs.contains_key(&format!("{}__fuel", def.qualified)),
                shapes,
                literals,
                constants: info.inlined_constants(def),
                elem_decoders,
                list_helpers,
            })
        })();
        match derived {
            Ok(b) => {
                string_literals(&e.plan.body, &mut plan.literals);
                plan.with_default |= e.plan.body.lean().contains("(.lazy .resWithDefault)");
                plan.fns.insert(e.func_idx, b);
            }
            Err(reason) => {
                reasons.insert(e.func_idx, reason);
            }
        }
    }
    // A function whose callee has no bridge has none either (its step lemma
    // needs the callee's image).
    loop {
        let missing: Vec<(u32, u32)> = plan
            .fns
            .values()
            .filter_map(|b| {
                b.callees
                    .iter()
                    .find(|c| !plan.fns.contains_key(c))
                    .map(|c| (b.func_idx, *c))
            })
            .collect();
        if missing.is_empty() {
            break;
        }
        for (f, c) in missing {
            plan.fns.remove(&f);
            let callee = analysis
                .fn_names
                .get(&c)
                .cloned()
                .unwrap_or_else(|| format!("#{c}"));
            reasons.insert(f, format!("callee `{callee}` has no source bridge"));
        }
    }
    // One element-decoder table, over the surviving functions alone and in
    // their order, so it is the same for the same bridged functions.
    for b in plan.fns.values_mut() {
        let (shapes, elem_decoders) = decoder_shapes(&b.params, &mut plan.elems)
            .expect("the shapes derived once derive again");
        b.shapes = shapes;
        if !elem_decoders.is_empty() {
            plan.elem_roots.insert(b.model_root.clone());
        }
        b.elem_decoders = elem_decoders;
    }
    // Depth over the functions whose closure has no recursion.
    fn depth_of(
        f: u32,
        fns: &BTreeMap<u32, BridgedFn>,
        memo: &mut BTreeMap<u32, Option<u32>>,
        onpath: &mut BTreeSet<u32>,
    ) -> Option<u32> {
        if let Some(d) = memo.get(&f) {
            return *d;
        }
        if !onpath.insert(f) {
            return None;
        }
        let mut d = Some(0u32);
        for c in &fns[&f].callees {
            match depth_of(*c, fns, memo, onpath) {
                Some(cd) => d = d.map(|x| x.max(cd + 1)),
                None => d = None,
            }
        }
        onpath.remove(&f);
        memo.insert(f, d);
        d
    }
    let mut memo = BTreeMap::new();
    for f in plan.fns.keys().copied().collect::<Vec<_>>() {
        if let Some(d) = depth_of(f, &plan.fns, &mut memo, &mut BTreeSet::new()) {
            plan.depth.insert(f, d);
        }
    }
    for c in &analysis.certified {
        let Some(b) = plan.fns.get(&c.func_idx) else {
            declined.push((
                c.name.clone(),
                reasons
                    .get(&c.func_idx)
                    .cloned()
                    .unwrap_or_else(|| "no source function".to_string()),
            ));
            continue;
        };
        let kind = if plan.depth.contains_key(&c.func_idx) {
            BridgeKind::Exact
        } else {
            BridgeKind::Adequate
        };
        let bridge = SourceBridge {
            export: c.name.clone(),
            theorem: SourceBridge::theorem_name(&c.name),
            corollary: SourceBridge::corollary_name(&c.name),
            model: b.model.clone(),
            kind,
            params: b.params.clone(),
            result: b.result.clone(),
        };
        if !bridge_survives_checker_gates(&bridge) {
            declined.push((
                c.name.clone(),
                "statement or identifiers would be refused by the checker's source-bridge gates"
                    .into(),
            ));
            continue;
        }
        plan.bridges.push((bridge, c.func_idx));
    }
    plan.declined = declined
        .into_iter()
        .map(|(name, reason)| (name, clean_reason(&reason)))
        .collect();
    plan
}

// ---- rendering `Bridge.lean` --------------------------------------------------

const TYPING_SIMPS: &str = "AverCert.GrammarBridge.ArgsTyped, AverCert.Grammar.HasTyL, \
     AverCert.Grammar.HasTy, AverCert.Grammar.HasTyAll, AverCert.AcceptedArtifact.obligationOf, \
     AverCert.TypeTable.mctxOf, AverCert.TypeTable.recordOf, AverCert.TypeTable.sumOf, \
     AverCert.Grammar.ctorFields, AverCert.Plans.types, AverCert.Bridge.hasTy_listInt, \
     AverCert.Bridge.hasTy_listBool, AverCert.Bridge.hasTy_listString";

fn arg_type(params: &[SourceEncoder]) -> String {
    match params.len() {
        0 => "_root_.Unit".into(),
        1 => params[0].binder_type(),
        _ => format!(
            "({})",
            params
                .iter()
                .map(SourceEncoder::binder_type)
                .collect::<Vec<_>>()
                .join(" × ")
        ),
    }
}

fn source_args(value: &str, n: usize) -> Vec<String> {
    match n {
        0 => Vec::new(),
        1 => vec![format!("({value})")],
        _ => tuple_components(value, n),
    }
}

fn render_fn_defs(b: &BridgedFn, s: &mut String) {
    let f = b.func_idx;
    // The decoder.
    s.push_str(&format!(
        "/-- The argument shapes `{}` is expanded at. -/\nnoncomputable def dec_{f} : \
         _root_.List _root_.AverCert.Grammar.SVal → _root_.Option {} := fun a =>\n  match a with\n",
        b.model,
        arg_type(&b.params)
    ));
    for (binds, patterns, sources) in &b.shapes {
        let value = match sources.len() {
            0 => "()".to_string(),
            1 => sources[0].clone(),
            _ => format!("({})", sources.join(", ")),
        };
        let mut rhs = format!("_root_.Option.some {value}");
        for (v, t, decode) in binds.iter().rev() {
            rhs = format!("({decode} {v}).bind (fun {t} => {rhs})");
        }
        s.push_str(&format!("  | [{}] => {rhs}\n", patterns.join(", ")));
    }
    s.push_str("  | _ => _root_.Option.none\n\n");
    // The image.
    let args = source_args("y", b.params.len());
    let call = if args.is_empty() {
        format!("{ROOT_PREFIX}{}", b.model)
    } else {
        format!("{ROOT_PREFIX}{} {}", b.model, args.join(" "))
    };
    let mut fresh = 0;
    s.push_str(&format!(
        "/-- The encoded source result of `{}`. -/\ndef img_{f} (y : {}) : \
         _root_.AverCert.Grammar.SVal :=\n  {}\n\n",
        b.model,
        arg_type(&b.params),
        b.result.encode(&call, &mut fresh)
    ));
}

fn render_image_table(fns: &BTreeMap<u32, BridgedFn>, s: &mut String) {
    s.push_str(
        "/-- The source image of every bridged function. -/\n\
         noncomputable def I : AverCert.GrammarBridge.Table := fun g a =>\n  match g with\n",
    );
    for f in fns.keys() {
        s.push_str(&format!(
            "  | {f} => (dec_{f} a).map img_{f}\n"
        ));
    }
    s.push_str("  | _ => _root_.Option.none\n\n");
    // One unfolding lemma per entry: the step proofs of another module
    // rewrite with these, since `simp` unfolding the whole table there
    // rebuilds its equations and runs out of recursion depth on a large one.
    for f in fns.keys() {
        s.push_str(&format!(
            "theorem I_{f} (a : _root_.List _root_.AverCert.Grammar.SVal) :\n    \
             I {f} a = (dec_{f} a).map img_{f} := rfl\n"
        ));
    }
    s.push('\n');
}

/// The fixed lemmas every step proof rewrites with, rendered once into
/// `BridgeSupport.lean`: arm-by-arm evaluation of a match (one rewrite per
/// pattern kind, so a symbolic subject never unfolds `patMatch`), a plan `if`
/// as a Lean `if`, splitting an `if` without naming its condition, and the
/// normal forms that meet the source's spelling (`==`, `!=`, String `+`,
/// interpolation). Producer data like every bridge proof: the kernel checks
/// each one.
const STEP_LEMMAS: &str = r#"section StepLemmas
open AverCert.Grammar

/-! Arm-by-arm evaluation of a match: one rewrite per pattern kind, so a
    step proof evaluates a match without unfolding `patMatch`/`bindVals`
    under a symbolic subject. -/

theorem arms_nil (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (v : SVal) :
    evalArms F env v .nil = none := by simp [evalArms]

theorem arms_wild (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (v : SVal) (b : Expr) (rest : Arms) :
    evalArms F env v (.cons .wild b rest) = eval F env b := by simp [evalArms, patMatch, bindVals]

theorem arms_bind (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (v : SVal) (s : Nat) (b : Expr) (rest : Arms) :
    evalArms F env v (.cons (.bind s) b rest) =
      eval F (if s = noSlot then env else upd env s v) b := by
  by_cases h : s = noSlot <;> simp [evalArms, patMatch, bindVals, h]

theorem arms_litInt (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (x k : Int) (b : Expr) (rest : Arms) :
    evalArms F env (.i x) (.cons (.litInt k) b rest) =
      if x = k then eval F env b else evalArms F env (.i x) rest := by
  by_cases h : x = k <;> simp [evalArms, patMatch, bindVals, h]

theorem arms_litStr (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (x k : List Nat) (b : Expr) (rest : Arms) :
    evalArms F env (.s x) (.cons (.litStr k) b rest) =
      if x = k then eval F env b else evalArms F env (.s x) rest := by
  by_cases h : x = k <;> simp [evalArms, patMatch, bindVals, h]

theorem arms_litBool (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (x k : Bool) (b : Expr) (rest : Arms) :
    evalArms F env (.b x) (.cons (.litBool k) b rest) =
      if x = k then eval F env b else evalArms F env (.b x) rest := by
  by_cases h : x = k <;> simp [evalArms, patMatch, bindVals, h]

/-- The plan decides a comparison; the source spells it with `==`/`!=`. -/
theorem dec_eq_beq {α : Type} [DecidableEq α] (a b : α) : decide (a = b) = (a == b) := rfl

theorem dec_ne_bne {α : Type} [DecidableEq α] (a b : α) : decide (a ≠ b) = (a != b) := by
  cases h : decide (a = b) <;> simp_all [bne]

/-- A plan `if` as a Lean `if` on its evaluated condition. -/
theorem eval_ite (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (c t e : Expr) :
    eval F env (.ifThenElse c t e) =
      match eval F env c with
      | some (.b b) => if b = true then eval F env t else eval F env e
      | _ => none := by
  simp only [eval]; split <;> simp_all

/-- Split an `if` on the left of an equation without naming its condition. -/
theorem ite_eq_of {α : Type} {c : Prop} [Decidable c] {A B R : α} (h1 : c → A = R) (h2 : ¬c → B = R) :
    (if c then A else B) = R := by
  by_cases h : c <;> simp [h, h1, h2]

/-- Two encoded Strings are equal exactly when the Strings are. -/
theorem strBytes_eq_iff (x y : String) :
    AverCert.GrammarBridge.strBytes x = AverCert.GrammarBridge.strBytes y ↔ x = y :=
  ⟨AverCert.GrammarBridge.strBytes_inj, fun h => h ▸ rfl⟩

/-- A model's String interpolation renders a String part as itself. -/
theorem str_toString (s : String) : toString s = s := rfl

theorem arms_ctor (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (v : SVal) (c : CtorTag)
    (bs : List Nat) (b : Expr) (rest : Arms) :
    evalArms F env v (.cons (.ctor c bs) b rest) =
      match patMatch (.ctor c bs) v with
      | some (bs', vs) =>
          match bindVals env bs' vs with
          | some env' => eval F env' b
          | none => none
      | none => evalArms F env v rest := by
  simp only [evalArms]; rfl

theorem arms_tuple (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (v : SVal)
    (bs : List Nat) (b : Expr) (rest : Arms) :
    evalArms F env v (.cons (.tuple bs) b rest) =
      match patMatch (.tuple bs) v with
      | some (bs', vs) =>
          match bindVals env bs' vs with
          | some env' => eval F env' b
          | none => none
      | none => evalArms F env v rest := by
  simp only [evalArms]; rfl

/-- A model's `+` on Strings is `++`. The instance is spelled out: a model
    declares it only when it uses it, and this block must elaborate in every
    package. -/
theorem str_hadd (a b : String) :
    @HAdd.hAdd String String String ⟨String.append⟩ a b = a ++ b := rfl

/-- A plan compares two Strings by their bytes; the source compares the Strings. -/
theorem strBytes_beq (a b : String) :
    (AverCert.GrammarBridge.strBytes a == AverCert.GrammarBridge.strBytes b) = (a == b) :=
  (AverCert.GrammarBridge.string_beq a b).symm

/-- `Option.withDefault` / `Result.withDefault` on an evaluated subject, as a
    function, so an `if` in the subject can be pulled out. -/
def optDefault (x d : Option SVal) : Option SVal :=
  match x with
  | some (.some _ v) => some v
  | some (.none _) => d
  | _ => none

def resDefault (x d : Option SVal) : Option SVal :=
  match x with
  | some (.ok _ _ v) => some v
  | some (.err _ _ _) => d
  | _ => none

theorem eval_optDefault (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (o d : Expr) :
    eval F env (.call (.lazy .optWithDefault) [o, d]) = optDefault (eval F env o) (eval F env d) := by
  simp only [eval, optDefault]; split <;> simp_all

theorem eval_resDefault (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (o d : Expr) :
    eval F env (.call (.lazy .resWithDefault) [o, d]) = resDefault (eval F env o) (eval F env d) := by
  simp only [eval, resDefault]; split <;> simp_all

theorem optDefault_ite {c : Prop} [Decidable c] (a b d : Option SVal) :
    optDefault (if c then a else b) d = if c then optDefault a d else optDefault b d := by
  split <;> rfl

theorem resDefault_ite {c : Prop} [Decidable c] (a b d : Option SVal) :
    resDefault (if c then a else b) d = if c then resDefault a d else resDefault b d := by
  split <;> rfl

theorem optDefault_some (t : Ty) (v : SVal) (d : Option SVal) : optDefault (some (.some t v)) d = some v := rfl
theorem optDefault_none (t : Ty) (d : Option SVal) : optDefault (some (.none t)) d = d := rfl
theorem resDefault_ok (t e : Ty) (v : SVal) (d : Option SVal) : resDefault (some (.ok t e v)) d = some v := rfl
theorem resDefault_err (t e : Ty) (v : SVal) (d : Option SVal) : resDefault (some (.err t e v)) d = d := rfl

/-! A List argument of Ints, Bools or Strings is decoded as a whole value,
    one cons cell at a time. A step splits a decoded List into its empty and
    cons shapes (`…_cases`); an export's image reads an encoded List back
    (`…_enc`); an encoded List inhabits its List type (`hasTy_list…`). -/

theorem decListInt_enc : ∀ l : List Int,
    decListInt (List.foldr (fun y acc => SVal.cons .int (SVal.i y) acc) (SVal.nil .int) l) = some l
  | [] => by simp [decListInt]
  | y :: ys => by simp [decListInt, decListInt_enc ys]

theorem decListBool_enc : ∀ l : List Bool,
    decListBool (List.foldr (fun y acc => SVal.cons .bool (SVal.b y) acc) (SVal.nil .bool) l) = some l
  | [] => by simp [decListBool]
  | y :: ys => by simp [decListBool, decListBool_enc ys]

theorem decListString_enc : ∀ l : List String,
    decListString (List.foldr (fun y acc =>
      SVal.cons .string (SVal.s (AverCert.GrammarBridge.strBytes y)) acc) (SVal.nil .string) l) = some l
  | [] => by simp [decListString]
  | y :: ys => by
      simp [decListString, decListString_enc ys, AverCert.GrammarBridge.decodeStr_strBytes]

theorem decListInt_cases {v : SVal} {a : List Int} (h : decListInt v = some a) :
    (v = .nil .int ∧ a = []) ∨
      ∃ x r t, v = .cons .int (.i x) r ∧ decListInt r = some t ∧ a = x :: t := by
  unfold decListInt at h
  split at h
  · simp only [Option.some.injEq] at h
    exact Or.inl ⟨rfl, h.symm⟩
  · rename_i x r
    cases hr : decListInt r with
    | none => simp [hr] at h
    | some t =>
        simp only [hr, Option.map_some, Option.some.injEq] at h
        exact Or.inr ⟨x, r, t, rfl, hr, h.symm⟩
  · simp at h

theorem decListBool_cases {v : SVal} {a : List Bool} (h : decListBool v = some a) :
    (v = .nil .bool ∧ a = []) ∨
      ∃ x r t, v = .cons .bool (.b x) r ∧ decListBool r = some t ∧ a = x :: t := by
  unfold decListBool at h
  split at h
  · simp only [Option.some.injEq] at h
    exact Or.inl ⟨rfl, h.symm⟩
  · rename_i x r
    cases hr : decListBool r with
    | none => simp [hr] at h
    | some t =>
        simp only [hr, Option.map_some, Option.some.injEq] at h
        exact Or.inr ⟨x, r, t, rfl, hr, h.symm⟩
  · simp at h

theorem decListString_cases {v : SVal} {a : List String} (h : decListString v = some a) :
    (v = .nil .string ∧ a = []) ∨
      ∃ x r t, v = .cons .string (.s (AverCert.GrammarBridge.strBytes x)) r ∧
        decListString r = some t ∧ a = x :: t := by
  unfold decListString at h
  split at h
  · simp only [Option.some.injEq] at h
    exact Or.inl ⟨rfl, h.symm⟩
  · rename_i hv r
    cases hx : AverCert.GrammarBridge.decodeStr hv with
    | none => simp [hx] at h
    | some x =>
        cases hr : decListString r with
        | none => simp [hx, hr] at h
        | some t =>
            simp only [hx, hr, Option.bind_some, Option.map_some, Option.some.injEq] at h
            exact Or.inr ⟨x, r, t, by rw [AverCert.GrammarBridge.decodeStr_eq_some.mp hx], hr,
              h.symm⟩
  · simp at h

/-- A decoded List of Ints is the encoding of what it decodes to. -/
theorem decListInt_sound : ∀ (a : List Int) {v : SVal}, decListInt v = some a →
    v = List.foldr (fun y acc => SVal.cons .int (SVal.i y) acc) (SVal.nil .int) a := by
  intro a
  induction a with
  | nil =>
      intro v h
      rcases decListInt_cases h with ⟨rfl, _⟩ | ⟨_, _, _, _, _, h2⟩
      · rfl
      · cases h2
  | cons x t ih =>
      intro v h
      rcases decListInt_cases h with ⟨_, h2⟩ | ⟨x', r, t', rfl, hr, h2⟩
      · cases h2
      · simp only [List.cons.injEq] at h2
        obtain ⟨rfl, rfl⟩ := h2
        simp only [List.foldr, ih hr]

/-- A decoded List of Ints, split into its empty and cons shapes, the tail
    an encoding. -/
theorem decListInt_split {v : SVal} {a : List Int} (h : decListInt v = some a) :
    (v = .nil .int ∧ a = []) ∨ ∃ x t, v = .cons .int (SVal.i x) (List.foldr (fun y acc => SVal.cons .int (SVal.i y) acc) (SVal.nil .int) t) ∧ a = x :: t := by
  have hv := decListInt_sound _ h
  cases a with
  | nil => exact Or.inl ⟨hv, rfl⟩
  | cons x t => exact Or.inr ⟨x, t, by simpa [List.foldr] using hv, rfl⟩

/-- A decoded List of Bools is the encoding of what it decodes to. -/
theorem decListBool_sound : ∀ (a : List Bool) {v : SVal}, decListBool v = some a →
    v = List.foldr (fun y acc => SVal.cons .bool (SVal.b y) acc) (SVal.nil .bool) a := by
  intro a
  induction a with
  | nil =>
      intro v h
      rcases decListBool_cases h with ⟨rfl, _⟩ | ⟨_, _, _, _, _, h2⟩
      · rfl
      · cases h2
  | cons x t ih =>
      intro v h
      rcases decListBool_cases h with ⟨_, h2⟩ | ⟨x', r, t', rfl, hr, h2⟩
      · cases h2
      · simp only [List.cons.injEq] at h2
        obtain ⟨rfl, rfl⟩ := h2
        simp only [List.foldr, ih hr]

/-- A decoded List of Bools, split into its empty and cons shapes, the tail
    an encoding. -/
theorem decListBool_split {v : SVal} {a : List Bool} (h : decListBool v = some a) :
    (v = .nil .bool ∧ a = []) ∨ ∃ x t, v = .cons .bool (SVal.b x) (List.foldr (fun y acc => SVal.cons .bool (SVal.b y) acc) (SVal.nil .bool) t) ∧ a = x :: t := by
  have hv := decListBool_sound _ h
  cases a with
  | nil => exact Or.inl ⟨hv, rfl⟩
  | cons x t => exact Or.inr ⟨x, t, by simpa [List.foldr] using hv, rfl⟩

/-- A decoded List of Strings is the encoding of what it decodes to. -/
theorem decListString_sound : ∀ (a : List String) {v : SVal}, decListString v = some a →
    v = List.foldr (fun y acc => SVal.cons .string (SVal.s (AverCert.GrammarBridge.strBytes y)) acc) (SVal.nil .string) a := by
  intro a
  induction a with
  | nil =>
      intro v h
      rcases decListString_cases h with ⟨rfl, _⟩ | ⟨_, _, _, _, _, h2⟩
      · rfl
      · cases h2
  | cons x t ih =>
      intro v h
      rcases decListString_cases h with ⟨_, h2⟩ | ⟨x', r, t', rfl, hr, h2⟩
      · cases h2
      · simp only [List.cons.injEq] at h2
        obtain ⟨rfl, rfl⟩ := h2
        simp only [List.foldr, ih hr]

/-- A decoded List of Strings, split into its empty and cons shapes, the tail
    an encoding. -/
theorem decListString_split {v : SVal} {a : List String} (h : decListString v = some a) :
    (v = .nil .string ∧ a = []) ∨ ∃ x t, v = .cons .string (SVal.s (AverCert.GrammarBridge.strBytes x)) (List.foldr (fun y acc => SVal.cons .string (SVal.s (AverCert.GrammarBridge.strBytes y)) acc) (SVal.nil .string) t) ∧ a = x :: t := by
  have hv := decListString_sound _ h
  cases a with
  | nil => exact Or.inl ⟨hv, rfl⟩
  | cons x t => exact Or.inr ⟨x, t, by simpa [List.foldr] using hv, rfl⟩

theorem hasTy_listEnc {α : Type} (M : MCtx) (t : Ty) (e : α → SVal) (he : ∀ y, HasTy M (e y) t) :
    ∀ l : List α, HasTy M (List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) l) (.list t)
  | [] => by simp [HasTy]
  | y :: ys => by
      simp only [List.foldr]
      exact ⟨rfl, he y, hasTy_listEnc M t e he ys⟩

theorem hasTy_listInt (M : MCtx) (l : List Int) :
    HasTy M (List.foldr (fun y acc => SVal.cons .int (SVal.i y) acc) (SVal.nil .int) l) (.list .int) :=
  hasTy_listEnc M .int _ (fun y => by simp [HasTy]) l

theorem hasTy_listBool (M : MCtx) (l : List Bool) :
    HasTy M (List.foldr (fun y acc => SVal.cons .bool (SVal.b y) acc) (SVal.nil .bool) l)
      (.list .bool) :=
  hasTy_listEnc M .bool _ (fun y => by simp [HasTy]) l

theorem hasTy_listString (M : MCtx) (l : List String) :
    HasTy M (List.foldr (fun y acc =>
      SVal.cons .string (SVal.s (AverCert.GrammarBridge.strBytes y)) acc) (SVal.nil .string) l)
      (.list .string) :=
  hasTy_listEnc M .string _ (fun y => by simp [HasTy]) l

/-- An encoded List inhabits its List type exactly when every element's
    encoding inhabits the element type: a rewrite, so `simp` decides the
    element typing under the binder instead of discharging a side goal. -/
theorem hasTy_listEnc_iff {α : Type} (M : MCtx) (t : Ty) (e : α → SVal) :
    ∀ l : List α, HasTy M (List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) l) (.list t) ↔
      ∀ y ∈ l, HasTy M (e y) t
  | [] => by simp [HasTy]
  | y :: ys => by
      simp only [List.foldr]
      simp [HasTy, hasTy_listEnc_iff M t e ys]

/-! Arm-by-arm evaluation of a List match. -/

theorem arms_emptyList_nil (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (t : Ty)
    (b : Expr) (rest : Arms) :
    evalArms F env (.nil t) (.cons .emptyList b rest) = eval F env b := by
  simp [evalArms, patMatch, bindVals]

theorem arms_emptyList_cons (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (t : Ty)
    (x r : SVal) (b : Expr) (rest : Arms) :
    evalArms F env (.cons t x r) (.cons .emptyList b rest) = evalArms F env (.cons t x r) rest := by
  simp [evalArms, patMatch]

theorem arms_cons_nil (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (t : Ty)
    (hd tl : Nat) (b : Expr) (rest : Arms) :
    evalArms F env (.nil t) (.cons (.cons hd tl) b rest) = evalArms F env (.nil t) rest := by
  simp [evalArms, patMatch]

theorem arms_cons_cons (F : Nat → List SVal → Option SVal) (env : Nat → Option SVal) (t : Ty)
    (x r : SVal) (hd tl : Nat) (b : Expr) (rest : Arms) :
    evalArms F env (.cons t x r) (.cons (.cons hd tl) b rest) =
      eval F (if tl = noSlot then (if hd = noSlot then env else upd env hd x)
        else upd (if hd = noSlot then env else upd env hd x) tl r) b := by
  by_cases h1 : hd = noSlot <;> by_cases h2 : tl = noSlot <;>
    simp [evalArms, patMatch, bindVals, h1, h2]

/-! A List of records, tuples, sums, options, results or Lists is decoded by
    `decList` over its element's decoder. Given that the element decoder
    reads back the element encoding and decodes nothing else, `decList` reads
    back the List encoding (`…_enc`), decodes nothing else (`…_sound`), and
    splits like the scalar ones (`…_split`). -/

theorem decList_enc {α : Type} (t : Ty) (d : SVal → Option α) (e : α → SVal)
    (h : ∀ x, d (e x) = some x) :
    ∀ l : List α, decList t d (List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) l) = some l
  | [] => by simp [decList]
  | y :: ys => by simp [decList, h, decList_enc t d e h ys]

theorem decList_sound {α : Type} (t : Ty) (d : SVal → Option α) (e : α → SVal)
    (h : ∀ v x, d v = some x → v = e x) :
    ∀ (a : List α) {v : SVal}, decList t d v = some a →
      v = List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) a := by
  intro a
  induction a with
  | nil =>
      intro v hv
      unfold decList at hv
      split at hv
      · split at hv
        · subst_vars; rfl
        · simp at hv
      · split at hv
        · simp only [Option.bind_eq_some_iff, Option.map_eq_some_iff] at hv
          obtain ⟨_, _, _, _, hv⟩ := hv
          cases hv
        · simp at hv
      · simp at hv
  | cons x tl ih =>
      intro v hv
      unfold decList at hv
      split at hv
      · split at hv <;> simp at hv
      · split at hv
        · simp only [Option.bind_eq_some_iff, Option.map_eq_some_iff, List.cons.injEq] at hv
          obtain ⟨x', hx, tl', hr, rfl, rfl⟩ := hv
          subst_vars
          simp only [List.foldr, h _ _ hx, ih hr]
        · simp at hv
      · simp at hv

theorem decList_split {α : Type} (t : Ty) (d : SVal → Option α) (e : α → SVal)
    (h : ∀ v x, d v = some x → v = e x) {v : SVal} {a : List α} (hv : decList t d v = some a) :
    (v = .nil t ∧ a = []) ∨ ∃ x tl, v = .cons t (e x)
      (List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) tl) ∧ a = x :: tl := by
  have hs := decList_sound t d e h _ hv
  cases a with
  | nil => exact Or.inl ⟨hs, rfl⟩
  | cons x tl => exact Or.inr ⟨x, tl, by simpa [List.foldr] using hs, rfl⟩

/-! The List helpers over encoded Lists: a helper reads its List arguments
    with `listOf?` and builds its result with `consAll`; on an encoding both
    meet the source's `List` function again (`…_enc`). A cons cell of an
    encoding is first read as the encoding of the longer List
    (`cons_map_…`, one per element encoding, so no higher-order unification
    has to guess the encoding from one element). -/

theorem listOf_enc {α : Type} (t : Ty) (e : α → SVal) (l : List α) :
    listOf? (List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) l) = some (t, l.map e) := by
  induction l with
  | nil => rfl
  | cons y ys ih => simp [listOf?, ih]

theorem enc_consAll {α : Type} (t : Ty) (e : α → SVal) (l : List α) :
    List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) l = consAll t (l.map e) := by
  induction l with
  | nil => rfl
  | cons y ys ih => simp [consAll, ih]

theorem consAll_enc {α : Type} (t : Ty) (e : α → SVal) (l : List α) :
    consAll t (l.map e) = List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) l :=
  (enc_consAll t e l).symm

theorem reverse_enc {α : Type} (t : Ty) (e : α → SVal) (l : List α) :
    consAll t (l.map e).reverse =
      List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) l.reverse := by
  rw [enc_consAll, List.map_reverse]

theorem take_enc {α : Type} (t : Ty) (e : α → SVal) (l : List α) (n : Nat) :
    consAll t ((l.map e).take n) =
      List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) (l.take n) := by
  rw [enc_consAll, List.map_take]

theorem drop_enc {α : Type} (t : Ty) (e : α → SVal) (l : List α) (n : Nat) :
    consAll t ((l.map e).drop n) =
      List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) (l.drop n) := by
  rw [enc_consAll, List.map_drop]

theorem append_enc {α : Type} (t : Ty) (e : α → SVal) (l m : List α) :
    consAll t (l.map e ++ m.map e) =
      List.foldr (fun y acc => SVal.cons t (e y) acc) (SVal.nil t) (l ++ m) := by
  rw [enc_consAll, List.map_append]

theorem listOf_nil_int : listOf? (SVal.nil .int) = some (.int, ([] : List Int).map (fun z => SVal.i z)) := rfl

theorem listOf_nil_bool : listOf? (SVal.nil .bool) = some (.bool, ([] : List Bool).map (fun z => SVal.b z)) := rfl

theorem listOf_nil_string : listOf? (SVal.nil .string) =
    some (.string, ([] : List String).map (fun z => SVal.s (AverCert.GrammarBridge.strBytes z))) := rfl

/-- The empty String literal, whose bytes the steps never read back. -/
theorem decodeStr_empty : AverCert.GrammarBridge.decodeStr (SVal.s []) = some "" :=
  AverCert.GrammarBridge.decodeStr_eq_some.mpr rfl

theorem cons_map_int (x : Int) (l : List Int) :
    SVal.i x :: l.map (fun z => SVal.i z) = (x :: l).map (fun z => SVal.i z) := rfl

theorem cons_map_bool (x : Bool) (l : List Bool) :
    SVal.b x :: l.map (fun z => SVal.b z) = (x :: l).map (fun z => SVal.b z) := rfl

theorem cons_map_string (x : String) (l : List String) :
    SVal.s (AverCert.GrammarBridge.strBytes x) :: l.map (fun z => SVal.s (AverCert.GrammarBridge.strBytes z)) =
      (x :: l).map (fun z => SVal.s (AverCert.GrammarBridge.strBytes z)) := rfl

/-- `contains` compares by `svEq`; the source compares with `==`. -/
theorem svEq_int (x a : Int) : svEq (.i x) (.i a) = (a == x) := by
  by_cases h : x = a
  · subst h; simp [svEq]
  · have h' : ¬a = x := fun e => h e.symm
    simp [svEq, h, h']

theorem svEq_bool (x a : Bool) : svEq (.b x) (.b a) = (a == x) := by
  cases a <;> cases x <;> rfl

theorem svEq_string (x a : String) :
    svEq (.s (AverCert.GrammarBridge.strBytes x)) (.s (AverCert.GrammarBridge.strBytes a)) = (a == x) := by
  by_cases h : x = a
  · subst h; simp [svEq]
  · have h' : ¬a = x := fun e => h e.symm
    have hb : ¬AverCert.GrammarBridge.strBytes x = AverCert.GrammarBridge.strBytes a :=
      fun e => h (AverCert.GrammarBridge.strBytes_inj e)
    simp [svEq, hb, h']

theorem any_svEq_int (l : List Int) (a : Int) :
    (l.map SVal.i).any (fun v => svEq v (.i a)) = l.contains a := by
  induction l with
  | nil => rfl
  | cons y ys ih => rw [List.map_cons, List.any_cons, List.contains_cons, ih, svEq_int]

theorem any_svEq_bool (l : List Bool) (a : Bool) :
    (l.map SVal.b).any (fun v => svEq v (.b a)) = l.contains a := by
  induction l with
  | nil => rfl
  | cons y ys ih => rw [List.map_cons, List.any_cons, List.contains_cons, ih, svEq_bool]

theorem any_svEq_string (l : List String) (a : String) :
    (l.map (fun y => SVal.s (AverCert.GrammarBridge.strBytes y))).any
      (fun v => svEq v (.s (AverCert.GrammarBridge.strBytes a))) = l.contains a := by
  induction l with
  | nil => rfl
  | cons y ys ih => rw [List.map_cons, List.any_cons, List.contains_cons, ih, svEq_string]

end StepLemmas
"#;

/// The decoders of a List argument of Ints, Bools or Strings, rendered into
/// `BridgeSupport.lean` ahead of the functions' decoders that call them: the
/// whole value, one cons cell at a time. Their lemmas are in [`STEP_LEMMAS`].
const LIST_DECODERS: &str = r#"section ListDecoders
open AverCert.Grammar

noncomputable def decListInt : SVal → Option (List Int)
  | .nil .int => some []
  | .cons .int (.i x) r => (decListInt r).map (fun t => x :: t)
  | _ => none

noncomputable def decListBool : SVal → Option (List Bool)
  | .nil .bool => some []
  | .cons .bool (.b x) r => (decListBool r).map (fun t => x :: t)
  | _ => none

noncomputable def decListString : SVal → Option (List String)
  | .nil .string => some []
  | .cons .string hv r =>
      (AverCert.GrammarBridge.decodeStr hv).bind (fun x => (decListString r).map (fun t => x :: t))
  | _ => none

/-- A List of any other element type, over the decoder of one element: the
    cons cells must carry exactly the List's element type. -/
noncomputable def decList {α : Type} (t : Ty) (d : SVal → Option α) : SVal → Option (List α)
  | .nil t' => if t' = t then some [] else none
  | .cons t' hv r =>
      if t' = t then (d hv).bind (fun x => (decList t d r).map (fun tl => x :: tl)) else none
  | _ => none

end ListDecoders

"#;

/// Evaluation lemmas of a step proof: the plan's equations with a match
/// taken arm by arm, and `if` / `withDefault` as Lean functions of their
/// subject (pre-rewrites, so they win over `eval`'s own equations), the
/// callee table, and the normal forms the source spells (`==`, `!=`, decided
/// comparisons, literal indices, String parts folded into one `strBytes`).
const STEP_EVAL: &str = "AverCert.Grammar.eval, AverCert.Grammar.evalArgs, arms_nil, arms_wild, \
     arms_bind, arms_litInt, arms_litStr, arms_litBool, arms_ctor, arms_tuple, arms_emptyList_nil, \
     arms_emptyList_cons, arms_cons_nil, arms_cons_cons, ↓eval_ite, \
     ↓eval_optDefault, ↓eval_resDefault, optDefault_ite, resDefault_ite, optDefault_some, \
     optDefault_none, resDefault_ok, resDefault_err, AverCert.Grammar.argsEnv, \
     AverCert.Grammar.upd, AverCert.Grammar.intBin, AverCert.Grammar.boolBin, \
     AverCert.Grammar.strBin, AverCert.Grammar.strCat, AverCert.Grammar.builtinEval, \
     AverCert.Grammar.intrinsicEval, AverCert.Grammar.ctorVal, AverCert.Grammar.patMatch, \
     AverCert.Grammar.bindVals, AverCert.Grammar.noSlot, AverCert.GrammarBridge.over, \
     _root_.decide_eq_true_eq, _root_.List.getElem?_cons_zero, _root_.List.getElem?_cons_succ, \
     _root_.List.mem_cons, _root_.List.mem_singleton, _root_.List.not_mem_nil, \
     _root_.Option.map_some, _root_.Option.map_none, _root_.true_or, _root_.or_true, \
     _root_.ite_true, _root_.ite_false, ↓reduceIte, dec_eq_beq, dec_ne_bne, \
     _root_.eq_self_iff_true, _root_.and_self, _root_.and_true, _root_.true_and, \
     _root_.false_and, _root_.and_false, AverCert.GrammarBridge.decodeStr_strBytes, \
     _root_.Option.bind_some, _root_.Function.comp_apply, strBytes_eq_iff, strBytes_beq, \
     Nat.reduceEqDiff, Int.reduceEq, ← AverCert.GrammarBridge.strBytes_append, \
     _root_.List.append_nil, str_toString, str_hadd, _root_.List.isEmpty_nil, \
     _root_.List.isEmpty_cons, _root_.Bool.false_eq_true, AverCert.Grammar.consAll, \
     decListInt, decListBool, decListString, decListInt_enc, decListBool_enc, decListString_enc";

/// What a step whose plan calls a List helper evaluates the helper with: the
/// helper reads its List arguments back as Lists and builds an encoding
/// again (see the List-helper lemmas of [`STEP_LEMMAS`]).
const LIST_HELPER_EVAL: &str = ", AverCert.Grammar.listOf?, ↓listOf_nil_int, ↓listOf_nil_bool, \
     ↓listOf_nil_string, listOf_enc, cons_map_int, \
     cons_map_bool, cons_map_string, reverse_enc, take_enc, drop_enc, append_enc, consAll_enc, \
     _root_.List.length_map, _root_.List.any_nil, any_svEq_int, any_svEq_bool, any_svEq_string, \
     _root_.List.reverse_nil, _root_.List.take_nil, _root_.List.drop_nil, _root_.List.nil_append, \
     _root_.List.length_nil";

/// Normal forms a leaf closes with, on top of the source definition.
const STEP_NORM: &str = "_root_.bne, dec_eq_beq, dec_ne_bne, strBytes_eq_iff, str_toString, \
     str_hadd, _root_.String.append_assoc, decListInt, decListBool, decListString";

/// One step lemma, proved by construction rather than by search. The
/// decoder's argument shapes are expanded; the plan body is evaluated by
/// `simp only` over an exact lemma list (every `if` becomes a Lean `if`,
/// every match is taken arm by arm, every String literal is read back as the
/// `strBytes` of its text, whole list at once); each `if` on the plan side is
/// split without naming its condition; and each leaf meets the source
/// function unfolded once (on the right only, for a self-recursive one). No
/// rung searches: each is bounded by the plan's size, so a leaf that does not
/// close fails fast and falls to `sorry`.
fn render_step_body(
    b: &BridgedFn,
    fns: &BTreeMap<u32, BridgedFn>,
    lit_index: &BTreeMap<Vec<u8>, usize>,
    with_default: bool,
    elems: &ElemDecoders,
    s: &mut String,
) {
    let f = b.func_idx;
    // The element decoders the step meets: its own arguments' (split by
    // their `…_split`) and its callees' (whose decoders read an encoding
    // back by their `…_enc`). Empty for a function without such a List, so
    // its proof text is exactly what it was before element decoders.
    let mut met: BTreeSet<usize> = b.elem_decoders.clone();
    for c in &b.callees {
        if let Some(callee) = fns.get(c) {
            met.extend(callee.elem_decoders.iter().copied());
        }
    }
    let mut elem_simps = String::new();
    // An element encoding reads back by `…_enc` before its decoder unfolds
    // (a pre-rewrite); a literal element, which is no encoding, by unfolding.
    if !met.is_empty() {
        elem_simps.push_str(", decList, decodeStr_empty");
        for k in &met {
            elem_simps.push_str(&format!(", ↓decElem_{k}_enc, decList_{k}_enc, decElem_{k}"));
        }
    }
    if b.list_helpers {
        elem_simps.push_str(LIST_HELPER_EVAL);
        for k in &met {
            elem_simps.push_str(&format!(", ↓listOf_nil_{k}, cons_map_{k}"));
        }
    }
    let mut splits_lists = false;
    let elem_splits: String = b
        .elem_decoders
        .iter()
        .map(|k| {
            // A head that is itself a List stays whole: its own decoder
            // already reads it back, and a helper meets it as an encoding.
            let mut budget = MAX_ELEM_LIST_SPLITS;
            let enc = &elems.elems[*k].enc;
            let head = (!matches!(enc, SourceEncoder::List(_)))
                .then(|| elem_split_pattern(enc, &mut budget))
                .flatten()
                .unwrap_or_else(|| "_".to_string());
            splits_lists |= budget < MAX_ELEM_LIST_SPLITS;
            format!(
                "\n            \
                 | (rcases decList_{k}_split hl with (⟨rfl, rfl⟩ | ⟨{head}, _, rfl, rfl⟩))"
            )
        })
        .collect();
    s.push_str(&format!(
        "-- Proof-local body; the step binding checks it against Plans.fn{f}.\n\
         def body_{f} : AverCert.Grammar.Expr :=\n  {}\n\n",
        b.body
    ));
    let images: String = image_dependencies(b)
        .iter()
        .map(|g| format!(
            "\n    (I_{g} : ∀ a : _root_.List AverCert.Grammar.SVal, I {g} a = (dec_{g} a).map img_{g})"
        ))
        .collect();
    let callees = b
        .callees
        .iter()
        .map(u32::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    let mut callee_simps = String::new();
    for c in &b.callees {
        if let Some(callee) = fns.get(c) {
            callee_simps.push_str(&format!(", I_{c}, dec_{c}, img_{c}"));
            if callee.fuel {
                callee_simps.push_str(&format!(", _root_.{}", callee.model));
            }
        }
    }
    // Forward (`strBytes "…" = [bytes]`) for the leaves; backward, and as a
    // pre-rewrite so a literal whose bytes end another's never rewrites
    // inside it, for the plan's evaluation.
    let mut forward = String::new();
    let mut backward = String::new();
    let mut empty = String::new();
    for bytes in &b.literals {
        if let Some(k) = lit_index.get(bytes) {
            forward.push_str(&format!(", strLit_{k}"));
            if bytes.is_empty() {
                empty = format!(", strLit_{k}");
            } else {
                backward.push_str(&format!(", ↓ ← strLit_{k}"));
            }
        }
    }
    let constants: String = b
        .constants
        .iter()
        .map(|c| format!(", _root_.{c}"))
        .collect();
    let model = format!("_root_.{}", b.model);
    let (unfold_list, unfold_words) = if b.fuel {
        (
            format!("{model}, {model}__fuel"),
            format!("{model} {model}__fuel"),
        )
    } else {
        (model.clone(), model.clone())
    };
    let with_default = if with_default {
        ", _root_.Except.withDefault, withDefault_ite"
    } else {
        ""
    };
    // A callee with a List parameter answers a call on a decoded List only
    // once its decoder meets the step's `decList…` hypothesis, which the
    // leaves rewrite with: the leaves unfold its image too.
    let list_images: String = b
        .callees
        .iter()
        .filter(|c| {
            fns.get(c)
                .is_some_and(|callee| callee.params.iter().any(|p| matches!(p, SourceEncoder::List(_))))
        })
        .map(|c| format!(", img_{c}"))
        .collect();
    if splits_lists {
        elem_simps.push_str(", _root_.List.foldr_cons, _root_.List.foldr_nil");
    }
    let norm = format!("{STEP_NORM}{empty}{constants}{with_default}{list_images}{elem_simps}");
    // A self-recursive source function is unfolded once, on the right: `simp`
    // with its equation would unfold the recursive call on the left as well.
    // The last rung of the other leaves meets a constant the source writes by
    // name and the plan carries as a numeral: equal only up to the numeral's
    // instance, which `rfl` sees and `simp` does not. A fuel wrapper's leaf
    // meets its callee at two fuel spellings (`natAbs (n - 1) + 1` against
    // `natAbs n` under `0 < n`): equal arguments, by `omega`.
    let fuel = if b.fuel {
        format!("\n           | (simp [{unfold_list}, {norm}, *] <;> congr 1 <;> omega)")
    } else {
        String::new()
    };
    // A plan that matches on a helper's result meets an encoding of a List
    // the source matches on too: split the source's match, and evaluate the
    // plan's arms again on the shape it fixes.
    let helper_split = if b.list_helpers {
        format!(
            "\n           | (unfold {unfold_words}; split <;> simp_all [{norm}, {STEP_EVAL}, \
             I_{f}{callee_simps}]; done)"
        )
    } else {
        String::new()
    };
    let leaf = if b.recursive && !b.fuel {
        format!(
            "(with_reducible rfl)\n           \
             | (symm; rw [{model}]; (try simp [{norm}, *]); done)\n           \
             | (symm; rw [{model}]; (try simp_all [{norm}]); done)\n           \
             | (symm; rw [{model}]; simp_all [{norm}] <;> omega)"
        )
    } else {
        format!(
            "(with_reducible rfl)\n           \
             | (simp only [{unfold_list}{forward}{constants}]; done)\n           \
             | (simp [{unfold_list}, {norm}, *]; done){fuel}\n           \
             | (simp_all [{unfold_list}, {norm}]; done)\n           \
             | (simp_all [{unfold_list}, {norm}] <;> omega){helper_split}\n           \
             | (unfold {unfold_words}; split <;> simp_all [{norm}])\n           \
             | (simp only [{unfold_list}{forward}{constants}]; rfl)"
        )
    };
    // Match encoders emitted in different image modules have distinct
    // auxiliary definitions. A simplification leaf can leave them equal
    // only definitionally; the final `rfl` closes that equality in the kernel.
    s.push_str(&format!(
        "/-- One step of `{m}`: its plan body, with every call answered by the\n    \
         callees' images, returns its own image. -/\n\
         theorem stepBody_{f} (I : AverCert.GrammarBridge.Table){images} :\n    \
           ∀ (F : AverCert.GrammarBridge.Table) (a : _root_.List AverCert.Grammar.SVal)\n      \
             (w : AverCert.Grammar.SVal), I {f} a = _root_.Option.some w →\n        \
             AverCert.Grammar.eval (AverCert.GrammarBridge.over I [{callees}] F)\n          \
               (AverCert.Grammar.argsEnv a) body_{f} = _root_.Option.some w := by\n  \
         first\n  \
         | (set_option maxHeartbeats {cap} in\n      \
             (intro F a w h\n       \
              simp only [I_{f}, _root_.Option.map_eq_some_iff] at h\n       \
              obtain ⟨y, hy, rfl⟩ := h\n       \
              unfold dec_{f} at hy\n       \
              split at hy <;> simp only [_root_.Option.bind_eq_some_iff, _root_.Option.some.injEq, \
                reduceCtorEq, AverCert.GrammarBridge.decodeStr_eq_some] at hy\n       \
              all_goals (repeat' (first\n         \
                | (obtain ⟨_, rfl, hy⟩ := hy)\n         \
                | (obtain ⟨_, hl, hy⟩ := hy\n            \
                   first\n            \
                   | (rcases decListInt_split hl with (⟨rfl, rfl⟩ | ⟨_, _, rfl, rfl⟩))\n            \
                   | (rcases decListBool_split hl with (⟨rfl, rfl⟩ | ⟨_, _, rfl, rfl⟩))\n            \
                   | (rcases decListString_split hl with (⟨rfl, rfl⟩ | ⟨_, _, rfl, rfl⟩)){elem_splits})))\n       \
              all_goals (try subst hy)\n       \
              all_goals simp only [body_{f}, eval_ite, eval_optDefault, eval_resDefault]\n       \
              all_goals simp only [img_{f}, {STEP_EVAL}, I_{f}{callee_simps}{elem_simps}{backward}]\n       \
              all_goals (repeat' (refine ite_eq_of (fun h => ?_) (fun h => ?_)))\n       \
              all_goals (try subst_vars)\n       \
              all_goals\n         \
                first\n           \
                | {leaf}\n       \
              all_goals rfl\n       \
              done))\n  \
         | sorry\n\n",
        m = b.model,
        cap = STEP_HEARTBEATS,
    ));
}

/// Only the function itself and its direct callees constrain the image
/// table of a body proof. Self calls share the function's own hypothesis.
fn image_dependencies(b: &BridgedFn) -> BTreeSet<u32> {
    std::iter::once(b.func_idx).chain(b.callees.iter().copied()).collect()
}

/// Bind the cached body theorem to the authoritative plan and image tables.
/// Lean checks both the plan lookup and the equality of the local body with
/// the selected plan body here; the public bridge statement is unchanged.
fn render_step_binding(b: &BridgedFn, s: &mut String) {
    let f = b.func_idx;
    let callees = b.callees.iter().map(u32::to_string).collect::<Vec<_>>().join(", ");
    let images: String = image_dependencies(b).iter().map(|g| format!(" I_{g}")).collect();
    s.push_str(&format!(
        "theorem step_{f} : AverCert.GrammarBridge.Step AverCert.Plans.fnPlans I [{callees}] {f} := by\n  \
         first\n  \
         | exact ⟨AverCert.Plans.fn{f}, rfl, stepBody_{f} I{images}⟩\n  \
         | sorry\n\n"
    ));
}

/// Per-attempt heartbeat cap. The declaration as a whole carries the file's
/// larger budget, so a step that gives up leaves headroom for the `sorry`
/// beside it: a not-credited bridge instead of a failed build (heartbeats
/// count from the start of each declaration). A step proof does no search,
/// so its cap is small; an export theorem assembles a whole call closure.
const STEP_HEARTBEATS: u32 = 400_000;
const EXPORT_HEARTBEATS: u32 = 1_000_000;
const FILE_HEARTBEATS: u32 = 4_000_000;

/// The `∀ g ∈ D, ∃ Cs, … ∧ Step …` argument both engines take: one
/// conjunct per closure member, in order, citing that member's step lemma
/// (the callee side condition is decided).
fn render_steps_proof(closure: &[u32]) -> String {
    // One `List.forall_mem_cons` per member, in closure order: each member
    // cites its own step lemma directly (trying every lemma on every member
    // costs a failed unification per pair).
    closure.iter().rev().fold(
        "(fun _ h => nomatch h)".to_string(),
        |rest, f| format!("(List.forall_mem_cons.2 ⟨⟨_, by decide, step_{f}⟩, {rest}⟩)"),
    )
}

fn render_export(
    bridge: &SourceBridge,
    func_idx: u32,
    plan: &BridgePlan,
    s: &mut String,
    corollaries: &mut String,
) {
    let b = &plan.fns[&func_idx];
    let binders = binder_names(b.params.len());
    let intro = if binders.is_empty() {
        String::new()
    } else {
        format!("intro {}; ", binders.join(" "))
    };
    let split_cases = export_split_cases(b);
    let kind_proof = export_assembly_binding(func_idx, plan);
    // An encoded List of records, tuples, sums or Lists inhabits its List
    // type exactly when every element encoding inhabits the element type,
    // which the same `simp` decides under the binder.
    let list_typing = if b.elem_decoders.is_empty() { "" } else { ", AverCert.Bridge.hasTy_listEnc_iff" };
    let typing = format!(
        "{intro}{split_cases}all_goals simp [{TYPING_SIMPS}{list_typing}{pieces}, AverCert.Plans.fn{func_idx}]",
        pieces = plan
            .type_pieces
            .iter()
            .map(|piece| format!(", {piece}"))
            .collect::<String>()
    );
    let statement = bridge.expanded_statement();
    s.push_str(&format!(
        "/-- plan-equals-source bridge for `{export}` ({kind}): the plan its obligation\n    \
         evaluates computes `{model}`. -/\n\
         theorem _root_.{theorem} :\n    ",
        export = bridge.export,
        kind = bridge.kind.tag(),
        model = bridge.model,
        theorem = bridge.theorem,
    ));
    // Concatenated, never interpolated into a format string: a statement
    // carrying `{`/`}` must stay inert text.
    s.push_str(&statement);
    // The obligation is selected by its entry and the package's distinct
    // export names, never by deciding `exportObligation` (String equality
    // against every earlier export, which the kernel evaluates slowly).
    let (entry_index, entry) = &plan.entries[&func_idx];
    s.push_str(" := by\n  first\n  | (set_option maxHeartbeats ");
    s.push_str(&EXPORT_HEARTBEATS.to_string());
    s.push_str(&format!(
        " in\n      (refine ⟨_, AverCert.GrammarBridge.exportObligation_of_entry \
         (s := AverCert.subject) (tt := AverCert.Plans.types) (fns := AverCert.Plans.fnPlans) \
         rfl export_names_nodup (e := {entry}) \
         (List.getElem_mem (l := AverCert.Plans.fnPlans) (n := {entry_index}) (by decide)) rfl, \
         ?_, ?_⟩\n       next => "
    ));
    s.push_str(&typing);
    s.push_str("\n       next => ");
    s.push_str(&kind_proof);
    s.push_str("\n       done))\n  | sorry\n\n");
    let c = corollaries;
    c.push_str("/-- The claim the manifest names: the bridge conjoined with the\n    \
                artifact-level `Holds` fact, so one kernel-checked name ties the\n    \
                plan-equals-source identity to exactly the certified bytes. -/\n\
                theorem _root_.");
    c.push_str(&bridge.corollary);
    c.push_str(" :\n    (");
    c.push_str(&bridge.statement());
    c.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  ⟨");
    c.push_str(&pinned_from_expanded(
        &format!("_root_.{}", bridge.theorem),
        bridge.kind,
        bridge.params.len(),
    ));
    c.push_str(", _root_.AverCert.Final.cert⟩\n\n");
}

/// `BridgeNames.lean`: `export_names_nodup`, the obligations' export names
/// pairwise distinct, read from the export accounting the byte facts already
/// prove (`exports_ok`, in `exports_module`): it requires the names' numeric
/// keys distinct (`SortedKeys.obligationNamesNodup_of_accounted`). Deciding
/// it again over the names' characters cost btc-listener's 823 names about
/// 45 s and most of a 9 GB module. A failure costs every bridge its credit,
/// never the package.
fn render_bridge_names(exports_module: &str) -> String {
    format!(
        "-- The obligations' export names are pairwise distinct, read from the\n\
         -- export accounting of the byte facts.\n\
         import {exports_module}\n\
         import SortedKeys\n\n\
         set_option autoImplicit false\n\n\
         namespace AverCert.Bridge\n\n\
         theorem export_names_nodup :\n    \
         (AverCert.manifest.obligations.map (·.export_)).Nodup :=\n  \
         AverCert.SortedKeys.obligationNamesNodup_of_accounted AverCert.Artifact.exports_ok\n\n\
         end AverCert.Bridge\n"
    )
}

/// The package modules that carry the bridge proofs themselves,
/// `BridgeProof0`, `BridgeProof1`, ….
pub const BRIDGE_PROOF_MODULE: &str = "BridgeProof";
/// Bridge theorems per `BridgeProof<i>` module, at most. The kernel does not
/// return memory to the system inside one process, so a module's peak grows
/// with its declarations: btc-listener's 636 bridges in one module took
/// 297 s and 9 GB, and slices of 100 up to 64 s each. Slices of about this
/// size build in parallel and keep the critical path short.
const BRIDGE_PROOFS_PER_MODULE: usize = 50;
/// The module of `export_names_nodup`, which every bridge proof cites.
pub const BRIDGE_NAMES_MODULE: &str = "BridgeNames";
/// The complete image table the thin step bindings import.
pub const BRIDGE_DEFS_MODULE: &str = "BridgeDefs";
/// The String literals' bytes every step slice imports.
pub const BRIDGE_LITS_MODULE: &str = "BridgeLits";
/// The step lemma slices, `BridgeSteps0`, `BridgeSteps1`, ….
pub const BRIDGE_STEPS_MODULE: &str = "BridgeSteps";
/// Step lemmas per slice module.
const BRIDGE_STEPS_PER_MODULE: usize = 24;

// Package partitioning and import routing live apart from proof rendering.
include!("source_bridge_parts.rs");
include!("source_bridge_assembly.rs");

// ---- law coverage -------------------------------------------------------------

/// Every source function a law statement mentions, in first-appearance
/// order: a token is a maximal run of Lean identifier characters, and it
/// counts when it is `_root_.` followed by the qualified name of a def the
/// model declares (the statement is root-qualified by then, and that is the
/// only spelling the checker counts). A def spelled any other way would be a
/// mention the checker refuses in a bridged law, so it costs the law its
/// bridged corollary. Over-recognition only adds a TRUE conjunct;
/// under-recognition only costs the law its bridged corollary — fail-closed
/// for the claim either way.
fn law_statement_model_fns(statement: &str, info: &ModelInfo) -> Option<Vec<String>> {
    let mut fns = Vec::new();
    for token in crate::bridge_statement::statement_tokens(statement) {
        match token.strip_prefix(crate::bridge_statement::ROOT_PREFIX) {
            Some(named) if info.defs.contains_key(named) => fns.push(named.to_string()),
            Some(_) => {}
            None => {
                if info.defs.keys().any(|def| {
                    crate::bridge_statement::token_names_model_unqualified(token, def)
                }) {
                    return None;
                }
            }
        }
    }
    Some(fns)
}

/// The bridges covering every source function a law mentions (`None` when
/// some mentioned function has no bridge; empty when it mentions none). The
/// list itself is the checker's rule
/// ([`crate::bridge_statement::law_mentioned_bridges`]), so the checker finds
/// exactly the list the manifest carries.
fn law_bridge_coverage(statement: &str, info: &ModelInfo, bridges: &[SourceBridge]) -> Option<Vec<usize>> {
    for model in law_statement_model_fns(statement, info)? {
        bridges.iter().position(|bridge| bridge.model == model)?;
    }
    let models: Vec<&str> = bridges.iter().map(|bridge| bridge.model.as_str()).collect();
    Some(crate::bridge_statement::law_mentioned_bridges(statement, &models))
}

/// The directory every model file ships under, and the first segment of every
/// model module root.
///
/// The model modules are named after the user's Aver modules, and nothing
/// stops a user from naming one `Laws`, `Bridge`, `Manifest`, `Final`,
/// `Grammar` or `Schema` — names the package itself or the checker's wall
/// already own. Shipped flat, one such module used to shadow a certificate
/// file, and the producer then had to leave out the whole model, declining
/// every bridge and every law. Nested under this one reserved directory, a
/// model root can never equal or case-insensitively prefix a package, wall or
/// toolchain root (none of them starts with it), whatever the user named the
/// module. Only the FILE location moves: the Lean namespaces inside, and so
/// every name a bridge or a law-claim cites, stay exactly as emitted.
pub const MODEL_PACKAGE_DIR: &str = "AverModel";

/// The model as the package ships it: the module roots `Bridge.lean` and
/// `Laws.lean` import, and the files under [`MODEL_PACKAGE_DIR`].
#[derive(Debug, Default)]
struct PackagedModel {
    roots: Vec<String>,
    files: Vec<(String, String)>,
}

/// Prepare the model files for the package, or say why none can ship.
///
/// Every check here is one the checker applies to the staged tree, run with
/// the checker's own code (`lean_gate`), so a model the checker would refuse
/// — and with it the WHOLE package, byte certificate included — is declined
/// here instead, costing only the bridges and law-claims that needed it.
fn package_model(model: &SourceModel) -> Result<PackagedModel, String> {
    let mut roots = Vec::new();
    for (path, _) in &model.files {
        let root = crate::lean_gate::lean_module_root(path)
            .map_err(|_| format!("model file `{path}` is not a plain Lean module path"))?;
        roots.push(root);
    }
    let wall_roots: Vec<&str> = wall::SOURCES
        .iter()
        .filter_map(|source| source.name.strip_suffix(".lean"))
        .collect();
    let mut packaged = PackagedModel::default();
    let mut seen_paths = std::collections::BTreeSet::new();
    for (path, content) in &model.files {
        let package_path = format!("{MODEL_PACKAGE_DIR}/{path}");
        if !seen_paths.insert(package_path.to_ascii_lowercase()) {
            return Err(format!(
                "model files collide case-insensitively at `{package_path}`"
            ));
        }
        let text = rewrite_model_imports(content, &roots, &wall_roots)?;
        let text = isolate_theorems(&keep_admitted_deriving(&text));
        if let Some(token) = crate::lean_gate::code_exec_token(&text) {
            return Err(format!(
                "model file `{path}` carries `{token}`, which the checker's token gate refuses"
            ));
        }
        packaged.files.push((package_path, text));
    }
    packaged.roots = roots
        .iter()
        .map(|root| format!("{MODEL_PACKAGE_DIR}.{root}"))
        .collect();
    Ok(packaged)
}

/// Point every model-to-model import at the model's package location
/// (`import Domain.Rational` becomes `import AverModel.Domain.Rational`).
/// Imports of a checker-owned wall module (the model prelude) and of the
/// toolchain stay as they are; any other import names a module the package
/// would not contain, so the model is declined rather than shipped broken.
fn rewrite_model_imports(
    content: &str,
    model_roots: &[String],
    wall_roots: &[&str],
) -> Result<String, String> {
    let mut out = String::with_capacity(content.len() + 64);
    for line in content.lines() {
        if let Some(module) = line.strip_prefix("import ") {
            let module = module.trim();
            let first = module.split('.').next().unwrap_or_default();
            if model_roots.iter().any(|root| root == module) {
                out.push_str(&format!("import {MODEL_PACKAGE_DIR}.{module}\n"));
                continue;
            }
            if !(wall_roots.contains(&module) || matches!(first, "Init" | "Std")) {
                return Err(format!(
                    "a model file imports `{module}`, which is neither a model module nor a \
                     checker-owned one"
                ));
            }
        }
        out.push_str(line);
        out.push('\n');
    }
    Ok(out)
}

/// Keep, of every `deriving` line, only the classes the checker's gate admits
/// (`lean_gate::DERIVING_CLASSES`, and `DERIVING_INSTANCE_CLASSES` for the
/// stand-alone `deriving instance … for T` form); a line left with none is
/// dropped. The emitter already writes the certificate model's clauses from
/// the admitted classes; this also covers the fixed prelude records whose
/// clauses name `Repr`, which the model never needs.
fn keep_admitted_deriving(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    for line in content.lines() {
        let trimmed = line.trim_start();
        let Some(rest) = trimmed.strip_prefix("deriving ") else {
            out.push_str(line);
            out.push('\n');
            continue;
        };
        let indent = &line[..line.len() - trimmed.len()];
        let keep = |classes: &str, admitted: &[&str]| -> Vec<String> {
            classes
                .split(',')
                .map(str::trim)
                .filter(|class| admitted.contains(class))
                .map(str::to_string)
                .collect()
        };
        let rewritten = if let Some(instance) = rest.strip_prefix("instance ") {
            instance.split_once(" for ").and_then(|(classes, types)| {
                let kept = keep(classes, &crate::lean_gate::DERIVING_INSTANCE_CLASSES);
                (!kept.is_empty())
                    .then(|| format!("{indent}deriving instance {} for {}", kept.join(", "), types.trim()))
            })
        } else {
            let kept = keep(rest, &crate::lean_gate::DERIVING_CLASSES);
            (!kept.is_empty()).then(|| format!("{indent}deriving {}", kept.join(", ")))
        };
        if let Some(rewritten) = rewritten {
            out.push_str(&rewritten);
            out.push('\n');
        }
    }
    out
}

/// The command prefix that confines a declaration's elaboration errors to
/// that declaration.
const ISOLATE_DECLARATION: &str = "#guard_msgs (drop error) in";

/// Put every theorem of a certificate Lean file behind
/// [`ISOLATE_DECLARATION`].
///
/// A proof that fails — including the deterministic `maxHeartbeats` timeout,
/// which no `first | … | sorry` ladder can catch because Lean re-throws
/// resource-limit exceptions past every tactic combinator — is an elaboration
/// ERROR, and one error anywhere used to fail `lake build` and with it the
/// whole package. Lean's error recovery already closes the failed goal with
/// `sorryAx` (or leaves the constant undeclared, in which case every
/// declaration citing it recovers to `sorryAx` in turn); the error MESSAGE is
/// all that fails the build. `#guard_msgs (drop error) in` drops exactly that
/// message for exactly that one command, so the build goes on and the
/// checker's per-claim axiom audit reports every claim resting on the failed
/// proof as not credited. It changes no declaration and admits nothing the
/// kernel did not check; warnings (`declaration uses 'sorry'`) still pass
/// through to the build log.
///
/// The prefix goes before the command's whole preamble — `set_option … in`,
/// `open … in` and a doc comment — since a doc comment right before
/// `#guard_msgs` would become its expected output. A theorem inside a
/// `mutual … end` block isolates the block, the one command it belongs to.
pub fn isolate_theorems(content: &str) -> String {
    let lines: Vec<&str> = content.lines().collect();
    let is_theorem = |line: &str| {
        ["theorem ", "private theorem ", "protected theorem "]
            .iter()
            .any(|keyword| line.starts_with(keyword))
    };
    // Which lines start a command to isolate.
    let mut starts: Vec<usize> = Vec::new();
    let mut index = 0;
    while index < lines.len() {
        if lines[index] == "mutual" {
            let end = (index + 1..lines.len())
                .find(|at| lines[*at] == "end")
                .unwrap_or(lines.len() - 1);
            if lines[index..=end].iter().any(|line| is_theorem(line.trim_start())) {
                starts.push(index);
            }
            index = end + 1;
            continue;
        }
        if is_theorem(lines[index]) {
            starts.push(command_preamble_start(&lines, index));
        }
        index += 1;
    }
    let mut out = String::with_capacity(content.len() + starts.len() * 32);
    let mut next = starts.iter().peekable();
    for (at, line) in lines.iter().enumerate() {
        if next.peek() == Some(&&at) {
            next.next();
            out.push_str(ISOLATE_DECLARATION);
            out.push('\n');
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

/// The first line of the preamble of the declaration at `keyword_line`: the
/// `set_option … in` / `open … in` lines and the doc comment in front of it,
/// looking past blank lines and `--` comments between them.
fn command_preamble_start(lines: &[&str], keyword_line: usize) -> usize {
    let mut start = keyword_line;
    let mut at = keyword_line;
    while at > 0 {
        let line = lines[at - 1];
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with("--") {
            at -= 1;
            continue;
        }
        if (line.starts_with("set_option ") || line.starts_with("open ")) && trimmed.ends_with(" in") {
            at -= 1;
            start = at;
            continue;
        }
        if trimmed.ends_with("-/") {
            let Some(open) = (0..at).rev().find(|j| lines[*j].trim_start().starts_with("/-")) else {
                break;
            };
            if lines[open].trim_start().starts_with("/--") {
                at = open;
                start = at;
                continue;
            }
        }
        break;
    }
    start
}

/// The rendered bridge Lean: the proofs module, the `Bridge.lean`
/// corollaries, and the named part files the proofs import.
/// `Bridge.lean` and every other module of the bridge surface.
type BridgeLean = (String, Vec<(String, String)>);

/// What `write_project` needs to render the bridge and law surfaces.
struct Surfaces {
    model: PackagedModel,
    bridge_lean: Option<BridgeLean>,
    laws_lean: Option<String>,
    bridges: Vec<SourceBridge>,
    law_claims: Vec<LawClaim>,
    law_bridge_exports: Vec<Vec<String>>,
    declined_bridges: Vec<(String, String)>,
    declined_laws: Vec<(String, String)>,
}

impl Surfaces {
    /// The bridges the package ships: none unless their Lean is written.
    fn packaged_bridges(&self) -> &[SourceBridge] {
        if self.bridge_lean.is_some() {
            &self.bridges
        } else {
            &[]
        }
    }
}

fn plan_surfaces(analysis: &Analysis, model: &SourceModel) -> Surfaces {
    let decline_all = |reason: String| Surfaces {
        model: PackagedModel::default(),
        bridge_lean: None,
        laws_lean: None,
        bridges: Vec::new(),
        law_claims: Vec::new(),
        law_bridge_exports: Vec::new(),
        declined_bridges: analysis
            .certified
            .iter()
            .map(|c| (c.name.clone(), clean_reason(&reason)))
            .collect(),
        declined_laws: model
            .law_claims
            .iter()
            .map(|claim| (claim.label.clone(), clean_reason(&reason)))
            .collect(),
    };
    if let Some(reason) = &model.failure {
        return decline_all(reason.clone());
    }
    let packaged = match package_model(model) {
        Ok(packaged) => packaged,
        Err(reason) => return decline_all(reason),
    };
    let roots = packaged.roots.clone();
    // The checker reads every law statement at the root, so each is rewritten
    // from the emitter's namespace-relative text to `_root_.`-qualified names
    // before the gates see it.
    let names = ModelNames::from_files(
        model
            .files
            .iter()
            .map(|(path, content)| (path.as_str(), content.as_str())),
    );
    let qualified_laws = root_qualify_law_claims(&model.law_claims, &names);
    let plan = plan_bridges(analysis, model, &qualified_laws);
    let bridges: Vec<SourceBridge> = plan.bridges.iter().map(|(b, _)| b.clone()).collect();
    let bridge_lean = (!plan.fns.is_empty() && !bridges.is_empty())
        .then(|| {
            let (corollaries, parts) =
                render_bridge_lean(&plan, &roots, exports_fact_module(analysis));
            let parts = parts
                .into_iter()
                .map(|(name, text)| (name, isolate_theorems(&text)))
                .collect();
            (isolate_theorems(&corollaries), parts)
        });
    let info = ModelInfo::from_model(model);
    let (law_claims, declined_laws) = admit_law_claims(qualified_laws);
    let law_bridges: Vec<Vec<usize>> = law_claims
        .iter()
        .map(|claim| law_bridge_coverage(&claim.statement, &info, &bridges).unwrap_or_default())
        .collect();
    let law_bridge_terms: Vec<Vec<(String, String)>> = law_bridges
        .iter()
        .map(|indices| {
            indices
                .iter()
                .map(|index| (bridges[*index].corollary.clone(), bridges[*index].statement()))
                .collect()
        })
        .collect();
    let laws_lean = (!law_claims.is_empty())
        .then(|| isolate_theorems(&render_laws_lean(&law_claims, &law_bridge_terms, &roots)));
    let law_bridge_exports = law_bridges
        .iter()
        .map(|indices| indices.iter().map(|i| bridges[*i].export.clone()).collect())
        .collect();
    Surfaces {
        model: packaged,
        bridge_lean,
        laws_lean,
        bridges,
        law_claims,
        law_bridge_exports,
        declined_bridges: plan.declined,
        declined_laws,
    }
}

#[cfg(test)]
#[path = "source_bridge_import_tests.rs"]
mod source_bridge_import_tests;

#[cfg(test)]
mod source_bridge_tests {
    use super::*;

    #[test]
    fn lean_types_parse_into_trees() {
        assert_eq!(
            parse_lean_ty("Except String (List Int)"),
            Some(LTy::App(
                "Except".into(),
                vec![
                    LTy::App("String".into(), vec![]),
                    LTy::App("List".into(), vec![LTy::App("Int".into(), vec![])])
                ]
            ))
        );
        assert_eq!(
            parse_lean_ty("(Int × Bool × Fraction)"),
            Some(LTy::Prod(vec![
                LTy::App("Int".into(), vec![]),
                LTy::App("Bool".into(), vec![]),
                LTy::App("Fraction".into(), vec![])
            ]))
        );
        assert_eq!(parse_lean_ty("(Int"), None);
    }

    fn model(files: Vec<(&str, &str)>, entry: &str, deps: Vec<(&str, &str)>) -> SourceModel {
        SourceModel {
            files: files
                .into_iter()
                .map(|(p, c)| (p.to_string(), c.to_string()))
                .collect(),
            entry_namespace: entry.to_string(),
            dependency_namespaces: deps
                .into_iter()
                .map(|(a, b)| (a.to_string(), b.to_string()))
                .collect(),
            law_claims: Vec::new(),
            failure: None,
            bridge_scope: BridgeScope::Every,
        }
    }

    const RATIONAL: &str = "import AverCommon\n\nnamespace Domain.Rational\n\nstructure Fraction where\n  top : Int\n  bottom : Int\n\ninductive Op where\n  | add (_ : Int)\n  | zero\n\nset_option smartUnfolding false in\n/-- doc -/\ndef plus (a : Fraction) (b : Fraction) : Fraction :=\n  a\n\ndef zeroFraction : Fraction :=\n  a\n\nmutual\n  def isEven__fuel (fuel : Nat) (n : Int) : Int :=\n    0\nend\n\nend Domain.Rational\n";

    #[test]
    fn model_defs_resolve_by_their_flat_wasm_name() {
        let m = model(
            vec![("Domain/Rational.lean", RATIONAL)],
            "Main",
            vec![("Domain.Rational", "Domain.Rational")],
        );
        let info = ModelInfo::from_model(&m);
        let plus = info.def_for("Domain_Rational_plus").expect("resolves");
        assert_eq!(plus.qualified, "Domain.Rational.plus");
        assert_eq!(plus.params, vec!["Fraction", "Fraction"]);
        assert_eq!(plus.ret, "Fraction");
        let zero = info.def_for("Domain_Rational_zeroFraction").expect("nullary");
        assert!(zero.params.is_empty());
        assert!(info.def_for("plus").is_err());
        assert!(info.def_for("Domain_Rational_isEven__fuel").is_ok());
        assert_eq!(
            info.structures["Domain.Rational.Fraction"].fields,
            vec![("top".into(), "Int".into()), ("bottom".into(), "Int".into())]
        );
        assert_eq!(
            info.inductives["Domain.Rational.Op"].ctors,
            vec![("add".into(), vec!["Int".into()]), ("zero".into(), vec![])]
        );
    }

    #[test]
    fn model_def_keeps_its_file_module_separate_from_its_namespace() {
        let m = model(
            vec![("Generated/Arithmetic.lean", RATIONAL)],
            "Main",
            vec![("Domain.Rational", "Domain.Rational")],
        );
        let info = ModelInfo::from_model(&m);
        let plus = info.def_for("Domain_Rational_plus").expect("resolves");
        assert_eq!(plus.qualified, "Domain.Rational.plus");
        assert_eq!(plus.module, "AverModel.Generated.Arithmetic");
    }

    #[test]
    fn encoders_follow_the_byte_pinned_layout() {
        let m = model(
            vec![("Domain/Rational.lean", RATIONAL)],
            "Main",
            vec![("Domain.Rational", "Domain.Rational")],
        );
        let info = ModelInfo::from_model(&m);
        let tt = PlanTypeTable {
            records: vec![PlanRecordDecl {
                tid: 0,
                struct_idx: 5,
                fields: vec![PlanTy::Int, PlanTy::Int],
            }],
            sums: vec![PlanSumDecl {
                tid: 1,
                root: 6,
                ctors: vec![(7, vec![PlanTy::Int]), (8, vec![])],
            }],
            ..PlanTypeTable::default()
        };
        let ns = "Domain.Rational";
        let fraction = info
            .encoder(&PlanTy::Record(0), &parse_lean_ty("Fraction").unwrap(), ns, &tt, &mut Vec::new())
            .expect("record");
        assert_eq!(fraction.kind(), "record");
        assert!(fraction.is_well_formed());
        let op = info
            .encoder(&PlanTy::Sum(1), &parse_lean_ty("Op").unwrap(), ns, &tt, &mut Vec::new())
            .expect("sum");
        assert!(op.is_well_formed());
        // A plan type that disagrees with the Lean type declines.
        assert!(
            info.encoder(&PlanTy::Bool, &parse_lean_ty("Int").unwrap(), ns, &tt, &mut Vec::new())
                .is_err()
        );
        // A layout with a different field count declines.
        let narrow = PlanTypeTable {
            records: vec![PlanRecordDecl {
                tid: 0,
                struct_idx: 5,
                fields: vec![PlanTy::Int],
            }],
            ..PlanTypeTable::default()
        };
        assert!(
            info.encoder(&PlanTy::Record(0), &parse_lean_ty("Fraction").unwrap(), ns, &narrow, &mut Vec::new())
                .is_err()
        );
        // Decoder shapes: a sum expands per constructor.
        let mut fresh = 0;
        let shapes = alts(&op, &mut fresh, &mut ElemDecoders::default()).expect("alts");
        assert_eq!(shapes.len(), 2);
        assert_eq!(shapes[0].source, "(_root_.Domain.Rational.Op.add t0)");
        assert_eq!(shapes[1].source, "_root_.Domain.Rational.Op.zero");
        // A String is matched whole and decoded by the wall's `decodeStr`.
        let strs = alts(&SourceEncoder::Str, &mut fresh, &mut ElemDecoders::default()).expect("a String decodes");
        assert_eq!(strs.len(), 1);
        assert_eq!(
            strs[0].binds,
            vec![(
                "v1".to_string(),
                "t1".to_string(),
                "AverCert.GrammarBridge.decodeStr".to_string()
            )]
        );
        assert!(alts(&SourceEncoder::Float, &mut fresh, &mut ElemDecoders::default()).is_err());
        let ints = alts(&SourceEncoder::List(Box::new(SourceEncoder::Int)), &mut fresh, &mut ElemDecoders::default())
            .expect("a List of Ints decodes");
        assert_eq!(ints.len(), 1);
        assert_eq!(ints[0].binds[0].2, "AverCert.Bridge.decListInt");
        // A List of Lists, or of records, decodes through the generic
        // `decList` over one element decoder per element ENCODER: the same
        // element encoder shares its decoder, another element type gets
        // another one, and an element without a decoder declines.
        let mut elems = ElemDecoders::default();
        let nested = SourceEncoder::List(Box::new(SourceEncoder::List(Box::new(SourceEncoder::Int))));
        let lists = alts(&nested, &mut fresh, &mut elems).expect("a List of Lists decodes");
        assert_eq!(
            lists[0].binds[0].2,
            "(AverCert.Bridge.decList (_root_.AverCert.Grammar.Ty.list _root_.AverCert.Grammar.Ty.int) \
             AverCert.Bridge.decElem_0)"
        );
        let records = SourceEncoder::List(Box::new(fraction.clone()));
        let again = alts(&records, &mut fresh, &mut elems).expect("a List of records decodes");
        assert!(again[0].binds[0].2.ends_with("AverCert.Bridge.decElem_1)"), "{again:?}");
        alts(&nested, &mut fresh, &mut elems).expect("decodes again");
        assert_eq!(elems.elems.len(), 2, "one decoder per element encoder");
        assert_eq!(elems.elems[1].enc, fraction);
        assert!(
            alts(&SourceEncoder::List(Box::new(SourceEncoder::Float)), &mut fresh, &mut elems).is_err()
        );
    }

    #[test]
    fn string_literals_render_as_lean_literals_and_sums_split_structurally() {
        assert_eq!(lean_string_literal(b"a\"b\\c\n"), Some("\"a\\\"b\\\\c\\n\"".to_string()));
        assert_eq!(lean_string_literal(b"\x01"), Some("\"\\x01\"".to_string()));
        assert_eq!(lean_string_literal(&[0xff]), None);
        let op = SourceEncoder::Sum {
            tid: 1,
            lean_type: "_root_.M.Op".to_string(),
            ctors: vec![
                ("_root_.M.Op.add".to_string(), vec![SourceEncoder::Int]),
                ("_root_.M.Op.zero".to_string(), Vec::new()),
            ],
        };
        let record = SourceEncoder::Record {
            tid: 0,
            lean_type: "_root_.M.R".to_string(),
            fields: vec![
                ("_root_.M.R.a".to_string(), SourceEncoder::Int),
                ("_root_.M.R.op".to_string(), op.clone()),
            ],
        };
        assert_eq!(rcases_pattern(&SourceEncoder::Int), None);
        assert_eq!(rcases_pattern(&op).as_deref(), Some("(⟨_⟩ | ⟨⟩)"));
        assert_eq!(rcases_pattern(&record).as_deref(), Some("⟨_, (⟨_⟩ | ⟨⟩)⟩"));
        assert_eq!(
            rcases_pattern(&SourceEncoder::Option(Box::new(SourceEncoder::Int))).as_deref(),
            Some("(⟨⟩ | _)")
        );
    }

    /// A user module named like a package or wall file (`Laws`, `Grammar`) used
    /// to shadow it, and the producer then left the whole model out. Nested
    /// under the reserved model directory, every model root is one no package,
    /// wall or toolchain root can equal or prefix, and the model's own imports
    /// follow it there while the wall prelude's import stays put.
    #[test]
    fn model_modules_named_like_certificate_files_ship_nested() {
        let m = model(
            vec![
                ("AverCommon.lean", "import ModelPrelude\n\nset_option autoImplicit false\n"),
                ("Laws.lean", "import AverCommon\n\nnamespace Laws\ndef f (x : Int) : Int := x\nend Laws\n"),
                ("Grammar.lean", "import AverCommon\nimport Laws\n\nnamespace Grammar\nend Grammar\n"),
            ],
            "Laws",
            vec![],
        );
        let packaged = package_model(&m).expect("the model ships");
        assert_eq!(
            packaged.roots,
            vec!["AverModel.AverCommon", "AverModel.Laws", "AverModel.Grammar"]
        );
        let files: std::collections::BTreeMap<_, _> = packaged.files.into_iter().collect();
        assert!(files["AverModel/AverCommon.lean"].starts_with("import ModelPrelude\n"));
        assert!(files["AverModel/Grammar.lean"].starts_with("import AverModel.AverCommon\nimport AverModel.Laws\n"));
        for root in &packaged.roots {
            let first = root.split('.').next().unwrap();
            assert!(wall::SOURCES.iter().all(|s| !s.name.eq_ignore_ascii_case(&format!("{first}.lean"))));
            assert!(!["Init", "Lean", "Lake", "Std", "Laws", "Bridge", "Manifest"].contains(&first));
        }
        // Namespaces — the names bridges and laws cite — are untouched.
        assert!(files["AverModel/Laws.lean"].contains("namespace Laws\ndef f"));
    }

    /// Everything the checker would refuse in a model file declines the model
    /// (every bridge and law-claim) at the producer instead of shipping a
    /// package the checker refuses whole.
    #[test]
    fn a_model_the_checker_would_refuse_is_declined_by_the_producer() {
        for (content, why) in [
            ("@[simp] theorem t : True := trivial\n", "@["),
            ("syntax \"x\" : tactic\n", "syntax"),
            ("import Mathlib\n", "Mathlib"),
        ] {
            let m = model(vec![("M.lean", content)], "M", vec![]);
            let reason = package_model(&m).expect_err(why);
            assert!(reason.contains(why), "{reason}");
        }
        let m = model(vec![("M.lean", ""), ("m.lean", "")], "M", vec![]);
        assert!(package_model(&m).unwrap_err().contains("case-insensitively"));
        let m = model(vec![("Type'.lean", "")], "M", vec![]);
        assert!(package_model(&m).is_err());
    }

    #[test]
    fn deriving_clauses_keep_only_the_admitted_classes() {
        let text = "structure P where\n  a : Int\n  deriving Repr, BEq, Inhabited, DecidableEq\n\
                    structure Q where\n  deriving Repr\n\
                    deriving instance ReflBEq, LawfulBEq for P\n\
                    deriving instance Repr for Q\n";
        let kept = keep_admitted_deriving(text);
        assert_eq!(
            kept,
            "structure P where\n  a : Int\n  deriving BEq, Inhabited, DecidableEq\n\
             structure Q where\n\
             deriving instance ReflBEq, LawfulBEq for P\n"
        );
        assert_eq!(crate::lean_gate::code_exec_token(&kept), None);
    }

    /// Every theorem sits behind `#guard_msgs (drop error) in`, placed before
    /// its whole preamble: a doc comment left in front of `#guard_msgs` would
    /// become its expected output. A `mutual` block holding a theorem is
    /// isolated as the one command it is; definitions are left alone.
    #[test]
    fn theorems_are_isolated_before_their_preamble() {
        let text = "def f (x : Int) : Int := x\n\
                    \n\
                    set_option maxHeartbeats 800000 in\n\
                    /-- doc\n    more -/\n\
                    -- aver:law-class t universal M.f.l\n\
                    theorem t : True := by\n  trivial\n\
                    /- plain comment -/\n\
                    private theorem u : True := trivial\n\
                    mutual\n  theorem a : True := trivial\n  theorem b : True := trivial\nend\n\
                    mutual\n  def g : Nat → Nat\n    | _ => 0\nend\n";
        let isolated = isolate_theorems(text);
        assert_eq!(
            isolated,
            "def f (x : Int) : Int := x\n\
             \n\
             #guard_msgs (drop error) in\n\
             set_option maxHeartbeats 800000 in\n\
             /-- doc\n    more -/\n\
             -- aver:law-class t universal M.f.l\n\
             theorem t : True := by\n  trivial\n\
             /- plain comment -/\n\
             #guard_msgs (drop error) in\n\
             private theorem u : True := trivial\n\
             #guard_msgs (drop error) in\n\
             mutual\n  theorem a : True := trivial\n  theorem b : True := trivial\nend\n\
             mutual\n  def g : Nat → Nat\n    | _ => 0\nend\n"
        );
        assert_eq!(crate::lean_gate::code_exec_token(&isolated), None);
    }

    /// The decoder of a function with an Int and a String parameter binds the
    /// String by `decodeStr`, and the steps argument of an export cites every
    /// member of its call closure.
    #[test]
    fn decoders_and_steps_render_their_exact_text() {
        let mut fresh = 0;
        let parts = vec![
            alts(&SourceEncoder::Int, &mut fresh, &mut ElemDecoders::default()).unwrap(),
            alts(&SourceEncoder::Str, &mut fresh, &mut ElemDecoders::default()).unwrap(),
        ];
        let shapes = product(parts).unwrap().iter().map(|row| join_row(row)).collect();
        let b = BridgedFn {
            func_idx: 7,
            model: "M.greet".to_string(),
            model_root: "AverModel.M".into(),
            body: PlanExpr::Literal(PlanLit::Str(Vec::new())).lean(),
            params: vec![SourceEncoder::Int, SourceEncoder::Str],
            result: SourceEncoder::Str,
            callees: Vec::new(),
            fuel: false,
            shapes,
            literals: BTreeSet::new(),
            recursive: false,
            constants: Vec::new(),
            elem_decoders: BTreeSet::new(),
            list_helpers: false,
        };
        let mut s = String::new();
        render_fn_defs(&b, &mut s);
        assert!(
            s.contains(
                "  | [_root_.AverCert.Grammar.SVal.i t0, v1] => \
                 (AverCert.GrammarBridge.decodeStr v1).bind (fun t1 => _root_.Option.some (t0, t1))\n"
            ),
            "{s}"
        );
        assert!(s.contains("noncomputable def dec_7 : _root_.List _root_.AverCert.Grammar.SVal → _root_.Option (_root_.Int × _root_.String)"), "{s}");
        assert!(
            s.contains("def img_7 (y : (_root_.Int × _root_.String)) : _root_.AverCert.Grammar.SVal :=\n  _root_.AverCert.Grammar.SVal.s (_root_.AverCert.GrammarBridge.strBytes (_root_.M.greet (_root_.Prod.fst (y)) (_root_.Prod.snd (y))))"),
            "{s}"
        );
        assert_eq!(
            render_steps_proof(&[3, 9]),
            "(List.forall_mem_cons.2 ⟨⟨_, by decide, step_3⟩, \
             (List.forall_mem_cons.2 ⟨⟨_, by decide, step_9⟩, (fun _ h => nomatch h)⟩)⟩)"
        );
        let chars = lean_char_lists(&["ab".to_string(), "a'\"".to_string()], "\n       ");
        assert_eq!(
            chars, "[['a', 'b'],\n       ['a', (Char.ofNat 39), (Char.ofNat 34)]]",
            "{chars}"
        );
        let names = render_bridge_names("ArtifactInterface");
        assert!(
            names.contains("import ArtifactInterface\n")
                && names.contains(
                    "obligationNamesNodup_of_accounted AverCert.Artifact.exports_ok"
                ),
            "{names}"
        );
    }

    /// A step lemma is proved by construction: the plan is evaluated by
    /// `simp only` (String literals read back whole, before their bytes can
    /// match inside a longer literal), each plan-side `if` is split by
    /// `ite_eq_of`, and a self-recursive source unfolds on the right only.
    /// No rung runs the default simp set over the plan.
    #[test]
    fn step_lemmas_are_constructive_and_self_recursion_unfolds_once() {
        let mut literals = BTreeSet::new();
        literals.insert(b" ".to_vec());
        literals.insert(Vec::new());
        let b = BridgedFn {
            func_idx: 9,
            model: "M.spaces".to_string(),
            model_root: "AverModel.M".into(),
            body: PlanExpr::Literal(PlanLit::Str(Vec::new())).lean(),
            params: vec![SourceEncoder::Int, SourceEncoder::Str],
            result: SourceEncoder::Str,
            callees: vec![9],
            fuel: false,
            shapes: Vec::new(),
            literals,
            recursive: true,
            constants: vec!["M.width".to_string()],
            elem_decoders: BTreeSet::new(),
            list_helpers: false,
        };
        let lit_index: BTreeMap<Vec<u8>, usize> =
            [(Vec::new(), 0), (b" ".to_vec(), 1)].into_iter().collect();
        let mut fns = BTreeMap::new();
        fns.insert(9, b.clone());
        let mut s = String::new();
        render_step_body(&b, &fns, &lit_index, false, &ElemDecoders::default(), &mut s);
        assert!(s.contains("all_goals simp only [body_9, eval_ite, eval_optDefault, eval_resDefault]\n"), "{s}");
        assert!(s.contains(", ↓ ← strLit_1]"), "{s}");
        assert!(!s.contains("↓ ← strLit_0"), "the empty literal is never read back: {s}");
        assert!(s.contains("all_goals (repeat' (refine ite_eq_of (fun h => ?_) (fun h => ?_)))"), "{s}");
        assert!(s.contains("| (symm; rw [_root_.M.spaces]; (try simp [_root_.bne"), "{s}");
        assert!(s.contains(", strLit_0, _root_.M.width"), "{s}");
        assert!(!s.contains("simp [_root_.M.spaces"), "a recursive source never unfolds by simp: {s}");
        assert!(!s.contains("simp [AverCert.Plans.fn9"), "{s}");
        assert!(s.contains("  | sorry\n"), "{s}");
        assert!(STEP_LEMMAS.contains("theorem ite_eq_of"));
        assert!(STEP_LEMMAS.contains("theorem arms_litInt"));
    }
}
