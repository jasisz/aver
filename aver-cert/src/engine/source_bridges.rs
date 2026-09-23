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
    is_plain_dotted_name, param_binders, render_bridge_statement, statement_is_root_qualified,
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
}

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

    pub fn statement(&self) -> String {
        render_bridge_statement(
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
    if !is_plain_dotted_name(&bridge.export)
        || bridge.export.contains('.')
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
/// as written.
#[derive(Debug, Clone)]
struct LeanDef {
    qualified: String,
    namespace: String,
    params: Vec<String>,
    ret: String,
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
            if path.ends_with(".lean") {
                info.parse(content);
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

    fn parse(&mut self, content: &str) {
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
                        if let Some((fname, fty)) = field.split_once(" : ") {
                            if is_plain_dotted_name(fname) && !fname.contains('.') {
                                fields.push((fname.to_string(), fty.trim().to_string()));
                            }
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
                if !name.is_empty() && is_plain_dotted_name(name) {
                    if let Some((params, ret)) = parse_def_tail(&rest[name.len()..]) {
                        let qualified = qualify(&ns, name);
                        self.defs.insert(
                            qualified.clone(),
                            LeanDef {
                                qualified,
                                namespace: ns.clone(),
                                params,
                                ret,
                            },
                        );
                    }
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
            if scope.pop().is_none() {
                return None;
            }
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
    /// Atomic values the shape binds as a whole `SVal` and decodes by the
    /// wall's decoder: `(SVal variable, source variable)`.
    binds: Vec<(String, String)>,
    pattern: String,
    source: String,
}

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

fn join_row(row: &[Alt]) -> (Vec<(String, String)>, Vec<String>, Vec<String>) {
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

fn alts(enc: &SourceEncoder, fresh: &mut usize) -> Result<Vec<Alt>, String> {
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
                binds: vec![(v.clone(), t.clone())],
                pattern: v,
                source: t,
            }])
        }
        SourceEncoder::Float | SourceEncoder::List(_) | SourceEncoder::Vector(_) => {
            Err(format!("a `{}` argument has no decoder in this version", enc.kind()))
        }
        SourceEncoder::Record {
            tid,
            lean_type,
            fields,
        } => {
            let parts = fields
                .iter()
                .map(|(_, f)| alts(f, fresh))
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
                .map(|f| alts(f, fresh))
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
                    .map(|f| alts(f, fresh))
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
            for alt in alts(elem, fresh)? {
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
            for alt in alts(ok, fresh)? {
                out.push(Alt {
                    binds: alt.binds,
                    pattern: format!("{SVAL}.ok {t} {e} ({})", alt.pattern),
                    source: format!("(_root_.Except.ok {})", alt.source),
                });
            }
            for alt in alts(err, fresh)? {
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

// ---- planning ---------------------------------------------------------------

/// Everything the renderer needs for one bridged function (exported or an
/// internal callee).
#[derive(Debug, Clone)]
struct BridgedFn {
    func_idx: u32,
    model: String,
    params: Vec<SourceEncoder>,
    result: SourceEncoder,
    /// Direct callees, deduplicated.
    callees: Vec<u32>,
    /// Whether the model definition is a fuel wrapper (`f x = f__fuel
    /// (natAbs x + 1) x`, the transpiler's shape for mutual recursion): the
    /// step unfolds both, and callers unfold the wrapper.
    fuel: bool,
    /// The decoder's argument shapes: the atomic values each one decodes,
    /// its argument patterns, and the source arguments it denotes.
    shapes: Vec<(Vec<(String, String)>, Vec<String>, Vec<String>)>,
}

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
        PlanExpr::Neg(x) | PlanExpr::Project(_, _, x) => string_literals(x, out),
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
        if seen.insert(f) {
            if let Some(b) = fns.get(&f) {
                work.extend(b.callees.iter().copied());
            }
        }
    }
    seen.into_iter().collect()
}

fn plan_bridges(analysis: &Analysis, model: &SourceModel) -> BridgePlan {
    let mut declined: Vec<(String, String)> = Vec::new();
    let mut plan = BridgePlan {
        fns: BTreeMap::new(),
        bridges: Vec::new(),
        declined: Vec::new(),
        depth: BTreeMap::new(),
        literals: BTreeSet::new(),
    };
    if let Some(reason) = &model.failure {
        for c in &analysis.certified {
            declined.push((c.name.clone(), reason.clone()));
        }
        plan.declined = declined;
        return plan;
    }
    let info = ModelInfo::from_model(model);
    // Candidate functions: every planned function whose source definition
    // and encoders resolve.
    let mut reasons: BTreeMap<u32, String> = BTreeMap::new();
    for e in &analysis.entries {
        let flat = analysis
            .fn_names
            .get(&e.func_idx)
            .cloned()
            .unwrap_or_else(|| e.name.clone());
        let derived = (|| -> Result<BridgedFn, String> {
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
            let mut fresh = 0;
            let parts = params
                .iter()
                .map(|p| alts(p, &mut fresh))
                .collect::<Result<Vec<_>, _>>()?;
            let shapes = product(parts)?.iter().map(|row| join_row(row)).collect();
            Ok(BridgedFn {
                func_idx: e.func_idx,
                model: def.qualified.clone(),
                params,
                result,
                callees: direct_callees(&e.plan.body),
                fuel: info.defs.contains_key(&format!("{}__fuel", def.qualified)),
                shapes,
            })
        })();
        match derived {
            Ok(b) => {
                string_literals(&e.plan.body, &mut plan.literals);
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

const EVAL_SIMPS: &str = "AverCert.Grammar.eval, AverCert.Grammar.evalArgs, \
     AverCert.Grammar.evalArms, AverCert.Grammar.argsEnv, AverCert.Grammar.upd, \
     AverCert.Grammar.intBin, AverCert.Grammar.boolBin, AverCert.Grammar.floatBin, \
     AverCert.Grammar.strBin, AverCert.Grammar.strCat, AverCert.Grammar.builtinEval, \
     AverCert.Grammar.ctorVal, AverCert.Grammar.patMatch, AverCert.Grammar.bindVals, \
     AverCert.Grammar.noSlot, AverCert.GrammarBridge.over, \
     AverCert.GrammarBridge.decodeStr_strBytes, AverCert.GrammarBridge.strBytes_append, AverCert.GrammarBridge.strBytes_hadd, AverCert.GrammarBridge.strBytes_toString, \
     AverCert.GrammarBridge.string_eq_iff, AverCert.GrammarBridge.string_beq";

/// Bool normal forms: the plan's comparisons are `decide`, the source's `==`.
const BOOL_SIMPS: &str = "_root_.beq_iff_eq, _root_.bne_iff_ne, _root_.Bool.beq_eq_decide_eq, \
     _root_.decide_eq_decide, _root_.Bool.decide_eq_true, _root_.decide_not";

const TYPING_SIMPS: &str = "AverCert.GrammarBridge.ArgsTyped, AverCert.Grammar.HasTyL, \
     AverCert.Grammar.HasTy, AverCert.Grammar.HasTyAll, AverCert.AcceptedArtifact.obligationOf, \
     AverCert.TypeTable.mctxOf, AverCert.TypeTable.recordOf, AverCert.TypeTable.sumOf, \
     AverCert.Grammar.ctorFields, AverCert.Plans.types";

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
        for (v, t) in binds.iter().rev() {
            rhs = format!("(AverCert.GrammarBridge.decodeStr {v}).bind (fun {t} => {rhs})");
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
}

fn render_step(b: &BridgedFn, fns: &BTreeMap<u32, BridgedFn>, literals: &str, s: &mut String) {
    let f = b.func_idx;
    let callees = b
        .callees
        .iter()
        .map(u32::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    let mut callee_simps = String::new();
    for c in &b.callees {
        if let Some(callee) = fns.get(c) {
            callee_simps.push_str(&format!(", dec_{c}, img_{c}"));
            if callee.fuel {
                callee_simps.push_str(&format!(", _root_.{}", callee.model));
            }
        }
    }
    let unfold = if b.fuel {
        format!("(try unfold _root_.{m})\n         (try unfold _root_.{m}__fuel)", m = b.model)
    } else {
        format!("(try unfold _root_.{})", b.model)
    };
    s.push_str(&format!(
        "/-- One step of `{model}`: its plan body, with every call answered by the\n    \
         callees' images, returns its own image. -/\n\
         theorem step_{f} : AverCert.GrammarBridge.Step AverCert.Plans.fnPlans I [{callees}] {f} := by\n  \
         first\n  \
         | (set_option maxHeartbeats {cap} in\n      \
             (refine ⟨AverCert.Plans.fn{f}, rfl, ?_⟩\n       \
              intro F a w h\n       \
              simp only [I, _root_.Option.map_eq_some_iff] at h\n       \
              obtain ⟨y, hy, rfl⟩ := h\n       \
              unfold dec_{f} at hy\n       \
              split at hy <;> simp only [_root_.Option.bind_eq_some_iff, _root_.Option.some.injEq, \
                reduceCtorEq, AverCert.GrammarBridge.decodeStr_eq_some] at hy\n       \
              all_goals (repeat (obtain ⟨_, rfl, hy⟩ := hy))\n       \
              all_goals (try subst hy)\n       \
              all_goals\n         \
                simp only [img_{f}]\n         \
                {unfold}\n         \
                simp [AverCert.Plans.fn{f}, {EVAL_SIMPS}{literals}, I{callee_simps}]\n       \
              all_goals (try (repeat' split))\n       \
              all_goals (try simp_all [{EVAL_SIMPS}{literals}, {BOOL_SIMPS}])\n       \
              all_goals (try omega)\n       \
              all_goals (try (apply _root_.Bool.eq_iff_iff.mpr; simp only [_root_.Bool.or_eq_true, \
                _root_.Bool.and_eq_true, _root_.decide_eq_true_eq, _root_.Bool.not_eq_true']; omega))\n       \
              all_goals (try (congr 1 <;> omega))\n       \
              all_goals (try rfl)\n       \
              all_goals (try decide)\n       \
              done))\n  \
         | sorry\n\n",
        model = b.model,
        cap = STEP_HEARTBEATS,
    ));
}

/// Per-attempt heartbeat cap. The declaration as a whole carries the file's
/// larger budget, so a step that gives up leaves headroom for the `sorry`
/// beside it: a not-credited bridge instead of a failed build (heartbeats
/// count from the start of each declaration).
const STEP_HEARTBEATS: u32 = 1_000_000;
const FILE_HEARTBEATS: u32 = 4_000_000;

/// The `∀ g ∈ D, ∃ Cs, … ∧ Step …` argument both engines take: split the
/// membership into one goal per closure member, in order, and cite that
/// member's step lemma (the callee side condition is decided).
fn render_steps_proof(closure: &[u32]) -> String {
    let pats = closure.iter().map(|_| "rfl").collect::<Vec<_>>().join(" | ");
    let steps = closure
        .iter()
        .map(|f| format!("exact ⟨_, by decide, step_{f}⟩"))
        .collect::<Vec<_>>()
        .join(" | ");
    format!("(by intro g hg; simp at hg; rcases hg with {pats} <;> first | {steps})")
}

fn render_export(
    bridge: &SourceBridge,
    func_idx: u32,
    plan: &BridgePlan,
    s: &mut String,
) {
    let b = &plan.fns[&func_idx];
    let closure = closure_of(func_idx, &plan.fns);
    let members = closure
        .iter()
        .map(u32::to_string)
        .collect::<Vec<_>>()
        .join(", ");
    let binders = binder_names(b.params.len());
    let intro = if binders.is_empty() {
        String::new()
    } else {
        format!("intro {}; ", binders.join(" "))
    };
    // Splitting every sum, option or result the encoding matches on (at any
    // depth) into its constructors lets the encoded argument reduce.
    let split_cases: String = b
        .params
        .iter()
        .enumerate()
        .filter_map(|(i, p)| rcases_pattern(p).map(|pat| format!("rcases x{i} with {pat}; ")))
        .collect();
    let image = format!(
        "(by {split_cases}all_goals first | rfl | simp [I, dec_{func_idx}, img_{func_idx}, \
         AverCert.GrammarBridge.decodeStr_strBytes])"
    );
    let steps = render_steps_proof(&closure);
    let kind_proof = match bridge.kind {
        BridgeKind::Exact => {
            let depth = plan.depth[&func_idx];
            format!(
                "refine ⟨{bound}, ?_⟩; \
                 intro fuel hk{bs}; \
                 exact AverCert.GrammarBridge.exact_of_step AverCert.Plans.fnPlans I [{members}] depth \
                 {steps} \
                 fuel {func_idx} (by decide) (by simp only [depth]; omega) _ _ {image}",
                bound = depth + 1,
                bs = if binders.is_empty() {
                    String::new()
                } else {
                    format!(" {}", binders.join(" "))
                },
            )
        }
        BridgeKind::Adequate => format!(
            "intro fuel{bs} v h; \
             exact AverCert.GrammarBridge.bridge_of_step AverCert.Plans.fnPlans I [{members}] \
             {steps} \
             fuel {func_idx} (by decide) _ v _ h {image}",
            bs = if binders.is_empty() {
                String::new()
            } else {
                format!(" {}", binders.join(" "))
            },
        ),
    };
    let typing = format!(
        "{intro}{split_cases}all_goals simp [{TYPING_SIMPS}, AverCert.Plans.fn{func_idx}]"
    );
    let statement = bridge.statement();
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
    s.push_str(" := by\n  first\n  | (set_option maxHeartbeats ");
    s.push_str(&STEP_HEARTBEATS.to_string());
    s.push_str(" in\n      (refine ⟨_, rfl, ?_, ?_⟩\n       next => ");
    s.push_str(&typing);
    s.push_str("\n       next => ");
    s.push_str(&kind_proof);
    s.push_str("\n       done))\n  | sorry\n\n");
    s.push_str("/-- The claim the manifest names: the bridge conjoined with the\n    \
                artifact-level `Holds` fact, so one kernel-checked name ties the\n    \
                plan-equals-source identity to exactly the certified bytes. -/\n\
                theorem _root_.");
    s.push_str(&bridge.corollary);
    s.push_str(" :\n    (");
    s.push_str(&statement);
    s.push_str(") ∧ (_root_.AverCert.Schema.Holds _root_.AverCert.manifest) :=\n  ⟨_root_.");
    s.push_str(&bridge.theorem);
    s.push_str(", _root_.AverCert.Final.cert⟩\n\n");
    let _ = param_binders;
}

/// Render the package's `Bridge.lean`.
fn render_bridge_lean(plan: &BridgePlan, model_roots: &[String]) -> String {
    let mut s = String::from(
        "-- Plan-equals-source bridges of this certificate. Each bridge identifies\n\
         -- the plan an export's obligation evaluates with the transpiled source\n\
         -- function the model modules and the law-claims speak about, through\n\
         -- the source-value encoders the checker renders. Producer data: the\n\
         -- checker re-states every bridge from structure and audits its axioms.\n\
         import Manifest\n\
         import Final\n\
         import GrammarBridge\n",
    );
    for root in model_roots {
        s.push_str(&format!("import {root}\n"));
    }
    s.push_str(&format!(
        "\nset_option autoImplicit false\n\
         set_option maxRecDepth 200000\n\
         set_option linter.unusedSimpArgs false\n\
         set_option linter.unusedVariables false\n\
         set_option maxHeartbeats {FILE_HEARTBEATS}\n\n\
         namespace AverCert.Bridge\n\n"
    ));
    for b in plan.fns.values() {
        render_fn_defs(b, &mut s);
    }
    render_image_table(&plan.fns, &mut s);
    // The depth of every function whose call closure has no recursion.
    s.push_str("/-- Call depth over the acyclic part of the call graph. -/\ndef depth : _root_.Nat → _root_.Nat := fun g =>\n  match g with\n");
    for (f, d) in &plan.depth {
        s.push_str(&format!("  | {f} => {d}\n"));
    }
    s.push_str("  | _ => 0\n\n");
    // The bytes of every String literal the plans mention, as rewrite
    // lemmas: `simp` cannot evaluate `strBytes "…"` itself.
    let mut literal_names = String::new();
    for (index, bytes) in plan.literals.iter().enumerate() {
        let Some(text) = lean_string_literal(bytes) else {
            continue;
        };
        let list = bytes
            .iter()
            .map(u8::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        s.push_str(&format!(
            "theorem strLit_{index} : AverCert.GrammarBridge.strBytes {text} = [{list}] := by\n  \
             first | decide | rfl | sorry\n\n"
        ));
        literal_names.push_str(&format!(", strLit_{index}"));
    }
    for b in plan.fns.values() {
        render_step(b, &plan.fns, &literal_names, &mut s);
    }
    for (bridge, func_idx) in &plan.bridges {
        render_export(bridge, *func_idx, plan, &mut s);
    }
    s.push_str("end AverCert.Bridge\n");
    s
}

// ---- law coverage -------------------------------------------------------------

/// Every source function a law statement mentions, in first-appearance
/// order: a token is a maximal run of Lean identifier characters, and it
/// counts when it is the qualified name of a def the model declares.
/// Over-recognition only adds a TRUE conjunct; under-recognition only costs
/// the law its bridged corollary — fail-closed for the claim either way.
fn law_statement_model_fns(statement: &str, info: &ModelInfo) -> Vec<String> {
    let mut found: Vec<String> = Vec::new();
    for token in statement.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_' || c == '.' || c == '\'')) {
        let token = token.trim_matches('.');
        if token.is_empty() || !info.defs.contains_key(token) {
            continue;
        }
        if !found.iter().any(|seen| seen == token) {
            found.push(token.to_string());
        }
    }
    found
}

/// The bridges covering every source function a law mentions (`None` when
/// some mentioned function has no bridge; empty when it mentions none).
fn law_bridge_coverage(statement: &str, info: &ModelInfo, bridges: &[SourceBridge]) -> Option<Vec<usize>> {
    let mut covering = Vec::new();
    for model in law_statement_model_fns(statement, info) {
        let index = bridges.iter().position(|bridge| bridge.model == model)?;
        if !covering.contains(&index) {
            covering.push(index);
        }
    }
    Some(covering)
}

/// The model files the package ships, and the module roots they define.
/// Every model file is a nested or flat `.lean` file; a root that would
/// shadow a package or wall file is refused (the whole model is then left
/// out and every bridge and law declined).
fn model_roots(model: &SourceModel) -> Result<Vec<String>, String> {
    const PACKAGE_ROOTS: &[&str] = &[
        "Module",
        "Plans",
        "Manifest",
        "Artifact",
        "ArtifactHostRoles",
        "Final",
        "ArtifactCertificate",
        "Bridge",
        "Laws",
        "ArtifactBytes",
        "ArtifactComponentBytes",
        "CheckerWitness",
    ];
    let mut roots = Vec::new();
    for (path, _) in &model.files {
        let Some(stem) = path.strip_suffix(".lean") else {
            continue;
        };
        let segments: Vec<&str> = stem.split('/').collect();
        let valid = segments.iter().all(|segment| {
            let mut chars = segment.chars();
            matches!(chars.next(), Some(first) if first.is_ascii_alphabetic())
                && chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '\'')
        });
        if !valid {
            return Err(format!("model file `{path}` is not a plain Lean module path"));
        }
        let root = segments.join(".");
        if PACKAGE_ROOTS.contains(&root.as_str())
            || wall::SOURCES
                .iter()
                .any(|source| source.name.strip_suffix(".lean") == Some(segments[0]))
        {
            return Err(format!("model module `{root}` would shadow a certificate file"));
        }
        roots.push(root);
    }
    Ok(roots)
}

/// Drop the lines the checker's code-execution wall refuses (`deriving`);
/// the certificate model mode emits nothing else it would.
fn sanitize_model_for_cert(content: &str) -> String {
    let mut out = String::with_capacity(content.len());
    for line in content.lines() {
        if line.trim_start().starts_with("deriving ") {
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    out
}

/// What `write_project` needs to render the bridge and law surfaces.
struct Surfaces {
    model_roots: Vec<String>,
    bridge_lean: Option<String>,
    laws_lean: Option<String>,
    bridges: Vec<SourceBridge>,
    law_claims: Vec<LawClaim>,
    law_bridge_exports: Vec<Vec<String>>,
    declined_bridges: Vec<(String, String)>,
    declined_laws: Vec<(String, String)>,
}

fn plan_surfaces(analysis: &Analysis, model: &SourceModel) -> Surfaces {
    let decline_all = |reason: String| Surfaces {
        model_roots: Vec::new(),
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
    let roots = match model_roots(model) {
        Ok(roots) => roots,
        Err(reason) => return decline_all(reason),
    };
    let plan = plan_bridges(analysis, model);
    let bridges: Vec<SourceBridge> = plan.bridges.iter().map(|(b, _)| b.clone()).collect();
    let bridge_lean = (!plan.fns.is_empty() && !bridges.is_empty())
        .then(|| render_bridge_lean(&plan, &roots));
    let info = ModelInfo::from_model(model);
    let (law_claims, declined_laws) = admit_law_claims(model.law_claims.clone());
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
        .then(|| render_laws_lean(&law_claims, &law_bridge_terms, &roots));
    let law_bridge_exports = law_bridges
        .iter()
        .map(|indices| indices.iter().map(|i| bridges[*i].export.clone()).collect())
        .collect();
    Surfaces {
        model_roots: roots,
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
        let shapes = alts(&op, &mut fresh).expect("alts");
        assert_eq!(shapes.len(), 2);
        assert_eq!(shapes[0].source, "(_root_.Domain.Rational.Op.add t0)");
        assert_eq!(shapes[1].source, "_root_.Domain.Rational.Op.zero");
        // A String is matched whole and decoded by the wall's `decodeStr`.
        let strs = alts(&SourceEncoder::Str, &mut fresh).expect("a String decodes");
        assert_eq!(strs.len(), 1);
        assert_eq!(strs[0].binds, vec![("v1".to_string(), "t1".to_string())]);
        assert!(alts(&SourceEncoder::Float, &mut fresh).is_err());
        assert!(alts(&SourceEncoder::List(Box::new(SourceEncoder::Int)), &mut fresh).is_err());
    }

    /// The decoder of a function with an Int and a String parameter binds the
    /// String by `decodeStr`, and the steps argument of an export cites every
    /// member of its call closure.
    #[test]
    fn decoders_and_steps_render_their_exact_text() {
        let mut fresh = 0;
        let parts = vec![
            alts(&SourceEncoder::Int, &mut fresh).unwrap(),
            alts(&SourceEncoder::Str, &mut fresh).unwrap(),
        ];
        let shapes = product(parts).unwrap().iter().map(|row| join_row(row)).collect();
        let b = BridgedFn {
            func_idx: 7,
            model: "M.greet".to_string(),
            params: vec![SourceEncoder::Int, SourceEncoder::Str],
            result: SourceEncoder::Str,
            callees: Vec::new(),
            fuel: false,
            shapes,
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
            "(by intro g hg; simp at hg; rcases hg with rfl | rfl <;> first | \
             exact ⟨_, by decide, step_3⟩ | exact ⟨_, by decide, step_9⟩)"
        );
    }
}
