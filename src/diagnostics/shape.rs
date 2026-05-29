//! `aver shape` — static module-shape analyzer.
//!
//! Walks the resolved AST and emits:
//!
//! - per-fn archetype classification (extracted from #104 research; see
//!   `tests/research_archetype_clustering.rs` for the corpus-survey
//!   harness that still uses these primitives)
//! - per-module `ModuleShape` 5-dim vector (purity / entry / state_shape /
//!   type_surface / api_shape) + derived `Kind` label
//! - verify-block coverage report
//! - archetype histogram + nearest-Layer fingerprint with confidence
//!
//! Layer fingerprints default to the built-in v0 table (see
//! `builtin_v0_layer_fingerprints`). Projects override via
//! `[[shape.layer]]` in `aver.toml`; `[[shape.expected]]` arms `--lint`.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::Path;

use crate::ast::TopLevel;
use crate::ir::hir::{
    ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedFnDef, ResolvedMatchArm, ResolvedPattern,
    ResolvedStmt, ResolvedStrPart, ResolvedTopLevel,
};
use crate::ir::{FnId, PipelineConfig, TypecheckMode};
use crate::types::Type;

// ─── Per-fn AST facts ────────────────────────────────────────────────────────

#[derive(Debug, Default)]
pub struct Facts {
    pub calls_to: HashSet<FnId>,
    pub tail_calls: HashSet<FnId>,
    pub builtin_calls: Vec<String>,
    pub ctor_constructs: Vec<ResolvedCtor>,
    pub ctor_match_patterns: usize,
    pub other_match_patterns: usize,
    pub has_match: bool,
    pub has_error_prop: bool,
    pub record_creates: usize,
    pub last_stmt_is_match: bool,
    pub only_stmt_is_literal: bool,
    pub body_stmt_count: usize,
    pub has_interp_str: bool,
    pub string_builtin_calls: usize,
    pub has_match_with_err_arm: bool,
}

fn walk_expr(e: &ResolvedExpr, facts: &mut Facts) {
    match e {
        ResolvedExpr::Literal(_) | ResolvedExpr::Ident(_) | ResolvedExpr::Resolved { .. } => {}
        ResolvedExpr::Attr(inner, _) => walk_expr(&inner.node, facts),
        ResolvedExpr::Call(callee, args) => {
            match callee {
                ResolvedCallee::Fn(id) => {
                    facts.calls_to.insert(*id);
                }
                ResolvedCallee::Builtin(name) => {
                    if name.starts_with("String.") {
                        facts.string_builtin_calls += 1;
                    }
                    facts.builtin_calls.push(name.clone());
                }
                ResolvedCallee::Intrinsic(_) => {}
                ResolvedCallee::LocalSlot { .. } => {}
                ResolvedCallee::Unresolved { callee } => walk_expr(&callee.node, facts),
            }
            for a in args {
                walk_expr(&a.node, facts);
            }
        }
        ResolvedExpr::BinOp(_, a, b) => {
            walk_expr(&a.node, facts);
            walk_expr(&b.node, facts);
        }
        ResolvedExpr::Neg(inner) => walk_expr(&inner.node, facts),
        ResolvedExpr::Match { subject, arms } => {
            facts.has_match = true;
            walk_expr(&subject.node, facts);
            for arm in arms {
                count_arm_pattern(&arm.pattern, facts);
                walk_match_arm(arm, facts);
            }
        }
        ResolvedExpr::Ctor(c, args) => {
            facts.ctor_constructs.push(c.clone());
            for a in args {
                walk_expr(&a.node, facts);
            }
        }
        ResolvedExpr::ErrorProp(inner) => {
            facts.has_error_prop = true;
            walk_expr(&inner.node, facts);
        }
        ResolvedExpr::InterpolatedStr(parts) => {
            facts.has_interp_str = true;
            for p in parts {
                if let ResolvedStrPart::Parsed(e) = p {
                    walk_expr(&e.node, facts);
                }
            }
        }
        ResolvedExpr::List(xs) | ResolvedExpr::Tuple(xs) => {
            for x in xs {
                walk_expr(&x.node, facts);
            }
        }
        ResolvedExpr::MapLiteral(pairs) => {
            for (k, v) in pairs {
                walk_expr(&k.node, facts);
                walk_expr(&v.node, facts);
            }
        }
        ResolvedExpr::RecordCreate { fields, .. } => {
            facts.record_creates += 1;
            for (_, v) in fields {
                walk_expr(&v.node, facts);
            }
        }
        ResolvedExpr::RecordUpdate { base, updates, .. } => {
            walk_expr(&base.node, facts);
            for (_, v) in updates {
                walk_expr(&v.node, facts);
            }
        }
        ResolvedExpr::TailCall { target, args } => {
            facts.tail_calls.insert(*target);
            facts.calls_to.insert(*target);
            for a in args {
                walk_expr(&a.node, facts);
            }
        }
        ResolvedExpr::IndependentProduct(xs, _) => {
            for x in xs {
                walk_expr(&x.node, facts);
            }
        }
    }
}

fn count_arm_pattern(p: &ResolvedPattern, facts: &mut Facts) {
    match p {
        ResolvedPattern::Ctor(ctor, _) => {
            facts.ctor_match_patterns += 1;
            if matches!(
                ctor,
                ResolvedCtor::Builtin(crate::ir::hir::BuiltinCtor::ResultErr)
            ) {
                facts.has_match_with_err_arm = true;
            }
        }
        _ => facts.other_match_patterns += 1,
    }
}

fn walk_match_arm(arm: &ResolvedMatchArm, facts: &mut Facts) {
    walk_expr(&arm.body.node, facts);
}

pub fn extract_facts(fd: &ResolvedFnDef) -> Facts {
    let mut facts = Facts::default();
    let stmts = fd.body.stmts();
    facts.body_stmt_count = stmts.len();
    for stmt in stmts {
        match stmt {
            ResolvedStmt::Binding { value, .. } => walk_expr(&value.node, &mut facts),
            ResolvedStmt::Expr(value) => walk_expr(&value.node, &mut facts),
        }
    }
    if let Some(last) = stmts.last() {
        let expr = match last {
            ResolvedStmt::Binding { value, .. } => &value.node,
            ResolvedStmt::Expr(value) => &value.node,
        };
        facts.last_stmt_is_match = matches!(expr, ResolvedExpr::Match { .. });
    }
    facts.only_stmt_is_literal = stmts.len() == 1 && {
        let expr = match &stmts[0] {
            ResolvedStmt::Binding { value, .. } => &value.node,
            ResolvedStmt::Expr(value) => &value.node,
        };
        matches!(
            expr,
            ResolvedExpr::Literal(_)
                | ResolvedExpr::List(_)
                | ResolvedExpr::Tuple(_)
                | ResolvedExpr::MapLiteral(_)
                | ResolvedExpr::RecordCreate { .. }
                | ResolvedExpr::Ctor(_, _)
        )
    };
    facts
}

// ─── Per-fn classification ───────────────────────────────────────────────────

pub fn classify(fd: &ResolvedFnDef, facts: &Facts, scc: &HashSet<FnId>) -> Vec<&'static str> {
    let mut labels = Vec::new();

    let self_call = facts.calls_to.contains(&fd.fn_id) || facts.tail_calls.contains(&fd.fn_id);
    if self_call {
        labels.push("structural-recursion");
    }
    if scc.contains(&fd.fn_id) {
        labels.push("scc-mutual");
    }

    if facts.last_stmt_is_match {
        if facts.ctor_match_patterns >= facts.other_match_patterns && facts.ctor_match_patterns > 0
        {
            labels.push("match-dispatcher");
        } else {
            labels.push("match-on-value");
        }
    }

    if !fd.effects.is_empty() {
        let total_calls = facts.calls_to.len() + facts.builtin_calls.len();
        if total_calls >= 2 {
            labels.push("orchestration");
        } else {
            labels.push("effectful-leaf");
        }
    }

    let is_result_ret = type_is_result(&fd.return_type);
    if is_result_ret && facts.has_error_prop {
        labels.push("pipeline-result");
    } else if is_result_ret && facts.has_match_with_err_arm {
        labels.push("manual-result-adapter");
    }

    let is_string_ret = matches!(&fd.return_type, Type::Named { name, .. } if name == "String");
    if is_string_ret
        && fd.effects.is_empty()
        && (facts.has_interp_str || facts.string_builtin_calls >= 1)
        && (facts.body_stmt_count >= 2 || facts.has_interp_str)
    {
        labels.push("renderer-formatter");
    }

    if facts.body_stmt_count == 1
        && (!facts.ctor_constructs.is_empty() || facts.record_creates > 0)
        && !facts.has_match
    {
        labels.push("constructor-wrapper");
    }

    if fd.params.is_empty()
        && facts.calls_to.is_empty()
        && facts.builtin_calls.is_empty()
        && fd.effects.is_empty()
        && facts.only_stmt_is_literal
    {
        labels.push("data-as-function");
    }

    if facts.body_stmt_count == 1
        && !facts.has_match
        && !self_call
        && fd.effects.is_empty()
        && (!facts.builtin_calls.is_empty() || !facts.calls_to.is_empty())
    {
        labels.push("trivial-helper");
    }

    if facts.body_stmt_count == 1
        && !facts.has_match
        && !self_call
        && fd.effects.is_empty()
        && facts.builtin_calls.is_empty()
        && facts.calls_to.is_empty()
        && facts.ctor_constructs.is_empty()
        && !fd.params.is_empty()
    {
        labels.push("pure-expression");
    }

    if facts.body_stmt_count >= 2
        && !facts.last_stmt_is_match
        && fd.effects.is_empty()
        && (!facts.builtin_calls.is_empty() || !facts.calls_to.is_empty())
        && !self_call
    {
        labels.push("let-pipeline");
    }

    labels
}

pub fn type_is_result(t: &Type) -> bool {
    matches!(t, Type::Named { name, .. } if name == "Result")
}

pub const ARCHETYPE_ORDER: &[&str] = &[
    "scc-mutual",
    "structural-recursion",
    "match-dispatcher",
    "pipeline-result",
    "manual-result-adapter",
    "renderer-formatter",
    "match-on-value",
    "orchestration",
    "effectful-leaf",
    "let-pipeline",
    "constructor-wrapper",
    "data-as-function",
    "trivial-helper",
    "pure-expression",
];

pub fn primary_label(labels: &[&str]) -> &'static str {
    for &want in ARCHETYPE_ORDER {
        if labels.contains(&want) {
            return want;
        }
    }
    "unclassified"
}

// ─── Call-graph SCC (multi-node strongly connected components) ───────────────

pub fn compute_sccs(fns: &[&ResolvedFnDef], facts_by_id: &HashMap<FnId, &Facts>) -> HashSet<FnId> {
    let mut graph: HashMap<FnId, Vec<FnId>> = HashMap::new();
    let fn_ids: HashSet<FnId> = fns.iter().map(|f| f.fn_id).collect();
    for fd in fns {
        if let Some(facts) = facts_by_id.get(&fd.fn_id) {
            let edges: Vec<FnId> = facts
                .calls_to
                .iter()
                .copied()
                .filter(|c| fn_ids.contains(c) && *c != fd.fn_id)
                .collect();
            graph.insert(fd.fn_id, edges);
        }
    }
    let mut index = 0u32;
    let mut stack: Vec<FnId> = Vec::new();
    let mut on_stack: HashSet<FnId> = HashSet::new();
    let mut indices: HashMap<FnId, u32> = HashMap::new();
    let mut lowlinks: HashMap<FnId, u32> = HashMap::new();
    let mut multi_scc: HashSet<FnId> = HashSet::new();

    #[allow(clippy::too_many_arguments)]
    fn strongconnect(
        v: FnId,
        graph: &HashMap<FnId, Vec<FnId>>,
        index: &mut u32,
        stack: &mut Vec<FnId>,
        on_stack: &mut HashSet<FnId>,
        indices: &mut HashMap<FnId, u32>,
        lowlinks: &mut HashMap<FnId, u32>,
        multi_scc: &mut HashSet<FnId>,
    ) {
        indices.insert(v, *index);
        lowlinks.insert(v, *index);
        *index += 1;
        stack.push(v);
        on_stack.insert(v);
        if let Some(neighbors) = graph.get(&v).cloned() {
            for w in neighbors {
                if !indices.contains_key(&w) {
                    strongconnect(
                        w, graph, index, stack, on_stack, indices, lowlinks, multi_scc,
                    );
                    let lv = *lowlinks.get(&v).unwrap();
                    let lw = *lowlinks.get(&w).unwrap();
                    lowlinks.insert(v, lv.min(lw));
                } else if on_stack.contains(&w) {
                    let lv = *lowlinks.get(&v).unwrap();
                    let iw = *indices.get(&w).unwrap();
                    lowlinks.insert(v, lv.min(iw));
                }
            }
        }
        if lowlinks.get(&v) == indices.get(&v) {
            let mut scc: Vec<FnId> = Vec::new();
            loop {
                let w = stack.pop().unwrap();
                on_stack.remove(&w);
                scc.push(w);
                if w == v {
                    break;
                }
            }
            if scc.len() > 1 {
                multi_scc.extend(scc);
            }
        }
    }

    let nodes: Vec<FnId> = graph.keys().copied().collect();
    for v in nodes {
        if !indices.contains_key(&v) {
            strongconnect(
                v,
                &graph,
                &mut index,
                &mut stack,
                &mut on_stack,
                &mut indices,
                &mut lowlinks,
                &mut multi_scc,
            );
        }
    }
    multi_scc
}

// ─── ModuleShape vector + derived Kind ───────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Purity {
    Pure,
    ClassifiedEffectful,
    ShellEffectful,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Entry {
    None,
    Main,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StateShape {
    Stateless,
    PureStateMachine,
    ExternalWorld,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeSurface {
    NoTypes,
    PlainDataTypes,
    UserOpaque,
    RuntimeHandle,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ApiShape {
    Closed,
    ExposedLibrary,
    ServiceBoundary,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ModuleShape {
    pub purity: Purity,
    pub entry: Entry,
    pub state_shape: StateShape,
    pub type_surface: TypeSurface,
    pub api_shape: ApiShape,
}

impl Purity {
    pub fn as_str(&self) -> &'static str {
        match self {
            Purity::Pure => "Pure",
            Purity::ClassifiedEffectful => "ClassifiedEffectful",
            Purity::ShellEffectful => "ShellEffectful",
        }
    }

    /// One-line user-facing explanation of what each purity value means.
    /// Surfaced in CLI help / hover text — answers "what makes this
    /// module ShellEffectful vs ClassifiedEffectful?".
    pub fn description(&self) -> &'static str {
        match self {
            Purity::Pure => "no effects declared",
            Purity::ClassifiedEffectful => {
                "all effects are Oracle-classified (one-shot req/resp shape)"
            }
            Purity::ShellEffectful => {
                "contains at least one shell/lifecycle effect (e.g. HttpServer.listen) — Oracle skips by design"
            }
        }
    }
}

impl Entry {
    pub fn as_str(&self) -> &'static str {
        match self {
            Entry::None => "None",
            Entry::Main => "Main",
        }
    }
}

impl StateShape {
    pub fn as_str(&self) -> &'static str {
        match self {
            StateShape::Stateless => "Stateless",
            StateShape::PureStateMachine => "PureStateMachine",
            StateShape::ExternalWorld => "ExternalWorld",
        }
    }
}

impl TypeSurface {
    pub fn as_str(&self) -> &'static str {
        match self {
            TypeSurface::NoTypes => "NoTypes",
            TypeSurface::PlainDataTypes => "PlainDataTypes",
            TypeSurface::UserOpaque => "UserOpaque",
            TypeSurface::RuntimeHandle => "RuntimeHandle",
        }
    }
}

impl ApiShape {
    pub fn as_str(&self) -> &'static str {
        match self {
            ApiShape::Closed => "Closed",
            ApiShape::ExposedLibrary => "ExposedLibrary",
            ApiShape::ServiceBoundary => "ServiceBoundary",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kind {
    PureHelpers,
    DataModule,
    SmartConstructor,
    Library,
    Orchestration,
    ServiceClient,
    EffectfulLibrary,
    EffectfulShell,
}

impl Kind {
    pub fn as_str(&self) -> &'static str {
        match self {
            Kind::PureHelpers => "PureHelpers",
            Kind::DataModule => "DataModule",
            Kind::SmartConstructor => "SmartConstructor",
            Kind::Library => "Library",
            Kind::Orchestration => "Orchestration",
            Kind::ServiceClient => "ServiceClient",
            Kind::EffectfulLibrary => "EffectfulLibrary",
            Kind::EffectfulShell => "EffectfulShell",
        }
    }

    /// Plain-English explanation of the rule that picked this Kind from
    /// the vector. Used as JSON `rule` field and as a hover snippet.
    pub fn rule(&self) -> &'static str {
        match self {
            Kind::PureHelpers => "pure module, no exposed surface, plain types",
            Kind::DataModule => "pure module exposing library API over plain data types",
            Kind::SmartConstructor => "pure module exposing an opaque user type",
            Kind::Library => "classified effects with exposed API, no main",
            Kind::Orchestration => "classified effects with main entry point",
            Kind::ServiceClient => "classified effects threaded through a runtime handle",
            Kind::EffectfulLibrary => "effectful library — exposed surface, no main",
            Kind::EffectfulShell => {
                "shell/lifecycle effects (e.g. HttpServer.listen) — long-running, not Oracle-classified"
            }
        }
    }
}

pub fn derive_kind(shape: &ModuleShape) -> Kind {
    // Effectful Kinds (both Classified and Shell flavors): a Shell effect
    // is a long-running lifecycle (HttpServer.listen, Tcp.listen) — Oracle
    // skips it by design, not because the classifier doesn't recognize it.
    // From the user's perspective a module with a shell-effect entrypoint
    // is still Orchestration (or ServiceClient if it threads a handle);
    // the `purity` field tells the rest of the story.
    if !matches!(shape.purity, Purity::Pure) {
        // ServiceClient = module that re-exports its handle-using API
        // surface (api_shape == ServiceBoundary, i.e. some exposed fn
        // takes/returns the handle). Threading a handle internally is
        // not enough — those modules show up as Orchestration / Library
        // depending on whether they have `main`.
        if matches!(shape.api_shape, ApiShape::ServiceBoundary) {
            return Kind::ServiceClient;
        }
        if matches!(shape.entry, Entry::Main) {
            return Kind::Orchestration;
        }
        if matches!(shape.api_shape, ApiShape::ExposedLibrary) {
            return Kind::Library;
        }
        if matches!(shape.purity, Purity::ShellEffectful) {
            return Kind::EffectfulShell;
        }
        return Kind::EffectfulLibrary;
    }
    // Pure from here.
    if matches!(shape.type_surface, TypeSurface::UserOpaque) {
        return Kind::SmartConstructor;
    }
    if matches!(
        shape.api_shape,
        ApiShape::ExposedLibrary | ApiShape::ServiceBoundary
    ) {
        return Kind::DataModule;
    }
    Kind::PureHelpers
}

// ─── Verification report (orthogonal to Kind mapping) ────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VerifyLevel {
    None,
    Cases,
    Laws,
    Trace,
    Mixed,
}

impl VerifyLevel {
    pub fn as_str(&self) -> &'static str {
        match self {
            VerifyLevel::None => "None",
            VerifyLevel::Cases => "Cases",
            VerifyLevel::Laws => "Laws",
            VerifyLevel::Trace => "Trace",
            VerifyLevel::Mixed => "Mixed",
        }
    }
}

#[derive(Debug, Clone)]
pub struct VerifyReport {
    pub level: VerifyLevel,
    pub blocks: usize,
    pub covered_fns: usize,
    pub eligible_fns: usize,
}

// ─── Histogram ──────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Default)]
pub struct Histogram {
    pub counts: BTreeMap<&'static str, usize>,
    pub total_fns: usize,
}

impl Histogram {
    pub fn percentage(&self, archetype: &str) -> f64 {
        if self.total_fns == 0 {
            return 0.0;
        }
        let c = self.counts.get(archetype).copied().unwrap_or(0) as f64;
        100.0 * c / self.total_fns as f64
    }

    /// Returns archetype-percentage pairs in descending count order.
    pub fn sorted(&self) -> Vec<(&'static str, usize, f64)> {
        let mut entries: Vec<(&'static str, usize)> = self
            .counts
            .iter()
            .filter(|(_, c)| **c > 0)
            .map(|(k, c)| (*k, *c))
            .collect();
        entries.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.cmp(b.0)));
        entries
            .into_iter()
            .map(|(name, count)| {
                let pct = if self.total_fns == 0 {
                    0.0
                } else {
                    100.0 * count as f64 / self.total_fns as f64
                };
                (name, count, pct)
            })
            .collect()
    }
}

// ─── Layer fingerprint + distance ────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Layer {
    Domain,
    Parse,
    Command,
    AiStrategy,
    RenderUi,
    Infra,
}

impl Layer {
    pub fn as_str(&self) -> &'static str {
        match self {
            Layer::Domain => "Domain",
            Layer::Parse => "Parse",
            Layer::Command => "Command",
            Layer::AiStrategy => "AiStrategy",
            Layer::RenderUi => "RenderUi",
            Layer::Infra => "Infra",
        }
    }

    pub fn parse(s: &str) -> Option<Layer> {
        match s {
            "Domain" => Some(Layer::Domain),
            "Parse" => Some(Layer::Parse),
            "Command" => Some(Layer::Command),
            "AiStrategy" | "AI-strategy" | "AIStrategy" => Some(Layer::AiStrategy),
            "RenderUi" | "RenderUI" | "render-UI" => Some(Layer::RenderUi),
            "Infra" => Some(Layer::Infra),
            _ => None,
        }
    }

    pub fn all() -> &'static [Layer] {
        &[
            Layer::Domain,
            Layer::Parse,
            Layer::Command,
            Layer::AiStrategy,
            Layer::RenderUi,
            Layer::Infra,
        ]
    }
}

/// Five-bucket histogram a `Layer` fingerprint is defined against.
/// Mapped from the 14 archetype labels via `archetype_to_bucket`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Bucket {
    Match,
    Recursion,
    Pipeline,
    Orchestration,
    Helpers,
}

impl Bucket {
    pub fn as_str(&self) -> &'static str {
        match self {
            Bucket::Match => "match",
            Bucket::Recursion => "recursion",
            Bucket::Pipeline => "pipeline",
            Bucket::Orchestration => "orchestration",
            Bucket::Helpers => "helpers",
        }
    }
}

pub fn archetype_to_bucket(archetype: &str) -> Option<Bucket> {
    match archetype {
        "match-dispatcher" | "match-on-value" => Some(Bucket::Match),
        "scc-mutual" | "structural-recursion" => Some(Bucket::Recursion),
        "pipeline-result" | "let-pipeline" | "manual-result-adapter" => Some(Bucket::Pipeline),
        "orchestration" | "effectful-leaf" => Some(Bucket::Orchestration),
        "trivial-helper"
        | "pure-expression"
        | "constructor-wrapper"
        | "data-as-function"
        | "renderer-formatter" => Some(Bucket::Helpers),
        _ => None,
    }
}

#[derive(Debug, Clone)]
pub struct LayerFingerprint {
    pub layer: Layer,
    /// Bucket percentages summing to ~100.
    pub buckets: [(Bucket, f64); 5],
}

/// Built-in v0 fingerprints sourced from the casual single-author
/// corpus measurement that landed on the tweet. Concrete numbers:
///
/// ```text
/// LAYER       match  recursion  pipeline  orchestration  helpers
/// Domain      ~40%   ~25%       ~0%       ~5%            ~30%
/// Parse       ~15%   ~10%       ~65%      ~10%           ~0%
/// Command     ~10%    ~5%       ~75%      ~10%           ~0%
/// AiStrategy  ~60%   ~15%       ~0%       ~10%           ~15%
/// RenderUi    ~50%    ~5%       ~0%       ~25%           ~20%
/// Infra       ~20%   ~10%       ~30%      ~40%            ~0%
/// ```
pub fn builtin_v0_layer_fingerprints() -> Vec<LayerFingerprint> {
    use Bucket::*;
    let mk = |layer, m, r, p, o, h| LayerFingerprint {
        layer,
        buckets: [
            (Match, m),
            (Recursion, r),
            (Pipeline, p),
            (Orchestration, o),
            (Helpers, h),
        ],
    };
    vec![
        mk(Layer::Domain, 40.0, 25.0, 0.0, 5.0, 30.0),
        mk(Layer::Parse, 15.0, 10.0, 65.0, 10.0, 0.0),
        mk(Layer::Command, 10.0, 5.0, 75.0, 10.0, 0.0),
        mk(Layer::AiStrategy, 60.0, 15.0, 0.0, 10.0, 15.0),
        mk(Layer::RenderUi, 50.0, 5.0, 0.0, 25.0, 20.0),
        mk(Layer::Infra, 20.0, 10.0, 30.0, 40.0, 0.0),
    ]
}

fn histogram_to_buckets(hist: &Histogram) -> [(Bucket, f64); 5] {
    use Bucket::*;
    let mut counts: HashMap<Bucket, usize> = HashMap::new();
    for (arch, c) in &hist.counts {
        if let Some(b) = archetype_to_bucket(arch) {
            *counts.entry(b).or_insert(0) += c;
        }
    }
    let total = hist.total_fns.max(1) as f64;
    let pct = |b: Bucket| 100.0 * counts.get(&b).copied().unwrap_or(0) as f64 / total;
    [
        (Match, pct(Match)),
        (Recursion, pct(Recursion)),
        (Pipeline, pct(Pipeline)),
        (Orchestration, pct(Orchestration)),
        (Helpers, pct(Helpers)),
    ]
}

#[derive(Debug, Clone)]
pub struct LayerVerdict {
    pub layer: Layer,
    pub confidence: f64,
    pub basis: String,
    pub support_fns: usize,
}

/// Nearest fingerprint by Euclidean distance over bucket %.
/// Confidence is `(max_dist - chosen_dist) / max_dist`, penalized by
/// small `total_fns` (< 5 fns flattens confidence regardless of fit).
pub fn classify_layer(
    hist: &Histogram,
    fingerprints: &[LayerFingerprint],
    basis: &str,
) -> Option<LayerVerdict> {
    if fingerprints.is_empty() || hist.total_fns == 0 {
        return None;
    }
    let observed = histogram_to_buckets(hist);
    let mut scored: Vec<(Layer, f64)> = Vec::new();
    for fp in fingerprints {
        let mut sq = 0.0_f64;
        for ((b1, p1), (b2, p2)) in observed.iter().zip(fp.buckets.iter()) {
            debug_assert_eq!(b1, b2);
            let d = p1 - p2;
            sq += d * d;
        }
        scored.push((fp.layer, sq.sqrt()));
    }
    scored.sort_by(|a, b| a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal));
    let (best_layer, best_dist) = scored[0];
    // Max meaningful Euclidean distance across 5 bucket pcts is sqrt(5 * 100^2) ≈ 223.6.
    let max_dist = (5.0_f64 * 100.0 * 100.0).sqrt();
    let raw_conf = ((max_dist - best_dist) / max_dist).clamp(0.0, 1.0);
    let n = hist.total_fns;
    let small_n_penalty = if n < 5 {
        // Hard-cap confidence for tiny modules — 3 fns is noise.
        0.2_f64.min(raw_conf)
    } else if n < 10 {
        // Soft-penalize medium modules at most ~30% of headroom.
        raw_conf * 0.7
    } else {
        raw_conf
    };
    Some(LayerVerdict {
        layer: best_layer,
        confidence: small_n_penalty,
        basis: basis.to_string(),
        support_fns: n,
    })
}

// ─── Top-level analysis ──────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct FnShape {
    pub name: String,
    pub primary: &'static str,
    pub labels: Vec<&'static str>,
}

#[derive(Debug, Clone)]
pub struct ShapeReport {
    pub module: String,
    pub file: String,
    pub depends: Vec<String>,
    pub effects: Vec<String>,
    pub exposes_opaque: Vec<String>,
    pub has_main: bool,
    pub shape: ModuleShape,
    pub kind: Kind,
    pub verify: VerifyReport,
    pub histogram: Histogram,
    pub layer: Option<LayerVerdict>,
    pub fns: Vec<FnShape>,
}

pub fn analyze_path(path: &Path, module_root_hint: Option<&str>) -> Result<ShapeReport, String> {
    let source = std::fs::read_to_string(path).map_err(|e| format!("read: {}", e))?;
    let mut items = crate::source::parse_source(&source).map_err(|e| format!("parse: {}", e))?;

    let module_root = match module_root_hint {
        Some(r) => r.to_string(),
        None => path
            .parent()
            .and_then(|p| p.to_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| ".".to_string()),
    };

    let dep_modules = crate::source::load_compile_deps(&items, &module_root)
        .map_err(|e| format!("deps: {}", e))?;
    let pipeline_result = crate::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::Full {
                base_dir: Some(&module_root),
            }),
            dep_modules: &dep_modules,
            ..Default::default()
        },
    );
    if let Some(tc) = pipeline_result.typecheck.as_ref()
        && !tc.errors.is_empty()
    {
        let first = tc
            .errors
            .first()
            .map(|e| e.message.clone())
            .unwrap_or_default();
        return Err(format!("typecheck errors: {first}"));
    }

    let module = items.iter().find_map(|i| match i {
        TopLevel::Module(m) => Some(m.clone()),
        _ => None,
    });
    let (module_name, depends, effects, exposes_opaque, exposes) = match module {
        Some(m) => (
            m.name.clone(),
            m.depends.clone(),
            m.effects.clone().unwrap_or_default(),
            m.exposes_opaque.clone(),
            m.exposes.clone(),
        ),
        None => (
            path.file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("<unnamed>")
                .to_string(),
            vec![],
            vec![],
            vec![],
            vec![],
        ),
    };

    let resolved_fns: Vec<&ResolvedFnDef> = pipeline_result
        .resolved_items
        .iter()
        .filter_map(|t| match t {
            ResolvedTopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();

    let mut facts_by_id: HashMap<FnId, Facts> = HashMap::new();
    for fd in &resolved_fns {
        facts_by_id.insert(fd.fn_id, extract_facts(fd));
    }
    let facts_refs: HashMap<FnId, &Facts> = facts_by_id.iter().map(|(k, v)| (*k, v)).collect();
    let scc = compute_sccs(&resolved_fns, &facts_refs);

    let exposes_set: HashSet<&str> = exposes.iter().map(|s| s.as_str()).collect();
    let mut fn_shapes = Vec::with_capacity(resolved_fns.len());
    let mut histogram = Histogram::default();
    let mut has_main = false;
    let mut classified_uses_handle = false;
    let mut exposed_uses_handle = false;

    for fd in &resolved_fns {
        if fd.name == "main" {
            has_main = true;
        }
        let this_uses_handle = uses_runtime_handle(fd);
        if this_uses_handle {
            classified_uses_handle = true;
            if exposes_set.contains(fd.name.as_str()) {
                exposed_uses_handle = true;
            }
        }
        let facts = &facts_by_id[&fd.fn_id];
        let labels = classify(fd, facts, &scc);
        let primary = primary_label(&labels);
        if fd.name != "main" {
            *histogram.counts.entry(primary).or_insert(0) += 1;
            histogram.total_fns += 1;
        }
        fn_shapes.push(FnShape {
            name: fd.name.clone(),
            primary,
            labels,
        });
    }

    let verify = compute_verify_report(&items, &resolved_fns);

    let shape = derive_shape(
        &effects,
        has_main,
        &exposes_opaque,
        classified_uses_handle,
        exposed_uses_handle,
        &exposes,
    );
    let kind = derive_kind(&shape);
    let layer = classify_layer(&histogram, &builtin_v0_layer_fingerprints(), "built-in v0");

    Ok(ShapeReport {
        module: module_name,
        file: path.to_string_lossy().to_string(),
        depends,
        effects,
        exposes_opaque,
        has_main,
        shape,
        kind,
        verify,
        histogram,
        layer,
        fns: fn_shapes,
    })
}

fn uses_runtime_handle(fd: &ResolvedFnDef) -> bool {
    fd.params.iter().any(|(_, ty)| is_runtime_handle_type(ty))
        || is_runtime_handle_type(&fd.return_type)
}

fn is_runtime_handle_type(t: &Type) -> bool {
    use crate::types::checker::effect_classification::is_verify_fabricable_handle;
    walk_type_named(t, &is_verify_fabricable_handle)
}

fn walk_type_named(t: &Type, pred: &dyn Fn(&str) -> bool) -> bool {
    match t {
        Type::Named { name, .. } => pred(name),
        Type::Result(a, b) | Type::Map(a, b) => {
            walk_type_named(a, pred) || walk_type_named(b, pred)
        }
        Type::Option(a) | Type::List(a) | Type::Vector(a) => walk_type_named(a, pred),
        Type::Tuple(xs) => xs.iter().any(|x| walk_type_named(x, pred)),
        Type::Fn(args, ret, _) => {
            args.iter().any(|a| walk_type_named(a, pred)) || walk_type_named(ret, pred)
        }
        _ => false,
    }
}

fn compute_verify_report(items: &[TopLevel], resolved_fns: &[&ResolvedFnDef]) -> VerifyReport {
    use crate::ast::VerifyKind;
    let mut blocks = 0usize;
    let mut covered: HashSet<String> = HashSet::new();
    let mut has_cases = false;
    let mut has_laws = false;
    let mut has_trace = false;
    for it in items {
        if let TopLevel::Verify(v) = it {
            blocks += 1;
            covered.insert(v.fn_name.clone());
            if v.trace {
                has_trace = true;
            } else {
                match &v.kind {
                    VerifyKind::Cases => has_cases = true,
                    VerifyKind::Law(_) => has_laws = true,
                }
            }
        }
    }
    let level = match (has_cases, has_laws, has_trace) {
        (false, false, false) => VerifyLevel::None,
        (true, false, false) => VerifyLevel::Cases,
        (false, true, false) => VerifyLevel::Laws,
        (false, false, true) => VerifyLevel::Trace,
        _ => VerifyLevel::Mixed,
    };
    // Eligible = non-main fns. Coverage = eligible fns that appear in at
    // least one verify block under their name.
    let eligible_names: HashSet<String> = resolved_fns
        .iter()
        .filter(|f| f.name != "main")
        .map(|f| f.name.clone())
        .collect();
    let covered_count = eligible_names.intersection(&covered).count();
    VerifyReport {
        level,
        blocks,
        covered_fns: covered_count,
        eligible_fns: eligible_names.len(),
    }
}

fn derive_shape(
    effects: &[String],
    has_main: bool,
    exposes_opaque: &[String],
    classified_uses_handle: bool,
    exposed_uses_handle: bool,
    exposes: &[String],
) -> ModuleShape {
    use crate::types::checker::effect_classification::is_classified;

    let any_effect = !effects.is_empty();
    let all_classified = any_effect
        && effects
            .iter()
            .all(|e| is_classified_effect_or_namespace(e, &is_classified));
    let purity = if !any_effect {
        Purity::Pure
    } else if all_classified {
        Purity::ClassifiedEffectful
    } else {
        Purity::ShellEffectful
    };
    let entry = if has_main { Entry::Main } else { Entry::None };
    let state_shape = match purity {
        Purity::Pure => StateShape::Stateless,
        _ if classified_uses_handle => StateShape::PureStateMachine,
        _ => StateShape::ExternalWorld,
    };
    let type_surface = if classified_uses_handle {
        TypeSurface::RuntimeHandle
    } else if !exposes_opaque.is_empty() {
        TypeSurface::UserOpaque
    } else if !exposes.is_empty() {
        TypeSurface::PlainDataTypes
    } else {
        TypeSurface::NoTypes
    };
    // ServiceBoundary only when the handle actually reaches the exposed
    // surface — i.e. at least one fn in `exposes [...]` takes/returns the
    // handle. A module that threads `Tcp.Connection` only internally
    // (a webapp that talks to Redis but doesn't re-export its client)
    // stays at `ExposedLibrary` so its Kind doesn't get mis-labeled as
    // `ServiceClient`.
    let api_shape = if exposed_uses_handle {
        ApiShape::ServiceBoundary
    } else if !exposes.is_empty() {
        ApiShape::ExposedLibrary
    } else {
        ApiShape::Closed
    };
    ModuleShape {
        purity,
        entry,
        state_shape,
        type_surface,
        api_shape,
    }
}

fn is_classified_effect_or_namespace(eff: &str, is_classified: &dyn Fn(&str) -> bool) -> bool {
    if is_classified(eff) {
        return true;
    }
    // Namespace shorthand: `Disk`, `Tcp`, `Http`, etc. Accept if at least
    // one classified method has this prefix.
    let with_dot = format!("{eff}.");
    use crate::types::checker::effect_classification::classifications_for_proof_subset;
    classifications_for_proof_subset()
        .iter()
        .any(|c| c.method.starts_with(&with_dot))
}

// ─── Renderers ───────────────────────────────────────────────────────────────

pub struct RenderOptions {
    pub summary: bool,
}

pub fn render_text(report: &ShapeReport, opts: &RenderOptions) -> String {
    let mut out = String::new();
    out.push_str(&format!("Module: {}\n", report.module));
    out.push_str(&format!("  Kind:  {}\n\n", report.kind.as_str()));
    out.push_str("  ModuleShape:\n");
    out.push_str(&format!(
        "    purity        {}\n",
        report.shape.purity.as_str()
    ));
    out.push_str(&format!(
        "    entry         {}\n",
        report.shape.entry.as_str()
    ));
    out.push_str(&format!(
        "    state_shape   {}\n",
        report.shape.state_shape.as_str()
    ));
    out.push_str(&format!(
        "    type_surface  {}\n",
        report.shape.type_surface.as_str()
    ));
    out.push_str(&format!(
        "    api_shape     {}\n\n",
        report.shape.api_shape.as_str()
    ));

    out.push_str(&format!(
        "  Verification:   {} {} block{}, {}/{} fns covered\n\n",
        report.verify.blocks,
        match report.verify.level {
            VerifyLevel::None => "(no)",
            VerifyLevel::Cases => "cases",
            VerifyLevel::Laws => "law",
            VerifyLevel::Trace => "trace",
            VerifyLevel::Mixed => "mixed",
        },
        if report.verify.blocks == 1 { "" } else { "s" },
        report.verify.covered_fns,
        report.verify.eligible_fns,
    ));

    out.push_str(&format!(
        "  Histogram ({} fns):\n",
        report.histogram.total_fns
    ));
    let sorted = report.histogram.sorted();
    let name_w = sorted
        .iter()
        .map(|(n, _, _)| n.len())
        .max()
        .unwrap_or(0)
        .max(20);
    for (name, count, pct) in &sorted {
        let bar = histogram_bar(*pct, 20);
        out.push_str(&format!(
            "    {:<width$}  {}  {:>3.0}%  ({})\n",
            name,
            bar,
            pct,
            count,
            width = name_w,
        ));
    }
    if let Some(verdict) = &report.layer {
        out.push_str(&format!(
            "\n  Layer: {}  (confidence {:.2}, basis: {})\n",
            verdict.layer.as_str(),
            verdict.confidence,
            verdict.basis,
        ));
    } else {
        out.push_str("\n  Layer: insufficient data\n");
    }

    if !opts.summary {
        out.push_str("\n  Functions:\n");
        let name_w = report
            .fns
            .iter()
            .map(|f| f.name.len())
            .max()
            .unwrap_or(0)
            .max(16);
        for f in &report.fns {
            let labels: Vec<&str> = f.labels.to_vec();
            let labels_str = if labels.is_empty() {
                "unclassified".to_string()
            } else {
                labels.join(", ")
            };
            out.push_str(&format!(
                "    {:<width$}  {}\n",
                f.name,
                labels_str,
                width = name_w,
            ));
        }
    }

    out
}

fn histogram_bar(pct: f64, width: usize) -> String {
    let filled = ((pct / 100.0) * width as f64).round() as usize;
    let filled = filled.min(width);
    let mut s = String::with_capacity(width * 3);
    for _ in 0..filled {
        s.push('█');
    }
    for _ in filled..width {
        s.push('░');
    }
    s
}

pub fn render_json(report: &ShapeReport) -> serde_json::Value {
    use serde_json::json;
    let layer = report.layer.as_ref().map(|v| {
        json!({
            "name": v.layer.as_str(),
            "confidence": v.confidence,
            "basis": v.basis,
            "support_fns": v.support_fns,
        })
    });
    let histogram_counts: serde_json::Value = report
        .histogram
        .counts
        .iter()
        .filter(|(_, c)| **c > 0)
        .map(|(k, c)| (k.to_string(), serde_json::Value::from(*c)))
        .collect::<serde_json::Map<_, _>>()
        .into();
    let fns: Vec<serde_json::Value> = report
        .fns
        .iter()
        .map(|f| {
            json!({
                "name": f.name,
                "primary": f.primary,
                "labels": f.labels,
            })
        })
        .collect();
    json!({
        "module": report.module,
        "file": report.file,
        "facts": {
            "fn_count": report.fns.len(),
            "has_main": report.has_main,
            "exposes_opaque": report.exposes_opaque,
            "depends": report.depends,
            "effects": report.effects,
        },
        "vector": {
            "purity": report.shape.purity.as_str(),
            "entry": report.shape.entry.as_str(),
            "state_shape": report.shape.state_shape.as_str(),
            "type_surface": report.shape.type_surface.as_str(),
            "api_shape": report.shape.api_shape.as_str(),
        },
        "kind": {
            "name": report.kind.as_str(),
            "rule": report.kind.rule(),
        },
        "verification": {
            "level": report.verify.level.as_str(),
            "blocks": report.verify.blocks,
            "covered_fns": report.verify.covered_fns,
            "eligible_fns": report.verify.eligible_fns,
        },
        "histogram": {
            "counts": histogram_counts,
            "total_fns": report.histogram.total_fns,
        },
        "layer": layer,
        "fns": fns,
    })
}
