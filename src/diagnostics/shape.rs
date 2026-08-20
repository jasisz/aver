//! `aver shape` — module-shape presentation layer.
//!
//! Stage 1 of issue #232 (0.23 "Shape") split this file into two
//! tiers:
//!
//! - **Recognition** (`aver::analysis::shape`) — per-fn facts walk,
//!   archetype classifier, call-graph SCC. Stable typed primitives.
//! - **Presentation** (this module) — `ModuleShape` vector + `Kind` +
//!   verify report + histogram + Layer fingerprint distance + corpus
//!   walker + renderer + `aver.toml [[shape.layer]]` / `[[shape.expected]]`
//!   bridges. Everything the CLI / LSP / JSON consumers see.
//!
//! The recognition primitives are re-exported below so existing
//! callers (and the research test) keep working through this module
//! during the transition; downstream stages will migrate them to
//! import directly from `aver::analysis::shape`.

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::Path;

use crate::ast::TopLevel;
use crate::ir::hir::{ResolvedFnDef, ResolvedTopLevel};
use crate::ir::{PipelineConfig, TypecheckMode};
use crate::types::Type;

/// ANSI styling shim for the `render_text` renderer. With
/// `tty-render` enabled (the default for the CLI / LSP / playground
/// builds), `Colorize` is re-exported from the `colored` crate so
/// calls like `"X".bold().cyan()` produce the actual escape
/// sequences. Without `tty-render` (the fuzz crates and any
/// future `--no-default-features` consumer), the trait still
/// resolves but every method is a no-op that returns the input as
/// `String` — the renderer keeps working, just emits plain text.
#[cfg(feature = "tty-render")]
use colored::Colorize as _shape_colorize;
#[cfg(not(feature = "tty-render"))]
use shape_color::Colorize as _shape_colorize;

#[cfg(not(feature = "tty-render"))]
mod shape_color {
    /// No-op stand-in for `colored::Colorize` when the `tty-render`
    /// feature is off. Every method returns the input as `String`,
    /// so `"X".bold().cyan()` chains the same way it does with the
    /// real crate — just without escapes.
    pub trait Colorize {
        fn bold(&self) -> String;
        fn cyan(&self) -> String;
        fn yellow(&self) -> String;
        fn magenta(&self) -> String;
        fn dimmed(&self) -> String;
    }
    impl<S: AsRef<str> + ?Sized> Colorize for S {
        fn bold(&self) -> String {
            self.as_ref().to_string()
        }
        fn cyan(&self) -> String {
            self.as_ref().to_string()
        }
        fn yellow(&self) -> String {
            self.as_ref().to_string()
        }
        fn magenta(&self) -> String {
            self.as_ref().to_string()
        }
        fn dimmed(&self) -> String {
            self.as_ref().to_string()
        }
    }
}

// Recognition primitives moved to `aver::analysis::shape` in stage 1
// (#232). Stage 2 drops the re-exports — callers import from
// `analysis::shape` directly so the presentation tier doesn't double
// as an API entry point for the recognition tier. This file holds
// only the presentation surface from here on: `ModuleShape` vector +
// `Kind` + Layer fingerprints + verify report + histogram + corpus
// walker + renderers + `aver.toml` bridges. Internal use of the
// recognition API in this file goes through `crate::analysis::shape`
// directly (see imports below).
use crate::analysis::shape::{Archetype, ModulePattern, analyze_program_with_modules};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

/// Threshold for "this module is genuinely effectful": at least this
/// fraction of its non-main fns must declare effects in their `! [...]`.
/// Below the threshold, a `main` that prints `Console.print("done")`
/// after running pure code shouldn't drag the module into `Orchestration`.
const EFFECTFUL_FN_RATIO_FOR_ORCHESTRATION: f64 = 0.3;

pub fn derive_kind(shape: &ModuleShape, effectful_fn_ratio: f64) -> Kind {
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
            // "Has main + has effects" used to unconditionally pick
            // Orchestration. That dragged pure algorithm modules with
            // a demo `main(); Console.print(result)` (quicksort,
            // calculator, …) into Orchestration even though only one
            // fn out of N actually touches an effect. Now we only
            // commit to Orchestration when ≥30% of non-main fns are
            // effectful — below that the module is library-shaped
            // with a demo entry, and falls through to the api-shape
            // and pure paths below.
            if effectful_fn_ratio >= EFFECTFUL_FN_RATIO_FOR_ORCHESTRATION {
                return Kind::Orchestration;
            }
            // Fall through: treat as library-with-demo by api/purity rules.
        }
        if matches!(shape.api_shape, ApiShape::ExposedLibrary) {
            return Kind::Library;
        }
        if matches!(shape.purity, Purity::ShellEffectful) {
            return Kind::EffectfulShell;
        }
        // Pure-dominated module with a demo main and no exposes —
        // ergonomically the same as PureHelpers, even though one fn
        // has effects.
        if matches!(shape.entry, Entry::Main)
            && effectful_fn_ratio < EFFECTFUL_FN_RATIO_FOR_ORCHESTRATION
        {
            return Kind::PureHelpers;
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
    pub counts: BTreeMap<Archetype, usize>,
    pub total_fns: usize,
}

impl Histogram {
    pub fn percentage(&self, archetype: Archetype) -> f64 {
        if self.total_fns == 0 {
            return 0.0;
        }
        let c = self.counts.get(&archetype).copied().unwrap_or(0) as f64;
        100.0 * c / self.total_fns as f64
    }

    /// Returns archetype-percentage pairs in descending count order.
    pub fn sorted(&self) -> Vec<(Archetype, usize, f64)> {
        let mut entries: Vec<(Archetype, usize)> = self
            .counts
            .iter()
            .filter(|(_, c)| **c > 0)
            .map(|(k, c)| (*k, *c))
            .collect();
        entries.sort_by(|a, b| b.1.cmp(&a.1).then_with(|| a.0.as_str().cmp(b.0.as_str())));
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

pub fn archetype_to_bucket(archetype: Archetype) -> Option<Bucket> {
    match archetype {
        Archetype::MatchDispatcher | Archetype::MatchOnValue => Some(Bucket::Match),
        Archetype::SccMutual | Archetype::StructuralRecursion => Some(Bucket::Recursion),
        Archetype::PipelineResult | Archetype::LetPipeline | Archetype::ManualResultAdapter => {
            Some(Bucket::Pipeline)
        }
        Archetype::Orchestration | Archetype::EffectfulLeaf => Some(Bucket::Orchestration),
        Archetype::TrivialHelper
        | Archetype::PureExpression
        | Archetype::ConstructorWrapper
        | Archetype::DataAsFunction
        | Archetype::RendererFormatter => Some(Bucket::Helpers),
        Archetype::Unclassified => None,
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
/// Translate `aver.toml [[shape.layer]]` entries into the runtime
/// fingerprint representation. Returns an error if `name` doesn't match
/// any known `Layer` variant. Unknown layers are deliberately rejected
/// here, not silently dropped — typo-detection is cheap and a silently
/// missing fingerprint would change the nearest-neighbor result.
pub fn fingerprints_from_config(
    entries: &[crate::config::ShapeLayerFingerprint],
) -> Result<Vec<LayerFingerprint>, String> {
    use Bucket::*;
    entries
        .iter()
        .map(|e| {
            let layer = Layer::parse(&e.name).ok_or_else(|| {
                format!(
                    "aver.toml: [[shape.layer]] name '{}' is not a known Layer (expected one of: Domain, Parse, Command, AiStrategy, RenderUi, Infra)",
                    e.name
                )
            })?;
            Ok(LayerFingerprint {
                layer,
                buckets: [
                    (Match, e.match_pct),
                    (Recursion, e.recursion_pct),
                    (Pipeline, e.pipeline_pct),
                    (Orchestration, e.orchestration_pct),
                    (Helpers, e.helpers_pct),
                ],
            })
        })
        .collect()
}

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
        if let Some(b) = archetype_to_bucket(*arch) {
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
    /// Absolute fit: how close `layer` is to the observed histogram,
    /// normalised against the max possible Euclidean distance in the
    /// 5-bucket simplex. Penalised on small modules (<5 fns capped at
    /// 0.2, <10 fns softened by 0.7×).
    pub confidence: f64,
    /// Distance gap between the chosen layer and the runner-up.
    /// High margin = clear winner. Low margin = classifier is hesitating
    /// between multiple fingerprints — read with care.
    pub margin: f64,
    /// True when the verdict shouldn't be read as a hard label —
    /// either confidence is low OR margin to the runner-up is small.
    /// Renderers should surface this with explicit "uncertain" wording.
    pub uncertain: bool,
    /// Top three nearest layers with their raw distances. Lets the
    /// caller show "next: Domain Δ4.1, RenderUi Δ8.3" so the verdict
    /// is auditable without pretending the second-best didn't exist.
    pub candidates: Vec<(Layer, f64)>,
    pub basis: String,
    pub support_fns: usize,
}

/// Confidence threshold below which a verdict is marked `uncertain`.
const UNCERTAIN_CONFIDENCE_THRESHOLD: f64 = 0.4;
/// Margin (distance gap to runner-up) below which a verdict is marked
/// `uncertain` even at high confidence — fingerprints are close to
/// each other and the classifier could plausibly pick either.
const UNCERTAIN_MARGIN_THRESHOLD: f64 = 10.0;

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
    // Margin = how far ahead the winner is from the second-best.
    // Captures ambivalence the confidence number alone misses
    // (two fingerprints both 0.2 fit is ambivalent; one at 0.5 + one
    // at 0.1 is decisive even if confidence is the same).
    let margin = if scored.len() >= 2 {
        scored[1].1 - scored[0].1
    } else {
        f64::INFINITY
    };
    let uncertain =
        small_n_penalty < UNCERTAIN_CONFIDENCE_THRESHOLD || margin < UNCERTAIN_MARGIN_THRESHOLD;
    let candidates: Vec<(Layer, f64)> = scored.into_iter().take(3).collect();
    Some(LayerVerdict {
        layer: best_layer,
        confidence: small_n_penalty,
        margin,
        uncertain,
        candidates,
        basis: basis.to_string(),
        support_fns: n,
    })
}

// ─── Top-level analysis ──────────────────────────────────────────────────────

#[derive(Debug, Clone)]
pub struct FnShape {
    pub name: String,
    pub primary: Archetype,
    pub labels: Vec<Archetype>,
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
    /// Fraction of non-`main` fns that declare effects in their
    /// `! [...]`. Drives the Orchestration vs PureHelpers decision in
    /// `derive_kind` so a pure module with a demo `main()` doesn't
    /// read as Orchestration.
    pub effectful_fn_ratio: f64,
    pub verify: VerifyReport,
    pub histogram: Histogram,
    pub layer: Option<LayerVerdict>,
    pub fns: Vec<FnShape>,
    /// Module-level typed patterns recognized by `analysis::shape`
    /// (stage 6 of #232). Populated from `program_shape.patterns`;
    /// the renderer surfaces them in their own section so callers see
    /// every refinement / wrapper / pipeline / renderer / fold the
    /// substrate identified.
    pub patterns: Vec<ModulePattern>,
}

pub fn analyze_path(path: &Path, module_root_hint: Option<&str>) -> Result<ShapeReport, String> {
    analyze_path_with(
        path,
        module_root_hint,
        &builtin_v0_layer_fingerprints(),
        "built-in v0",
    )
}

/// Same as `analyze_path` but lets the caller supply a custom layer
/// fingerprint table + basis label (used by the CLI when `aver.toml` has
/// `[[shape.layer]]` overrides).
pub fn analyze_path_with(
    path: &Path,
    module_root_hint: Option<&str>,
    fingerprints: &[LayerFingerprint],
    basis: &str,
) -> Result<ShapeReport, String> {
    let source = std::fs::read_to_string(path).map_err(|e| format!("read: {}", e))?;
    let module_root = match module_root_hint {
        Some(r) => r.to_string(),
        None => path
            .parent()
            .and_then(|p| p.to_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| ".".to_string()),
    };
    let file_label = path.to_string_lossy().to_string();
    let fallback_module_name = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("<unnamed>")
        .to_string();
    analyze_source_with(
        &source,
        &module_root,
        &file_label,
        &fallback_module_name,
        fingerprints,
        basis,
    )
}

/// Analyze a source string directly, without touching the filesystem
/// for the entry file. Used by the LSP (editor buffer may differ from
/// on-disk content) and any future embedder that holds the program in
/// memory. `module_root` resolves `depends [...]`; `file_label` is the
/// path string surfaced in the report's `file` field; `fallback_module_name`
/// is used when the source has no `module` declaration.
pub fn analyze_source_with(
    source: &str,
    module_root: &str,
    file_label: &str,
    fallback_module_name: &str,
    fingerprints: &[LayerFingerprint],
    basis: &str,
) -> Result<ShapeReport, String> {
    let mut items = crate::source::parse_source(source).map_err(|e| format!("parse: {}", e))?;
    let module_root = module_root.to_string();

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
            fallback_module_name.to_string(),
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

    // Stage 4/6 of #232: recognition substrate.
    // `analyze_program_with_modules` runs facts walk + SCC + classify
    // and also populates `patterns` (module-level typed patterns:
    // `RefinementSmartConstructor`, `WrapperOverRecursion`,
    // `ResultPipelineChain`, `RendererFormatter`,
    // `MatchDispatcherFold`). Presentation tier reads both —
    // per-fn archetypes drive the histogram + Kind / Layer
    // scaffolding; patterns get their own section in the renderer.
    let program_shape = analyze_program_with_modules(&resolved_fns, &items, &dep_modules);

    let exposes_set: HashSet<&str> = exposes.iter().map(|s| s.as_str()).collect();
    let mut fn_shapes = Vec::with_capacity(resolved_fns.len());
    let mut histogram = Histogram::default();
    let mut has_main = false;
    let mut classified_uses_handle = false;
    let mut exposed_uses_handle = false;
    let mut non_main_fns = 0usize;
    let mut effectful_non_main_fns = 0usize;

    for fd in &resolved_fns {
        if fd.name == "main" {
            has_main = true;
        } else {
            non_main_fns += 1;
            if !fd.effects.is_empty() {
                effectful_non_main_fns += 1;
            }
        }
        let this_uses_handle = uses_runtime_handle(fd);
        if this_uses_handle {
            classified_uses_handle = true;
            if exposes_set.contains(fd.name.as_str()) {
                exposed_uses_handle = true;
            }
        }
        let recognition = program_shape
            .for_fn(fd.fn_id)
            .expect("analyze_program populates per_fn for every resolved fn");
        let primary = recognition.primary;
        let labels = recognition.labels.clone();
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

    let effectful_fn_ratio = if non_main_fns > 0 {
        effectful_non_main_fns as f64 / non_main_fns as f64
    } else {
        // No non-main fns to classify — `derive_kind` falls through to
        // the pure/api-shape paths anyway, so the value here is moot.
        0.0
    };

    let verify = compute_verify_report(&items, &resolved_fns);

    let shape = derive_shape(
        &effects,
        has_main,
        &exposes_opaque,
        classified_uses_handle,
        exposed_uses_handle,
        &exposes,
    );
    let kind = derive_kind(&shape, effectful_fn_ratio);
    let layer = classify_layer(&histogram, fingerprints, basis);

    Ok(ShapeReport {
        module: module_name,
        file: file_label.to_string(),
        depends,
        effects,
        exposes_opaque,
        has_main,
        shape,
        kind,
        effectful_fn_ratio,
        verify,
        histogram,
        layer,
        fns: fn_shapes,
        patterns: program_shape.patterns,
    })
}

fn uses_runtime_handle(fd: &ResolvedFnDef) -> bool {
    fd.params.iter().any(|(_, ty)| is_runtime_handle_type(ty))
        || is_runtime_handle_type(&fd.return_type)
}

fn is_runtime_handle_type(t: &Type) -> bool {
    walk_type_named(t, &|name| {
        crate::stdlib::standard_capability_registry_ref()
            .resource_types()
            .any(|resource| resource == name)
    })
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

/// One human-readable line for a single `ModulePattern`. Format:
/// `<VariantName>  <key facts>` with the variant on the left so a
/// `grep RefinementSmartConstructor` over the corpus output works.
/// Scope-qualified names are rendered as `prefix::name` when the
/// pattern lives in a dep module.
fn render_module_pattern_line(p: &ModulePattern) -> String {
    // `_shape_colorize` is in module scope above — its methods
    // (`bold`, `cyan`, …) are usable directly without a local import.
    let scoped = |scope: &Option<String>, name: &str| -> String {
        match scope {
            Some(prefix) => format!("{prefix}::{name}"),
            None => name.to_string(),
        }
    };
    match p {
        ModulePattern::RefinementSmartConstructor {
            scope,
            type_name,
            carrier_field,
            carrier_type,
            constructor_fn,
            ..
        } => format!(
            "{}  {}({}: {})  via {}",
            "RefinementSmartConstructor".magenta().bold(),
            scoped(scope, type_name).bold(),
            carrier_field,
            carrier_type.cyan(),
            scoped(scope, constructor_fn).bold(),
        ),
        ModulePattern::WrapperOverRecursion {
            wrapper_scope,
            wrapper_fn,
            inner_scope,
            inner_fn,
        } => format!(
            "{}        {} → {}",
            "WrapperOverRecursion".magenta().bold(),
            scoped(wrapper_scope, wrapper_fn).bold(),
            scoped(inner_scope, inner_fn).bold(),
        ),
        ModulePattern::ResultPipelineChain {
            scope,
            fn_name,
            step_count,
            ..
        } => format!(
            "{}         {}  ({} steps)",
            "ResultPipelineChain".magenta().bold(),
            scoped(scope, fn_name).bold(),
            step_count.to_string().yellow(),
        ),
        ModulePattern::RendererFormatter { scope, fn_name } => format!(
            "{}           {}",
            "RendererFormatter".magenta().bold(),
            scoped(scope, fn_name).bold(),
        ),
        ModulePattern::MatchDispatcherFold {
            scope,
            fn_name,
            list_param,
        } => format!(
            "{}         {}  (over {})",
            "MatchDispatcherFold".magenta().bold(),
            scoped(scope, fn_name).bold(),
            list_param.cyan(),
        ),
        ModulePattern::AccumulatorFold {
            scope,
            wrapper_fn,
            loop_fn,
            step_fn,
            step_op,
            finish_fn,
            ..
        } => {
            let step = step_fn
                .clone()
                .unwrap_or_else(|| step_op.map(|o| format!("{o:?}")).unwrap_or_default());
            let finish = finish_fn.clone().unwrap_or_else(|| "id".to_string());
            format!(
                "{}           {} → {}  (step {}, finish {})",
                "AccumulatorFold".magenta().bold(),
                scoped(scope, wrapper_fn).bold(),
                scoped(scope, loop_fn).bold(),
                step.cyan(),
                finish.cyan(),
            )
        }
    }
}

pub fn render_text(report: &ShapeReport, opts: &RenderOptions) -> String {
    // `_shape_colorize` is in module scope above (see top-of-file
    // shim). Auto-disables coloring on non-TTY pipes and when the
    // `tty-render` feature is off (e.g. fuzz / mutator builds).
    let mut out = String::new();
    // Header: `Module:` and `Kind:` share the value column. `Module:` is
    // 7 chars + space, `Kind:` is 5 chars + 3 spaces — same total width.
    // Colors auto-disable on non-TTY (the `colored` crate detects
    // pipes and the NO_COLOR env var), so tests + JSON consumers see
    // plain text.
    out.push_str(&format!("{}  {}\n", "Module:".bold(), report.module.bold()));
    out.push_str(&format!(
        "{}    {}\n\n",
        "Kind:".bold(),
        report.kind.as_str().cyan().bold()
    ));
    out.push_str(&format!("{}\n", "ModuleShape:".bold()));
    out.push_str(&format!(
        "  purity        {}\n",
        report.shape.purity.as_str()
    ));
    out.push_str(&format!(
        "  entry         {}\n",
        report.shape.entry.as_str()
    ));
    out.push_str(&format!(
        "  state_shape   {}\n",
        report.shape.state_shape.as_str()
    ));
    out.push_str(&format!(
        "  type_surface  {}\n",
        report.shape.type_surface.as_str()
    ));
    out.push_str(&format!(
        "  api_shape     {}\n\n",
        report.shape.api_shape.as_str()
    ));

    // Verification: collapse the zero-block case to a single line — the
    // "0 (no) blocks, 0/0 fns covered" reading was confusing.
    if report.verify.blocks == 0 {
        out.push_str(&format!("{}  no verify blocks\n\n", "Verification:".bold()));
    } else {
        out.push_str(&format!(
            "{}  {} {} block{}, {}/{} fns covered\n\n",
            "Verification:".bold(),
            report.verify.blocks.to_string().yellow(),
            match report.verify.level {
                VerifyLevel::None => "(no)",
                VerifyLevel::Cases => "cases",
                VerifyLevel::Laws => "law",
                VerifyLevel::Trace => "trace",
                VerifyLevel::Mixed => "mixed",
            },
            if report.verify.blocks == 1 { "" } else { "s" },
            report.verify.covered_fns.to_string().yellow(),
            report.verify.eligible_fns.to_string().yellow(),
        ));
    }

    if report.histogram.total_fns == 0 {
        out.push_str(&format!(
            "{}     no classifiable fns (module only has `main` or no fns)\n",
            "Histogram:".bold()
        ));
    } else {
        out.push_str(&format!(
            "{} ({} fns):\n",
            "Histogram".bold(),
            report.histogram.total_fns.to_string().yellow()
        ));
        let sorted = report.histogram.sorted();
        let name_w = sorted
            .iter()
            .map(|(n, _, _)| n.as_str().len())
            .max()
            .unwrap_or(0)
            .max(20);
        for (name, count, pct) in &sorted {
            let bar = histogram_bar(*pct, 20);
            out.push_str(&format!(
                "  {:<width$}  {}  {:>3}  ({})\n",
                name.as_str(),
                bar.cyan(),
                format!("{:.0}%", pct).yellow(),
                count.to_string().dimmed(),
                width = name_w,
            ));
        }
    }
    if let Some(verdict) = &report.layer {
        if verdict.uncertain {
            out.push_str(&format!(
                "\n{} {}  (best: {}, confidence {}, margin Δ{}, basis: {})\n",
                "Layer:".bold(),
                "uncertain".yellow().bold(),
                verdict.layer.as_str().cyan().bold(),
                format!("{:.2}", verdict.confidence).yellow(),
                format!("{:.1}", verdict.margin).yellow(),
                verdict.basis.dimmed(),
            ));
        } else {
            out.push_str(&format!(
                "\n{} {}  (confidence {}, margin Δ{}, basis: {})\n",
                "Layer:".bold(),
                verdict.layer.as_str().cyan().bold(),
                format!("{:.2}", verdict.confidence).yellow(),
                format!("{:.1}", verdict.margin).yellow(),
                verdict.basis.dimmed(),
            ));
        }
        // Runners-up (top 3 minus best). Hides nothing — even when
        // confidence is high the user sees how far the alternatives
        // are. "Δ" is the raw Euclidean distance in the 5-bucket %
        // simplex; higher = further from observed histogram.
        let runners: Vec<String> = verdict
            .candidates
            .iter()
            .skip(1)
            .map(|(layer, dist)| format!("{} Δ{:.1}", layer.as_str(), dist))
            .collect();
        if !runners.is_empty() {
            out.push_str(&format!(
                "       {} {}\n",
                "next:".dimmed(),
                runners.join(", ").dimmed()
            ));
        }
    } else {
        out.push_str(&format!("\n{} insufficient data\n", "Layer:".bold()));
    }

    if !report.patterns.is_empty() {
        out.push_str(&format!("\n{}\n", "Module patterns:".bold()));
        for p in &report.patterns {
            out.push_str("  ");
            out.push_str(&render_module_pattern_line(p));
            out.push('\n');
        }
    }

    if !opts.summary {
        out.push_str(&format!("\n{}\n", "Functions:".bold()));
        let name_w = report
            .fns
            .iter()
            .map(|f| f.name.len())
            .max()
            .unwrap_or(0)
            .max(16);
        for f in &report.fns {
            let labels_str = if f.labels.is_empty() {
                "unclassified".to_string()
            } else {
                f.labels
                    .iter()
                    .map(|a| a.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            out.push_str(&format!(
                "  {:<width$}  {}\n",
                f.name,
                labels_str.dimmed(),
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
        let candidates: Vec<serde_json::Value> = v
            .candidates
            .iter()
            .map(|(layer, dist)| json!({"name": layer.as_str(), "distance": dist}))
            .collect();
        json!({
            "name": v.layer.as_str(),
            "confidence": v.confidence,
            "margin": v.margin,
            "uncertain": v.uncertain,
            "candidates": candidates,
            "basis": v.basis,
            "support_fns": v.support_fns,
        })
    });
    let histogram_counts: serde_json::Value = report
        .histogram
        .counts
        .iter()
        .filter(|(_, c)| **c > 0)
        .map(|(k, c)| (k.as_str().to_string(), serde_json::Value::from(*c)))
        .collect::<serde_json::Map<_, _>>()
        .into();
    let fns: Vec<serde_json::Value> = report
        .fns
        .iter()
        .map(|f| {
            let labels: Vec<&str> = f.labels.iter().map(|a| a.as_str()).collect();
            json!({
                "name": f.name,
                "primary": f.primary.as_str(),
                "labels": labels,
            })
        })
        .collect();
    let patterns: Vec<serde_json::Value> =
        report.patterns.iter().map(module_pattern_to_json).collect();
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
        "patterns": patterns,
    })
}

/// JSON encoding of one [`ModulePattern`]. Each variant becomes an
/// object with a `kind` tag plus the variant's typed payload, mirror
/// of the text renderer's grep-friendly format. `null` instead of
/// `None` keeps the schema explicit for downstream tooling.
fn module_pattern_to_json(p: &ModulePattern) -> serde_json::Value {
    use serde_json::json;
    let scope_json = |s: &Option<String>| match s {
        Some(prefix) => serde_json::Value::String(prefix.clone()),
        None => serde_json::Value::Null,
    };
    match p {
        ModulePattern::RefinementSmartConstructor {
            scope,
            type_name,
            carrier_field,
            carrier_type,
            constructor_fn,
            param_name,
            ..
        } => json!({
            "kind": "RefinementSmartConstructor",
            "scope": scope_json(scope),
            "type_name": type_name,
            "carrier_field": carrier_field,
            "carrier_type": carrier_type,
            "constructor_fn": constructor_fn,
            "param_name": param_name,
        }),
        ModulePattern::WrapperOverRecursion {
            wrapper_scope,
            wrapper_fn,
            inner_scope,
            inner_fn,
        } => json!({
            "kind": "WrapperOverRecursion",
            "wrapper_scope": scope_json(wrapper_scope),
            "wrapper_fn": wrapper_fn,
            "inner_scope": scope_json(inner_scope),
            "inner_fn": inner_fn,
        }),
        ModulePattern::ResultPipelineChain {
            scope,
            fn_name,
            step_count,
            step_fns,
        } => json!({
            "kind": "ResultPipelineChain",
            "scope": scope_json(scope),
            "fn_name": fn_name,
            "step_count": step_count,
            "step_fns": step_fns,
        }),
        ModulePattern::RendererFormatter { scope, fn_name } => json!({
            "kind": "RendererFormatter",
            "scope": scope_json(scope),
            "fn_name": fn_name,
        }),
        ModulePattern::MatchDispatcherFold {
            scope,
            fn_name,
            list_param,
        } => json!({
            "kind": "MatchDispatcherFold",
            "scope": scope_json(scope),
            "fn_name": fn_name,
            "list_param": list_param,
        }),
        ModulePattern::AccumulatorFold {
            scope,
            wrapper_fn,
            loop_fn,
            list_param,
            acc_param,
            step_fn,
            step_op,
            finish_fn,
            driver_type,
            step_value_first,
        } => json!({
            "kind": "AccumulatorFold",
            "scope": scope_json(scope),
            "wrapper_fn": wrapper_fn,
            "loop_fn": loop_fn,
            "list_param": list_param,
            "acc_param": acc_param,
            "step_fn": step_fn,
            "step_op": step_op.map(|o| format!("{o:?}")),
            "finish_fn": finish_fn,
            "driver_type": driver_type,
            "step_value_first": step_value_first,
        }),
    }
}

// ─── Corpus (directory) mode ────────────────────────────────────────────────

/// Outcome of analyzing a single file inside a corpus walk. Files that
/// fail typecheck or parse get the error attached instead of being
/// silently skipped — the corpus view should show what's broken.
///
/// `report` is boxed because `ShapeReport` carries the full per-fn
/// listing — a large size delta between the two variants would inflate
/// every `Vec<CorpusEntry>` with padding (clippy's `large_enum_variant`).
#[derive(Debug, Clone)]
pub enum CorpusEntry {
    Analyzed {
        rel_path: String,
        report: Box<ShapeReport>,
    },
    Skipped {
        rel_path: String,
        reason: String,
    },
}

/// Walk a directory recursively and analyze every `.av` file.
/// `root` is also used as the default `module_root` for dep resolution
/// — callers that want a different module root can pass it explicitly.
pub fn analyze_dir(
    root: &Path,
    module_root_hint: Option<&str>,
    fingerprints: &[LayerFingerprint],
    basis: &str,
) -> Result<Vec<CorpusEntry>, String> {
    let mut files = Vec::new();
    collect_av_files(root, &mut files)?;
    files.sort();
    let effective_module_root = match module_root_hint {
        Some(r) => r.to_string(),
        None => root.to_string_lossy().to_string(),
    };
    let mut entries = Vec::with_capacity(files.len());
    for path in files {
        let rel = path
            .strip_prefix(root)
            .unwrap_or(&path)
            .to_string_lossy()
            .to_string();
        match analyze_path_with(&path, Some(&effective_module_root), fingerprints, basis) {
            Ok(report) => entries.push(CorpusEntry::Analyzed {
                rel_path: rel,
                report: Box::new(report),
            }),
            Err(reason) => entries.push(CorpusEntry::Skipped {
                rel_path: rel,
                reason,
            }),
        }
    }
    Ok(entries)
}

fn collect_av_files(path: &Path, out: &mut Vec<std::path::PathBuf>) -> Result<(), String> {
    if path.is_file() {
        if path.extension().and_then(|s| s.to_str()) == Some("av") {
            out.push(path.to_path_buf());
        }
        return Ok(());
    }
    let entries = std::fs::read_dir(path).map_err(|e| {
        format!(
            "aver shape: cannot read directory '{}': {}",
            path.display(),
            e
        )
    })?;
    for entry in entries {
        let entry = entry
            .map_err(|e| format!("aver shape: read_dir error in '{}': {}", path.display(), e))?;
        let name = entry.file_name();
        let name_str = name.to_string_lossy();
        // Skip hidden + common build/dep directories.
        if name_str.starts_with('.')
            || name_str == "target"
            || name_str == "node_modules"
            || name_str == "pkg"
        {
            continue;
        }
        let p = entry.path();
        if p.is_dir() {
            collect_av_files(&p, out)?;
        } else if p.extension().and_then(|s| s.to_str()) == Some("av") {
            out.push(p);
        }
    }
    Ok(())
}

/// Per-Kind / per-Layer aggregate over a corpus.
#[derive(Debug, Clone, Default)]
pub struct CorpusSummary {
    pub total_files: usize,
    pub analyzed_files: usize,
    pub skipped_files: usize,
    pub total_fns: usize,
    pub kind_counts: BTreeMap<Kind, usize>,
    pub layer_counts: BTreeMap<Layer, usize>,
    pub archetype_counts: BTreeMap<Archetype, usize>,
}

pub fn summarize_corpus(entries: &[CorpusEntry]) -> CorpusSummary {
    let mut s = CorpusSummary {
        total_files: entries.len(),
        ..Default::default()
    };
    for e in entries {
        match e {
            CorpusEntry::Analyzed { report, .. } => {
                s.analyzed_files += 1;
                s.total_fns += report.histogram.total_fns;
                *s.kind_counts.entry(report.kind).or_insert(0) += 1;
                if let Some(v) = &report.layer {
                    *s.layer_counts.entry(v.layer).or_insert(0) += 1;
                }
                for (arch, c) in &report.histogram.counts {
                    *s.archetype_counts.entry(*arch).or_insert(0) += c;
                }
            }
            CorpusEntry::Skipped { .. } => {
                s.skipped_files += 1;
            }
        }
    }
    s
}

pub fn render_corpus_text(entries: &[CorpusEntry], opts: &RenderOptions) -> String {
    let mut out = String::new();
    let summary = summarize_corpus(entries);

    if !opts.summary {
        out.push_str(&format!(
            "Corpus: {} files ({} analyzed, {} skipped)\n\n",
            summary.total_files, summary.analyzed_files, summary.skipped_files,
        ));
        let path_w = entries
            .iter()
            .map(|e| match e {
                CorpusEntry::Analyzed { rel_path, .. } | CorpusEntry::Skipped { rel_path, .. } => {
                    rel_path.len()
                }
            })
            .max()
            .unwrap_or(0)
            .max(30);
        for e in entries {
            match e {
                CorpusEntry::Analyzed { rel_path, report } => {
                    // Layer cell carries confidence + margin always; when
                    // uncertain, prefix with "uncertain — " so the user
                    // can scan a corpus column and see ambivalent verdicts
                    // without losing the best-fit name.
                    let layer = report
                        .layer
                        .as_ref()
                        .map(|v| {
                            let suffix = format!(
                                "{} (conf {:.2}, Δ{:.1})",
                                v.layer.as_str(),
                                v.confidence,
                                v.margin
                            );
                            if v.uncertain {
                                format!("uncertain — {}", suffix)
                            } else {
                                suffix
                            }
                        })
                        .unwrap_or_else(|| "—".to_string());
                    out.push_str(&format!(
                        "  {:<width$}  {:<16}  layer: {}\n",
                        rel_path,
                        report.kind.as_str(),
                        layer,
                        width = path_w,
                    ));
                }
                CorpusEntry::Skipped { rel_path, reason } => {
                    out.push_str(&format!(
                        "  {:<width$}  SKIPPED        ({})\n",
                        rel_path,
                        reason,
                        width = path_w,
                    ));
                }
            }
        }
        out.push('\n');
    }

    // Aggregate block — printed in both modes; --summary just omits the
    // per-file table above.
    out.push_str(&format!(
        "Corpus summary: {}/{} analyzed, {} fns classified\n",
        summary.analyzed_files, summary.total_files, summary.total_fns,
    ));
    if !summary.kind_counts.is_empty() {
        out.push_str("\n  Kind distribution:\n");
        let mut kinds: Vec<_> = summary.kind_counts.iter().collect();
        kinds.sort_by(|a, b| b.1.cmp(a.1));
        for (kind, count) in kinds {
            out.push_str(&format!("    {:<24}  {}\n", kind.as_str(), count));
        }
    }
    if !summary.layer_counts.is_empty() {
        out.push_str("\n  Layer distribution:\n");
        let mut layers: Vec<_> = summary.layer_counts.iter().collect();
        layers.sort_by(|a, b| b.1.cmp(a.1));
        for (layer, count) in layers {
            out.push_str(&format!("    {:<24}  {}\n", layer.as_str(), count));
        }
    }
    if !summary.archetype_counts.is_empty() {
        out.push_str("\n  Archetype distribution (across all fns):\n");
        let mut archs: Vec<_> = summary
            .archetype_counts
            .iter()
            .filter(|(_, c)| **c > 0)
            .collect();
        archs.sort_by(|a, b| b.1.cmp(a.1));
        let name_w = archs
            .iter()
            .map(|(n, _)| n.as_str().len())
            .max()
            .unwrap_or(0)
            .max(20);
        for (arch, count) in archs {
            let pct = if summary.total_fns == 0 {
                0.0
            } else {
                100.0 * *count as f64 / summary.total_fns as f64
            };
            let bar = histogram_bar(pct, 20);
            out.push_str(&format!(
                "    {:<width$}  {}  {:>3.0}%  ({})\n",
                arch.as_str(),
                bar,
                pct,
                count,
                width = name_w,
            ));
        }
    }
    // Skipped files: list per-file reason so the user sees what got
    // dropped and why. Without this the summary's "X analyzed, Y
    // skipped" line gave a count but no actionable info.
    let skipped: Vec<(&str, &str)> = entries
        .iter()
        .filter_map(|e| match e {
            CorpusEntry::Skipped { rel_path, reason } => Some((rel_path.as_str(), reason.as_str())),
            _ => None,
        })
        .collect();
    if !skipped.is_empty() {
        out.push_str(&format!("\n  Skipped ({} files):\n", skipped.len()));
        let path_w = skipped
            .iter()
            .map(|(p, _)| p.len())
            .max()
            .unwrap_or(0)
            .max(30);
        for (path, reason) in skipped {
            // Trim reason: typecheck errors can carry full module-line
            // diagnostics; one-liner is enough at corpus summary level.
            let one_line = reason.lines().next().unwrap_or(reason);
            out.push_str(&format!(
                "    {:<width$}  {}\n",
                path,
                one_line,
                width = path_w
            ));
        }
    }
    out
}

pub fn render_corpus_json(entries: &[CorpusEntry]) -> Vec<serde_json::Value> {
    use serde_json::json;
    entries
        .iter()
        .map(|e| match e {
            CorpusEntry::Analyzed { rel_path, report } => {
                let mut v = render_json(report);
                if let Some(obj) = v.as_object_mut() {
                    obj.insert("rel_path".to_string(), json!(rel_path));
                }
                v
            }
            CorpusEntry::Skipped { rel_path, reason } => json!({
                "rel_path": rel_path,
                "skipped": true,
                "reason": reason,
            }),
        })
        .collect()
}
