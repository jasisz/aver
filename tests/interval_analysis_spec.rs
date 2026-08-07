//! Integration tests for the per-module interval analysis
//! (`src/ir/interval.rs`), driven through the real pipeline on the
//! shipped `examples/refinement/*` types.
//!
//! These pin the headline behavior: a two-sided refinement
//! (`IntRange`, `[0,100]`) classifies its `add` op as `OverflowFree`
//! (the native-`i64` candidate), while single-sided refinements
//! (`Natural`/`Positive`, `[0,+inf]`/`[1,+inf]`) classify their ops
//! `Unbounded` (must stay bignum). Float / structural carriers never
//! reach the analysis at all (the refinement-lift stage upstream
//! declines them), so the analysis simply sees zero refined types for
//! those files.

use aver::codegen::ModuleInfo;
use aver::ir::interval::{Bound, IntervalAnalysisResult, OpClass};
use aver::ir::{Interval, PipelineConfig, TypecheckMode};
use aver::source::{LoadedModule, parse_source};
use std::path::PathBuf;

/// Run the proof-mode pipeline with interval analysis on and return
/// its result. Mirrors `tests/proof_ir_diff.rs::build_ctx` (rewrite
/// stages off so source-level shapes survive) but flips
/// `run_interval_analyze` on.
fn analyze_src(src: &str) -> IntervalAnalysisResult {
    let mut items = parse_source(src).expect("parse");
    let result = aver::ir::pipeline::run(
        &mut items,
        PipelineConfig {
            run_tco: true,
            typecheck: Some(TypecheckMode::Full { base_dir: None }),
            run_interp_lower: false,
            run_buffer_build: false,
            run_resolve: false,
            run_last_use: false,
            run_analyze: true,
            run_escape: false,
            run_refinement_lower: true,
            run_interval_analyze: true,
            run_contract_lower: false,
            run_law_lower: false,
            run_build_symbols: true,
            dep_modules: &[],
            alloc_policy: None,
            call_ctx: None,
            on_after_pass: None,
        },
    );
    let tc = result.typecheck.expect("typecheck requested");
    assert!(tc.errors.is_empty(), "source typechecks: {:?}", tc.errors);
    result
        .interval_analysis
        .expect("interval analysis present when run_interval_analyze=true")
}

/// Run the analysis over an entry file plus pre-loaded dep modules
/// (multi-module shape). Mirrors `tests/cross_module_type_keys.rs`.
fn analyze_multi(entry: &str, deps: &[(&str, &str)]) -> IntervalAnalysisResult {
    let mut entry_items = parse_source(entry).expect("entry parse");
    let loaded: Vec<LoadedModule> = deps
        .iter()
        .map(|(name, src)| LoadedModule {
            dep_name: name.to_string(),
            items: parse_source(src).unwrap_or_else(|e| panic!("{name} parse: {e}")),
            path: PathBuf::from(format!("{name}.av")),
        })
        .collect();
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let result = aver::ir::pipeline::run(
        &mut entry_items,
        PipelineConfig {
            typecheck: Some(TypecheckMode::WithLoaded(&loaded)),
            run_refinement_lower: true,
            run_interval_analyze: true,
            run_build_symbols: true,
            run_analyze: true,
            dep_modules: &modules,
            ..Default::default()
        },
    );
    let tc = result.typecheck.expect("typecheck requested");
    assert!(
        tc.errors.is_empty(),
        "multi-module typechecks: {:?}",
        tc.errors
    );
    result
        .interval_analysis
        .expect("interval analysis present when run_interval_analyze=true")
}

/// Look up the single refined type's analysis entry by source name.
fn only_type<'a>(r: &'a IntervalAnalysisResult, name: &str) -> &'a aver::ir::RefinedTypeInterval {
    let matches: Vec<_> = r.types.values().filter(|t| t.name == name).collect();
    assert_eq!(
        matches.len(),
        1,
        "expected exactly one `{name}` refined type, got {}",
        matches.len()
    );
    matches[0]
}

fn op_class(t: &aver::ir::RefinedTypeInterval, op: &str) -> OpClass {
    t.ops
        .iter()
        .find(|(name, _)| name == op)
        .map(|(_, c)| *c)
        .unwrap_or_else(|| panic!("op `{op}` not classified; ops = {:?}", t.ops))
}

// ── int_range: the keystone OverflowFree case ──────────────────────

#[test]
fn int_range_add_is_overflow_free() {
    let src = include_str!("../examples/refinement/int_range/int_range.av");
    let r = analyze_src(src);
    let t = only_type(&r, "IntRange");

    assert!(t.interval_known, "IntRange's [0,100] guard is recognized");
    assert_eq!(
        t.interval,
        Interval::between(0, 100),
        "Bool.and(n>=0, n<=100) → [0,100]"
    );

    // The whole point: `add` of two `[0,100]` values has intermediate
    // `[0,200]`, which fits i64, so the i64 fast path is licensed
    // (the `fromInt` guard still re-validates the [0,100] bound).
    assert_eq!(
        op_class(t, "add"),
        OpClass::OverflowFree,
        "IntRange.add intermediate [0,200] fits i64"
    );
}

// ── natural: single-sided → Unbounded ──────────────────────────────

#[test]
fn natural_ops_are_unbounded() {
    let src = include_str!("../examples/refinement/natural/natural.av");
    let r = analyze_src(src);
    let t = only_type(&r, "Natural");

    assert!(t.interval_known, "n>=0 is a recognized one-sided shape");
    assert_eq!(t.interval.lo, Bound::Finite(0));
    assert_eq!(t.interval.hi, Bound::PosInf, "Natural is [0,+inf]");

    // `[0,+inf] + [0,+inf] = [0,+inf]` — NOT overflow-free.
    assert_eq!(op_class(t, "add"), OpClass::Unbounded);
    assert_eq!(op_class(t, "mul"), OpClass::Unbounded);
}

// ── positive: n>0 → [1,+inf], ops Unbounded ────────────────────────

#[test]
fn positive_interval_and_ops() {
    let src = include_str!("../examples/refinement/positive/positive.av");
    let r = analyze_src(src);
    let t = only_type(&r, "Positive");

    assert!(t.interval_known);
    // `n > 0` strictness → lo = 1.
    assert_eq!(t.interval.lo, Bound::Finite(1), "n>0 → lo=1");
    assert_eq!(t.interval.hi, Bound::PosInf);
    assert_eq!(op_class(t, "mul"), OpClass::Unbounded);
}

// ── nonneg_float: Float carrier → declined upstream (0 types) ───────

#[test]
fn nonneg_float_yields_no_refined_types() {
    // The carrier is `Float`, which the refinement-lift stage does NOT
    // lift to a refined type (the i64 story is Int-only). So the
    // interval analysis sees zero refined types — there is nothing to
    // classify, and no spurious `OverflowFree` ever appears.
    let src = include_str!("../examples/refinement/nonneg_float/nonneg_float.av");
    let r = analyze_src(src);
    assert_eq!(
        r.types_analyzed(),
        0,
        "Float-carrier refinement is declined before the analysis sees it"
    );
}

// ── bigint: List<Int> structural carrier → declined upstream ───────

#[test]
fn bigint_yields_no_refined_types() {
    // BigInt's carrier is `List<Int>` (a structural, unbounded-width
    // type), so it is never lifted to a constant-interval refinement
    // type. The analysis sees zero refined types.
    let src = include_str!("../examples/refinement/bigint/bigint.av");
    let r = analyze_src(src);
    assert_eq!(
        r.types_analyzed(),
        0,
        "structural List carrier is declined before the analysis sees it"
    );
}

// ── aggregate counters on int_range ────────────────────────────────

#[test]
fn int_range_aggregate_counters() {
    let src = include_str!("../examples/refinement/int_range/int_range.av");
    let r = analyze_src(src);
    assert_eq!(r.types_analyzed(), 1);
    assert_eq!(r.two_sided_bounded(), 1, "IntRange is two-sided [0,100]");
    assert_eq!(r.ops_overflow_free(), 1, "only `add` is arithmetic");
    assert_eq!(r.ops_needs_wider(), 0);
    assert_eq!(r.ops_unbounded(), 0);
}

// ── raw-i64-carrier recognizer ─────────────────────────────────────
//
// The per-refined-type "raw i64 eligible" determination derived from
// the analysis facts (`RefinedTypeInterval::raw_i64_eligible` and the
// `IntervalAnalysisResult::raw_i64_eligible` aggregate). Pure diagnostic
// — nothing consumes it — but it is the gate a later carrier-lowering
// slice will trust, so a wrong `true` is a latent unsoundness.

#[test]
fn int_range_is_raw_i64_eligible() {
    // [0,100] with an overflow-free `add` → eligible.
    let src = include_str!("../examples/refinement/int_range/int_range.av");
    let r = analyze_src(src);
    let t = only_type(&r, "IntRange");
    assert!(
        t.raw_i64_eligible(),
        "IntRange [0,100], `add` OverflowFree → raw-i64-eligible"
    );
    assert_eq!(r.raw_i64_eligible(), 1, "exactly one eligible type");
}

#[test]
fn natural_is_not_raw_i64_eligible() {
    // One-sided [0,+inf] → open upper bound never fits i64 → NOT eligible.
    let src = include_str!("../examples/refinement/natural/natural.av");
    let r = analyze_src(src);
    let t = only_type(&r, "Natural");
    assert!(
        !t.raw_i64_eligible(),
        "Natural's open upper bound → NOT raw-i64-eligible"
    );
    assert_eq!(r.raw_i64_eligible(), 0);
}

#[test]
fn positive_is_not_raw_i64_eligible() {
    // [1,+inf] → open upper bound → NOT eligible.
    let src = include_str!("../examples/refinement/positive/positive.av");
    let r = analyze_src(src);
    let t = only_type(&r, "Positive");
    assert!(!t.raw_i64_eligible());
    assert_eq!(r.raw_i64_eligible(), 0);
}

#[test]
fn aggregate_eligible_matches_explain_passes_count() {
    // The aggregate the `--explain-passes` `raw_i64_eligible: N` field
    // reports is the count of per-type eligible determinations. For the
    // shipped IntRange program that is exactly 1.
    let src = include_str!("../examples/refinement/int_range/int_range.av");
    let r = analyze_src(src);
    let by_hand = r.types.values().filter(|t| t.raw_i64_eligible()).count();
    assert_eq!(r.raw_i64_eligible(), by_hand);
    assert_eq!(r.raw_i64_eligible(), 1);
}

// ── soundness regressions: no spurious OverflowFree ────────────────
//
// Three independent under-approximations once let the analysis certify
// an operation `OverflowFree` even though a real i64 intermediate
// wraps. Each module below reproduces one of them; all three must
// classify NOT `OverflowFree` (the conservative direction), while the
// legitimate `IntRange.add` shape they are built from stays
// `OverflowFree`. The malicious modules share `IntRange`'s `[0,100]`
// carrier so the only difference from the legitimate case is the
// op-body shape under test.

/// Carrier + smart constructor shared by the regression modules below:
/// the exact `IntRange` `[0,100]` refinement from `int_range.av`.
const RANGE_HEADER: &str = "\
record IntRange
    value: Int

fn fromInt(n: Int) -> Result<IntRange, String>
    ? \"Smart constructor — admits only ints in 0..=100.\"
    match Bool.and(n >= 0, n <= 100)
        true  -> Result.Ok(IntRange(value = n))
        false -> Result.Err(\"IntRange must be in 0..=100\")
";

/// Look up the single arithmetic op a regression module exposes and
/// assert it is NOT `OverflowFree` (the soundness requirement).
fn assert_not_overflow_free(src: &str, type_name: &str, op: &str) -> OpClass {
    let r = analyze_src(src);
    let t = only_type(&r, type_name);
    let class = op_class(t, op);
    assert_ne!(
        class,
        OpClass::OverflowFree,
        "`{type_name}.{op}` must NOT be certified OverflowFree (got {class:?})"
    );
    class
}

// Regression 1 — transparent-wrapper hole. A one-arg helper `widen`
// must NOT be peeled as identity: it scales the carrier far past i64,
// so the intermediate handed to `fromInt` wraps. Before the fix the
// transparent arm fired for ANY bare-ident one-arg call → OverflowFree.
#[test]
fn widen_helper_is_not_overflow_free() {
    let src = format!(
        "\
module Scaled
    exposes [fromInt, toInt, scaledAdd]
    exposes opaque [IntRange]
    intent = \"A widening helper hides an out-of-i64 intermediate.\"
    effects []

{RANGE_HEADER}
fn toInt(n: IntRange) -> Int
    ? \"Unwrap.\"
    n.value

fn widen(x: Int) -> Int
    ? \"Scales far past i64 — must not be peeled as identity.\"
    x * 100000000000000000

fn scaledAdd(a: IntRange, b: IntRange) -> Result<IntRange, String>
    ? \"widen(a+b) overflows i64 before fromInt re-validates.\"
    fromInt(widen(a.value + b.value))
"
    );
    // `widen` is opaque (not the constructor) → its argument's widening
    // is invisible, so the whole op evaluates Unbounded.
    assert_eq!(
        assert_not_overflow_free(&src, "IntRange", "scaledAdd"),
        OpClass::Unbounded
    );
}

// Regression 2 — non-tail statements ignored. An earlier binding whose
// value overflows i64 must demote the op even though the tail itself is
// the legitimate `fromInt(a + b)`. Before the fix only the tail was
// classified → OverflowFree.
#[test]
fn non_tail_overflowing_binding_is_not_overflow_free() {
    let src = format!(
        "\
module Hidden
    exposes [fromInt, toInt, hiddenOverflow]
    exposes opaque [IntRange]
    intent = \"An earlier binding overflows i64; the tail is benign.\"
    effects []

{RANGE_HEADER}
fn toInt(n: IntRange) -> Int
    ? \"Unwrap.\"
    n.value

fn hiddenOverflow(a: IntRange, b: IntRange) -> Result<IntRange, String>
    ? \"`doomed` overflows i64 before the benign tail runs.\"
    doomed = a.value * 100000000000000000
    fromInt(a.value + b.value)
"
    );
    // `doomed`'s interval is `[0,100] * 1e17 = [0, 1e19]` — finite but
    // out of i64 → NeedsWiderScratch. (If the binding were unbounded it
    // would be Unbounded; either way it is not OverflowFree.)
    let class = assert_not_overflow_free(&src, "IntRange", "hiddenOverflow");
    assert!(
        matches!(class, OpClass::NeedsWiderScratch | OpClass::Unbounded),
        "non-tail binding overflows i64 → NeedsWiderScratch/Unbounded, got {class:?}"
    );
}

// Regression 3 — intermediate intervals discarded within the tail. The
// inner `a + i64::MAX` overflows even though the outer `- i64::MAX`
// cancels it back into `[0,100]`. This uses the GENUINE `fromInt`, so
// the constructor gate (regression 1's fix) does not catch it; the wide
// value is a true intermediate, so only joining intermediate intervals
// (regression 2's fix walking inside the tail) demotes it.
#[test]
fn cancelling_intermediate_is_not_overflow_free() {
    let src = format!(
        "\
module Cancel
    exposes [fromInt, toInt, addBig]
    exposes opaque [IntRange]
    intent = \"An inner term overflows i64, then cancels in the tail.\"
    effects []

{RANGE_HEADER}
fn toInt(n: IntRange) -> Int
    ? \"Unwrap.\"
    n.value

fn addBig(a: IntRange) -> Result<IntRange, String>
    ? \"(a + i64::MAX) - i64::MAX cancels, but the inner add wraps.\"
    fromInt((a.value + 9223372036854775807) - 9223372036854775807)
"
    );
    // The inner `[i64::MAX, i64::MAX+100]` is finite but out of i64 →
    // NeedsWiderScratch (or Unbounded); never OverflowFree.
    let class = assert_not_overflow_free(&src, "IntRange", "addBig");
    assert!(
        matches!(class, OpClass::NeedsWiderScratch | OpClass::Unbounded),
        "inner add is finite-out-of-i64 → NeedsWiderScratch/Unbounded, got {class:?}"
    );
}

// Anti-regression — the legitimate case the whole analysis exists for
// must stay OverflowFree even under the worst-intermediate walk: the
// only intermediate is `[0,200]`, which fits i64. Phrased exactly like
// the malicious modules above (same header, real `fromInt`) so it
// isolates the op-body shape as the sole difference.
#[test]
fn legitimate_add_stays_overflow_free_under_worst_intermediate() {
    let src = format!(
        "\
module Legit
    exposes [fromInt, toInt, add]
    exposes opaque [IntRange]
    intent = \"The keystone OverflowFree case, in regression form.\"
    effects []

{RANGE_HEADER}
fn toInt(n: IntRange) -> Int
    ? \"Unwrap.\"
    n.value

fn add(a: IntRange, b: IntRange) -> Result<IntRange, String>
    ? \"Sum of two [0,100] values — intermediate [0,200] fits i64.\"
    fromInt(a.value + b.value)
"
    );
    let r = analyze_src(&src);
    let t = only_type(&r, "IntRange");
    assert_eq!(
        op_class(t, "add"),
        OpClass::OverflowFree,
        "intermediate [0,200] fits i64 — must stay OverflowFree"
    );
}

// ── cross-module distinctness: same name, different bounds ─────────

#[test]
fn cross_module_same_named_types_get_distinct_intervals() {
    // Two dep modules each declare a refined `Natural` with the SAME
    // bare name but DIFFERENT bounds (`n>=0` vs `n>=10`). The analysis
    // is keyed by opaque `TypeId`, so the two must land in distinct
    // entries with distinct intervals — nothing merges by bare name.
    // This is the same shape as
    // `tests/proof_spec/export_structure.rs`'s AAA/BBB Natural test,
    // driven in-memory through the pipeline.
    const ENTRY: &str = "\
module Entry
    depends [AAA, BBB]
    intent = \"Touches both Naturals so both surface in the analysis.\"

fn main() -> Int
    0
";
    const AAA: &str = "\
module AAA
    exposes [fromInt]
    exposes opaque [Natural]
    intent = \"Module AAA's Natural — non-negative.\"
    effects []

record Natural
    value: Int

fn fromInt(n: Int) -> Result<Natural, String>
    ? \"Smart constructor — non-negative.\"
    match n >= 0
        true  -> Result.Ok(Natural(value = n))
        false -> Result.Err(\"AAA: must be non-negative\")
";
    const BBB: &str = "\
module BBB
    exposes [fromInt]
    exposes opaque [Natural]
    intent = \"Module BBB's Natural — at least 10.\"
    effects []

record Natural
    value: Int

fn fromInt(n: Int) -> Result<Natural, String>
    ? \"Smart constructor — at least 10.\"
    match n >= 10
        true  -> Result.Ok(Natural(value = n))
        false -> Result.Err(\"BBB: must be >= 10\")
";
    let r = analyze_multi(ENTRY, &[("AAA", AAA), ("BBB", BBB)]);
    assert_eq!(
        r.types_analyzed(),
        2,
        "both modules' Natural surface as distinct refined types"
    );

    // Two entries named `Natural`, distinct TypeIds, distinct lower
    // bounds — proving the opaque-TypeId keying carries through the
    // interval map and nothing merges by bare name.
    let naturals: Vec<_> = r.types.values().filter(|t| t.name == "Natural").collect();
    assert_eq!(naturals.len(), 2, "two distinct `Natural` entries");
    assert_ne!(
        naturals[0].type_id, naturals[1].type_id,
        "distinct opaque TypeIds"
    );
    let mut los: Vec<Bound> = naturals.iter().map(|t| t.interval.lo).collect();
    los.sort_by_key(|b| match b {
        Bound::Finite(k) => *k,
        Bound::NegInf => i128::MIN,
        Bound::PosInf => i128::MAX,
    });
    assert_eq!(
        los,
        vec![Bound::Finite(0), Bound::Finite(10)],
        "AAA.Natural is [0,+inf], BBB.Natural is [10,+inf] — not merged"
    );
}

// ── carrier table: bare-name twins must fail closed ────────────────

/// Build `ProofLowerInputs` over an entry file plus dep modules and
/// return the carrier-`i64` interval table.
fn carrier_table(
    entry: &str,
    deps: &[(&str, &str)],
) -> std::collections::HashMap<String, (Interval, bool)> {
    let entry_items = parse_source(entry).expect("entry parse");
    let loaded: Vec<LoadedModule> = deps
        .iter()
        .map(|(name, src)| LoadedModule {
            dep_name: name.to_string(),
            items: parse_source(src).unwrap_or_else(|e| panic!("{name} parse: {e}")),
            path: PathBuf::from(format!("{name}.av")),
        })
        .collect();
    let modules: Vec<ModuleInfo> = loaded.iter().map(ModuleInfo::from_loaded).collect();
    let symbols = aver::ir::SymbolTable::build(&entry_items, &modules);
    let prefixes: std::collections::HashSet<String> =
        modules.iter().map(|m| m.prefix.clone()).collect();
    let recursive: std::collections::HashSet<aver::ir::FnId> = std::collections::HashSet::new();
    let inputs = aver::codegen::proof_lower::ProofLowerInputs {
        entry_items: &entry_items,
        dep_modules: &modules,
        module_prefixes: &prefixes,
        recursive_fns: &recursive,
        symbol_table: &symbols,
        program_shape: None,
    };
    aver::codegen::proof_lower::carrier_interval_table(&inputs)
}

const TWIN_ENTRY: &str = "\
module Entry
    depends [AAA, BBB]
    intent = \"Touches both Naturals so both are in scope.\"

fn main() -> Int
    0
";

/// A module declaring a refined `Natural` whose smart-ctor guard is
/// the given match subject expression.
fn natural_module(label: &str, guard: &str) -> String {
    format!(
        "\
module {label}
    exposes [fromInt]
    exposes opaque [Natural]
    intent = \"Module {label}'s Natural.\"
    effects []

record Natural
    value: Int

fn fromInt(n: Int) -> Result<Natural, String>
    ? \"Smart constructor.\"
    match {guard}
        true  -> Result.Ok(Natural(value = n))
        false -> Result.Err(\"{label}: out of range\")
"
    )
}

#[test]
fn carrier_table_drops_bare_name_twins_with_mismatched_bounds() {
    // Two modules declare a carrier with the SAME bare name `Natural`
    // but DIFFERENT proven bounds: AAA's is `[0,+inf]` (does NOT fit
    // i64 — must stay boxed) while BBB's is `[0,100]` (fits). The
    // table is keyed by the bare name only, and the MIR seed site
    // cannot tell the twins apart — intersecting the bounds would
    // narrow the interval to `[0,100]` and thereby wrongly license the
    // i64 fast path for AAA's unbounded carrier. The collision must
    // fail closed: no entry at all, both twins stay boxed.
    let aaa = natural_module("AAA", "n >= 0");
    let bbb = natural_module("BBB", "Bool.and(n >= 0, n <= 100)");
    let table = carrier_table(TWIN_ENTRY, &[("AAA", &aaa), ("BBB", &bbb)]);
    assert!(
        !table.contains_key("Natural"),
        "mismatched bare-name twins must be dropped, got {:?}",
        table.get("Natural")
    );
}

#[test]
fn carrier_table_keeps_bare_name_twins_with_identical_bounds() {
    // Identical twins prove the SAME bound, so the shared entry is the
    // right fact for every slot regardless of which twin it came from.
    let aaa = natural_module("AAA", "Bool.and(n >= 0, n <= 100)");
    let bbb = natural_module("BBB", "Bool.and(n >= 0, n <= 100)");
    let table = carrier_table(TWIN_ENTRY, &[("AAA", &aaa), ("BBB", &bbb)]);
    assert_eq!(
        table.get("Natural"),
        Some(&(Interval::between(0, 100), true)),
        "identical bare-name twins keep their shared bound"
    );
}
