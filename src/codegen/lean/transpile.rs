//! Unified Lean project emission: per-module files, the entry file,
//! `AverCommon.lean`, and Oracle-lifted effectful functions.

use std::collections::{HashMap, HashSet};

use crate::ast::TopLevel;
use crate::codegen::{CodegenContext, ProjectOutput};

use super::prelude::{build_common_lean, generate_lakefile_with_roots, generate_toolchain};
use super::{
    VerifyEmitMode, lean_project_name, pure_fns, recursive_pure_fn_names, recursive_type_names,
    toplevel, verify_counter_key,
};

/// Oracle v1: for each effectful FnDef of one scope — the entry or a
/// dependency module — whose effects are all classified and that some proof
/// cone reaches, run effect_lifting::lift_fn_def to produce a pure (lifted)
/// FnDef and emit it via the standard pure-fn path, inside that scope's
/// namespace. Effectful functions that use unclassified (stateful /
/// interactive / higher-order-callback) effects are still skipped entirely —
/// matches the pre-Oracle behavior.
///
/// `reachable` is the whole-program verify cone keyed by `FnId`.
/// `foreign_helpers` keys every other
/// module's lifted function by its qualified name (`Infra.Store.get`) so a
/// dotted call site receives the callee's `(path, oracle...)` prefix; this
/// scope's own lifted functions join the map under their bare names.
///
/// Lifted functions that call each other are grouped by the same SCC
/// analysis pure components use and emitted in one `mutual … end` block, so
/// no member is a forward reference Lean rejects. An effectful function has
/// no recursion contract (those are planned for pure functions), so a group
/// takes the contract-less path a pure group takes: `partial` members, and
/// the component is kernel-opaque for the sampled-claim classifier.
#[allow(clippy::too_many_arguments)]
fn emit_lifted_effectful_functions(
    ctx: &CodegenContext,
    fn_defs: &[&crate::ast::FnDef],
    scope: Option<&str>,
    reachable: &HashSet<crate::ir::FnId>,
    foreign_helpers: &HashMap<String, Vec<String>>,
    recursive_fns: &HashSet<String>,
    capability_opacity: &super::capability_opaque::CapabilityOpacity,
    sampled_fns: &mut SampledFnClassification,
    sections: &mut Vec<String>,
) {
    // Oracle v1: collect the effect list for every eligible
    // effectful fn *first* — call sites to these helpers in any
    // lifted body get `(path, oracle...)` injected so the arity
    // matches the helper's lifted form.
    let eligible: Vec<&crate::ast::FnDef> = fn_defs
        .iter()
        .copied()
        .filter(|fd| is_liftable_effectful_fn(ctx, fd, reachable))
        .collect();
    let mut helpers = foreign_helpers.clone();
    for fd in &eligible {
        helpers.insert(fd.name.clone(), declared_effect_names(fd));
    }

    let mut lifted_fns: Vec<(String, crate::ast::FnDef)> = Vec::new();
    for fd in &eligible {
        let Ok(Some(lifted)) =
            crate::types::checker::effect_lifting::lift_fn_def_with_helpers_and_registry(
                fd,
                &helpers,
                &ctx.capabilities,
            )
        else {
            continue;
        };
        lifted_fns.push((fd.name.clone(), lifted));
    }

    // Units of emission: one per strongly connected component of the lifted
    // call graph, in source order of their first member. A cycle becomes one
    // `mutual` unit; everything else stays a single definition.
    let lifted_refs: Vec<&crate::ast::FnDef> = lifted_fns.iter().map(|(_, fd)| fd).collect();
    let index_of: HashMap<&str, usize> = lifted_fns
        .iter()
        .enumerate()
        .map(|(index, (name, _))| (name.as_str(), index))
        .collect();
    let mut units: Vec<Vec<usize>> =
        crate::call_graph::ordered_fn_components(&lifted_refs, &ctx.module_prefixes)
            .iter()
            .map(|component| {
                let mut unit: Vec<usize> = component
                    .iter()
                    .map(|fd| index_of[fd.name.as_str()])
                    .collect();
                unit.sort_unstable();
                unit
            })
            .collect();
    units.sort_by_key(|unit| unit[0]);

    // Oracle v1: topologically sort so callees are emitted before
    // callers. Without this, a lifted effectful fn that calls another
    // lifted effectful helper (e.g. `handle(msg) -> printErr(msg)`)
    // can land before the helper and Lean complains about an unknown
    // identifier. Units that are ready at the same time keep their source
    // order; a call within a unit is never a dependency on another unit.
    let eligible_names: HashSet<String> = lifted_fns.iter().map(|(n, _)| n.clone()).collect();
    let mut emitted: HashSet<String> = HashSet::new();
    let mut order: Vec<Vec<usize>> = Vec::new();
    let mut remaining = units;
    while !remaining.is_empty() {
        let before = remaining.len();
        remaining.retain(|unit| {
            let members: HashSet<&str> = unit.iter().map(|&i| lifted_fns[i].0.as_str()).collect();
            let ready = unit.iter().all(|&i| {
                collect_called_idents_in_body(&lifted_fns[i].1.body)
                    .iter()
                    .all(|name| {
                        members.contains(name.as_str())
                            || !eligible_names.contains(name)
                            || emitted.contains(name)
                    })
            });
            if ready {
                emitted.extend(members.iter().map(|name| name.to_string()));
                order.push(unit.clone());
                false
            } else {
                true
            }
        });
        if remaining.len() == before {
            // Unreachable once every cycle is one unit: the unit graph is
            // acyclic. Kept as the fail-visible fallback — Lean names the
            // forward reference rather than the compiler dropping a fn.
            order.append(&mut remaining);
        }
    }

    ctx.with_module_scope(scope, || {
        for unit in order {
            let component: Vec<&crate::ast::FnDef> =
                unit.iter().map(|&i| &lifted_fns[i].1).collect();
            let code = if component.len() > 1 {
                Some(toplevel::emit_mutual_group(&component, ctx))
            } else {
                let fd = component[0];
                // A self call (a tail-recursive loop) has no termination
                // story the backend can state for a lifted fn: it is emitted
                // `partial`, as an uncontracted recursive pure fn is.
                let self_recursive = !recursive_fns.contains(&fd.name)
                    && collect_called_idents_in_body(&fd.body).contains(&fd.name);
                if self_recursive {
                    toplevel::emit_fn_def(fd, &HashSet::from([fd.name.clone()]), ctx)
                } else {
                    toplevel::emit_fn_def(fd, recursive_fns, ctx)
                }
            };
            let Some(mut code) = code else { continue };
            if capability_opacity.emitted_component_reaches_result_proven(&component, ctx) {
                code = format!("noncomputable section\n\n{code}\nend");
            }
            if component_is_kernel_opaque(&component, std::slice::from_ref(&code)) {
                sampled_fns.opaque.extend(
                    component
                        .iter()
                        .filter_map(|fd| crate::codegen::common::fn_id_for_emitted_decl(ctx, fd)),
                );
            }
            sections.push(code);
            sections.push(String::new());
        }
    });
}

/// An effectful fn the Oracle lift exports: not `main`, every declared
/// effect classified, and reached by some proof cone. Non-terminating
/// effectful fns (e.g. REPL loops that loop forever on `Console.readLine`)
/// would otherwise make Lean reject the whole module — and if nobody is
/// proving anything about them, that's dead code in the proof output.
fn is_liftable_effectful_fn(
    ctx: &CodegenContext,
    fd: &crate::ast::FnDef,
    reachable: &HashSet<crate::ir::FnId>,
) -> bool {
    use crate::types::checker::effect_classification::classify_with_registry;
    !fd.effects.is_empty()
        && fd.name != "main"
        && fd
            .effects
            .iter()
            .all(|e| classify_with_registry(&ctx.capabilities, &e.node).is_some())
        && crate::codegen::common::fn_id_for_decl(ctx, fd)
            .is_some_and(|fn_id| reachable.contains(&fn_id))
}

fn declared_effect_names(fd: &crate::ast::FnDef) -> Vec<String> {
    fd.effects.iter().map(|e| e.node.clone()).collect()
}

/// Every dependency module's liftable effectful fn, keyed by the qualified
/// name a consumer spells (`Infra.Store.get`), with its declared effects —
/// the `effectful_helpers` entries a caller in any other scope injects
/// `(path, oracle...)` for.
fn lifted_dependency_helpers(
    ctx: &CodegenContext,
    reachable: &HashSet<crate::ir::FnId>,
) -> HashMap<String, Vec<String>> {
    let mut helpers = HashMap::new();
    for module in &ctx.modules {
        for fd in &module.fn_defs {
            if is_liftable_effectful_fn(ctx, fd, reachable) {
                helpers.insert(
                    format!("{}.{}", module.prefix, fd.name),
                    declared_effect_names(fd),
                );
            }
        }
    }
    helpers
}

/// Names of the fns a lifted body calls, tail calls included: after TCO a
/// self- or mutual tail call is an `Expr::TailCall` whose target is a name,
/// not a callee expression, and a hand-rolled walk here never saw it.
fn collect_called_idents_in_body(body: &crate::ast::FnBody) -> std::collections::HashSet<String> {
    use crate::ast::{Expr, Stmt};
    let mut out = std::collections::HashSet::new();
    for stmt in body.stmts() {
        let expr = match stmt {
            Stmt::Expr(e) | Stmt::Binding(_, _, e) => e,
        };
        crate::codegen::expr_walk::walk(expr, &mut |node| match &node.node {
            Expr::FnCall(callee, _) => {
                if let Expr::Ident(name) | Expr::Resolved { name, .. } = &callee.node {
                    out.insert(name.clone());
                }
            }
            Expr::TailCall(tc) => {
                out.insert(tc.target.clone());
            }
            _ => {}
        });
    }
    out
}

#[derive(Clone, Copy)]
pub(super) enum LeanEmitMode {
    Standard,
    Proof,
}

/// `true` iff `fd` is the self-recursive inner loop of a recognized
/// `WrapperOverRecursion` law (`sumTR`, `factTR`). The strategy's
/// accumulator-decomposition lemma rewrites with the inner fn's definitional
/// equations, which a `partial def` would not expose — so such a fn must emit
/// as a terminating structural `def` even when the generic recursion classifier
/// (which conservatively rejects a growing accumulator) leaves it unclassified.
/// Scoped to wrapper inners: an accumulator fn the backend can't prove a law
/// about (no strategy fires) stays `partial`, preserving the honest decline.
fn is_wrapper_over_recursion_inner(ctx: &CodegenContext, fd: &crate::ast::FnDef) -> bool {
    ctx.proof_ir.law_theorems.iter().any(|t| {
        matches!(
            &t.strategy,
            crate::ir::ProofStrategy::WrapperOverRecursion { inner_fn, .. }
                if *inner_fn == fd.name
        ) || matches!(
            &t.strategy,
            // The `TailRecFixedBaseFold` loop (`qexp`) recurses on its DRIVER
            // (2nd param); Lean's equation compiler still infers the structural
            // measure across all params, so it must emit as a terminating `def`
            // for the accumulator-decomposition lemma's `rw` to see its
            // definitional equations — a `partial def` would withhold them.
            crate::ir::ProofStrategy::TailRecFixedBaseFold { loop_fn, .. }
                if *loop_fn == fd.name
        )
    })
}

/// Tokens that make an emitted declaration unreducible in the kernel: a
/// `partial def` / `opaque` / `unsafe` constant has no definitional unfolding,
/// and a `sorry` is not a value at all.
///
/// A `panic!` arm (the fuel wrappers' exhaustion case) is listed for a
/// different reason: it DOES reduce — silently, to `default` — where native
/// evaluation prints the `PANIC at …` line that `aver proof --check` charges
/// as a hard failure. Kernel-deciding such a case would blind that gate.
const KERNEL_OPAQUE_TOKENS: [&str; 5] = ["partial def", "opaque ", "unsafe ", "sorry", "panic!"];

/// `true` iff the kernel cannot see through what the emitter just wrote for
/// this component.
///
/// Reads the fact off the emitted TEXT rather than re-deriving it from the
/// source shape, so a new emission strategy cannot silently widen the
/// kernel-decided set. Mutual groups are rejected wholesale: whether fuelized
/// or well-founded, their compiled recursor is not something to bet a user's
/// `lake build` on.
fn component_is_kernel_opaque(comp: &[&crate::ast::FnDef], emitted: &[String]) -> bool {
    comp.len() > 1
        || emitted
            .iter()
            .any(|code| code_has_kernel_opaque_token(code))
}

/// `true` iff this component was actually emitted through the fuel fallback.
/// Read the compiler-owned exhaustion arm from generated code rather than the
/// recursion plan: some planned fuel functions graduate to native
/// `termination_by` when their whole SCC admits a stronger recognizer.
fn component_is_fuel_lowered(emitted: &[String]) -> bool {
    let marker = format!("panic! \"{}\"", super::toplevel::PROOF_FUEL_EXHAUSTED_MSG);
    emitted
        .iter()
        .any(|code| code_has_non_comment_token(code, &marker))
}

/// Why this component's fuel seed is not a statically justified bound on
/// every recursive step, when it is not: the refusal the claims whose
/// evaluation would reach the component are declined with.
///
/// Fuel is not inherently approximate in Aver. Int countdown, string-position
/// and ordinary ranked-sizeOf helpers derive a budget that bounds their exact
/// recursion shape. The #1018 hole is the fallback used after native
/// termination rejects a computed successor: its seed is executable, but is
/// not a proof that evaluation cannot reach zero. Only that class must be
/// barred from sampled native evaluation. A growing accumulator or a wrapper
/// without its own measured parameter is not sufficient evidence: both occur
/// in bounded mutual traversals such as the JSON parser and serializer.
fn component_unbounded_fuel_refusal(
    comp: &[&crate::ast::FnDef],
    ctx: &CodegenContext,
    emitted: &[String],
) -> Option<String> {
    if !component_is_fuel_lowered(emitted) || comp.len() < 2 {
        return None;
    }
    let is_mutual_sizeof = comp.iter().all(|fd| {
        crate::codegen::common::find_fn_contract_for_fn(ctx, fd).is_some_and(|contract| {
            matches!(
                contract.recursion.as_ref(),
                Some(crate::ir::RecursionContract::Fuel {
                    fuel_metric: crate::ir::FuelMetric::Lex { params, .. },
                }) if params.is_empty()
            )
        })
    });
    // The seed counts the group's structural parameters. It bounds the
    // recursion of a group for which the call edge analysis, run over the
    // parameters the seed counts, finds a measure — whether this backend
    // states it or backs off from it (a sum over two recursive types, say);
    // it bounds nothing the analysis refused, no recursion an `Int` counts
    // down, and none a computed value is handed into at a position it
    // counts — such a group is declined rather than evaluated until its
    // fuel runs out and a true claim reports a panic.
    is_mutual_sizeof
        .then(|| toplevel::fuel::native_measure_refusal(comp, ctx))
        .flatten()
}

/// Token scan over emitted Lean, ignoring `/-- … -/` doc comments and `--`
/// lines. The doc text is user prose lifted from the Aver `?` description, so
/// scanning it would classify a fn whose description merely says "opaque" as
/// kernel-opaque. Code lines are matched with a plain `contains`: a false hit
/// (the word inside a Lean string literal) only routes the case back to
/// `native_decide`, which is always safe.
fn code_has_kernel_opaque_token(code: &str) -> bool {
    KERNEL_OPAQUE_TOKENS
        .iter()
        .any(|token| code_has_non_comment_token(code, token))
}

/// Search generated Lean code while excluding doc/block and line comments.
/// Both opacity and fuel classification are semantic facts about declarations,
/// never about prose copied from the source function description.
fn code_has_non_comment_token(code: &str, token: &str) -> bool {
    let mut in_doc = false;
    for raw in code.lines() {
        let line = raw.trim_start();
        if in_doc {
            in_doc = !line.contains("-/");
            continue;
        }
        if line.starts_with("/-") {
            in_doc = !line.contains("-/");
            continue;
        }
        if line.starts_with("--") {
            continue;
        }
        if line.contains(token) {
            return true;
        }
    }
    false
}

#[derive(Default)]
struct SampledFnClassification {
    /// Declarations the kernel cannot reduce through.
    opaque: HashSet<crate::ir::FnId>,
    /// Exact subset whose emitted fuel seed is not a proven recursion bound,
    /// each with the call edge analysis's refusal when it has one — the call
    /// the exporter could not see shrink, cited by the claims it declines.
    unbounded_fuel: HashMap<crate::ir::FnId, Option<String>>,
}

#[derive(Clone, Copy)]
struct RecursiveFnNames<'a> {
    proof: &'a HashSet<String>,
    standard: &'a HashSet<String>,
}

fn emit_pure_component(
    comp: &[&crate::ast::FnDef],
    scope: Option<&str>,
    ctx: &CodegenContext,
    emit_mode: LeanEmitMode,
    recursive: RecursiveFnNames<'_>,
    sampled_fns: &mut SampledFnClassification,
    capability_opacity: &super::capability_opaque::CapabilityOpacity,
) -> Vec<String> {
    let unsupported = capability_opacity.unsupported_component_dependencies(comp, ctx);
    if !unsupported.is_empty() {
        return vec![
            format!(
                "-- function component was not exported: capability operation(s) {} have no sound Nonempty result witness",
                unsupported.join(", ")
            ),
            String::new(),
        ];
    }
    let mut out = emit_pure_component_code(comp, scope, ctx, emit_mode, recursive);
    if capability_opacity.component_is_noncomputable(comp, ctx)
        && let Some(code) = out.first_mut()
    {
        *code = format!("noncomputable section\n\n{code}\n\nend");
    }
    if component_is_kernel_opaque(comp, &out) {
        sampled_fns.opaque.extend(
            comp.iter()
                .filter_map(|fd| crate::codegen::common::fn_id_for_decl(ctx, fd)),
        );
    }
    if let Some(refusal) = component_unbounded_fuel_refusal(comp, ctx, &out) {
        sampled_fns.unbounded_fuel.extend(
            comp.iter()
                .filter_map(|fd| crate::codegen::common::fn_id_for_decl(ctx, fd))
                .map(|id| (id, Some(refusal.clone()))),
        );
    }
    out
}

fn emit_pure_component_code(
    comp: &[&crate::ast::FnDef],
    scope: Option<&str>,
    ctx: &CodegenContext,
    emit_mode: LeanEmitMode,
    recursive: RecursiveFnNames<'_>,
) -> Vec<String> {
    ctx.with_module_scope(scope, || {
        let mut out = Vec::new();
        if comp.len() > 1 {
            let code = match emit_mode {
                LeanEmitMode::Proof => {
                    let all_supported = comp
                        .iter()
                        .all(|fd| crate::codegen::common::fn_contract_exists_for_fn(ctx, fd));
                    if all_supported {
                        toplevel::emit_mutual_group_proof(comp, ctx)
                    } else {
                        toplevel::emit_mutual_group(comp, ctx)
                    }
                }
                LeanEmitMode::Standard => toplevel::emit_mutual_group(comp, ctx),
            };
            out.push(code);
            out.push(String::new());
        } else if let Some(fd) = comp.first() {
            let emitted = match emit_mode {
                LeanEmitMode::Proof => {
                    let is_recursive = recursive.proof.contains(&fd.name);
                    // Recursive fns without a proof contract remain partial,
                    // except recognized wrapper-recursion inner loops whose
                    // law strategy needs transparent equations.
                    if is_recursive
                        && !crate::codegen::common::fn_contract_exists_for_fn(ctx, fd)
                        && !is_wrapper_over_recursion_inner(ctx, fd)
                    {
                        toplevel::emit_fn_def(fd, recursive.proof, ctx)
                    } else {
                        toplevel::emit_fn_def_proof(fd, ctx)
                    }
                }
                LeanEmitMode::Standard => toplevel::emit_fn_def(fd, recursive.standard, ctx),
            };
            if let Some(code) = emitted {
                out.push(code);
                out.push(String::new());
            }
        }
        out
    })
}

fn emit_type_sections(
    td: &crate::ast::TypeDef,
    scope: Option<&str>,
    ctx: &CodegenContext,
    emit_mode: LeanEmitMode,
    cert_model: bool,
    recursive_types: &HashSet<String>,
    measure_sig_type_refs: &[String],
) -> Vec<String> {
    ctx.with_module_scope(scope, || {
        let mut sections = vec![toplevel::emit_type_def_in_scope(td, ctx, scope)];
        if scope == Some("Bytes") && crate::codegen::common::type_def_name(td) == "Bytes" {
            sections.push(
                "instance : Nonempty Bytes := ⟨⟨[], by simp [Bytes.allInRange]⟩⟩".to_string(),
            );
            // Pure builtin bridge for `String.toUtf8` / `String.fromUtf8`.
            // Lean's native String storage is UTF-8, so encoding maps its
            // ByteArray once and proves the ordinary Bytes refinement from
            // UInt8's bound. Decoding delegates the one validity check to
            // `String.fromUTF8?`; the error text matches every runtime.
            sections.push(
                r#"def stringToUtf8 (s : String) : Bytes :=
  ⟨s.toUTF8.toList.map (fun byte => (byte.toNat : Int)), by
    induction s.toUTF8.toList with
    | nil => simp [allInRange]
    | cons head tail ih =>
      have hlo : (head.toNat : Int) >= 0 := Int.natCast_nonneg _
      have hu8 := UInt8.toNat_lt head
      have hhi : (head.toNat : Int) <= 255 := by omega
      simp [allInRange, hlo, hhi, ih]⟩

def stringFromUtf8 (bytes : Bytes) : Except String String :=
  match String.fromUTF8? (bytes.val.map (fun byte => UInt8.ofNat byte.toNat)).toByteArray with
  | some text => Except.ok text
  | none => Except.error "invalid UTF-8""#
                    .to_string(),
            );
        }
        if cert_model {
            let inst = toplevel::emit_inhabited_instance(td, ctx, scope);
            if !inst.is_empty() {
                sections.push(inst);
            }
            let beq = toplevel::emit_beq_instance(td);
            if !beq.is_empty() {
                sections.push(beq);
            }
        }
        if toplevel::is_recursive_type_def(td)
            && crate::codegen::proof_recognize::detect_canonical_peano(td).is_none()
        {
            if !cert_model {
                sections.push(toplevel::emit_recursive_decidable_eq(
                    toplevel::type_def_name(td),
                ));
            }
            if matches!(emit_mode, LeanEmitMode::Proof)
                && let Some(measure) =
                    toplevel::emit_recursive_measure(td, recursive_types, measure_sig_type_refs)
            {
                sections.push(measure);
            }
        }
        sections.push(String::new());
        sections
    })
}

/// The endian codecs call the source-defined `Bytes.fromList`, so Lean must
/// see them after the Bytes module's ordinary declarations (Lean has no
/// forward references). Keeping this postlude in the owning namespace also
/// leaves every emitted builtin spelling as `Bytes.int…`.
fn emit_bytes_endian_builtins() -> String {
    let limit = aver_rt::MAX_MATERIALIZED_SEQUENCE_ELEMENTS;
    let big_width = aver_rt::int_endian_width_error_message("Int.toBigEndian");
    let little_width = aver_rt::int_endian_width_error_message("Int.toLittleEndian");
    let big_value = aver_rt::int_endian_value_error_message("Int.toBigEndian");
    let little_value = aver_rt::int_endian_value_error_message("Int.toLittleEndian");
    format!(
        r#"private def endianOctetsLittle : Int → Nat → List Int
  | _, 0 => []
  | value, width + 1 => value % 256 :: endianOctetsLittle (value / 256) width

private def endianFits : Int → Nat → Bool
  | value, 0 => value == 0
  | value, width + 1 => value >= 0 && endianFits (value / 256) width

def intToBigEndian (value width : Int) : Except String Bytes :=
  if width < 0 || width > {limit} then Except.error "{big_width}"
  else if value < 0 || !endianFits value width.toNat then Except.error "{big_value}"
  else fromList (endianOctetsLittle value width.toNat).reverse

def intToLittleEndian (value width : Int) : Except String Bytes :=
  if width < 0 || width > {limit} then Except.error "{little_width}"
  else if value < 0 || !endianFits value width.toNat then Except.error "{little_value}"
  else fromList (endianOctetsLittle value width.toNat)

def intFromBigEndian (bytes : Bytes) : Int :=
  bytes.val.foldl (fun value byte => value * 256 + byte) 0

def intFromLittleEndian (bytes : Bytes) : Int :=
  bytes.val.reverse.foldl (fun value byte => value * 256 + byte) 0"#
    )
}

fn emit_capability_resource_types(ctx: &CodegenContext, scope: Option<&str>) -> Vec<String> {
    let entry_module = ctx.entry_module_name();
    ctx.capabilities
        .resource_types()
        .filter_map(|canonical| {
            let (module, name) = canonical.rsplit_once('.')?;
            let belongs = match scope {
                Some(prefix) => module == prefix,
                None => {
                    // A loaded capability module owns its declaration file.
                    // Ad-hoc/synthetic programs can still receive the
                    // standard contract through type checking without that
                    // module appearing in `ctx.modules`; emit the qualified
                    // resource in the entry file so its type never vanishes.
                    entry_module.as_deref() == Some(module)
                        || !ctx
                            .modules
                            .iter()
                            .any(|candidate| candidate.prefix == module)
                }
            };
            belongs.then(|| {
                let declaration_name = if scope.is_some() || entry_module.as_deref() == Some(module)
                {
                    super::syntax::aver_name_to_lean(name)
                } else {
                    super::syntax::aver_path_to_lean(canonical)
                };
                // An identity, not an `opaque Type`. The handle a capability
                // hands back HAS identity at run time — the host compares and
                // hashes it — and the language's rule is only that a program
                // may not compare one itself. Emitting it as `opaque` denied
                // the model the equality it never needed to invent: a sum with
                // a handle-carrying constructor could then derive nothing, so
                // `Repr`, `BEq`, `Inhabited` and `DecidableEq` all failed at
                // once and every claim in the module was lost — including the
                // ones comparing constructors that carry no handle at all.
                //
                // The field is unreachable from Aver: no source can build a
                // handle or read its identity, and nothing emitted mentions
                // the field. Deliberately NO `Inhabited`: a default handle
                // would be a value the runtime never produces, and a
                // fuel-exhausted branch could return one.
                format!(
                    "structure {declaration_name} where\n  id : Nat\n  deriving Repr, BEq, DecidableEq"
                )
            })
        })
        .collect()
}

fn emit_pure_capability_operation(
    operation: &crate::capability::CapabilityOperation,
    scope: Option<&str>,
    ctx: &CodegenContext,
) -> String {
    let name = if scope.is_some() {
        super::syntax::aver_name_to_lean(&operation.name)
    } else {
        super::syntax::aver_path_to_lean(&operation.canonical_name)
    };
    let ty = crate::types::Type::Fn(
        operation.params.iter().map(|(_, ty)| ty.clone()).collect(),
        Box::new(operation.return_type.clone()),
        vec![],
    );
    super::capability_opaque::emit_operation(
        operation,
        &name,
        &super::types::type_to_lean(&ty),
        ctx,
    )
}

/// Lean names of the Aver functions (and capability operations) a module's
/// emitted body actually declares. Read off the emitted text rather than
/// `fn_defs`: a function the emitter skipped (an effectful dependency
/// function, an unsupported shape) declares no constant, and `open … hiding`
/// may only name constants that exist.
fn declared_fn_names(body: &str, module: &crate::codegen::ModuleInfo) -> HashSet<String> {
    let fn_names: HashSet<String> = module
        .fn_defs
        .iter()
        .map(|fd| fd.name.as_str())
        .chain(
            module
                .capability_items
                .iter()
                .filter_map(|item| match item {
                    crate::ast::CapabilityItem::Operation(op) => Some(op.name.as_str()),
                    crate::ast::CapabilityItem::Resource { .. } => None,
                }),
        )
        .map(super::syntax::aver_name_to_lean)
        .collect();
    body.lines()
        .filter_map(declared_constant_name)
        .filter(|name| fn_names.contains(*name))
        .map(str::to_string)
        .collect()
}

/// The constant a declaration line introduces, if any. An indented line
/// counts too: the members of a `mutual … end` block are declared indented,
/// and the intersection with the module's function names keeps a body line
/// from passing as one.
fn declared_constant_name(line: &str) -> Option<&str> {
    let line = line.trim_start();
    let line = match line.strip_prefix("@[") {
        Some(rest) => &rest[rest.find(']')? + 1..],
        None => line,
    };
    let mut words = line.split_whitespace();
    let head = words.find(|word| {
        !matches!(
            *word,
            "private" | "protected" | "noncomputable" | "partial" | "unsafe"
        )
    })?;
    if !matches!(head, "def" | "theorem" | "abbrev" | "opaque" | "axiom") {
        return None;
    }
    Some(words.next()?.trim_end_matches([':', '(']))
}

/// An emitted body with every Lean comment blanked out: `--` to end of line,
/// and `/- … -/` blocks (which nest, and which include the `/-- … -/` doc
/// comments that carry an Aver `?` intent). String literals are NOT blanked:
/// an interpolation carries real code, and a module path inside plain text
/// costs at most an import of a file the project compiles anyway.
///
/// Every blanked character becomes a space and every newline stays, so the
/// result lines up with the input.
fn code_only(text: &str) -> String {
    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();
    let mut depth = 0usize;
    let mut in_string = false;
    let mut in_line_comment = false;
    while let Some(c) = chars.next() {
        if in_line_comment {
            in_line_comment = c != '\n';
            out.push(if c == '\n' { '\n' } else { ' ' });
        } else if depth > 0 {
            match (c, chars.peek().copied()) {
                ('/', Some('-')) => {
                    chars.next();
                    depth += 1;
                    out.push_str("  ");
                }
                ('-', Some('/')) => {
                    chars.next();
                    depth -= 1;
                    out.push_str("  ");
                }
                ('\n', _) => out.push('\n'),
                _ => out.push(' '),
            }
        } else if in_string {
            out.push(c);
            match c {
                '\\' => {
                    if let Some(escaped) = chars.next() {
                        out.push(escaped);
                    }
                }
                '"' => in_string = false,
                _ => {}
            }
        } else {
            match (c, chars.peek().copied()) {
                ('/', Some('-')) => {
                    chars.next();
                    depth = 1;
                    out.push_str("  ");
                }
                ('-', Some('-')) => {
                    chars.next();
                    in_line_comment = true;
                    out.push_str("  ");
                }
                _ => {
                    in_string = c == '"';
                    out.push(c);
                }
            }
        }
    }
    out
}

/// Whether emitted code spells a constant that belongs to the Lean module
/// `lean_prefix` — `Tcp.Connection`, `Bytes.Bytes` — as opposed to merely
/// carrying those characters inside a longer name (`Tcp` in `TcpPort.x`) or
/// a longer module path (`Tcp` in `Net.Tcp.Connection`). Takes the
/// comment-free text [`code_only`] returns: a module path in an intent's
/// prose is not a reference and must not add an import.
fn code_names_module(code: &str, lean_prefix: &str) -> bool {
    let bytes = code.as_bytes();
    let mut from = 0;
    while let Some(offset) = code[from..].find(lean_prefix) {
        let start = from + offset;
        let end = start + lean_prefix.len();
        from = end;
        // `Prefix` must open a dotted name, and must not continue one.
        let opens_a_name = bytes.get(end) == Some(&b'.')
            && bytes
                .get(end + 1)
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_');
        let stands_alone = start == 0
            || !matches!(bytes[start - 1], b'.' | b'_' | b'\'')
                && !bytes[start - 1].is_ascii_alphanumeric();
        if opens_a_name && stands_alone {
            return true;
        }
    }
    false
}

/// `open` lines for a file's direct dependencies — the same rule for a
/// dependency file and for the entry. A function name that two or more
/// of those modules declare is hidden from each of them: the emitter
/// spells every cross-module function with its module path, so no
/// reference is lost, while a match binder of that name stays a plain
/// pattern variable instead of Lean's "ambiguous pattern, use fully
/// qualified name".
fn open_lines(depends: &[String], declared: &HashMap<String, HashSet<String>>) -> Vec<String> {
    let empty = HashSet::new();
    let names = |dep: &String| declared.get(dep).unwrap_or(&empty);
    depends
        .iter()
        .map(|dep| {
            let mut hidden: Vec<&str> = names(dep)
                .iter()
                .filter(|name| {
                    depends
                        .iter()
                        .any(|other| other != dep && names(other).contains(*name))
                })
                .map(String::as_str)
                .collect();
            hidden.sort_unstable();
            let module = super::syntax::aver_path_to_lean(dep);
            if hidden.is_empty() {
                format!("open {module}")
            } else {
                format!("open {module} hiding {}", hidden.join(" "))
            }
        })
        .collect()
}

/// Multi-file Lean output for multi-module Aver projects:
/// - `AverCommon.lean` carries built-in helpers + records (UNION decision
///   over every module + entry body, so a helper is included only if
///   something actually references it).
/// - `<Module>.lean` (one per `depends [...]` entry) wraps that module's
///   types and pure fns in `namespace M ... end M`. Submodules like
///   `Models.User` land at `Models/User.lean` to match Lean's path-as-
///   module-name convention.
/// - `<ProjectName>.lean` is the entry: trust header (here only),
///   top-level entry items, lifted effectful fns, decisions, verify
///   blocks. Imports `AverCommon` plus every dependent module.
pub(super) fn transpile_unified(
    ctx: &CodegenContext,
    verify_mode: VerifyEmitMode,
    emit_mode: LeanEmitMode,
    cert_model: bool,
) -> ProjectOutput {
    // Read recursion fact from `ctx.recursive_fns` — populated upstream
    // by `refresh_facts()` (test stubs) or `build_context` (production).
    // After phase C the set is keyed by `FnId`; project back to bare
    // names for scope-local `emit_fn_def` consumers via the symbol
    // table (the DAG invariant keeps bare-name unambiguous within a
    // single scope). The proof-mode auto-prove path also needs the
    // same bare-name projection.
    let recursive_fns: HashSet<String> = ctx
        .recursive_fns
        .iter()
        .map(|id| ctx.symbol_table.fn_entry(*id).key.name.clone())
        .collect();
    let recursive_names = recursive_pure_fn_names(ctx);
    let recursive_types = recursive_type_names(ctx);
    let recursive = RecursiveFnNames {
        proof: &recursive_names,
        standard: &recursive_fns,
    };
    // Lift every canonical Peano ADT's type annotations to builtin `Nat` for
    // this emit, matching the value/pattern lift — so a Peano type named other
    // than `Nat` is fully consistent (its binders' types agree with the `Nat`
    // literals its values lift to). The guard clears the set when this returns.
    let _peano_guard = crate::codegen::lean::types::scope_canonical_peano(
        crate::codegen::proof_recognize::collect_peano_types(ctx)
            .into_iter()
            .map(|p| p.type_name)
            .collect(),
    );
    let _resource_guard = crate::codegen::lean::types::scope_capability_resources(
        ctx.capabilities.resource_types().cloned().collect(),
    );
    // A user type declared in another module is spelled with that module's
    // path (`A.Fraction`), so a signature, a field or a constructor resolves
    // without an `open` of the owner; the module loop below names the module
    // being emitted so its own types keep their bare spelling.
    let _type_owner_guard = crate::codegen::lean::types::scope_type_owners(
        ctx.symbol_table.clone(),
        ctx.entry_module_name(),
    );
    let capability_opacity = super::capability_opaque::CapabilityOpacity::analyze(ctx);
    // Pure-fn param types + every type def's field types feed the
    // entries-measure emission scan: an entries-list spelling
    // (`Map<K, T>` / `List<Tuple<K, T>>`) may appear only in fn
    // signatures or in ANOTHER type's fields, never in `T`'s own
    // fields, yet the chooser (`type_measure_expr`) would reference
    // `averMeasure<T>Entries_<K>` for it all the same.
    let measure_sig_type_refs: Vec<String> = pure_fns(ctx)
        .iter()
        .flat_map(|fd| fd.params.iter().map(|(_, ty)| ty.clone()))
        .chain(
            ctx.modules
                .iter()
                .flat_map(|m| m.type_defs.iter())
                .chain(ctx.type_defs.iter())
                .flat_map(|td| match td {
                    crate::ast::TypeDef::Sum { variants, .. } => variants
                        .iter()
                        .flat_map(|v| v.fields.iter().cloned())
                        .collect::<Vec<_>>(),
                    crate::ast::TypeDef::Product { fields, .. } => {
                        fields.iter().map(|(_, ty)| ty.clone()).collect()
                    }
                }),
        )
        .collect();

    // Fns whose emission the kernel cannot see through, accumulated by every
    // `emit_pure_component` call and every lifted effectful component below.
    // The sampled-`verify` classifier reads it AFTER both declaration passes
    // have run — which is why the entry verify blocks are emitted at the end
    // of this fn rather than here (their position in the output is unchanged).
    let mut sampled_fns = SampledFnClassification::default();

    // Lifted effectful fns: the entry's here, each dependency module's inside
    // the module loop below, through the same scope-parametric pass. A
    // dependency's root set is what any consumer's cone reaches; its lifted
    // fns are keyed by qualified name so a consumer's call site threads the
    // callee's `(path, oracle...)`. Decisions + verifies remain entry-only.
    let lifted_recursive_names = match emit_mode {
        LeanEmitMode::Proof => &recursive_names,
        LeanEmitMode::Standard => &recursive_fns,
    };
    let proof_reachable = crate::codegen::common::proof_reachable_fn_ids(ctx);
    let dependency_helpers = lifted_dependency_helpers(ctx, &proof_reachable);
    let mut entry_lifted_sections: Vec<String> = Vec::new();
    let entry_fn_defs: Vec<&crate::ast::FnDef> = ctx
        .items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    emit_lifted_effectful_functions(
        ctx,
        &entry_fn_defs,
        None,
        &proof_reachable,
        &dependency_helpers,
        lifted_recursive_names,
        &capability_opacity,
        &mut sampled_fns,
        &mut entry_lifted_sections,
    );

    let mut entry_decision_sections: Vec<String> = Vec::new();
    for item in &ctx.items {
        if let TopLevel::Decision(db) = item {
            entry_decision_sections.push(toplevel::emit_decision(db));
            entry_decision_sections.push(String::new());
        }
    }

    // ---- Per-module file bodies ----
    let mut module_files: Vec<(String, String)> = Vec::new();
    let mut union_body = String::new();
    // Function names each emitted module file declares, by module prefix.
    // Modules arrive in dependency order, so a file's direct dependencies
    // are recorded before its `open` lines are written.
    let mut declared_fns: HashMap<String, HashSet<String>> = HashMap::new();

    // Cross-file citation remains visibility/topology gated even though every
    // declaring module now emits all of its own claims. Keep the producer and
    // consumer key spaces tied together: an admitted citation must name a law
    // known to the dependency theorem order. This is a debug-only integrity
    // check; it never filters the module-owned proof surface.
    #[cfg(debug_assertions)]
    if matches!(emit_mode, LeanEmitMode::Proof) {
        let admitted = super::law_auto::admitted_dep_law_theorems(ctx);
        let ordered = super::law_auto::dep_theorem_order_keys(ctx);
        debug_assert!(
            admitted.iter().all(|key| ordered.contains(key)),
            "cross-file citation admitted a dependency law absent from the theorem order"
        );
    }

    for (module_index, module) in ctx.modules.iter().enumerate() {
        let _emitting_guard = crate::codegen::lean::types::scope_emitting_module(&module.prefix);
        let mut body_sections = emit_capability_resource_types(ctx, Some(&module.prefix));
        let scope = Some(module.prefix.as_str());
        let decl_plan = super::decl_order::plan_scoped_declarations(
            ctx,
            &module.type_defs,
            &module.fn_defs,
            scope,
        );
        for decl in &decl_plan.order {
            match *decl {
                super::decl_order::ScopedDecl::Type(index) => {
                    body_sections.extend(emit_type_sections(
                        &module.type_defs[index],
                        scope,
                        ctx,
                        emit_mode,
                        cert_model,
                        &recursive_types,
                        &measure_sig_type_refs,
                    ));
                }
                super::decl_order::ScopedDecl::CapabilityOperation(index) => {
                    let operation = ctx
                        .capabilities
                        .operations()
                        .filter(|operation| {
                            !operation.is_effectful() && operation.module == module.prefix
                        })
                        .nth(index)
                        .expect("capability operation plan index");
                    body_sections.push(emit_pure_capability_operation(operation, scope, ctx));
                    body_sections.push(String::new());
                }
                super::decl_order::ScopedDecl::FnComponent(index) => {
                    body_sections.extend(emit_pure_component(
                        &decl_plan.components[index],
                        scope,
                        ctx,
                        emit_mode,
                        recursive,
                        &mut sampled_fns,
                        &capability_opacity,
                    ));
                }
            }
        }
        if module.prefix == "Bytes" {
            body_sections.push(emit_bytes_endian_builtins());
            body_sections.push(String::new());
        }
        // This module's effectful fns, lifted with the same oracle threading
        // the entry's get, after its pure declarations (a lifted body may call
        // them; nothing pure calls an effectful fn) and before its laws.
        let module_fn_defs: Vec<&crate::ast::FnDef> = module.fn_defs.iter().collect();
        emit_lifted_effectful_functions(
            ctx,
            &module_fn_defs,
            scope,
            &proof_reachable,
            &dependency_helpers,
            lifted_recursive_names,
            &capability_opacity,
            &mut sampled_fns,
            &mut body_sections,
        );
        // Whole-program proof surface: every dependency module emits every
        // `verify` block it owns inside its own namespace. Visibility affects
        // cross-file CITATION (`module.verify_laws`), never whether a module's
        // private obligation is checked. This is the same emit path the entry
        // uses and the active module scope pins bare names to this module's
        // `FnId`s. Certificate model files emit ONLY the universal law
        // theorems from this surface (the sampled cases/`native_decide`
        // machinery is dropped inside `emit_verify_block`), so a package's
        // law-claims can cite dependency-module laws such as the k5 ring.
        if matches!(emit_mode, LeanEmitMode::Proof) {
            ctx.with_module_scope(Some(module.prefix.as_str()), || {
                let mut dep_verify_counters: HashMap<String, usize> = HashMap::new();
                let decidability = super::kernel_decide::CaseDecidability::new(
                    sampled_fns.opaque.clone(),
                    sampled_fns.unbounded_fuel.clone(),
                    recursive_types.clone(),
                    capability_opacity.clone(),
                );
                for vb in &module.verify_blocks {
                    let key = verify_counter_key(vb);
                    let start_idx = *dep_verify_counters.get(&key).unwrap_or(&0);
                    let (emitted, next_idx) = toplevel::emit_verify_block(
                        vb,
                        ctx,
                        verify_mode,
                        start_idx,
                        &decidability,
                        cert_model,
                    );
                    dep_verify_counters.insert(key, next_idx);
                    body_sections.push(emitted);
                    body_sections.push(String::new());
                }
            });
        }
        let body = body_sections.join("\n");
        union_body.push_str(&body);
        union_body.push('\n');

        // Reserved-token guard on every module-name surface: `namespace` /
        // `end`, `import` / `open` lines, and the `.lean` file path itself
        // (a Lean module named `Type` is unimportable, so the file is named
        // after the escaped spelling and the lakefile root matches).
        let lean_prefix = super::syntax::aver_path_to_lean(&module.prefix);
        let mut imports = vec!["import AverCommon".to_string()];
        if body.contains("Crypto.sha256") {
            imports.push("import Crypto".to_string());
        }
        let mut imported: HashSet<String> = HashSet::new();
        for d in &module.depends {
            let dep = super::syntax::aver_path_to_lean(d);
            imports.push(format!("import {dep}"));
            imported.insert(dep);
        }
        // Oracle threading spells the types an effect carries by their owner
        // module — `Tcp.Connection`, `Bytes.Bytes` — and that owner is a
        // standard module the source never writes in `depends`: the loader
        // pulls it in because some call needs it. While only the entry lifted
        // effectful functions this cost nothing, the entry imports every
        // module of the project. A dependency file that lifts one names those
        // constants too, so it has to import their owners itself, the way the
        // `Crypto.sha256` line above imports the crypto model. Derived from
        // the emitted code, so a module that lifts nothing keeps the imports
        // it had. Only the modules ahead of this one are candidates:
        // `ctx.modules` arrives dependencies-first, so those are exactly the
        // ones this file can name, and no import added here closes a cycle.
        let code = code_only(&body);
        for candidate in &ctx.modules[..module_index] {
            let name = super::syntax::aver_path_to_lean(&candidate.prefix);
            if imported.contains(&name) || !code_names_module(&code, &name) {
                continue;
            }
            imports.push(format!("import {name}"));
            imported.insert(name);
        }
        // AverCommon has no surrounding namespace (top-level helpers / instances),
        // so `import` already brings them into scope. We `open` only the
        // user-defined direct dependencies.
        let opens = open_lines(&module.depends, &declared_fns);
        declared_fns.insert(module.prefix.clone(), declared_fn_names(&body, module));

        let opens_str = if opens.is_empty() {
            String::new()
        } else {
            format!("\n{}\n", opens.join("\n"))
        };
        // `autoImplicit false` in every emitted file: a type name the emitter
        // left unresolved is a build error, not an implicit type variable
        // Lean binds silently (see the entry's header below).
        let content = format!(
            "{}\n\nset_option linter.unusedVariables false\nset_option maxRecDepth 1000000\nset_option autoImplicit false\n{}\nnamespace {}\n\n{}\nend {}\n",
            imports.join("\n"),
            opens_str,
            lean_prefix,
            body,
            lean_prefix
        );
        let path = lean_prefix.replace('.', "/");
        module_files.push((format!("{}.lean", path), content));
    }

    // ---- Entry sections ----
    let mut entry_body_sections = emit_capability_resource_types(ctx, None);
    // Synthetic/proof-only callers may provide the validated capability
    // registry without materialising its embedded module in `ctx.modules`.
    // Emit those represented boundary types under their canonical name so a
    // source-owned `Http.Response`/`Terminal.Size` never falls back to a
    // compiler-prelude record or disappears from the proof model.
    for (canonical, type_def) in ctx.capabilities.boundary_types() {
        let Some((owner, _)) = canonical.rsplit_once('.') else {
            continue;
        };
        let owner_is_materialized = ctx.entry_module_name().as_deref() == Some(owner)
            || ctx.modules.iter().any(|module| module.prefix == owner);
        if owner_is_materialized {
            continue;
        }
        let mut qualified = type_def.clone();
        match &mut qualified {
            crate::ast::TypeDef::Product { name, .. } | crate::ast::TypeDef::Sum { name, .. } => {
                *name = canonical.clone()
            }
        }
        entry_body_sections.extend(emit_type_sections(
            &qualified,
            None,
            ctx,
            emit_mode,
            cert_model,
            &recursive_types,
            &measure_sig_type_refs,
        ));
    }
    let entry_plan =
        super::decl_order::plan_scoped_declarations(ctx, &ctx.type_defs, &ctx.fn_defs, None);
    for decl in &entry_plan.order {
        match *decl {
            super::decl_order::ScopedDecl::Type(index) => {
                entry_body_sections.extend(emit_type_sections(
                    &ctx.type_defs[index],
                    None,
                    ctx,
                    emit_mode,
                    cert_model,
                    &recursive_types,
                    &measure_sig_type_refs,
                ));
            }
            super::decl_order::ScopedDecl::CapabilityOperation(index) => {
                let entry_module = ctx.entry_module_name();
                let operation = ctx
                    .capabilities
                    .operations()
                    .filter(|operation| {
                        !operation.is_effectful()
                            && entry_module.as_deref() == Some(operation.module.as_str())
                    })
                    .nth(index)
                    .expect("entry capability operation plan index");
                entry_body_sections.push(emit_pure_capability_operation(operation, None, ctx));
                entry_body_sections.push(String::new());
            }
            super::decl_order::ScopedDecl::FnComponent(index) => {
                entry_body_sections.extend(emit_pure_component(
                    &entry_plan.components[index],
                    None,
                    ctx,
                    emit_mode,
                    recursive,
                    &mut sampled_fns,
                    &capability_opacity,
                ));
            }
        }
    }

    // ---- Sampled `verify` cases (entry only) ----
    // Emitted last so the per-case kernel-decidability classifier can read the
    // opacity of every declaration this transpile actually produced. Only
    // proof mode is classified: the standard emit spells every recursive fn
    // `partial`, which no kernel reduction sees through anyway.
    let case_decidability = match emit_mode {
        LeanEmitMode::Proof => super::kernel_decide::CaseDecidability::new(
            sampled_fns.opaque,
            sampled_fns.unbounded_fuel,
            recursive_types.clone(),
            capability_opacity,
        ),
        LeanEmitMode::Standard => super::kernel_decide::CaseDecidability::disabled(),
    };
    let mut entry_verify_sections: Vec<String> = Vec::new();
    let mut verify_case_counters: HashMap<String, usize> = HashMap::new();
    // Certificate model modules keep ONLY the universal law theorems from
    // this surface: the sample-check `example` blocks are decided by
    // `native_decide`, need the recursive-type `DecidableEq` shim the cert
    // mode also drops, and a certificate carries its own decode-to-Int/bytes
    // anti-vacuity guards instead. `emit_verify_block` owns that split.
    for item in &ctx.items {
        if let TopLevel::Verify(vb) = item {
            let key = verify_counter_key(vb);
            let start_idx = *verify_case_counters.get(&key).unwrap_or(&0);
            let (emitted, next_idx) = toplevel::emit_verify_block(
                vb,
                ctx,
                verify_mode,
                start_idx,
                &case_decidability,
                cert_model,
            );
            verify_case_counters.insert(key, next_idx);
            entry_verify_sections.push(emitted);
            entry_verify_sections.push(String::new());
        }
    }

    entry_body_sections.extend(entry_lifted_sections);
    entry_body_sections.extend(entry_decision_sections);
    entry_body_sections.extend(entry_verify_sections);

    let entry_body = entry_body_sections.join("\n");
    union_body.push_str(&entry_body);
    union_body.push('\n');

    let project_name = lean_project_name(ctx);
    let namespaced_entry_body = format!(
        "namespace {}\n\n{}\nend {}",
        project_name, entry_body, project_name
    );
    let mut entry_imports = vec!["import AverCommon".to_string()];
    if entry_body.contains("Crypto.sha256") {
        entry_imports.push("import Crypto".to_string());
    }
    for m in &ctx.modules {
        entry_imports.push(format!(
            "import {}",
            super::syntax::aver_path_to_lean(&m.prefix)
        ));
    }
    // The entry imports the transitive closure (every file has to compile)
    // but opens exactly what a dependency file opens: its own direct
    // `depends`. Opening the closure made a match binder spelled like a
    // function two transitive modules export an "ambiguous pattern", and
    // let a user function named `none` collide with `Option.none`.
    let entry_opens = open_lines(&ctx.entry_depends(), &declared_fns);
    let mut entry_parts = vec![entry_imports.join("\n")];
    if !entry_opens.is_empty() {
        entry_parts.push(entry_opens.join("\n"));
    }
    // Silence `unused variable` warnings for the named-match equation
    // binders (`h_NN :`) that the wf elaborator needs but the user-
    // source body never references. Without this every ListStructural
    // recursion would surface a warning per nested match. Per-file
    // because `set_option` is local; AverCommon already has the same
    // option for its prelude defs.
    entry_parts.push("set_option linter.unusedVariables false".to_string());
    // Lean 4.31's `simp` recurses deeper through large rewrite sets (the
    // discovered-law floors cite 40+ lemmas in one `simp [...]`); the
    // default `maxRecDepth` overflows on those. This is a pure
    // elaboration-depth limit (no runtime / soundness effect), raised
    // per-file so the big auto-generated simp blocks elaborate.
    entry_parts.push("set_option maxRecDepth 1000000".to_string());
    // Fail closed on an unresolved type name. Under Lean's default
    // `autoImplicit`, a bare `Handle` in a signature whose module is not
    // open is not an error: Lean binds it as an implicit type variable and
    // the theorem quietly changes its statement. Measured on the first
    // external project: an entry threading a transitive capability's
    // operation (`! [Infra.Kv.get]` with the capability two modules away)
    // emitted `rnd_Infra_Kv_get : BranchPath → Int → Handle → …` and built.
    // With the option off, the same line is `Unknown identifier Handle`.
    entry_parts.push("set_option autoImplicit false".to_string());
    let declared = crate::codegen::common::collect_declared_effects(ctx);
    let has_ip = union_body.contains("BranchPath");
    let has_classified =
        crate::types::checker::effect_classification::classifications_for_proof_subset()
            .iter()
            .any(|c| declared.includes(c.method))
            || ctx.capabilities.operations().any(|operation| {
                operation.is_effectful() && declared.includes(&operation.canonical_name)
            });
    if has_ip || has_classified || ctx.capabilities.contracts().next().is_some() {
        entry_parts.push(
            crate::types::checker::proof_trust_header::generate_commented_with_registry(
                "-- ",
                &declared,
                has_ip,
                &ctx.capabilities,
            ),
        );
    }
    let subtype_block = crate::types::checker::oracle_subtypes::lean_subtypes(&declared);
    if !subtype_block.is_empty() {
        // Fold subtype block into the union body BEFORE computing
        // `needed_helpers` — the Oracle subtype block is what
        // introduces `BranchPath` references (e.g. `abbrev
        // TimeUnixMsOracle := BranchPath → Int → Int`) for files that
        // declare classified effects but never spell `BranchPath` in
        // user code. Without this, AverCommon.lean misses the
        // `structure BranchPath` block and Main.lean fails build with
        // `unknown identifier 'BranchPath'`.
        union_body.push_str(&subtype_block);
        union_body.push('\n');
        entry_parts.push(subtype_block);
    }
    entry_parts.push(namespaced_entry_body);
    let entry_content = entry_parts.join("\n\n");

    // ---- AverCommon.lean ----
    let common_content = build_common_lean(&union_body, cert_model);
    let uses_crypto_sha256 = union_body.contains("Crypto.sha256");

    // Project files
    let mut extra_roots: Vec<String> = vec!["AverCommon".to_string()];
    if uses_crypto_sha256 {
        extra_roots.push("Crypto".to_string());
    }
    for m in &ctx.modules {
        extra_roots.push(super::syntax::aver_path_to_lean(&m.prefix));
    }
    let lakefile = generate_lakefile_with_roots(&project_name, &extra_roots);
    let toolchain = generate_toolchain();

    let mut files = module_files;
    files.push((format!("{}.lean", project_name), entry_content));
    files.push(("AverCommon.lean".to_string(), common_content));
    if uses_crypto_sha256 {
        files.push(("Crypto.lean".to_string(), super::crypto::SOURCE.to_string()));
    }
    files.push(("lakefile.lean".to_string(), lakefile));
    files.push(("lean-toolchain".to_string(), toolchain));
    let mut output = ProjectOutput::of(files);
    // Hand the law-claims the certificate model recorded to the caller and
    // clear the sink, so a context reused for a second emission starts empty
    // instead of carrying the previous run's claims.
    output.law_claims = std::mem::take(&mut *ctx.universal_law_claims.borrow_mut());
    output
}

#[cfg(test)]
mod lifted_order_tests {
    use super::collect_called_idents_in_body;
    use crate::ast::{Expr, Stmt, TopLevel};

    #[test]
    fn a_tail_call_counts_as_a_call_for_lifted_ordering() {
        let src = "module M\n    effects [Console.print]\n\n\
fn loopy(n: Int) -> Unit\n    ? \"Prints then loops.\"\n    ! [Console.print]\n    match n\n        0 -> Console.print(\"done\")\n        _ -> loopy(n - 1)\n";
        let mut items = crate::source::parse_source(src).expect("parse");
        crate::ir::pipeline::tco(&mut items);
        let fd = items
            .iter()
            .find_map(|item| match item {
                TopLevel::FnDef(fd) if fd.name == "loopy" => Some(fd),
                _ => None,
            })
            .expect("loopy");
        let has_tail_call = fd.body.stmts().iter().any(|stmt| {
            let expr = match stmt {
                Stmt::Expr(e) | Stmt::Binding(_, _, e) => e,
            };
            crate::codegen::expr_walk::any(expr, &mut |n| matches!(n.node, Expr::TailCall(_)))
        });
        assert!(has_tail_call, "TCO should have rewritten the self call");
        let called = collect_called_idents_in_body(&fd.body);
        assert!(called.contains("loopy"), "{called:?}");
    }
}

#[cfg(test)]
mod module_reference_tests {
    use super::{code_names_module, code_only};

    #[test]
    fn a_qualified_constant_names_its_module_and_a_longer_name_does_not() {
        let body = "def greet (rnd : Int \u{2192} Tcp.Connection) : Unit := ()";
        assert!(code_names_module(body, "Tcp"));
        // Same characters, not a reference: a longer identifier, a longer
        // module path, a bare mention, and a field selector.
        for other in [
            "def f (x : TcpPort.Handle) : Unit := ()",
            "def f (x : Net.Tcp.Connection) : Unit := ()",
            "def f (x : Tcp) : Unit := ()",
            "def f (x : Int) : Unit := connection.Tcp.id",
        ] {
            assert!(!code_names_module(other, "Tcp"), "{other}");
        }
    }

    #[test]
    fn a_module_path_in_an_intents_prose_is_not_a_reference() {
        // The `?` intent of the first external project's `Domain.Interp`
        // mentions `Domain.Policy.none()` in prose; the body never calls it.
        let body = "/-- The flag is Policy: Domain.Rules.at can only build \
                    Domain.Policy.none(). -/\ndef refuses (x : Int) : Bool := true\n\
                    -- Domain.Policy.none again\n";
        let code = code_only(body);
        assert!(!code_names_module(&code, "Domain.Policy"), "{code}");
        // Blanking keeps the line structure, and code outside the comment
        // still reads normally.
        assert_eq!(code.lines().count(), body.lines().count());
        assert!(
            code.contains("def refuses (x : Int) : Bool := true"),
            "{code}"
        );
    }

    #[test]
    fn a_string_literal_keeps_its_contents_so_an_interpolation_still_counts() {
        // `--` and `/-` inside a string must not open a comment, or the code
        // after them — an interpolated call included — would be blanked and
        // its module would lose its import.
        let body = "def label (x : Int) : String := s!\"a--b {Domain.Hash.hex x}\"\n";
        let code = code_only(body);
        assert_eq!(code, body);
        assert!(code_names_module(&code, "Domain.Hash"), "{code}");
    }

    #[test]
    fn nested_block_comments_close_at_the_right_depth() {
        let code = code_only("/- outer /- inner -/ still comment -/ def f := Tcp.x\n");
        assert!(code.trim_start().starts_with("def f := Tcp.x"), "{code:?}");
        assert!(code_names_module(&code, "Tcp"));
    }
}
