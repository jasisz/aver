//! Diagnostic coverage report for `--explain-mir-coverage --target
//! wasm-gc`: a ctx-free structural predicate (`mir_expr_coverable`)
//! approximating the dispatcher's reach. NOT a release/gate signal —
//! the real source of truth is `emit_fn_body_via_mir` returning
//! `Some`/`None` (the byte-differential test).

use super::*;

/// Backend reach over a lowered [`MirProgram`]: how many fns the
/// wasm-gc MIR walker emits standalone vs. how many would fall back to
/// the `ResolvedExpr` emitter. Mirror of
/// [`crate::codegen::rust::from_mir::CoverageReport`]; drives
/// `aver compile --explain-mir-coverage --target wasm-gc`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct CoverageReport {
    /// Total fns in the lowered program.
    pub total: usize,
    /// Fns whose entire body the walker emits standalone.
    pub mir_covered: usize,
    /// Fns that hit at least one unsupported variant (HIR fallback).
    pub hir_fallback: usize,
}

impl CoverageReport {
    /// Walker reach as a fraction of total fns. `0.0` for an empty
    /// program.
    pub fn ratio(&self) -> f64 {
        if self.total == 0 {
            0.0
        } else {
            self.mir_covered as f64 / self.total as f64
        }
    }
}

/// Walk every fn in `program` and report wasm-gc backend reach. Uses
/// the pure structural predicate [`mir_expr_coverable`] (no emission /
/// registry needed) so the diagnostic can run from the CLI without a
/// full module context. The predicate is kept in lock-step with
/// [`emit_mir_expr`]'s `Some` / `None` decisions; the byte-differential
/// test is the gate that catches drift.
pub fn coverage_report(program: &MirProgram) -> CoverageReport {
    let mut report = CoverageReport::default();
    for (_, mir_fn) in program.iter() {
        report.total += 1;
        if mir_expr_coverable(&mir_fn.body) {
            report.mir_covered += 1;
        } else {
            report.hir_fallback += 1;
        }
    }
    report
}

/// `true` when [`emit_mir_expr`] would emit `expr` standalone (no
/// `None`). Structural mirror of the emitter's match arms.
pub(crate) fn mir_expr_coverable(expr: &Spanned<MirExpr>) -> bool {
    match &expr.node {
        MirExpr::Literal(_) | MirExpr::Local(_) => true,
        MirExpr::BinOp(spanned_binop) => {
            let bop = &spanned_binop.node;
            // Numeric ops or the `String` concat / eq / compare ops.
            // Compound-type eq helpers fall back.
            let numeric = matches!(bop.lhs.ty(), Some(Type::Int) | Some(Type::Float));
            let string_op = aver_type_str_of(&bop.lhs).trim() == "String"
                && matches!(
                    bop.op,
                    BinOp::Add
                        | BinOp::Eq
                        | BinOp::Neq
                        | BinOp::Lt
                        | BinOp::Gt
                        | BinOp::Lte
                        | BinOp::Gte
                );
            (numeric || string_op) && mir_expr_coverable(&bop.lhs) && mir_expr_coverable(&bop.rhs)
        }
        MirExpr::Neg(inner) | MirExpr::Return(inner) => mir_expr_coverable(inner),
        MirExpr::Let(spanned_let) => {
            // Named bindings and statement-sequencing synthetic lets
            // (empty `binding_name`) are both covered.
            let l = &spanned_let.node;
            mir_expr_coverable(&l.value) && mir_expr_coverable(&l.body)
        }
        MirExpr::Call(spanned_call) => {
            matches!(spanned_call.node.callee, MirCallee::Fn(_))
                && spanned_call.node.args.iter().all(mir_expr_coverable)
        }
        MirExpr::TailCall(spanned_tc) => spanned_tc.node.args.iter().all(mir_expr_coverable),
        MirExpr::Construct(spanned_ctor) => spanned_ctor.node.args.iter().all(mir_expr_coverable),
        MirExpr::RecordCreate(spanned_rec) => spanned_rec
            .node
            .fields
            .iter()
            .all(|f| mir_expr_coverable(&f.value)),
        MirExpr::RecordUpdate(spanned_upd) => {
            mir_expr_coverable(&spanned_upd.node.base)
                && spanned_upd
                    .node
                    .updates
                    .iter()
                    .all(|f| mir_expr_coverable(&f.value))
        }
        MirExpr::Project(spanned_proj) => mir_expr_coverable(&spanned_proj.node.base),
        MirExpr::Tuple(items) => items.iter().all(mir_expr_coverable),
        MirExpr::MapLiteral(entries) => entries
            .iter()
            .all(|(k, v)| mir_expr_coverable(k) && mir_expr_coverable(v)),
        MirExpr::List(items) => items.iter().all(mir_expr_coverable),
        MirExpr::Try(inner) => mir_expr_coverable(inner),
        MirExpr::IndependentProduct(spanned_ip) => {
            spanned_ip.node.items.iter().all(mir_expr_coverable)
        }
        MirExpr::InterpolatedStr(parts) => parts.iter().all(|p| match p {
            // Coarse: a compound-type `Expr` part falls back at emit
            // time (the registry-free predicate can't see the type), a
            // tolerable over-count for `--explain-mir-coverage`.
            MirStrPart::Literal(_) => true,
            MirStrPart::Expr(e) => mir_expr_coverable(e),
        }),
        MirExpr::Match(spanned_match) => {
            // Coarse, ctx-free mirror of `emit_mir_match`'s dispatch (the
            // predicate has no registry, so it can't model the Map.get
            // fused-Option fallback — a tolerable over-count, since this
            // only feeds `--explain-mir-coverage`; the real per-fn
            // dispatch in `emit_mir_match` is what the byte-differential
            // test checks). Tuple-pattern arms fall back to the
            // `ResolvedExpr` emitter (mirror of `emit_mir_match`).
            let m = &spanned_match.node;
            let unsupported_pat = m
                .arms
                .iter()
                .any(|a| matches!(a.pattern, MirPattern::Tuple(_)));
            // A primitive-subject match takes the Bool/Int/String branches
            // (literal / wildcard arms only; `Bind` falls back); a
            // Result/Option match carries built-in constructor arms; a
            // list match carries `[]` / `[head, ..tail]` arms.
            let is_primitive = matches!(m.subject.ty(), Some(Type::Bool | Type::Int | Type::Str))
                && !m.arms.iter().any(|a| {
                    matches!(
                        a.pattern,
                        MirPattern::Bind(..)
                            | MirPattern::Ctor { .. }
                            | MirPattern::EmptyList
                            | MirPattern::Cons { .. }
                    )
                });
            let is_result_or_option = m.arms.iter().any(arm_is_mir_result_ctor)
                || m.arms.iter().any(arm_is_mir_option_ctor);
            let is_list = m
                .arms
                .iter()
                .any(|a| matches!(a.pattern, MirPattern::EmptyList | MirPattern::Cons { .. }));
            // A user-variant (sum type) match carries `MirCtor::User`
            // constructor arms (single-arm destructure or multi-arm
            // `ref.test` cascade).
            let is_variant = m.arms.iter().any(|a| {
                matches!(
                    a.pattern,
                    MirPattern::Ctor {
                        ctor: MirCtor::User(_),
                        ..
                    }
                )
            });
            !m.arms.is_empty()
                && !unsupported_pat
                && (is_primitive || is_result_or_option || is_list || is_variant)
                && mir_expr_coverable(&m.subject)
                && m.arms.iter().all(|a| mir_expr_coverable(&a.body))
        }
        _ => false,
    }
}
