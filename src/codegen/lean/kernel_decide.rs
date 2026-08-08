//! Per-case kernel-decidability classification for sampled `verify` cases.
//!
//! A sampled `verify` case emits as a ground equation (`f <args> = <literal>`),
//! so it is closed by *evaluation*. Two evaluators are available and they buy
//! different things:
//!
//! - `native_decide` runs Lean's compiler/interpreter and asks the kernel to
//!   trust the answer. It puts `Lean.ofReduceBool` into the theorem's axiom
//!   closure — the proof rests on native evaluation, not on the kernel.
//! - `decide +kernel` reduces the `Decidable` instance IN the kernel, so
//!   nothing beyond the kernel is trusted and the axiom closure stays inside
//!   Lean's core three. (The `+kernel` variant is required: plain `decide`
//!   stalls in the elaborator's `whnf` pre-check on these goals.)
//!
//! Kernel reduction only works when everything the case's term mentions can
//! actually unfold in the kernel, so this classifier routes a case to
//! `decide +kernel` only when it can positively establish that.
//!
//! CONSERVATIVE DEFAULT: anything this module does not positively recognise as
//! kernel-reducible emits `native_decide` — an unknown callee, an unresolvable
//! named type, an effectful fn, a higher-order argument, a new `Builtin`
//! variant. A wrong `native_decide` costs an axiom; a wrong `decide +kernel`
//! breaks the user's `lake build`.
//!
//! What the classifier rejects, and why:
//!
//! - **Float.** Lean ships no `DecidableEq Float`; the prelude supplies one
//!   through an `@[implemented_by]` `opaque` constant the kernel can never
//!   reduce. Any Float reachable through the closure — literal, signature,
//!   record field, or `Float.*` / `Int.fromFloat` / `String.fromFloat`
//!   builtin — disqualifies the case.
//! - **Recursive user types.** They carry the same `@[implemented_by]`
//!   `opaque` `DecidableEq` shim (`emit_recursive_decidable_eq`).
//! - **Kernel-opaque emissions.** Whatever this transpile actually spelled as
//!   `partial def`, `unsafe`, `opaque`, a `sorry`-floored proof, a `panic!`
//!   arm (the fuel wrappers), or a `mutual` group. The fact is read off the
//!   text the emitter produced — see `transpile::component_is_kernel_opaque` —
//!   not re-derived from the source shape, so a new emission strategy cannot
//!   silently widen the kernel path.
//! - **Cases without a VM ground-truth literal.** The expected side is then
//!   the source RHS, which routes through the model too. Lean's `panic!` does
//!   not abort: it returns `default`, printing `PANIC at …` under native
//!   evaluation (which `aver proof --check` charges as a hard failure) but
//!   reducing SILENTLY in the kernel. Requiring the literal keeps the equation
//!   pinned to the value the program actually computed, so a defaulted model
//!   cannot satisfy it and the anti-vacuity gate stays meaningful.
//!
//! - **Oversized cases.** Kernel reduction is real work and, unlike the
//!   elaborator, it has no heartbeat limit to stop it — see
//!   [`KERNEL_DECIDE_TERM_BUDGET`].
//!
//! The remaining question is per-builtin, and [`builtin_is_kernel_reducible`]
//! answers it from a table pinned empirically against Lean 4.32.

use std::collections::HashSet;

use crate::ast::{Literal, Spanned, Type, TypeDef};
use crate::codegen::CodegenContext;
use crate::codegen::builtins::{Builtin, recognize_builtin};
use crate::ir::FnId;
use crate::ir::hir::{
    BuiltinIntrinsic, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedPattern, ResolvedStmt,
    ResolvedStrPart,
};

/// Kernel-checked evaluation: no `Lean.ofReduceBool` in the axiom closure.
pub(super) const KERNEL_DECIDE_TACTIC: &str = "decide +kernel";
/// Native evaluation: fast, but the theorem trusts the compiler.
pub(super) const NATIVE_DECIDE_TACTIC: &str = "native_decide";

/// Data-size budget, in characters of the emitted left-hand term, above which
/// a case keeps `native_decide` no matter how kernel-reducible it is.
///
/// Kernel reduction is not heartbeat-limited — an oversized case does not
/// error out, it just makes `lake build` slow — so the only bound available at
/// emit time is the size of the literal data the term carries. Measured on
/// Lean 4.32 with the most expensive shape the backend has (SHA-256 over a
/// byte-list literal, which also folds the list through the `Bytes` range
/// check and the hex encoder):
///
/// ```text
/// 257 chars (56-byte FIPS vector)   3.1 s
/// 586 chars (128 bytes)             5.5 s
/// 1177 chars (256 bytes)            9.0 s
/// 2347 chars (512 bytes)           21.3 s
/// ```
///
/// 1 KiB keeps that worst shape under ten seconds while leaving four times the
/// headroom the FIPS vectors need. Cheaper shapes (plain Int / List cases) pay
/// far less per character, so the budget rejects some of them needlessly —
/// which costs nothing but a missed opportunity, the same trade every other
/// decline in this module makes.
const KERNEL_DECIDE_TERM_BUDGET: usize = 1024;

/// Per-transpile classifier for sampled `verify` cases.
pub(super) struct CaseDecidability {
    /// Fns whose Lean emission in THIS transpile the kernel cannot see
    /// through (`partial def`, fuel `panic!`, `mutual`, `sorry`, `unsafe`).
    opaque_fns: HashSet<FnId>,
    /// Recursive user type names — they carry the `opaque` `DecidableEq` shim.
    opaque_eq_types: HashSet<String>,
    /// `false` turns every case back to `native_decide`.
    enabled: bool,
}

impl CaseDecidability {
    pub(super) fn new(opaque_fns: HashSet<FnId>, opaque_eq_types: HashSet<String>) -> Self {
        Self {
            opaque_fns,
            opaque_eq_types,
            enabled: true,
        }
    }

    /// Classification off — every case emits `native_decide`.
    pub(super) fn disabled() -> Self {
        Self {
            opaque_fns: HashSet::new(),
            opaque_eq_types: HashSet::new(),
            enabled: false,
        }
    }

    /// Tactic for one sampled case.
    ///
    /// `lhs` is the case's left side (the side that routes through the model)
    /// and `emitted_lhs` is the Lean text the emitter produced for it.
    /// `ground_truth_expected` says the emitted right side is the literal the
    /// VM computed rather than the source RHS — required, see the module docs.
    /// The literal itself needs no walk: it is pure data of the left side's
    /// type, which the walk already proves kernel-safe.
    pub(super) fn tactic_for(
        &self,
        lhs: &Spanned<crate::ast::Expr>,
        emitted_lhs: &str,
        ground_truth_expected: bool,
        ctx: &CodegenContext,
    ) -> &'static str {
        if self.enabled
            && ground_truth_expected
            && emitted_lhs.len() <= KERNEL_DECIDE_TERM_BUDGET
            && self.closure_is_kernel_decidable(lhs, ctx)
        {
            KERNEL_DECIDE_TACTIC
        } else {
            NATIVE_DECIDE_TACTIC
        }
    }

    fn closure_is_kernel_decidable(
        &self,
        lhs: &Spanned<crate::ast::Expr>,
        ctx: &CodegenContext,
    ) -> bool {
        let scope = ctx.active_module_scope();
        let resolved = ctx.resolve_expr(lhs, scope.as_deref());
        let mut walk = Walk {
            opaque_fns: &self.opaque_fns,
            opaque_eq_types: &self.opaque_eq_types,
            ctx,
            seen_fns: HashSet::new(),
            seen_types: HashSet::new(),
            pending: Vec::new(),
        };
        if !walk.expr(&resolved) {
            return false;
        }
        while let Some(fn_id) = walk.pending.pop() {
            if !walk.fn_def(fn_id) {
                return false;
            }
        }
        true
    }
}

struct Walk<'a> {
    opaque_fns: &'a HashSet<FnId>,
    opaque_eq_types: &'a HashSet<String>,
    ctx: &'a CodegenContext,
    seen_fns: HashSet<FnId>,
    seen_types: HashSet<String>,
    pending: Vec<FnId>,
}

impl Walk<'_> {
    /// One callee's signature plus its whole body.
    fn fn_def(&mut self, fn_id: FnId) -> bool {
        if !self.seen_fns.insert(fn_id) {
            return true;
        }
        if self.opaque_fns.contains(&fn_id) {
            return false;
        }
        let Some(rfd) = self.ctx.resolved_program.fn_by_id(fn_id) else {
            // Synthetic / un-indexed fn: no body to inspect.
            return false;
        };
        // Effectful fns emit through the Oracle lifting path (extra oracle
        // params, stub injection). Out of scope for kernel classification.
        if !rfd.effects.is_empty() {
            return false;
        }
        if !self.type_is_kernel_safe(&rfd.return_type) {
            return false;
        }
        for (_, ty) in &rfd.params {
            if !self.type_is_kernel_safe(ty) {
                return false;
            }
        }
        // `rfd` borrows `self.ctx`; the body is behind an `Arc`, so clone the
        // handle and drop the borrow before recursing with `&mut self`.
        let body = rfd.body.clone();
        for stmt in body.stmts() {
            let ok = match stmt {
                ResolvedStmt::Expr(e) => self.expr(e),
                ResolvedStmt::Binding { ty_ann, value, .. } => {
                    ty_ann
                        .as_ref()
                        .is_none_or(|ty| self.type_is_kernel_safe(ty))
                        && self.expr(value)
                }
            };
            if !ok {
                return false;
            }
        }
        true
    }

    fn exprs(&mut self, items: &[Spanned<ResolvedExpr>]) -> bool {
        items.iter().all(|e| self.expr(e))
    }

    fn expr(&mut self, expr: &Spanned<ResolvedExpr>) -> bool {
        match &expr.node {
            ResolvedExpr::Literal(lit) => !matches!(lit, Literal::Float(_)),
            // A local slot's value came from a param, a binding, or a pattern
            // binder — each already visited by this walk.
            ResolvedExpr::Resolved { .. } => true,
            // A name the resolver left unclassified (top-level binding, fn
            // value). Nothing to unfold from here.
            ResolvedExpr::Ident(_) => false,
            ResolvedExpr::Attr(obj, _) => self.expr(obj),
            ResolvedExpr::Neg(inner) | ResolvedExpr::ErrorProp(inner) => self.expr(inner),
            ResolvedExpr::BinOp(_, l, r) => self.expr(l) && self.expr(r),
            ResolvedExpr::List(items)
            | ResolvedExpr::Tuple(items)
            | ResolvedExpr::IndependentProduct(items, _) => self.exprs(items),
            ResolvedExpr::MapLiteral(entries) => {
                entries.iter().all(|(k, v)| self.expr(k) && self.expr(v))
            }
            ResolvedExpr::InterpolatedStr(parts) => parts.iter().all(|part| match part {
                ResolvedStrPart::Literal(_) => true,
                ResolvedStrPart::Parsed(e) => self.expr(e),
            }),
            ResolvedExpr::Match { subject, arms } => {
                self.expr(subject)
                    && arms
                        .iter()
                        .all(|arm| self.pattern(&arm.pattern) && self.expr(&arm.body))
            }
            ResolvedExpr::Ctor(ctor, args) => self.ctor(ctor) && self.exprs(args),
            ResolvedExpr::RecordCreate {
                type_name, fields, ..
            } => {
                self.named_type_is_kernel_safe(type_name)
                    && fields.iter().all(|(_, e)| self.expr(e))
            }
            ResolvedExpr::RecordUpdate {
                type_name,
                base,
                updates,
                ..
            } => {
                self.named_type_is_kernel_safe(type_name)
                    && self.expr(base)
                    && updates.iter().all(|(_, e)| self.expr(e))
            }
            ResolvedExpr::TailCall { target, args } => {
                self.enqueue(*target);
                self.exprs(args)
            }
            ResolvedExpr::Call(callee, args) => self.callee(callee) && self.exprs(args),
        }
    }

    fn callee(&mut self, callee: &ResolvedCallee) -> bool {
        match callee {
            ResolvedCallee::Fn(id) => {
                self.enqueue(*id);
                true
            }
            ResolvedCallee::Builtin(name) => {
                recognize_builtin(name).is_some_and(builtin_is_kernel_reducible)
            }
            // Total Euclidean `Int` division / modulo — the literal-divisor
            // discharge. Lean's `Int` division reduces in the kernel.
            ResolvedCallee::Intrinsic(intrinsic) => matches!(
                intrinsic,
                BuiltinIntrinsic::IntDivEuclid | BuiltinIntrinsic::IntModEuclid
            ),
            // Higher-order fn value / resolver give-up.
            ResolvedCallee::LocalSlot { .. } | ResolvedCallee::Unresolved { .. } => false,
        }
    }

    fn ctor(&mut self, ctor: &ResolvedCtor) -> bool {
        match ctor {
            ResolvedCtor::Builtin(_) => true,
            ResolvedCtor::User { type_id, .. } => {
                let type_name = self.ctx.symbol_table.type_entry(*type_id).key.name.clone();
                self.named_type_is_kernel_safe(&type_name)
            }
            ResolvedCtor::Unresolved { .. } => false,
        }
    }

    fn pattern(&mut self, pattern: &ResolvedPattern) -> bool {
        match pattern {
            ResolvedPattern::Wildcard
            | ResolvedPattern::Ident(_)
            | ResolvedPattern::EmptyList
            | ResolvedPattern::Cons(_, _) => true,
            ResolvedPattern::Literal(lit) => !matches!(lit, Literal::Float(_)),
            ResolvedPattern::Tuple(items) => items.iter().all(|p| self.pattern(p)),
            ResolvedPattern::Ctor(ctor, _) => self.ctor(ctor),
        }
    }

    fn enqueue(&mut self, fn_id: FnId) {
        if !self.seen_fns.contains(&fn_id) {
            self.pending.push(fn_id);
        }
    }

    /// `true` iff no Float and no `opaque`-`DecidableEq` type can reach the
    /// emitted term through this type.
    fn type_is_kernel_safe(&mut self, ty: &Type) -> bool {
        match ty {
            Type::Int | Type::Str | Type::Bool | Type::Unit => true,
            Type::Float => false,
            Type::Option(inner) | Type::List(inner) | Type::Vector(inner) => {
                self.type_is_kernel_safe(inner)
            }
            Type::Result(a, b) | Type::Map(a, b) => {
                self.type_is_kernel_safe(a) && self.type_is_kernel_safe(b)
            }
            Type::Tuple(items) => items.iter().all(|item| self.type_is_kernel_safe(item)),
            Type::Named { name, .. } => self.named_type_is_kernel_safe(name),
            // Fn values, uninstantiated type vars, checker-recovery
            // sentinels: nothing positively known.
            Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => false,
        }
    }

    /// Resolve a named type to its declaration and scan its field types. An
    /// unresolvable name (builtin service record, foreign type) is rejected —
    /// the conservative default.
    fn named_type_is_kernel_safe(&mut self, name: &str) -> bool {
        let bare = name.rsplit('.').next().unwrap_or(name).to_string();
        if self.opaque_eq_types.contains(&bare) {
            return false;
        }
        if !self.seen_types.insert(bare.clone()) {
            // Already being scanned higher in this walk; a genuinely
            // recursive type was rejected by `opaque_eq_types` above.
            return true;
        }
        let Some(annotations) = self.field_annotations_of(&bare) else {
            return false;
        };
        annotations.iter().all(|annotation| {
            let ty = crate::types::parse_type_str(annotation);
            self.type_is_kernel_safe(&ty)
        })
    }

    /// Every field / variant-field type annotation declared under the bare
    /// name `bare`, across the entry scope and every dependency module.
    /// `None` when no declaration carries that name.
    fn field_annotations_of(&self, bare: &str) -> Option<Vec<String>> {
        let mut found = false;
        let mut annotations = Vec::new();
        let all_defs = self
            .ctx
            .type_defs
            .iter()
            .chain(self.ctx.modules.iter().flat_map(|m| m.type_defs.iter()));
        for td in all_defs {
            if crate::codegen::common::type_def_name(td) != bare {
                continue;
            }
            found = true;
            match td {
                TypeDef::Product { fields, .. } => {
                    annotations.extend(fields.iter().map(|(_, ty)| ty.clone()));
                }
                TypeDef::Sum { variants, .. } => {
                    annotations.extend(variants.iter().flat_map(|v| v.fields.iter().cloned()));
                }
            }
        }
        found.then_some(annotations)
    }
}

/// Whether a builtin's Lean lowering reduces in the KERNEL.
///
/// Pinned empirically against Lean 4.32: every entry was exported through
/// `aver proof --backend lean` as a concrete sample and put to a real
/// `decide +kernel` (`tests/fixtures/kernel_decide_split.av` keeps the
/// discriminating pairs as a regression). The match is exhaustive on purpose —
/// a new `Builtin` variant is a compile error here rather than a silent
/// reclassification.
fn builtin_is_kernel_reducible(builtin: Builtin) -> bool {
    use Builtin::*;
    match builtin {
        // Result / Option — plain inductives.
        ResultOk | ResultErr | OptionSome | ResultWithDefault | OptionWithDefault
        | OptionToResult => true,

        // Int — literals and arithmetic have kernel GMP acceleration.
        IntAbs | IntFromString | IntMin | IntMax | IntMod | IntDiv => true,
        // Lowers through `AverFloat.toInt`.
        IntFromFloat => false,

        // Float — `DecidableEq Float` is an `@[implemented_by]` `opaque`
        // constant, and Float literals are `OfScientific` applications the
        // kernel does not evaluate. No Float goal reduces.
        FloatAbs | FloatSqrt | FloatPow | FloatRound | FloatFloor | FloatCeil | FloatFromInt
        | FloatFromString | FloatPi | FloatMin | FloatMax | FloatSin | FloatCos | FloatAtan2 => {
            false
        }

        // String — prelude helpers plus Lean core operations that unfold on
        // concrete strings.
        StringLen | StringCharAt | StringChars | StringSlice | StringStartsWith
        | StringEndsWith | StringSplit | StringJoin | StringToUpper | StringToLower
        | StringFromInt | StringByteLength => true,
        // Probed stuck on Lean 4.32: `containsSubstr` goes through
        // `String.Slice` iteration, `trim`/`replace` through `String.Pos`
        // arithmetic — the kernel does not get these to `isTrue`/`isFalse`.
        StringContains | StringTrim | StringReplace => false,
        // Unreachable from source today (the checker registers no signature),
        // so they have never been probed.
        StringRepeat | StringIndexOf => false,
        // Carries a Float; and `String.fromBool` has no prelude definition at
        // all (its native emission is already broken).
        StringFromFloat | StringFromBool => false,

        BoolOr | BoolAnd | BoolNot => true,

        CharToCode | CharFromCode => true,

        // The exported SHA-256 model is total and axiom-free (it folds over a
        // computed block count instead of a kernel-opaque `while`), so a
        // concrete digest reduces in the kernel.
        CryptoSha256 => true,

        ListLen | ListHead | ListTail | ListPrepend | ListTake | ListDrop | ListConcat
        | ListReverse | ListContains | ListZip => true,
        // Take a fn value; the walk cannot follow a higher-order argument.
        ListFind | ListAny => false,

        VectorGet | VectorSet | VectorLen | VectorFromList | ListFromVector => true,
        // Lowers to `Array.mkArray`, which Lean 4.32 no longer defines.
        VectorNew => false,

        MapGet | MapSet | MapHas | MapRemove | MapKeys | MapValues | MapEntries | MapLen
        | MapFromList => true,
    }
}
