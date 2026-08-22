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
//! - **Builtins whose lowering can panic, or can narrow past the VM.** The
//!   literal alone does NOT close the vacuity hole above: a defaulted model
//!   satisfies the equation whenever `default` happens to equal the value the
//!   VM computed. That is not exotic — `Bool`'s default is `false`, and half
//!   of all predicate cases expect `false`. So the second half of the gate is
//!   per-builtin: a lowering that can reach `panic!` on an input a program
//!   can supply is not kernel-eligible. Neither is one that NARROWS an
//!   argument the VM rejected into one it accepts (`Int.toNat` maps every
//!   negative index to `0`), because the model then evaluates a branch the VM
//!   never took — and from that branch any other builtin's `panic!` is back
//!   in play, defaulted and silent. See [`builtin_panic_capability`] for the
//!   audit of every entry against the prelude definition it reaches.
//!
//! - **Oversized cases.** Kernel reduction is real work and, unlike the
//!   elaborator, it has no heartbeat limit to stop it — see
//!   [`KERNEL_DECIDE_TERM_BUDGET`].
//!
//! The remaining question is per-builtin, and two tables answer it: does the
//! lowering REDUCE in the kernel ([`builtin_reduces_in_kernel`], pinned
//! empirically against Lean 4.32), and is the reduction FAITHFUL to what the
//! VM computed ([`builtin_panic_capability`], audited against the prelude).
//! Both are exhaustive matches, so a new `Builtin` variant must be classified
//! on both axes before it compiles.

use std::collections::{HashMap, HashSet};

use crate::ast::{Literal, Spanned, Type, TypeDef};
use crate::codegen::CodegenContext;
use crate::codegen::builtins::{Builtin, recognize_builtin};
use crate::ir::hir::{
    BuiltinIntrinsic, ResolvedCallee, ResolvedCtor, ResolvedExpr, ResolvedPattern, ResolvedStmt,
    ResolvedStrPart,
};
use crate::ir::{FnId, TypeId};

/// Kernel-checked evaluation: no `Lean.ofReduceBool` in the axiom closure.
pub(super) const KERNEL_DECIDE_TACTIC: &str = "decide +kernel";
/// Native evaluation: fast, but the theorem trusts the compiler.
pub(super) const NATIVE_DECIDE_TACTIC: &str = "native_decide";

/// Data-size budget, in characters of the emitted EQUATION — both sides —
/// above which a case keeps `native_decide` no matter how kernel-reducible it
/// is.
///
/// Kernel reduction is not heartbeat-limited — an oversized case does not
/// error out, it just makes `lake build` slow — so the only bound available at
/// emit time is the size of the literal data the term carries. Both sides
/// count: the expected side is the VM ground-truth literal, and the kernel
/// must reduce the `Decidable` instance for the WHOLE equation, so a
/// three-token call returning a four-kilobyte list is exactly as much work as
/// the four-kilobyte argument that produced it. Budgeting the left side alone
/// let that shape through.
///
/// Measured on Lean 4.32 with the most expensive shape the backend has
/// (SHA-256 over a byte-list literal, which also folds the list through the
/// `Bytes` range check and the hex encoder). Each case carries a constant
/// 75-character expected side (`Except.ok "<64 hex chars>"`), so as full
/// equations the measured points read:
///
/// ```text
/// 332 chars (56-byte FIPS vector)   3.1 s
/// 661 chars (128 bytes)             5.5 s
/// 1252 chars (256 bytes)            9.0 s
/// 2422 chars (512 bytes)           21.3 s
/// ```
///
/// 1 KiB admits the 128-byte shape at 5.5 s and declines everything past it,
/// while still leaving three times the headroom the FIPS vectors need.
/// Cheaper shapes (plain Int / List cases) pay far less per character, so the
/// budget rejects some of them needlessly — which costs nothing but a missed
/// opportunity, the same trade every other decline in this module makes.
const KERNEL_DECIDE_TERM_BUDGET: usize = 1024;

/// Per-transpile classifier for sampled `verify` cases.
pub(super) struct CaseDecidability {
    /// Fns whose Lean emission in THIS transpile the kernel cannot see
    /// through (`partial def`, fuel `panic!`, `mutual`, `sorry`, `unsafe`).
    opaque_fns: HashSet<FnId>,
    /// Fns whose declarations in this exact proof export use a fuel seed that
    /// is executable but not a statically justified recursion bound. A ground
    /// claim whose call cone reaches one must never be handed to
    /// `native_decide`: Lean's `panic!` returns `default`, so exhaustion can
    /// make a false equality evaluate to true. The value is the call edge
    /// analysis's refusal for the fn's group when it has one — the call the
    /// exporter could not see shrink — so the claim's decline can say so.
    unbounded_fuel_fns: HashMap<FnId, Option<String>>,
    /// Recursive user type names — they carry the `opaque` `DecidableEq` shim.
    opaque_eq_types: HashSet<String>,
    /// Provider-owned pure operations reached by each user function.  These
    /// declarations are logically opaque and deliberately noncomputable, so
    /// ground cases in their cone are refused before evaluation.
    capability_opacity: super::capability_opaque::CapabilityOpacity,
    /// `false` turns every case back to `native_decide`.
    enabled: bool,
}

impl CaseDecidability {
    pub(super) fn new(
        opaque_fns: HashSet<FnId>,
        unbounded_fuel_fns: HashMap<FnId, Option<String>>,
        opaque_eq_types: HashSet<String>,
        capability_opacity: super::capability_opaque::CapabilityOpacity,
    ) -> Self {
        Self {
            opaque_fns,
            unbounded_fuel_fns,
            opaque_eq_types,
            capability_opacity,
            enabled: true,
        }
    }

    /// Classification off — every case emits `native_decide`.
    pub(super) fn disabled() -> Self {
        Self {
            opaque_fns: HashSet::new(),
            unbounded_fuel_fns: HashMap::new(),
            opaque_eq_types: HashSet::new(),
            capability_opacity: super::capability_opaque::CapabilityOpacity::default(),
            enabled: false,
        }
    }

    /// The refusal for a claim whose `roots` reach a provider-owned capability
    /// operation; `None` when they reach none.
    pub(super) fn capability_decline_reason(
        &self,
        roots: &[&Spanned<crate::ast::Expr>],
        ctx: &CodegenContext,
    ) -> Option<String> {
        if !self.enabled {
            return None;
        }
        self.capability_opacity.decline_reason(roots, ctx)
    }

    /// Canonical names of every function with an unbounded fuel fallback
    /// reachable from `roots`, including transitive user-function calls, each
    /// with its group's refusal when the call edge analysis has one.
    ///
    /// This is deliberately separate from [`Self::closure_is_kernel_decidable`].
    /// Kernel opacity has many causes (`mutual`, `partial`, recursive equality,
    /// Float), and that walk stops at the first one. The soundness gate must
    /// keep walking through all user functions so an opaque wrapper cannot
    /// hide a fuel-lowered callee.
    pub(super) fn unbounded_fuel_dependencies(
        &self,
        roots: &[&Spanned<crate::ast::Expr>],
        ctx: &CodegenContext,
    ) -> Vec<(String, Option<String>)> {
        if !self.enabled || self.unbounded_fuel_fns.is_empty() {
            return Vec::new();
        }

        let scope = ctx.active_module_scope();
        let mut pending = HashSet::new();
        for root in roots {
            let resolved = ctx.resolve_expr(root, scope.as_deref());
            super::decl_order::collect_resolved_fn_refs(&resolved, &mut pending);
        }

        let mut pending: Vec<FnId> = pending.into_iter().collect();
        let mut seen = HashSet::new();
        let mut hits = std::collections::BTreeMap::new();
        while let Some(fn_id) = pending.pop() {
            if !seen.insert(fn_id) {
                continue;
            }
            if let Some(refusal) = self.unbounded_fuel_fns.get(&fn_id) {
                hits.insert(
                    ctx.symbol_table.fn_entry(fn_id).key.canonical(),
                    refusal.clone(),
                );
            }
            let Some(rfd) = ctx.resolved_program.fn_by_id(fn_id) else {
                continue;
            };
            let body = rfd.body.clone();
            let mut callees = HashSet::new();
            for stmt in body.stmts() {
                match stmt {
                    ResolvedStmt::Expr(expr) => {
                        super::decl_order::collect_resolved_fn_refs(expr, &mut callees)
                    }
                    ResolvedStmt::Binding { value, .. } => {
                        super::decl_order::collect_resolved_fn_refs(value, &mut callees)
                    }
                }
            }
            pending.extend(callees.into_iter().filter(|id| !seen.contains(id)));
        }
        hits.into_iter().collect()
    }

    /// Canonical names of kernel-opaque user functions called directly by
    /// `root`.
    ///
    /// A ground case may still be evaluated through a `partial def` by
    /// `native_decide`. A case with a theorem-local symbolic capability
    /// oracle cannot: Lean rejects native evaluation of the free function,
    /// while reverting it produces a universally quantified proposition with
    /// no `Decidable` instance. The caller uses this narrower query only for
    /// that symbolic-oracle shape, so ordinary executable samples keep their
    /// existing native-evaluation path.
    pub(super) fn direct_opaque_dependencies(
        &self,
        root: &Spanned<crate::ast::Expr>,
        ctx: &CodegenContext,
    ) -> Vec<String> {
        if !self.enabled || self.opaque_fns.is_empty() {
            return Vec::new();
        }

        let scope = ctx.active_module_scope();
        let resolved = ctx.resolve_expr(root, scope.as_deref());
        let mut direct = HashSet::new();
        super::decl_order::collect_resolved_fn_refs(&resolved, &mut direct);
        let mut hits = std::collections::BTreeSet::new();
        for fn_id in direct {
            if self.opaque_fns.contains(&fn_id) {
                hits.insert(ctx.symbol_table.fn_entry(fn_id).key.canonical());
            }
        }
        hits.into_iter().collect()
    }

    /// Tactic for one sampled case.
    ///
    /// `lhs` is the case's left side (the side that routes through the model)
    /// and `emitted_lhs` / `emitted_rhs` are the Lean texts the emitter
    /// produced for the two sides. `ground_truth_expected` says the emitted
    /// right side is the literal the VM computed rather than the source RHS —
    /// required, see the module docs. The literal itself needs no walk: it is
    /// pure data of the left side's type, which the walk already proves
    /// kernel-safe. It does count against the budget, though — the kernel
    /// reduces the equation, not the left side.
    pub(super) fn tactic_for(
        &self,
        lhs: &Spanned<crate::ast::Expr>,
        emitted_lhs: &str,
        emitted_rhs: &str,
        ground_truth_expected: bool,
        ctx: &CodegenContext,
    ) -> &'static str {
        if self.enabled
            && ground_truth_expected
            && emitted_lhs.len() + emitted_rhs.len() <= KERNEL_DECIDE_TERM_BUDGET
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
                type_id,
                type_name,
                fields,
            } => {
                self.named_type_is_kernel_safe(*type_id, type_name)
                    && fields.iter().all(|(_, e)| self.expr(e))
            }
            ResolvedExpr::RecordUpdate {
                type_id,
                type_name,
                base,
                updates,
            } => {
                self.named_type_is_kernel_safe(*type_id, type_name)
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
                recognize_builtin(name).is_some_and(builtin_is_kernel_eligible)
            }
            // Total Euclidean `Int` division / modulo — the literal-divisor
            // discharge. Lean's `Int` division reduces in the kernel.
            ResolvedCallee::Intrinsic(intrinsic) => matches!(
                intrinsic,
                BuiltinIntrinsic::IntDivEuclid
                    | BuiltinIntrinsic::IntModEuclid
                    // Literal-count discharge. The count is a syntactic
                    // non-negative literal, so the unguarded prelude
                    // definition is evaluated only where the VM computes
                    // the same value, and it reduces in the kernel.
                    | BuiltinIntrinsic::BitsShiftLeft
                    | BuiltinIntrinsic::BitsShiftRight
                    | BuiltinIntrinsic::BitsLow
            ),
            // Higher-order fn value / resolver give-up.
            ResolvedCallee::LocalSlot { .. } | ResolvedCallee::Unresolved { .. } => false,
        }
    }

    fn ctor(&mut self, ctor: &ResolvedCtor) -> bool {
        match ctor {
            ResolvedCtor::Builtin(_) => true,
            ResolvedCtor::User { type_id, name, .. } => {
                self.named_type_is_kernel_safe(Some(*type_id), name)
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
            Type::Named { id, name } => self.named_type_is_kernel_safe(*id, name),
            // Fn values, uninstantiated type vars, checker-recovery
            // sentinels: nothing positively known.
            Type::Fn(_, _, _) | Type::Var(_) | Type::Invalid => false,
        }
    }

    /// Resolve a named type to its declaration and scan its field types. An
    /// unresolvable name (builtin service record, foreign type) is rejected —
    /// the conservative default.
    ///
    /// IDENTITY: the declaration is found through the `TypeId` the
    /// typechecker stamped on the reference, so two dependency modules'
    /// same-bare-name types are distinct keys here and one cannot be scanned
    /// in the other's place. Only a reference the symbol table never bound
    /// (`id: None` — a builtin service record, a foreign name) falls back to
    /// the source-faithful name, and that fallback either resolves to a
    /// declaration or declines the case.
    fn named_type_is_kernel_safe(&mut self, id: Option<TypeId>, name: &str) -> bool {
        let key = match id {
            Some(type_id) => self.ctx.symbol_table.type_entry(type_id).key.canonical(),
            None => name.to_string(),
        };
        // The recursive-type `DecidableEq` shim is registered under the BARE
        // name, because the Lean surface is flat: `recursive_type_names`
        // collects `type_def_name`, and `emit_recursive_decidable_eq` emits
        // against that same bare name. Asking the bare tail here is the
        // conservative side of that flattening — a same-bare-name twin of a
        // recursive type is rejected along with it.
        let bare = key.rsplit('.').next().unwrap_or(&key);
        if self.opaque_eq_types.contains(bare) {
            return false;
        }
        if !self.seen_types.insert(key.clone()) {
            // Already being scanned higher in this walk; a genuinely
            // recursive type was rejected by `opaque_eq_types` above.
            return true;
        }
        let Some(annotations) = self.field_annotations_of(id, &key) else {
            return false;
        };
        annotations.iter().all(|annotation| {
            let ty = crate::types::parse_type_str(annotation);
            self.type_is_kernel_safe(&ty)
        })
    }

    /// Every field / variant-field type annotation of the declaration the
    /// canonical `key` identifies, across the entry scope and every
    /// dependency module. `None` when no declaration carries that key.
    ///
    /// A STAMPED reference matches one declaration: the one whose own
    /// `type_key_for_decl` canonicalises to the same key. An UNSTAMPED one
    /// has nothing but a source name to go on, so it matches by bare name and
    /// unions whatever it finds — which can only widen the annotation set,
    /// i.e. decline more cases.
    fn field_annotations_of(&self, id: Option<TypeId>, key: &str) -> Option<Vec<String>> {
        let bare = key.rsplit('.').next().unwrap_or(key);
        let mut found = false;
        let mut annotations = Vec::new();
        let all_defs = self
            .ctx
            .type_defs
            .iter()
            .chain(self.ctx.modules.iter().flat_map(|m| m.type_defs.iter()));
        for td in all_defs {
            let matches = match id {
                Some(_) => {
                    crate::codegen::common::type_key_for_decl(self.ctx, td).canonical() == key
                }
                None => crate::codegen::common::type_def_name(td) == bare,
            };
            if !matches {
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

/// Whether a builtin may appear in a `decide +kernel` case at all: its
/// lowering must both REDUCE in the kernel and be FAITHFUL to the value the
/// VM computed.
fn builtin_is_kernel_eligible(builtin: Builtin) -> bool {
    builtin_reduces_in_kernel(builtin) && builtin_panic_capability(builtin).is_kernel_safe()
}

/// How a builtin's Lean lowering can leave the ground the VM pinned.
///
/// The anti-vacuity gate rests on ONE assumption: the equation states the
/// value the program actually computed, so a model that gives up and returns
/// `default` cannot satisfy it. Two things break that assumption, and Lean's
/// kernel breaks both of them silently.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum PanicCapability {
    /// Total in the Lean model, and it agrees with the VM across the whole
    /// argument domain.
    Total,
    /// Reaches a panicking construct (`panic!`, `Array.get!`, `Array.set!`),
    /// but only at indices the definition itself fixes — no argument a
    /// program can supply moves them out of bounds.
    UnreachableByConstruction,
    /// Reaches `panic!` on an input a program can supply. NOT kernel-eligible:
    /// Lean's `panic!` returns `default` and, under kernel reduction, does so
    /// with no diagnostic at all, so the case can prove `default = <literal>`
    /// whenever the two coincide.
    Reachable,
    /// Narrows an argument the VM REJECTED into one it accepts, so the model
    /// evaluates a branch the VM never took. NOT kernel-eligible: the values
    /// then have no VM ground truth behind them, and the branch can reach
    /// another builtin's `panic!` — which is how `Vector.get(v, -1)` ends up
    /// defaulting `Char.toCode ""` to `0`.
    NarrowsPastVm,
}

impl PanicCapability {
    fn is_kernel_safe(self) -> bool {
        match self {
            PanicCapability::Total | PanicCapability::UnreachableByConstruction => true,
            PanicCapability::Reachable | PanicCapability::NarrowsPastVm => false,
        }
    }
}

/// Audit of every builtin's Lean lowering (`lean::builtins::emit_builtin_call`)
/// against the prelude definitions it reaches (`lean::prelude`,
/// `lean::crypto_model`), for the two ways a model can drift off the VM's
/// ground truth. Exhaustive on purpose: a new `Builtin` variant is a compile
/// error here, so it cannot inherit kernel eligibility by default.
fn builtin_panic_capability(builtin: Builtin) -> PanicCapability {
    use Builtin::*;
    use PanicCapability::*;
    match builtin {
        // `Char.toCode` (prelude `LEAN_PRELUDE_CHAR_CODE`) is
        //   `match s.toList.head? with … | none => panic! "…: string is empty"`.
        // The empty string reaches it, and the panic defaults to `0` — equal
        // to the VM's answer for plenty of neighbouring inputs, and equal to
        // nothing the VM ever returns here (the VM raises a RuntimeError).
        CharToCode => Reachable,

        // `Vector.get` lowers to `arr[Int.toNat i]?`. `Int.toNat` maps EVERY
        // negative index to `0`, so a negative index reads element 0 in the
        // model while the VM (`types/vector.rs`, `idx.to_usize()` → `None`)
        // returns `Option.None`. The model then walks the `Some` arm the
        // program never took.
        VectorGet => NarrowsPastVm,
        // `Vector.set` lowers to
        //   `if i < arr.size then some (arr.set! (Int.toNat i) v) else none`.
        // Same narrowing (the VM returns `None` for a negative index), and
        // the guard does not cover it: on an EMPTY array `-1 < 0` holds, so
        // the model calls `Array.set!` out of bounds and panics.
        VectorSet => NarrowsPastVm,
        // `Vector.new` lowers its `Int` size through `Int.toNat`. Negative
        // sizes therefore become an empty model array, while the VM rejects
        // them; values beyond the host's machine-sized range are rejected by
        // the VM too. The lowering builds and is useful under explicit source
        // guards, but an unrestricted claim cannot use kernel evaluation.
        VectorNew => NarrowsPastVm,

        // The four total `Bits` operations are total in the model too:
        // infinite two's complement is defined for every pair of integers,
        // and `AverBits.and`/`or`/`xor`/`not` are closed-form over `Nat`
        // bitwise ops with no partial step anywhere.
        //
        // The three count-taking ones are the interesting case. Their Lean
        // definitions use `Int.toNat` on the count, which sends every
        // negative count to `0` — on its own that would narrow past the VM,
        // which returns `Result.Err` there. It does not, because the
        // lowering GUARDS the count (`if n < 0 then Except.error … else …`,
        // see `lean::builtins`), so the model never evaluates the
        // definition at a count the VM rejected. The guard is load-bearing:
        // remove it and this line becomes a lie.
        BitsAnd | BitsOr | BitsXor | BitsNot | BitsShiftLeft | BitsShiftRight | BitsLow => Total,

        // `Int.toNat` again, but here the VM narrows IDENTICALLY —
        // `list::clamp_count` sends every count `<= 0` to `0` — so no input
        // steers the model off the VM's path.
        ListTake | ListDrop => Total,
        // `String.sliceAv` clamps both bounds with `if x < 0 then 0 else …`,
        // and `runtime::string_slice` clamps to `[0, len]` the same way.
        StringSlice => Total,
        // `String.charAtAv` and `Char.fromCode` GUARD their conversions
        // (`if i < 0 then none`, `if n < 0 || n > 1114111 then none`), so the
        // `.toNat` is only reached where it is exact — matching the VM's
        // `to_usize()` / `to_u32()` `None`s.
        StringCharAt | CharFromCode => Total,

        // `AverCrypto.compress` uses `Array.get!` / `Array.set!`, but every
        // index is fixed by the definition: `words` is `Array.replicate 64`
        // indexed under 64, `constants` / `initial` are 64- and 8-element
        // literals, and the message offsets come from `List.range
        // (message.size / 64)`, which `padded` pads to a multiple of 64. The
        // `Bytes` refinement carries the 0..=255 proof its `UInt8.ofNat
        // byte.toNat` needs.
        CryptoSha256 => UnreachableByConstruction,

        // Plain inductives and their total combinators — `Except.ok/error`,
        // `some`, `Except.withDefault`, `Option.getD`, `Option.toExcept`.
        ResultOk | ResultErr | OptionSome | ResultWithDefault | OptionWithDefault
        | OptionToResult => Total,

        // `Int.natAbs`, `min`/`max`, the zero-guarded `Except`-returning
        // `%`/`/`, and `Int.fromString` (total, `Except`-returning, over the
        // total `AverDigits.parseNatChars`).
        IntAbs | IntMin | IntMax | IntMod | IntDiv | IntFromString => Total,
        // `AverFloat.toInt` saturates at both ends and maps NaN to `0` —
        // total. (Declined by the kernel table below regardless: Float.)
        IntFromFloat => Total,

        // Lean-core Float ops plus the total prelude wrappers
        // (`AverFloat.pow/round/floor/ceil`, `Float.fromString`, which
        // `takeWhile Char.isDigit`-guards its digit arithmetic). None panics.
        // All are declined by the kernel table below regardless: Lean has no
        // kernel-reducible `DecidableEq Float`.
        FloatAbs | FloatSqrt | FloatPow | FloatRound | FloatFloor | FloatCeil | FloatFromInt
        | FloatFromString | FloatPi | FloatMin | FloatMax | FloatSin | FloatCos | FloatAtan2 => {
            Total
        }

        // Lean-core string operations and the total prelude helpers
        // (`String.charsAv`, `AverString.split`, `String.fromInt` over
        // `AverDigits.natDigits`, `String.fromFloat`). No `panic!`, no
        // unguarded narrowing.
        StringLen | StringChars | StringContains | StringStartsWith | StringEndsWith
        | StringTrim | StringSplit | StringJoin | StringReplace | StringToUpper | StringToLower
        | StringFromInt | StringFromFloat | StringByteLength => Total,
        // No prelude definition exists for this one, so nothing can panic;
        // the kernel table below declines it for that same reason.
        StringFromBool => Total,

        BoolOr | BoolAnd | BoolNot => Total,

        // `List.head?` / `tail?` / `take` / `drop` and friends are the total
        // members of Lean's list API — no `head!` / `getElem!` anywhere.
        // `find?` / `any` are total too (declined below: higher-order).
        ListLen | ListHead | ListTail | ListPrepend | ListConcat | ListReverse | ListContains
        | ListZip | ListFind | ListAny => Total,

        // `Array.size` / `List.toArray` / `Array.toList` are total.
        VectorLen | VectorFromList | ListFromVector => Total,

        // `AverMap.*` is a total association-list API (`get` returns `Option`,
        // `remove` is `filter`, `len` is `length`) — no partial accessor.
        MapGet | MapSet | MapHas | MapRemove | MapKeys | MapValues | MapEntries | MapLen
        | MapFromList => Total,
    }
}

/// Whether a builtin's Lean lowering reduces in the KERNEL.
///
/// Pinned empirically against Lean 4.32: every entry was exported through
/// `aver proof --backend lean` as a concrete sample and put to a real
/// `decide +kernel` (`tests/fixtures/kernel_decide_split.av` keeps the
/// discriminating pairs as a regression). The match is exhaustive on purpose —
/// a new `Builtin` variant is a compile error here rather than a silent
/// reclassification. Reducibility is necessary but NOT sufficient: see
/// [`builtin_panic_capability`] for the faithfulness half.
fn builtin_reduces_in_kernel(builtin: Builtin) -> bool {
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
        // Carries a Float; and `String.fromBool` has no prelude definition at
        // all (its native emission is already broken).
        StringFromFloat | StringFromBool => false,

        BoolOr | BoolAnd | BoolNot => true,

        // Bits — `Nat.land` / `Nat.lor` / `Nat.xor` / `Nat.pow` and `Int`
        // arithmetic all have kernel GMP acceleration, so a concrete `Bits`
        // goal reduces to `isTrue`/`isFalse` without `native_decide`.
        // Probed on Lean 4.32 against every case in the specification.
        BitsAnd | BitsOr | BitsXor | BitsNot | BitsShiftLeft | BitsShiftRight | BitsLow => true,

        // Both reduce; `Char.toCode` is nevertheless declined, by the
        // faithfulness table (its `panic!` arm).
        CharToCode | CharFromCode => true,

        // The exported SHA-256 model is total and axiom-free (it folds over a
        // computed block count instead of a kernel-opaque `while`), so a
        // concrete digest reduces in the kernel.
        CryptoSha256 => true,

        ListLen | ListHead | ListTail | ListPrepend | ListTake | ListDrop | ListConcat
        | ListReverse | ListContains | ListZip => true,
        // Take a fn value; the walk cannot follow a higher-order argument.
        ListFind | ListAny => false,

        // All reduce; `Vector.get` / `Vector.set` are nevertheless declined,
        // by the faithfulness table (their `Int.toNat` index narrowing).
        VectorGet | VectorSet | VectorLen | VectorFromList | ListFromVector | VectorNew => true,

        MapGet | MapSet | MapHas | MapRemove | MapKeys | MapValues | MapEntries | MapLen
        | MapFromList => true,
    }
}
