//! Machine-word versions of the functions MIR's word-speculation summary
//! names (`crate::ir::mir::word_speculation`), and the entries that run
//! them.
//!
//! Each twin `__word_f` computes `f` on `i64`: every operation is checked,
//! and the first one whose result leaves the word abandons the whole call
//! tree by unwinding with a private payload (`__spec_abort`, which never runs
//! the panic hook). A twin calls only twins. An entry `f` keeps its name and
//! signature, so every caller is unchanged; its exact body becomes
//! `__exact_f`, and `f` runs the twin when every `Int` argument is an inline
//! value, catches the abandonment, and then runs `__exact_f` on the original
//! arguments. A per-entry counter turns the twin off for that entry after a
//! few abandonments, so an entry that always overflows pays the unwinding a
//! bounded number of times. Both change only how fast the program runs: the
//! twin's result, when it has one, is the exact result, because `i64`
//! arithmetic that does not overflow is integer arithmetic.
//!
//! The exact body of an entry calls the exact bodies of the entries of its
//! own cycle, so a restart does not start over again at every level of the
//! recursion; a call into another cycle goes through that cycle's entry.
//!
//! A build with `panic = "abort"` emits none of this: abandoning the tree
//! needs unwinding. The entry also checks `cfg!(panic = "unwind")`, so a
//! project whose profile is edited by hand still computes exactly.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use crate::ast::{BinOp, Literal, Spanned};
use crate::ir::FnId;
use crate::ir::hir::BuiltinCtor;
use crate::ir::mir::int_tree::{self, IntOp};
use crate::ir::mir::word_speculation::{WordKind, division};
use crate::ir::mir::{FnBareFacts, MirCallee, MirCtor, MirExpr, MirFn, MirPattern, MirProgram};

use super::CodegenContext;
use crate::codegen::RustWordSpec;

/// After this many abandonments an entry runs its exact body only.
const ABORTS_BEFORE_OFF: u32 = 8;

/// The twins and entries of `program` (the MIR the backend shares with the
/// other targets, before its own representation rewrite).
pub(super) fn plan(
    program: &MirProgram,
    ctx: &CodegenContext,
    panic_unwinds: bool,
) -> RustWordSpec {
    if !panic_unwinds {
        return RustWordSpec::default();
    }
    let summary = crate::ir::mir::word_speculation::analyze(program);
    if summary.is_empty() {
        return RustWordSpec::default();
    }
    // The twins live together in `runtime_support.rs`, whether or not the
    // module of their function is emitted; a name two modules share gets a
    // number.
    let mut names = BTreeMap::new();
    let mut taken = BTreeSet::new();
    for id in &summary.twins {
        let base = twin_name(&ctx.symbol_table.fn_entry(*id).key.name);
        let mut name = base.clone();
        let mut k = 1;
        while !taken.insert(name.clone()) {
            k += 1;
            name = format!("{base}_{k}");
        }
        names.insert(*id, name);
    }
    let mut twins = HashMap::new();
    for id in &summary.twins {
        let Some(f) = program.fn_by_id(*id) else {
            return RustWordSpec::default();
        };
        // Every twin or none: a twin calls the others by name.
        let Some(code) = Twin::new(f, program, &names).emit() else {
            return RustWordSpec::default();
        };
        twins.insert(*id, code);
    }
    RustWordSpec {
        summary,
        twins,
        names,
        wrapped: BTreeSet::new(),
    }
}

/// The twins, for `runtime_support.rs`, in a stable order.
pub(super) fn twin_items(ctx: &CodegenContext) -> String {
    let spec = &ctx.rust_word_spec;
    let mut out = String::from(RUNTIME);
    for (id, name) in &spec.names {
        if let Some(code) = spec.twins.get(id) {
            out.push_str("\n\n");
            out.push_str(TWIN_ALLOWS);
            // A twin outside every cycle is a step of its callers' loops.
            if !matches!(spec.summary.entries.get(id), Some(Some(_))) {
                out.push_str(INLINE_ATTR);
            }
            out.push_str(&format!("pub fn {name}{code}"));
        }
    }
    out
}

/// Choose the entries this backend wraps, once the representation rewrite
/// has settled signatures, and keep only the twins they reach. An entry with
/// a string-walk check on entry, a trampoline signature or the guest entry's
/// wrappers keeps its plain body.
pub(super) fn choose_wrapped(ctx: &mut CodegenContext) {
    let mut wrapped = BTreeSet::new();
    for id in ctx.rust_word_spec.summary.entries.keys() {
        let no_check = ctx
            .bare_i64
            .for_fn(*id)
            .is_none_or(|f| f.entry_check.is_none())
            && ctx
                .mir_program
                .as_ref()
                .and_then(|p| p.fn_by_id(*id))
                .is_some_and(|f| f.repr.entry_check.is_none());
        let name = &ctx.symbol_table.fn_entry(*id).key.name;
        let guest = ctx.guest_entry.as_deref() == Some(name.as_str());
        if no_check && !guest && !ctx.mutual_tco_members.contains(id) {
            wrapped.insert(*id);
        }
    }
    let reached = ctx
        .rust_word_spec
        .summary
        .reached_from(wrapped.iter().copied());
    ctx.rust_word_spec
        .twins
        .retain(|id, _| reached.contains(id));
    ctx.rust_word_spec.wrapped = wrapped;
}

/// The name of the exact body of a wrapped entry.
pub(super) fn exact_name(name: &str) -> String {
    format!("__exact_{name}")
}

fn twin_name(name: &str) -> String {
    format!("__word_{name}")
}

/// The Rust path of `id`'s item called `item`.
fn path(id: FnId, ctx: &CodegenContext, item: &str) -> String {
    match ctx.symbol_table.fn_entry(id).key.scope_str() {
        Some(prefix) => format!(
            "{}::{item}",
            crate::codegen::common::module_prefix_to_rust_path(prefix)
        ),
        None => format!("crate::aver_generated::entry::{item}"),
    }
}

/// The calls the exact body of `id` makes to exact bodies: the wrapped
/// entries of its own cycle.
pub(super) fn exact_callees(id: FnId, ctx: &CodegenContext) -> BTreeMap<FnId, String> {
    let spec = &ctx.rust_word_spec;
    spec.wrapped
        .iter()
        .filter(|other| spec.summary.same_cycle(id, **other))
        .map(|other| {
            let name = &ctx.symbol_table.fn_entry(*other).key.name;
            (*other, path(*other, ctx, &exact_name(name)))
        })
        .collect()
}

/// What follows the exact body of `id`: its entry, when it is wrapped.
pub(super) fn items_after(id: FnId, visibility: &str, ctx: &CodegenContext) -> String {
    let spec = &ctx.rust_word_spec;
    if !spec.wrapped.contains(&id) {
        return String::new();
    }
    let (Some(f), Some(twin)) = (
        ctx.mir_program.as_ref().and_then(|p| p.fn_by_id(id)),
        spec.names.get(&id),
    ) else {
        return String::new();
    };
    let name = &ctx.symbol_table.fn_entry(id).key.name;
    entry(f, name, twin, visibility, ctx.bare_i64.for_fn(id))
}

const INLINE_ATTR: &str = "#[inline(always)]\n";

/// A twin is written for the machine, not read: the parentheses and arms it
/// adds to stay correct in every position are not worth a warning.
const TWIN_ALLOWS: &str = "#[allow(unused_mut, unused_parens, unused_braces, unused_variables, unreachable_patterns, unreachable_code, clippy::all)]\n";

fn rust_type(kind: WordKind) -> &'static str {
    match kind {
        WordKind::Int => "aver_rt::AverInt",
        WordKind::Bool => "bool",
        WordKind::OptionInt => "Option<aver_rt::AverInt>",
    }
}

fn word_type(kind: WordKind) -> &'static str {
    match kind {
        WordKind::Int => "i64",
        WordKind::Bool => "bool",
        WordKind::OptionInt => "Option<i64>",
    }
}

fn kind_of(ty: &str) -> WordKind {
    WordKind::of_annotation(ty).expect("a twin's signature has word kinds")
}

/// The entry `f`: the twin under a restart point, then the exact body. Its
/// signature is the exact body's: an `Int` the representation analysis
/// proved within `i64` is a raw `i64` there, and here.
fn entry(
    f: &MirFn,
    name: &str,
    twin: &str,
    visibility: &str,
    facts: Option<&FnBareFacts>,
) -> String {
    let kinds: Vec<WordKind> = f.params.iter().map(|p| kind_of(&p.ty)).collect();
    let ret = kind_of(&f.return_type);
    let bare_param = |i: usize| facts.is_some_and(|f| f.param_is_bare(i));
    let bare_return = facts.is_some_and(|f| f.bare_return) && ret == WordKind::Int;
    let params = kinds
        .iter()
        .enumerate()
        .map(|(i, k)| {
            let ty = if bare_param(i) { "i64" } else { rust_type(*k) };
            format!("__p{i}: {ty}")
        })
        .collect::<Vec<_>>()
        .join(", ");
    let mut words = Vec::new();
    let mut twin_args = Vec::new();
    for (i, k) in kinds.iter().enumerate() {
        match k {
            WordKind::Int if bare_param(i) => twin_args.push(format!("__p{i}")),
            WordKind::Int => {
                words.push(format!(
                    "let Some(__w{i}) = __p{i}.inline_word() else {{ break 'word; }};"
                ));
                twin_args.push(format!("__w{i} >> 1"));
            }
            _ => twin_args.push(format!("__p{i}")),
        }
    }
    let back = match ret {
        WordKind::Int if bare_return => "__r",
        WordKind::Int => "aver_rt::AverInt::from_i64(__r)",
        WordKind::Bool => "__r",
        WordKind::OptionInt => "__r.map(aver_rt::AverInt::from_i64)",
    };
    let exact_args = (0..kinds.len())
        .map(|i| format!("__p{i}"))
        .collect::<Vec<_>>()
        .join(", ");
    let mut body = String::new();
    body.push_str(&format!(
        "\n\n{visibility}fn {}({params}) -> {} {{\n",
        aver_name(name),
        if bare_return { "i64" } else { rust_type(ret) }
    ));
    body.push_str(
        "    static ABORTS: std::sync::atomic::AtomicU32 = std::sync::atomic::AtomicU32::new(0);\n",
    );
    body.push_str(&format!(
        "    'word: {{\n        if !cfg!(panic = \"unwind\") || ABORTS.load(std::sync::atomic::Ordering::Relaxed) >= {ABORTS_BEFORE_OFF} {{ break 'word; }}\n"
    ));
    for w in &words {
        body.push_str(&format!("        {w}\n"));
    }
    body.push_str(&format!(
        "        match crate::__spec_run(|| crate::{twin}({})) {{\n            Some(__r) => return {back},\n            None => {{ ABORTS.fetch_add(1, std::sync::atomic::Ordering::Relaxed); }}\n        }}\n    }}\n",
        twin_args.join(", ")
    ));
    body.push_str(&format!("    {}({exact_args})\n}}", exact_name(name)));
    body
}

fn aver_name(name: &str) -> String {
    super::syntax::aver_name_to_rust(name)
}

/// The word version of one function, as the text after `fn __word_f`.
struct Twin<'a> {
    f: &'a MirFn,
    program: &'a MirProgram,
    names: &'a BTreeMap<FnId, String>,
    looped: bool,
}

const K: &str = "crate::__spec_k";

impl<'a> Twin<'a> {
    fn new(f: &'a MirFn, program: &'a MirProgram, names: &'a BTreeMap<FnId, String>) -> Self {
        Twin {
            f,
            program,
            names,
            looped: false,
        }
    }

    fn emit(mut self) -> Option<String> {
        self.looped = self.tail_calls_self(&self.f.body.node);
        let params = self
            .f
            .params
            .iter()
            .map(|p| {
                let m = if self.looped { "mut " } else { "" };
                Some(format!(
                    "{m}{}: {}",
                    local(p.local.0),
                    word_type(WordKind::of_annotation(&p.ty)?)
                ))
            })
            .collect::<Option<Vec<_>>>()?
            .join(", ");
        let ret = word_type(WordKind::of_annotation(&self.f.return_type)?);
        let body = self.expr(&self.f.body, true)?;
        let body = if self.looped {
            format!(
                "    loop {{\n        crate::cancel_checkpoint();\n        break {body};\n    }}"
            )
        } else {
            format!("    crate::cancel_checkpoint();\n    {body}")
        };
        Some(format!("({params}) -> {ret} {{\n{body}\n}}"))
    }

    fn tail_calls_self(&self, e: &MirExpr) -> bool {
        if let MirExpr::TailCall(tc) = e
            && tc.node.target == self.f.fn_id
        {
            return true;
        }
        let mut found = false;
        crate::ir::mir::expr::walk_children(e, &mut |c| found = found || self.tail_calls_self(c));
        found
    }

    fn expr(&self, e: &Spanned<MirExpr>, tail: bool) -> Option<String> {
        Some(match &e.node {
            MirExpr::Literal(l) => match l.node {
                Literal::Int(n) => int_lit(n),
                Literal::Bool(b) => b.to_string(),
                _ => return None,
            },
            MirExpr::Local(l) => local(l.node.slot.0),
            MirExpr::Let(l) => {
                let l = &l.node;
                let ty = word_type(l.value.ty().and_then(WordKind::of)?);
                format!(
                    "({{ let {}: {ty} = {}; {} }})",
                    local(l.binding.0),
                    self.expr(&l.value, false)?,
                    self.expr(&l.body, tail)?
                )
            }
            MirExpr::IfThenElse(i) => {
                let i = &i.node;
                format!(
                    "(if {} {{ {} }} else {{ {} }})",
                    self.expr(&i.cond, false)?,
                    self.expr(&i.then_branch, tail)?,
                    self.expr(&i.else_branch, tail)?
                )
            }
            MirExpr::Match(m) => {
                let m = &m.node;
                let mut arms = Vec::new();
                for arm in &m.arms {
                    arms.push(format!(
                        "{} => {{ {} }}",
                        pattern(&arm.pattern)?,
                        self.expr(&arm.body, tail)?
                    ));
                }
                // Aver's matches are exhaustive; the arm Rust may not see as
                // covered leaves for the exact body.
                arms.push("_ => crate::__spec_abort()".to_string());
                format!(
                    "(match {} {{ {} }})",
                    self.expr(&m.subject, false)?,
                    arms.join(", ")
                )
            }
            MirExpr::BinOp(b)
                if matches!(
                    b.node.op,
                    BinOp::Eq | BinOp::Neq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte
                ) =>
            {
                let op = match b.node.op {
                    BinOp::Eq => "==",
                    BinOp::Neq => "!=",
                    BinOp::Lt => "<",
                    BinOp::Gt => ">",
                    BinOp::Lte => "<=",
                    _ => ">=",
                };
                format!(
                    "({} {op} {})",
                    self.expr(&b.node.lhs, false)?,
                    self.expr(&b.node.rhs, false)?
                )
            }
            MirExpr::Call(c) => match &c.node.callee {
                MirCallee::Fn(id) => {
                    let name = self.names.get(id)?;
                    let args = c
                        .node
                        .args
                        .iter()
                        .map(|a| self.expr(a, false))
                        .collect::<Option<Vec<_>>>()?;
                    format!("crate::{name}({})", args.join(", "))
                }
                MirCallee::Builtin(id)
                    if self.program.builtin_name(*id) == "Result.withDefault"
                        && c.node.args.len() == 2 =>
                {
                    let (a, b) = division(&c.node.args[0], &self.program.builtins)?;
                    let MirExpr::Call(inner) = &c.node.args[0].node else {
                        return None;
                    };
                    let op = match &inner.node.callee {
                        MirCallee::Builtin(id) if self.program.builtin_name(*id) == "Int.div" => {
                            "checked_div_euclid"
                        }
                        _ => "checked_rem_euclid",
                    };
                    // Aver evaluates the dividend, the divisor and the
                    // fallback in that order, then divides.
                    format!(
                        "({{ let __x: i64 = {}; let __y: i64 = {}; let __d: i64 = {}; if __y == 0 {{ __d }} else {{ {K}(__x.{op}(__y)) }} }})",
                        self.expr(a, false)?,
                        self.expr(b, false)?,
                        self.expr(&c.node.args[1], false)?
                    )
                }
                MirCallee::Builtin(id)
                    if matches!(
                        self.program.builtin_name(*id),
                        "Bool.and" | "Bool.or" | "Bool.not"
                    ) =>
                {
                    let args = c
                        .node
                        .args
                        .iter()
                        .map(|a| self.expr(a, false))
                        .collect::<Option<Vec<_>>>()?;
                    match (self.program.builtin_name(*id), args.as_slice()) {
                        ("Bool.and", [a, b]) => format!("({a} & {b})"),
                        ("Bool.or", [a, b]) => format!("({a} | {b})"),
                        ("Bool.not", [a]) => format!("(!{a})"),
                        _ => return None,
                    }
                }
                _ => self.int_op(e)?,
            },
            MirExpr::TailCall(tc) if tail && self.looped && tc.node.target == self.f.fn_id => {
                let mut out = String::from("{ ");
                for (i, a) in tc.node.args.iter().enumerate() {
                    out.push_str(&format!("let __t{i} = {}; ", self.expr(a, false)?));
                }
                for (i, p) in self.f.params.iter().enumerate() {
                    out.push_str(&format!("{} = __t{i}; ", local(p.local.0)));
                }
                out.push_str("continue }");
                out
            }
            MirExpr::Construct(c) => match (&c.node.ctor, c.node.args.as_slice()) {
                (MirCtor::Builtin(BuiltinCtor::OptionSome), [a]) => {
                    format!("Some({})", self.expr(a, false)?)
                }
                (MirCtor::Builtin(BuiltinCtor::OptionNone), []) => "None".to_string(),
                _ => return None,
            },
            _ => self.int_op(e)?,
        })
    }

    /// One pure, total `Int` operation, checked: an overflow abandons the
    /// call tree.
    fn int_op(&self, e: &Spanned<MirExpr>) -> Option<String> {
        let (op, a, b) = int_tree::op_of(e, &self.program.builtins)?;
        let x = self.expr(a, false)?;
        let y = match b {
            Some(b) => self.expr(b, false)?,
            None => String::new(),
        };
        Some(match op {
            IntOp::Add => format!("{K}({x}.checked_add({y}))"),
            IntOp::Sub => format!("{K}({x}.checked_sub({y}))"),
            IntOp::Mul => format!("{K}({x}.checked_mul({y}))"),
            IntOp::Neg => format!("{K}({x}.checked_neg())"),
            IntOp::Abs => format!("{K}({x}.checked_abs())"),
            IntOp::Min => format!("{x}.min({y})"),
            IntOp::Max => format!("{x}.max({y})"),
            // On an `i64` the bit operations are those of the integer in
            // two's complement with its sign extended without end.
            IntOp::And => format!("({x} & {y})"),
            IntOp::Or => format!("({x} | {y})"),
            IntOp::Xor => format!("({x} ^ {y})"),
            IntOp::Not => format!("(!{x})"),
            IntOp::Div(k) => format!("{K}({x}.checked_div_euclid({}))", int_lit(k)),
            IntOp::Mod(k) => format!("{K}({x}.checked_rem_euclid({}))", int_lit(k)),
            IntOp::Shl(n) if n <= 62 => format!("{K}({x}.checked_mul({}i64))", 1i64 << n),
            // Past 62 only zero stays in the word.
            IntOp::Shl(_) => format!("{K}(Some({x}).filter(|__v| *__v == 0))"),
            IntOp::Shr(n) => format!("({x} >> {})", n.min(63)),
            IntOp::Low(w) if w <= 62 => format!("({x} & {}i64)", (1i64 << w) - 1),
            // The low 63 bits or more of a non-negative word are the word.
            IntOp::Low(_) => format!("{K}(Some({x}).filter(|__v| *__v >= 0))"),
        })
    }
}

fn local(slot: u32) -> String {
    format!("__l{slot}")
}

fn int_lit(n: i64) -> String {
    if n == i64::MIN {
        "i64::MIN".to_string()
    } else {
        format!("({n}i64)")
    }
}

fn pattern(p: &MirPattern) -> Option<String> {
    Some(match p {
        MirPattern::Wildcard => "_".to_string(),
        MirPattern::Bind(slot, _) => local(slot.0),
        MirPattern::Literal(Literal::Int(n)) if *n == i64::MIN => "i64::MIN".to_string(),
        MirPattern::Literal(Literal::Int(n)) => format!("{n}i64"),
        MirPattern::Literal(Literal::Bool(b)) => b.to_string(),
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionSome),
            bindings,
            ..
        } if bindings.len() == 1 => format!("Some({})", local(bindings[0].0)),
        MirPattern::Ctor {
            ctor: MirCtor::Builtin(BuiltinCtor::OptionNone),
            ..
        } => "None".to_string(),
        _ => return None,
    })
}

/// The runtime the twins and entries share, in `runtime_support.rs`.
pub(super) const RUNTIME: &str = r#"/// The payload a machine-word twin unwinds with when an operation overflows.
pub struct __SpecOverflow;

/// Abandon the machine-word call tree. `resume_unwind` does not run the
/// panic hook, so nothing is printed.
#[cold]
#[inline(never)]
pub fn __spec_abort() -> ! {
    std::panic::resume_unwind(Box::new(__SpecOverflow))
}

/// The value of a checked operation, or the abandonment of the tree.
#[inline(always)]
pub fn __spec_k<T>(value: Option<T>) -> T {
    match value {
        Some(value) => value,
        None => __spec_abort(),
    }
}

/// Run a machine-word twin: `None` when it abandoned the tree; any other
/// panic goes on unwinding.
#[inline(always)]
pub fn __spec_run<R>(twin: impl FnOnce() -> R) -> Option<R> {
    match std::panic::catch_unwind(std::panic::AssertUnwindSafe(twin)) {
        Ok(value) => Some(value),
        Err(payload) if payload.is::<__SpecOverflow>() => None,
        Err(payload) => std::panic::resume_unwind(payload),
    }
}"#;
