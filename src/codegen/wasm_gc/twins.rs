//! A twin of a function that hands its result back unpacked and takes its
//! `Int` params as machine words and small record params as their fields.
//!
//! On wasm-gc every `Int` is an `$AverInt` object, and an `Option` or a
//! `Result` is a struct. A function that computes its result on
//! a machine word, or builds the struct only to return it, allocates once per
//! call, and a caller that takes the result apart at once throws the object
//! away; an `Int` argument the caller boxes and the callee reads back as a
//! machine word costs the same. Binaryen cannot remove that unless it inlines
//! the function, because it does not change the type of a result and does
//! not know what `$AverInt` means.
//!
//! Such a function gets a twin with the same body:
//!
//! - an `Int` result is `(i64, ref null $AverInt)`, the ref non-null only for
//!   a Big value; an `Option`, `Result` or small record result is its fields;
//! - an `Int` param the body reads as a machine word is an `i64` plus a ref
//!   after the other params, null unless the value comes packed.
//! - a small record param observed only through its fields uses those fields
//!   as extra params. The shared MIR `param_fields` summary establishes that
//!   use property; this backend bounds the layout to four fields and chooses
//!   the wasm types. Fresh producers and forwarders need no record object.
//!
//! The function itself becomes a small wrapper that calls the twin and packs
//! the result, so its signature is unchanged for every caller that wants the
//! packed value, a function value included. A caller that takes the result
//! apart at once, or has an `Int` argument on a machine word, calls the twin;
//! a fn no caller would call so gets none. A twin's tail calls go to the
//! twins of their targets, so a tail group keeps running without the stack
//! growing. A `?`, `?!` or early `return` in a twin returns its result
//! unpacked too, and a `?` on a twin's `Result` takes it apart unpacked. A
//! twin holds an `Int` local bound to a value with its machine word as it
//! holds such a param, boxing it only where the body wants it packed.
//!
//! A `--certify` build keeps the bytes of every function the certificate plan
//! printer may print: such a function gets no twin
//! ([`crate::ir::cert_shape::printer_may_admit`], the superset the
//! fabricating passes use), and one it prints calls none.

use std::collections::{HashMap, HashSet};

mod callers;
use callers::{Caller, Use};

use wasm_encoder::{HeapType, ValType};

use super::WasmGcError;
use super::types::{TypeRegistry, aver_to_wasm};
use crate::ast::FnDef;
use crate::ir::FnId;
use crate::ir::hir::ResolvedFnDef;
use crate::ir::mir::{MirCallee, MirCtor, MirExpr, MirProgram};

/// What a twin returns instead of the packed result.
#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum TwinKind {
    /// The packed result, as the fn returns it: the twin is there for its
    /// params.
    Plain,
    /// `(i64, ref null $AverInt)`.
    Int,
    /// The fields of struct `struct_idx`.
    Fields {
        struct_idx: u32,
        fields: Vec<ValType>,
    },
}

/// A function's twin as its callers see it.
#[derive(Clone, Debug)]
pub(crate) struct TwinEntry {
    pub(crate) wasm_idx: u32,
    pub(crate) kind: TwinKind,
    /// Per param: an `Int` the twin takes unpacked, as an `i64` and, after
    /// the other params, a ref that is null when the value is the `i64` and
    /// is the packed value otherwise.
    pub(crate) unpacked: Vec<bool>,
    /// Small record params observed only through their fields. Fields replace
    /// the record ref and follow the ordinary params and unpacked Int refs.
    pub(crate) records: Vec<Option<RecordFields>>,
}

/// What `plan` gives a fn.
#[derive(Clone, Debug)]
pub(crate) struct TwinPlan {
    pub(crate) kind: TwinKind,
    pub(crate) unpacked: Vec<bool>,
    pub(crate) records: Vec<Option<RecordFields>>,
}

/// Target layout of a small record whose fields can travel independently.
/// The semantic eligibility comes from `mir::param_fields`, not this layout.
#[derive(Clone, Debug)]
pub(crate) struct RecordFields {
    pub(crate) struct_idx: u32,
    pub(crate) fields: Vec<(String, String, ValType)>,
}

impl RecordFields {
    pub(crate) fn of(name: &str, registry: &TypeRegistry) -> Option<Self> {
        // Development differential: retain the established Int/Option/Result
        // twins while measuring just record splitting on the same compiler.
        if std::env::var_os("AVER_NO_RECORD_TWINS").is_some() {
            return None;
        }
        let name = registry.canonical_type_name(name.trim());
        if registry.newtype_underlying(name).is_some() || registry.packed_sequence(name).is_some() {
            return None;
        }
        let decl = registry.record_fields.get(name)?;
        // Bound the calling convention's size. Large state records continue
        // to cross calls as references, rather than spilling dozens of fields.
        if !(2..=4).contains(&decl.len())
            || decl
                .iter()
                .any(|(field, _)| registry.is_eligible_carrier_field(name, field))
        {
            return None;
        }
        let fields = decl
            .iter()
            .map(|(field, ty)| {
                let val = aver_to_wasm(ty, Some(registry)).ok()??;
                Some((field.clone(), ty.clone(), val))
            })
            .collect::<Option<Vec<_>>>()?;
        Some(Self {
            struct_idx: registry.record_type_idx(name)?,
            fields,
        })
    }
}

impl TwinKind {
    /// The twin's results; `plain` is the fn's own.
    pub(crate) fn results(&self, registry: &TypeRegistry, plain: &[ValType]) -> Vec<ValType> {
        match self {
            TwinKind::Plain => plain.to_vec(),
            TwinKind::Int => vec![
                ValType::I64,
                super::types::struct_ref(registry.aint_struct_idx.unwrap_or_default()),
            ],
            TwinKind::Fields { fields, .. } => fields.clone(),
        }
    }
}

/// The struct fields a result of type `ret` unpacks into, when it does.
fn kind_of(ret: &str, registry: &TypeRegistry) -> Result<Option<TwinKind>, WasmGcError> {
    let ret = ret.trim();
    if ret == "Int" {
        return Ok((registry.bignum && registry.aint_struct_idx.is_some()).then_some(TwinKind::Int));
    }
    let Some(ValType::Ref(r)) = aver_to_wasm(ret, Some(registry))? else {
        return Ok(None);
    };
    let HeapType::Concrete(idx) = r.heap_type else {
        return Ok(None);
    };
    let wasm = |t: &str| -> Result<ValType, WasmGcError> {
        Ok(aver_to_wasm(t, Some(registry))?.unwrap_or(ValType::I32))
    };
    let option = registry.option_types.iter().find(|(_, i)| **i == idx);
    let result = registry.result_types.iter().find(|(_, i)| **i == idx);
    let fields = match (option, result) {
        (Some((c, _)), _) => match TypeRegistry::option_element_type(c) {
            Some(t) => vec![ValType::I32, wasm(t)?],
            None => return Ok(None),
        },
        (_, Some((c, _))) => match TypeRegistry::result_te(c) {
            Some((t, e)) => vec![ValType::I32, wasm(t)?, wasm(e)?],
            None => return Ok(None),
        },
        _ => match RecordFields::of(ret, registry) {
            Some(record) => {
                return Ok(Some(TwinKind::Fields {
                    struct_idx: record.struct_idx,
                    fields: record.fields.iter().map(|(_, _, v)| *v).collect(),
                }));
            }
            None => return Ok(None),
        },
    };
    Ok(Some(TwinKind::Fields {
        struct_idx: idx,
        fields,
    }))
}

pub(crate) use crate::ir::mir::result_values::collect as tails;

fn tail_targets(e: &MirExpr, out: &mut Vec<FnId>) {
    if let MirExpr::TailCall(tc) = e {
        out.push(tc.node.target);
    }
    crate::ir::mir::expr::walk_children(e, &mut |c| tail_targets(c, out));
}

/// Whether a tail `e` of a function with a `kind` twin can hand its value
/// back unpacked by itself: a literal or `Int` arithmetic, or the struct the
/// twin unpacks built in place.
fn produces_unpacked(e: &MirExpr, kind: &TwinKind) -> bool {
    match kind {
        TwinKind::Int => matches!(
            e,
            MirExpr::Literal(_)
                | MirExpr::Box(_)
                | MirExpr::BinOp(_)
                | MirExpr::Neg(_)
                | MirExpr::Call(_)
        ),
        TwinKind::Plain => false,
        TwinKind::Fields { .. } => {
            matches!(e, MirExpr::Construct(c) if matches!(c.node.ctor, MirCtor::Builtin(_)))
                || matches!(e, MirExpr::RecordCreate(_) | MirExpr::RecordUpdate(_))
        }
    }
}

/// Whether `body` reads the param in `slot` where its machine word serves:
/// in `Int` arithmetic or a comparison, as an arg of a call, a match's
/// subject, or a tail. A param read only packed (stored, stringified) is
/// taken packed, so the twin does not box what its caller had boxed already.
pub(crate) fn read_as_word(
    body: &crate::ast::Spanned<MirExpr>,
    slot: crate::ir::mir::LocalId,
) -> bool {
    fn walk(e: &MirExpr, slot: crate::ir::mir::LocalId) -> bool {
        let is = |x: &crate::ast::Spanned<MirExpr>| matches!(&x.node, MirExpr::Local(l) if l.node.slot == slot);
        let direct = match e {
            MirExpr::BinOp(b) => is(&b.node.lhs) || is(&b.node.rhs),
            MirExpr::Neg(x) | MirExpr::Unbox(x) => is(x),
            MirExpr::Call(c) => c.node.args.iter().any(is),
            MirExpr::TailCall(c) => c.node.args.iter().any(is),
            MirExpr::Match(m) => is(&m.node.subject),
            // A local an `Int` tree reads in place is an operand of one of
            // its operations or comparisons.
            MirExpr::IntWord(w) => w.node.trees().iter().any(|t| reads_in_place(t, slot)),
            _ => false,
        };
        let mut found = direct;
        if !found {
            crate::ir::mir::expr::walk_children(e, &mut |c| found = found || walk(c, slot));
        }
        found
    }
    let mut ts = Vec::new();
    tails(body, &mut ts);
    ts.iter()
        .any(|t| matches!(&t.node, MirExpr::Local(l) if l.node.slot == slot))
        || walk(&body.node, slot)
}

/// Whether the `Int` tree `t` reads the local in `slot` in place.
fn reads_in_place(t: &crate::ir::mir::IntWordNode, slot: crate::ir::mir::LocalId) -> bool {
    use crate::ir::mir::IntWordNode;
    match t {
        IntWordNode::Op { args, .. } => args.iter().any(|a| reads_in_place(a, slot)),
        IntWordNode::Local(e) => matches!(&e.node, MirExpr::Local(l) if l.node.slot == slot),
        IntWordNode::Lit(_) | IntWordNode::Ahead(..) => false,
    }
}

/// Whether `e`, an `Int`, comes with its machine word: a literal, arithmetic,
/// a builtin's result, or the result of a fn with an `Int` twin (`int_twin`).
pub(crate) fn has_word(e: &MirExpr, int_twin: &dyn Fn(FnId) -> bool) -> bool {
    match e {
        MirExpr::Literal(l) => matches!(l.node, crate::ast::Literal::Int(_)),
        MirExpr::Box(_) | MirExpr::BinOp(_) | MirExpr::Neg(_) | MirExpr::IntWord(_) => true,
        MirExpr::Call(c) => match c.node.callee {
            MirCallee::Fn(id) => int_twin(id),
            _ => true,
        },
        _ => false,
    }
}

/// The twin each user function gets, by position in `fn_defs`.
pub(super) fn plan(
    fn_defs: &[&FnDef],
    resolved: &[ResolvedFnDef],
    mir: &MirProgram,
    registry: &TypeRegistry,
    certify: bool,
) -> Result<Vec<Option<TwinPlan>>, WasmGcError> {
    let mut plans: Vec<Option<TwinPlan>> = vec![None; fn_defs.len()];
    if std::env::var_os("AVER_NO_TWINS").is_some() {
        return Ok(plans);
    }
    let ints = registry.bignum && registry.aint_struct_idx.is_some();
    let field_uses = crate::ir::mir::param_fields::analyze(mir);
    let params_too = std::env::var_os("AVER_NO_TWIN_PARAMS").is_none();
    let pos: HashMap<FnId, usize> = resolved
        .iter()
        .enumerate()
        .map(|(i, r)| (r.fn_id, i))
        .collect();
    for (i, fd) in fn_defs.iter().enumerate() {
        let Some(mf) = mir.fn_by_id(resolved[i].fn_id) else {
            continue;
        };
        if fd.name == "main"
            || mf.repr.bare_return
            || (certify && crate::ir::cert_shape::printer_may_admit(fd))
        {
            continue;
        }
        let unpacked: Vec<bool> = fd
            .params
            .iter()
            .enumerate()
            .map(|(k, (_, ty))| {
                params_too
                    && ints
                    && k < 32
                    && ty.trim() == "Int"
                    && !mf.repr.param_is_bare(k)
                    && mf
                        .params
                        .get(k)
                        .is_some_and(|p| read_as_word(&mf.body, p.local))
            })
            .collect();
        let kind = kind_of(&fd.return_type, registry)?.unwrap_or(TwinKind::Plain);
        let records = mf
            .params
            .iter()
            .enumerate()
            .map(|(k, _)| {
                let uses = field_uses.get(&mf.fn_id)?.get(k)?;
                let crate::ir::mir::param_fields::ParamFields::Fields(fields) = uses else {
                    return None;
                };
                let layout = RecordFields::of(&fd.params.get(k)?.1, registry)?;
                (!fields.is_empty()
                    && fields
                        .iter()
                        .all(|name| layout.fields.iter().any(|(f, _, _)| f == name)))
                .then_some(layout)
            })
            .collect();
        plans[i] = Some(TwinPlan {
            kind,
            unpacked,
            records,
        });
    }
    // A result comes back unpacked when a tail produces it (a literal,
    // arithmetic, an unpacked param, the struct built in place) or hands on a
    // call's of the same kind; any other twin returns it packed.
    let kinds: Vec<Option<TwinKind>> = plans
        .iter()
        .map(|p| p.as_ref().map(|p| p.kind.clone()))
        .collect();
    for (i, plan) in plans.iter_mut().enumerate() {
        let Some(plan) = plan else { continue };
        let mf = mir.fn_by_id(resolved[i].fn_id).expect("planned fn has MIR");
        let mut ts = Vec::new();
        tails(&mf.body, &mut ts);
        let same = |id: &FnId| {
            pos.get(id)
                .is_some_and(|j| kinds[*j].as_ref() == Some(&plan.kind))
        };
        let unpacked = ts.iter().any(|t| match &t.node {
            MirExpr::Local(l) => mf
                .params
                .iter()
                .zip(&plan.unpacked)
                .any(|(p, u)| *u && p.local == l.node.slot),
            MirExpr::Call(c) if matches!(c.node.callee, MirCallee::Fn(_)) => {
                matches!(c.node.callee, MirCallee::Fn(id) if same(&id))
            }
            MirExpr::TailCall(tc) => same(&tc.node.target),
            e => produces_unpacked(e, &plan.kind),
        });
        if !unpacked {
            plan.kind = TwinKind::Plain;
        }
    }
    // Keep a twin only where a caller uses it: it takes the result apart,
    // hands it a machine word, or is a twin tail-calling it.
    let mut kept: HashSet<usize> = HashSet::new();
    loop {
        let before = kept.len();
        for (i, r) in resolved.iter().enumerate() {
            if let Some(mf) = mir.fn_by_id(r.fn_id) {
                let caller = Caller {
                    at: i,
                    plans: &plans,
                    kept: &kept,
                    pos: &pos,
                    params: &mf.params,
                };
                let mut wanted = Vec::new();
                caller.visit(&mf.body.node, Use::Tail, &mut wanted);
                kept.extend(wanted);
            }
        }
        if kept.len() == before {
            break;
        }
    }
    for (i, plan) in plans.iter_mut().enumerate() {
        if !kept.contains(&i) {
            *plan = None;
        }
    }
    // A twin's tail calls go to twins of the same kind: drop a twin whose
    // tail call has none, until none is left.
    loop {
        let mut changed = false;
        for i in 0..plans.len() {
            let Some(kind) = plans[i].as_ref().map(|p| p.kind.clone()) else {
                continue;
            };
            let mf = mir.fn_by_id(resolved[i].fn_id).expect("planned fn has MIR");
            let mut targets = Vec::new();
            tail_targets(&mf.body.node, &mut targets);
            let closed = targets.iter().all(|t| {
                pos.get(t)
                    .is_some_and(|j| plans[*j].as_ref().map(|p| &p.kind) == Some(&kind))
            });
            if !closed {
                plans[i] = None;
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
    Ok(plans)
}
