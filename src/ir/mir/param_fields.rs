//! The parts of each parameter a function observes.
//!
//! A parameter used only through field projections need not cross a call
//! boundary as an aggregate. Forwarding it to another function observes the
//! fields that function observes, so the answer is a fixed point over MIR
//! calls, including recursive groups. This is a semantic use summary: it
//! contains no target types, allocation policy, calling convention or ABI.
//! A backend may split an aggregate only after choosing a suitable layout.
//!
//! An unrecognised use observes the whole value. In particular, storing,
//! returning, comparing, matching, capturing in a deferred call, or handing
//! a parameter to an unknown function prevents field-only treatment. Reading
//! only a local's last use does not prove that its contents are unshared.

use std::collections::{BTreeSet, HashMap};

use crate::ir::FnId;

use super::{LocalId, MirCallee, MirExpr, MirProgram};

/// What a function may observe of one parameter.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ParamFields {
    /// The value itself may be observed or retained.
    Whole,
    /// Only these immediate fields are observed. Empty means unused.
    Fields(BTreeSet<String>),
}

impl ParamFields {
    fn include(&mut self, other: &Self) {
        match (&mut *self, other) {
            (_, Self::Whole) => *self = Self::Whole,
            (Self::Fields(fields), Self::Fields(more)) => fields.extend(more.iter().cloned()),
            (Self::Whole, _) => {}
        }
    }
}

/// Parameter observations, by function identity and declaration order.
pub type ParamFieldUses = HashMap<FnId, Vec<ParamFields>>;

/// Compute field observations without changing the program. Starting with
/// no observations and only adding them gives the least fixed point; a
/// recursive forwarding cycle cannot hide a whole-value use in a member.
pub fn analyze(program: &MirProgram) -> ParamFieldUses {
    let mut uses: ParamFieldUses = program
        .iter()
        .map(|(id, f)| {
            (
                *id,
                vec![ParamFields::Fields(BTreeSet::new()); f.params.len()],
            )
        })
        .collect();
    loop {
        let mut next = uses.clone();
        for (id, f) in program.iter() {
            for (i, param) in f.params.iter().enumerate() {
                let mut observed = ParamFields::Fields(BTreeSet::new());
                observe(&f.body.node, param.local, &uses, &mut observed);
                next.get_mut(id).expect("every MIR function has a summary")[i].include(&observed);
            }
        }
        if next == uses {
            return uses;
        }
        uses = next;
    }
}

fn observe(expr: &MirExpr, param: LocalId, uses: &ParamFieldUses, out: &mut ParamFields) {
    if *out == ParamFields::Whole {
        return;
    }
    let is_param = |e: &MirExpr| matches!(e, MirExpr::Local(l) if l.node.slot == param);
    match expr {
        MirExpr::Local(l) if l.node.slot == param => *out = ParamFields::Whole,
        MirExpr::Project(p) if is_param(&p.node.base.node) => {
            if let ParamFields::Fields(fields) = out {
                fields.insert(p.node.field.clone());
            }
        }
        MirExpr::Call(call) => {
            if matches!(call.node.callee, MirCallee::LocalSlot { slot, .. } if u32::from(slot) == param.0)
            {
                *out = ParamFields::Whole;
                return;
            }
            let callee = match call.node.callee {
                MirCallee::Fn(id) if !call.lazy.is_deferred() => uses.get(&id),
                _ => None,
            };
            observe_args(&call.node.args, callee, param, uses, out);
        }
        MirExpr::TailCall(call) => {
            observe_args(
                &call.node.args,
                uses.get(&call.node.target),
                param,
                uses,
                out,
            );
        }
        _ => super::expr::walk_children(expr, &mut |child| observe(child, param, uses, out)),
    }
}

fn observe_args(
    args: &[crate::ast::Spanned<MirExpr>],
    callee: Option<&Vec<ParamFields>>,
    param: LocalId,
    uses: &ParamFieldUses,
    out: &mut ParamFields,
) {
    for (i, arg) in args.iter().enumerate() {
        if matches!(&arg.node, MirExpr::Local(l) if l.node.slot == param) {
            out.include(
                callee
                    .and_then(|params| params.get(i))
                    .unwrap_or(&ParamFields::Whole),
            );
        } else {
            observe(&arg.node, param, uses, out);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Spanned;
    use crate::ir::mir::{MirCall, MirFn, MirLocal, MirParam, MirProject};

    fn local() -> Spanned<MirExpr> {
        Spanned::bare(MirExpr::Local(Spanned::bare(MirLocal::at(LocalId(0)))))
    }

    fn field(name: &str) -> Spanned<MirExpr> {
        Spanned::bare(MirExpr::Project(Spanned::bare(MirProject {
            base: Box::new(local()),
            field: name.into(),
        })))
    }

    fn call(target: u32) -> Spanned<MirExpr> {
        Spanned::bare(MirExpr::Call(Spanned::bare(MirCall {
            callee: MirCallee::Fn(FnId(target)),
            args: vec![local()],
        })))
    }

    fn function(id: u32, body: Spanned<MirExpr>) -> MirFn {
        MirFn {
            fn_id: FnId(id),
            name: format!("f{id}"),
            params: vec![MirParam {
                local: LocalId(0),
                name: "p".into(),
                ty: "Pair".into(),
            }],
            return_type: "Int".into(),
            effects: vec![],
            body,
            local_count: 1,
            aliased_slots: Default::default(),
            repr: Default::default(),
        }
    }

    fn program(fns: Vec<MirFn>) -> MirProgram {
        MirProgram {
            fns: fns.into_iter().map(|f| (f.fn_id, f)).collect(),
            ..Default::default()
        }
    }

    #[test]
    fn projections_propagate_through_forwarders() {
        let p = program(vec![function(1, call(2)), function(2, field("end"))]);
        let uses = analyze(&p);
        let end = ParamFields::Fields(BTreeSet::from(["end".into()]));
        assert_eq!(uses[&FnId(1)], vec![end.clone()]);
        assert_eq!(uses[&FnId(2)], vec![end]);
    }

    #[test]
    fn storing_or_returning_the_parameter_prevents_splitting() {
        let p = program(vec![
            function(1, call(2)),
            function(2, Spanned::bare(MirExpr::Tuple(vec![local()]))),
        ]);
        let uses = analyze(&p);
        assert_eq!(uses[&FnId(1)], vec![ParamFields::Whole]);
        assert_eq!(uses[&FnId(2)], vec![ParamFields::Whole]);
    }

    #[test]
    fn unknown_and_deferred_calls_observe_the_whole_value() {
        let mut deferred = call(2);
        deferred.node = match deferred.node {
            MirExpr::Call(mut c) => {
                c.lazy = crate::ast::Lazy::Deferred;
                MirExpr::Call(c)
            }
            _ => unreachable!(),
        };
        let p = program(vec![
            function(1, deferred),
            function(2, field("end")),
            function(3, call(99)),
        ]);
        let uses = analyze(&p);
        assert_eq!(uses[&FnId(1)], vec![ParamFields::Whole]);
        assert_eq!(uses[&FnId(3)], vec![ParamFields::Whole]);
    }

    #[test]
    fn recursive_forwarding_cannot_hide_a_whole_value_use() {
        let p = program(vec![
            function(1, call(2)),
            function(2, Spanned::bare(MirExpr::Tuple(vec![call(1), local()]))),
        ]);
        assert!(analyze(&p).values().all(|p| p == &[ParamFields::Whole]));
    }

    #[test]
    fn nested_projection_observes_the_immediate_field() {
        let nested = Spanned::bare(MirExpr::Project(Spanned::bare(MirProject {
            base: Box::new(field("inner")),
            field: "value".into(),
        })));
        let p = program(vec![function(1, nested)]);
        assert_eq!(
            analyze(&p)[&FnId(1)],
            vec![ParamFields::Fields(BTreeSet::from(["inner".into()]))]
        );
    }

    #[test]
    fn calling_a_function_parameter_observes_the_whole_parameter() {
        let p = program(vec![function(
            1,
            Spanned::bare(MirExpr::Call(Spanned::bare(MirCall {
                callee: MirCallee::LocalSlot {
                    slot: 0,
                    name: "f".into(),
                    last_use: true,
                },
                args: vec![],
            }))),
        )]);
        assert_eq!(analyze(&p)[&FnId(1)], vec![ParamFields::Whole]);
    }
}
