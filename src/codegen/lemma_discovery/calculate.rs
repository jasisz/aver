//! IR-side lemma *calculation* substrate (charter Phase 2, the "Calculated"
//! provenance lane).
//!
//! Where the enumerator GUESSES candidate equations and the VM-filter prunes
//! them, calculation DERIVES equations mechanically by lifting a cone fn's
//! `match`-body into its *defining equations* and unfolding redexes against
//! them — the same rewriting a hand proof does on paper. This file is the
//! mechanical core only:
//!
//! * [`TermNode::substitute`] — capture-free substitution over the shared
//!   binder space (Var-index → TermNode).
//! * [`lift_fn_equations`] — AST `match`-body → guarded constructor-pattern
//!   [`DefEq`]s (one per reachable arm), handling `TailCall` recursion (post-
//!   TCO) and computed-`Bool` guards. Conservative: any sub-expression we
//!   cannot fully lift fails the whole function (it then contributes no
//!   equations rather than wrong ones).
//! * [`unfold_once`] — structural one-step unfold of an `App` redex against a
//!   set of `DefEq`s, yielding one [`Unfolded`] per applicable arm.
//!
//! This is substrate only: nothing here is wired into `aver proof --discover`.
//! PR2 adds the step-residual + generalization that consume these; PR3 wires
//! the calculated candidate into the existing proved-or-dropped gate.

use std::collections::BTreeMap;

use super::{Binder, TermNode};
use crate::ast::{self, Expr, Literal, Pattern, Spanned};
use crate::codegen::proof_lower::ProofLowerInputs;
use crate::types::Type;

/// One defining equation of a cone fn, lifted from its (possibly nested) match
/// body. Ranges over `binders` (fresh per equation): pattern-bound names and
/// any unmatched params become Var indices into `binders`.
///
/// No `PartialEq`: `Binder` (in `mod.rs`) is intentionally not `PartialEq`, and
/// equations are compared field-by-field (`.args` / `.guards` / `.rhs`) by the
/// tests rather than whole-struct.
#[derive(Debug, Clone)]
pub(crate) struct DefEq {
    /// The fn name this equation defines.
    pub callee: String,
    /// One per param: a pattern-template (`Var`, or `App{ctor, ..}`).
    pub args: Vec<TermNode>,
    /// Computed-`Bool` guards collected along the path: `(expr, expected-bool)`.
    pub guards: Vec<(TermNode, bool)>,
    /// The arm body.
    pub rhs: TermNode,
    /// The fresh binder pool this equation ranges over.
    // PR2 will consume this (the generalization step reads binder types to
    // name and quantify the lifted variables); PR1 only populates it.
    #[allow(dead_code)]
    pub binders: Vec<Binder>,
}

/// Result of unfolding one redex against one matching arm.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Unfolded {
    /// Rewritten rhs, in the REDEX's binder space.
    pub term: TermNode,
    /// Instantiated guards, in the REDEX's binder space.
    pub guards: Vec<(TermNode, bool)>,
}

impl TermNode {
    /// Capture-free substitution: replace each `Var(i)` present in `subst`
    /// with its mapped term (cloned), recursing into `App` args. Vars not in
    /// the map are left as-is. The substituted terms are assumed to already be
    /// in the target binder space (no shifting); calculation builds them that
    /// way by construction.
    pub(crate) fn substitute(&self, subst: &BTreeMap<usize, TermNode>) -> TermNode {
        match self {
            TermNode::Var(i) => subst.get(i).cloned().unwrap_or(TermNode::Var(*i)),
            TermNode::App { callee, args } => TermNode::App {
                callee: callee.clone(),
                args: args.iter().map(|a| a.substitute(subst)).collect(),
            },
        }
    }
}

/// Mutable state threaded through the lift: the fresh-binder pool and the
/// current name→binder-index map.
struct LiftCtx {
    binders: Vec<Binder>,
    /// Names currently in scope → their binder index. Params seed it; pattern
    /// matches extend it.
    scope: BTreeMap<String, usize>,
}

impl LiftCtx {
    /// Allocate a fresh binder, optionally recording a source name for it.
    fn fresh(&mut self, name: Option<&str>, ty: Type) -> usize {
        let idx = self.binders.len();
        let binder_name = match name {
            Some(n) => n.to_string(),
            None => format!("_w{idx}"),
        };
        if let Some(n) = name {
            self.scope.insert(n.to_string(), idx);
        }
        self.binders.push(Binder {
            name: binder_name,
            ty,
        });
        idx
    }
}

/// Resolve the name a scrutinee/ident references, if it is a bare ident or a
/// resolved local.
fn ident_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(n) => Some(n.clone()),
        Expr::Resolved { name, .. } => Some(name.clone()),
        _ => None,
    }
}

/// Resolve a callee `Expr` to its dotted-name string, per the conventions:
/// bare ident / resolved → its name; `Attr(base, field)` where base is an
/// ident → `base.field` (handles `List.concat`). Anything else fails.
fn callee_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(s) => Some(s.clone()),
        Expr::Resolved { name, .. } => Some(name.clone()),
        Expr::Attr(base, field) => {
            let b = ident_name(&base.node)?;
            Some(format!("{b}.{field}"))
        }
        _ => None,
    }
}

/// Lift one Aver `Expr` into the internal calc `TermNode` form, per the
/// representation conventions. Returns `None` (fail-soft) on any shape we do
/// not model — the caller then drops the whole function's equations.
fn lift_expr(expr: &Expr, ctx: &LiftCtx) -> Option<TermNode> {
    match expr {
        // Variable reference.
        Expr::Ident(name) => ctx.scope.get(name).map(|i| TermNode::Var(*i)),
        Expr::Resolved { name, .. } => ctx.scope.get(name).map(|i| TermNode::Var(*i)),

        // Fn call.
        Expr::FnCall(callee, args) => {
            let name = callee_name(&callee.node)?;
            let lifted = args
                .iter()
                .map(|a| lift_expr(&a.node, ctx))
                .collect::<Option<Vec<_>>>()?;
            Some(TermNode::App {
                callee: name,
                args: lifted,
            })
        }

        // Tail call (recursion after TCO).
        Expr::TailCall(data) => {
            let lifted = data
                .args
                .iter()
                .map(|a| lift_expr(&a.node, ctx))
                .collect::<Option<Vec<_>>>()?;
            Some(TermNode::App {
                callee: data.target.clone(),
                args: lifted,
            })
        }

        // Constructor application.
        Expr::Constructor(name, None) => Some(TermNode::App {
            callee: name.clone(),
            args: vec![],
        }),
        Expr::Constructor(name, Some(arg)) => {
            let args = if let Expr::Tuple(items) = &arg.node {
                items
                    .iter()
                    .map(|i| lift_expr(&i.node, ctx))
                    .collect::<Option<Vec<_>>>()?
            } else {
                vec![lift_expr(&arg.node, ctx)?]
            };
            Some(TermNode::App {
                callee: name.clone(),
                args,
            })
        }

        // Literals.
        Expr::Literal(Literal::Bool(true)) => Some(TermNode::App {
            callee: "true".to_string(),
            args: vec![],
        }),
        Expr::Literal(Literal::Bool(false)) => Some(TermNode::App {
            callee: "false".to_string(),
            args: vec![],
        }),
        Expr::Literal(Literal::Int(n)) => Some(TermNode::App {
            callee: n.to_string(),
            args: vec![],
        }),

        // List literal → right-folded cons form.
        Expr::List(elems) => {
            let mut acc = TermNode::App {
                callee: "[]".to_string(),
                args: vec![],
            };
            for e in elems.iter().rev() {
                let head = lift_expr(&e.node, ctx)?;
                acc = TermNode::App {
                    callee: "List.cons".to_string(),
                    args: vec![head, acc],
                };
            }
            Some(acc)
        }

        // Anything else (BinOp, Attr, nested Match, InterpolatedStr, …) → fail.
        _ => None,
    }
}

/// Convert one `ast::Pattern` to a `TermNode` template, allocating fresh
/// binders for each bound name and recording names in scope.
fn pattern_template(pat: &Pattern, ctx: &mut LiftCtx) -> Option<TermNode> {
    match pat {
        Pattern::Ident(name) => {
            let idx = ctx.fresh(Some(name), Type::Invalid);
            Some(TermNode::Var(idx))
        }
        Pattern::Wildcard => {
            let idx = ctx.fresh(None, Type::Invalid);
            Some(TermNode::Var(idx))
        }
        Pattern::EmptyList => Some(TermNode::App {
            callee: "[]".to_string(),
            args: vec![],
        }),
        Pattern::Cons(h, t) => {
            let hi = ctx.fresh(Some(h), Type::Invalid);
            let ti = ctx.fresh(Some(t), Type::Invalid);
            Some(TermNode::App {
                callee: "List.cons".to_string(),
                args: vec![TermNode::Var(hi), TermNode::Var(ti)],
            })
        }
        Pattern::Constructor(qname, binds) => {
            let args = binds
                .iter()
                .map(|b| TermNode::Var(ctx.fresh(Some(b), Type::Invalid)))
                .collect();
            Some(TermNode::App {
                callee: qname.clone(),
                args,
            })
        }
        Pattern::Literal(Literal::Bool(true)) => Some(TermNode::App {
            callee: "true".to_string(),
            args: vec![],
        }),
        Pattern::Literal(Literal::Bool(false)) => Some(TermNode::App {
            callee: "false".to_string(),
            args: vec![],
        }),
        Pattern::Literal(Literal::Int(n)) => Some(TermNode::App {
            callee: n.to_string(),
            args: vec![],
        }),
        Pattern::Tuple(ps) => {
            let args = ps
                .iter()
                .map(|p| pattern_template(p, ctx))
                .collect::<Option<Vec<_>>>()?;
            Some(TermNode::App {
                callee: "Tuple".to_string(),
                args,
            })
        }
        // Any other literal shape (Str/Float/Unit) is not modeled.
        Pattern::Literal(_) => None,
    }
}

/// Lift a cone fn's `match`-body into its defining equations. See the module
/// docs for the supported shapes; deeper/odd nesting returns `None`.
pub(crate) fn lift_fn_equations(fd: &ast::FnDef, _inputs: &ProofLowerInputs) -> Option<Vec<DefEq>> {
    // The body must be a single tail expr that is a `match`.
    let tail = fd.body.tail_expr()?;
    if !matches!(&tail.node, Expr::Match { .. }) {
        return None;
    }

    // Seed: each param → fresh Var (typed from its annotation). Initial args
    // are `[Var(param0), Var(param1), …]`.
    let mut ctx = LiftCtx {
        binders: Vec::new(),
        scope: BTreeMap::new(),
    };
    let mut args = Vec::with_capacity(fd.params.len());
    for (name, ann) in &fd.params {
        let ty = crate::codegen::common::parse_type_annotation(ann);
        let idx = ctx.fresh(Some(name), ty);
        args.push(TermNode::Var(idx));
    }

    let mut eqs = Vec::new();
    walk_match(&fd.name, tail, &args, &[], &mut ctx, &mut eqs)?;
    Some(eqs)
}

/// Walk a `match` arm-body, emitting `DefEq`s. `args` is the current arg
/// template (one per param), `guards` the guards collected along the path.
/// Returns `None` (failing the whole function) on any unsupported shape.
fn walk_match(
    callee: &str,
    expr: &Spanned<Expr>,
    args: &[TermNode],
    guards: &[(TermNode, bool)],
    ctx: &mut LiftCtx,
    out: &mut Vec<DefEq>,
) -> Option<()> {
    let Expr::Match { subject, arms } = &expr.node else {
        // Non-match arm body → lift to rhs and emit one DefEq.
        let rhs = lift_expr(&expr.node, ctx)?;
        out.push(DefEq {
            callee: callee.to_string(),
            args: args.to_vec(),
            guards: guards.to_vec(),
            rhs,
            binders: ctx.binders.clone(),
        });
        return Some(());
    };

    // Is the scrutinee a bound var (a param or an already-bound pattern var)?
    if let Some(name) = ident_name(&subject.node)
        && let Some(&var_idx) = ctx.scope.get(&name)
    {
        // BOUND VAR of an ADT/list: each arm refines the arg slot that
        // currently holds `Var(var_idx)`.
        for arm in arms {
            let tmpl = pattern_template(&arm.pattern, ctx)?;
            // Replace `Var(var_idx)` with the arm's pattern-template
            // everywhere in the current arg templates.
            let mut subst = BTreeMap::new();
            subst.insert(var_idx, tmpl);
            let new_args: Vec<TermNode> = args.iter().map(|a| a.substitute(&subst)).collect();
            walk_match(callee, &arm.body, &new_args, guards, ctx, out)?;
        }
        return Some(());
    }

    // COMPUTED scrutinee whose arms are `Bool` literals → emit a guard per arm.
    let computed = lift_expr(&subject.node, ctx)?;
    for arm in arms {
        let expected = match &arm.pattern {
            Pattern::Literal(Literal::Bool(b)) => *b,
            // Any non-Bool-literal arm over a computed scrutinee is a shape we
            // don't model — fail conservatively.
            _ => return None,
        };
        let mut new_guards = guards.to_vec();
        new_guards.push((computed.clone(), expected));
        walk_match(callee, &arm.body, args, &new_guards, ctx, out)?;
    }
    Some(())
}

/// One-step unfold of `redex` (an `App`) against `eqs`. For each `DefEq` whose
/// callee matches, structurally match `redex.args` against `eq.args` to build a
/// Var-substitution, then apply it to `eq.rhs` and each guard. Returns one
/// `Unfolded` per applicable arm (a guarded fn yields several); empty if
/// nothing applies.
pub(crate) fn unfold_once(redex: &TermNode, eqs: &[DefEq]) -> Vec<Unfolded> {
    let TermNode::App {
        callee: rcallee,
        args: rargs,
    } = redex
    else {
        return Vec::new();
    };

    let mut results = Vec::new();
    for eq in eqs {
        if &eq.callee != rcallee || eq.args.len() != rargs.len() {
            continue;
        }
        let mut subst = BTreeMap::new();
        let mut ok = true;
        for (pat, val) in eq.args.iter().zip(rargs.iter()) {
            if !match_arg(pat, val, &mut subst) {
                ok = false;
                break;
            }
        }
        if !ok {
            continue;
        }
        let term = eq.rhs.substitute(&subst);
        let guards = eq
            .guards
            .iter()
            .map(|(g, b)| (g.substitute(&subst), *b))
            .collect();
        results.push(Unfolded { term, guards });
    }
    results
}

/// Structural match of one eq-arg pattern against one redex subterm, extending
/// `subst`. `Var(j)` binds `j → val`; `App{c, sub}` requires the value to be
/// `App{c, rargs}` with the same ctor and arity, recursing pairwise. A ctor /
/// arity mismatch returns `false` (the DefEq does not apply).
fn match_arg(pat: &TermNode, val: &TermNode, subst: &mut BTreeMap<usize, TermNode>) -> bool {
    match pat {
        TermNode::Var(j) => {
            subst.insert(*j, val.clone());
            true
        }
        TermNode::App {
            callee: pc,
            args: pargs,
        } => {
            let TermNode::App {
                callee: vc,
                args: vargs,
            } = val
            else {
                return false;
            };
            if pc != vc || pargs.len() != vargs.len() {
                return false;
            }
            pargs
                .iter()
                .zip(vargs.iter())
                .all(|(p, v)| match_arg(p, v, subst))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::codegen::ModuleInfo;
    use std::collections::HashSet;

    /// The Nat/list cone fixture: `isZ`/`le`/`len`/`filterZ`/`rev` plus a
    /// trivial `verify` so the file parses. The law body is a dummy — these
    /// tests source `FnDef`s directly, not the law.
    const SRC: &str = r#"
type Nat
    Z
    S(Nat)

fn isZ(n: Nat) -> Bool
    match n
        Nat.Z -> true
        Nat.S(z) -> false

fn le(x: Nat, y: Nat) -> Bool
    match x
        Nat.Z -> true
        Nat.S(z) -> match y
            Nat.Z -> false
            Nat.S(x2) -> le(z, x2)

fn len(xs: List<Nat>) -> Int
    match xs
        [] -> 0
        [y, ..ys] -> len(ys)

fn filterZ(xs: List<Nat>) -> List<Nat>
    match xs
        [] -> []
        [y, ..ys] -> match isZ(y)
            true -> List.concat([y], filterZ(ys))
            false -> filterZ(ys)

fn rev(xs: List<Nat>) -> List<Nat>
    match xs
        [] -> []
        [y, ..ys] -> List.concat(rev(ys), [y])

fn id(n: Nat) -> Nat
    n

verify filterZ law dummy
    given xs: List<Nat> = [[], [Nat.Z]]
    filterZ(xs) => filterZ(xs)
"#;

    /// Lex→parse→tco→resolve→analyze and hand a `ProofLowerInputs` to `f`.
    /// Mirrors the harness in `mod.rs`'s test module.
    fn with_inputs<R>(src: &str, f: impl FnOnce(&ProofLowerInputs) -> R) -> R {
        let mut lexer = crate::lexer::Lexer::new(src);
        let tokens = lexer.tokenize().expect("lex");
        let mut items = crate::parser::Parser::new(tokens).parse().expect("parse");
        crate::ir::pipeline::tco(&mut items);
        crate::ir::pipeline::resolve(&mut items);
        let symbols = crate::ir::SymbolTable::build(&items, &[]);
        let resolved = crate::ir::hir::resolve_program(&symbols, &items);
        let resolved_fns: Vec<&crate::ir::hir::ResolvedFnDef> = resolved
            .iter()
            .filter_map(|t| match t {
                crate::ir::hir::ResolvedTopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .collect();
        let shape =
            crate::analysis::shape::analyze_program_with_modules(&resolved_fns, &items, &[]);
        let prefixes: HashSet<String> = HashSet::new();
        let recursive: HashSet<crate::ir::FnId> = HashSet::new();
        let no_modules: &[ModuleInfo] = &[];
        let inputs = ProofLowerInputs {
            entry_items: &items,
            dep_modules: no_modules,
            module_prefixes: &prefixes,
            recursive_fns: &recursive,
            symbol_table: &symbols,
            program_shape: Some(&shape),
        };
        f(&inputs)
    }

    fn app(callee: &str, args: Vec<TermNode>) -> TermNode {
        TermNode::App {
            callee: callee.to_string(),
            args,
        }
    }

    #[test]
    fn lifts_le_to_three_constructor_equations() {
        with_inputs(SRC, |inputs| {
            let fd = inputs.find_fn_def_by_call_name("le").expect("le def");
            let eqs = lift_fn_equations(fd, inputs).expect("le lifts");
            assert_eq!(eqs.len(), 3, "le has three reachable arms");

            // Binder allocation order:
            //   0: x (param), 1: y (param)
            // Arm Nat.Z (of x): replaces Var(0) → Nat.Z (no fresh binder).
            //   eq0: le(Nat.Z, Var(1)) = true
            let eq0 = &eqs[0];
            assert_eq!(eq0.callee, "le");
            assert_eq!(eq0.args, vec![app("Nat.Z", vec![]), TermNode::Var(1)]);
            assert_eq!(eq0.rhs, app("true", vec![]));
            assert!(eq0.guards.is_empty());

            // Arm Nat.S(z) of x: z = binder 2. args become [Nat.S(Var(2)), Var(1)].
            // Inner match on y:
            //   arm Nat.Z → eq1: le(Nat.S(z), Nat.Z) = false
            let eq1 = &eqs[1];
            assert_eq!(
                eq1.args,
                vec![app("Nat.S", vec![TermNode::Var(2)]), app("Nat.Z", vec![])]
            );
            assert_eq!(eq1.rhs, app("false", vec![]));
            assert!(eq1.guards.is_empty());

            //   arm Nat.S(x2) → x2 = binder 3. y slot → Nat.S(Var(3)).
            //   eq2: le(Nat.S(z), Nat.S(x2)) = le(z, x2)
            let eq2 = &eqs[2];
            assert_eq!(
                eq2.args,
                vec![
                    app("Nat.S", vec![TermNode::Var(2)]),
                    app("Nat.S", vec![TermNode::Var(3)])
                ]
            );
            assert_eq!(eq2.rhs, app("le", vec![TermNode::Var(2), TermNode::Var(3)]));
            assert!(eq2.guards.is_empty());
        });
    }

    #[test]
    fn unfolds_le_on_successor_pair() {
        with_inputs(SRC, |inputs| {
            let fd = inputs.find_fn_def_by_call_name("le").expect("le def");
            let eqs = lift_fn_equations(fd, inputs).expect("le lifts");
            // le(Nat.S(Var(0)), Nat.S(Var(1)))
            let redex = app(
                "le",
                vec![
                    app("Nat.S", vec![TermNode::Var(0)]),
                    app("Nat.S", vec![TermNode::Var(1)]),
                ],
            );
            let unf = unfold_once(&redex, &eqs);
            assert_eq!(unf.len(), 1, "exactly one arm applies");
            assert_eq!(
                unf[0].term,
                app("le", vec![TermNode::Var(0), TermNode::Var(1)])
            );
            assert!(unf[0].guards.is_empty());
        });
    }

    #[test]
    fn lifts_filterz_with_bool_guards() {
        with_inputs(SRC, |inputs| {
            let fd = inputs
                .find_fn_def_by_call_name("filterZ")
                .expect("filterZ def");
            let eqs = lift_fn_equations(fd, inputs).expect("filterZ lifts");
            assert_eq!(eqs.len(), 3);

            // Binder order: 0: xs (param). [] arm: replace Var(0) → [].
            let eq0 = &eqs[0];
            assert_eq!(eq0.args, vec![app("[]", vec![])]);
            assert_eq!(eq0.rhs, app("[]", vec![]));
            assert!(eq0.guards.is_empty());

            // Cons arm: y = binder 1, ys = binder 2. args → [List.cons(y, ys)].
            // Inner match on computed isZ(y):
            //   true arm → guard (isZ(Var(1)), true), rhs List.concat([y], filterZ(ys))
            //              = List.concat(List.cons(Var(1), []), filterZ(Var(2)))
            let eq1 = &eqs[1];
            assert_eq!(
                eq1.args,
                vec![app("List.cons", vec![TermNode::Var(1), TermNode::Var(2)])]
            );
            assert_eq!(eq1.guards, vec![(app("isZ", vec![TermNode::Var(1)]), true)]);
            assert_eq!(
                eq1.rhs,
                app(
                    "List.concat",
                    vec![
                        app("List.cons", vec![TermNode::Var(1), app("[]", vec![])]),
                        app("filterZ", vec![TermNode::Var(2)])
                    ]
                )
            );

            //   false arm → guard (isZ(Var(1)), false), rhs filterZ(ys)
            let eq2 = &eqs[2];
            assert_eq!(
                eq2.args,
                vec![app("List.cons", vec![TermNode::Var(1), TermNode::Var(2)])]
            );
            assert_eq!(
                eq2.guards,
                vec![(app("isZ", vec![TermNode::Var(1)]), false)]
            );
            assert_eq!(eq2.rhs, app("filterZ", vec![TermNode::Var(2)]));
        });
    }

    #[test]
    fn unfolds_filterz_cons_into_two_guarded_arms() {
        with_inputs(SRC, |inputs| {
            let fd = inputs
                .find_fn_def_by_call_name("filterZ")
                .expect("filterZ def");
            let eqs = lift_fn_equations(fd, inputs).expect("filterZ lifts");
            // filterZ(List.cons(Var(0), Var(1)))
            let redex = app(
                "filterZ",
                vec![app("List.cons", vec![TermNode::Var(0), TermNode::Var(1)])],
            );
            let unf = unfold_once(&redex, &eqs);
            assert_eq!(unf.len(), 2, "both guarded cons arms apply");

            // True arm.
            assert_eq!(
                unf[0].guards,
                vec![(app("isZ", vec![TermNode::Var(0)]), true)]
            );
            assert_eq!(
                unf[0].term,
                app(
                    "List.concat",
                    vec![
                        app("List.cons", vec![TermNode::Var(0), app("[]", vec![])]),
                        app("filterZ", vec![TermNode::Var(1)])
                    ]
                )
            );

            // False arm.
            assert_eq!(
                unf[1].guards,
                vec![(app("isZ", vec![TermNode::Var(0)]), false)]
            );
            assert_eq!(unf[1].term, app("filterZ", vec![TermNode::Var(1)]));
        });
    }

    #[test]
    fn lifts_rev_two_equations() {
        with_inputs(SRC, |inputs| {
            let fd = inputs.find_fn_def_by_call_name("rev").expect("rev def");
            let eqs = lift_fn_equations(fd, inputs).expect("rev lifts");
            assert_eq!(eqs.len(), 2);

            // [] arm.
            let eq0 = &eqs[0];
            assert_eq!(eq0.args, vec![app("[]", vec![])]);
            assert_eq!(eq0.rhs, app("[]", vec![]));
            assert!(eq0.guards.is_empty());

            // Cons arm: y = binder 1, ys = binder 2.
            // rev(List.cons(y, ys)) = List.concat(rev(ys), [y])
            //                       = List.concat(rev(Var(2)), List.cons(Var(1), []))
            let eq1 = &eqs[1];
            assert_eq!(
                eq1.args,
                vec![app("List.cons", vec![TermNode::Var(1), TermNode::Var(2)])]
            );
            assert_eq!(
                eq1.rhs,
                app(
                    "List.concat",
                    vec![
                        app("rev", vec![TermNode::Var(2)]),
                        app("List.cons", vec![TermNode::Var(1), app("[]", vec![])])
                    ]
                )
            );
            assert!(eq1.guards.is_empty());
        });
    }

    #[test]
    fn non_match_body_returns_none() {
        with_inputs(SRC, |inputs| {
            let fd = inputs.find_fn_def_by_call_name("id").expect("id def");
            assert!(
                lift_fn_equations(fd, inputs).is_none(),
                "a non-match body yields no defining equations"
            );
        });
    }
}
