//! `?` written out as the `match` it means, for the proof exports.
//!
//! ```text
//! x = e?                 match e
//! rest           →           Result.Ok(x) -> rest
//!                            Result.Err(__qm_err_0) -> Result.Err(__qm_err_0)
//! ```
//!
//! Decision: the expansion happens once, here, on the source function body,
//! and both proof exports read its result: the steps (`steps::env::read_def`)
//! and the Lean export (`lean::toplevel::fn_def::emit_fn_body_for`). The
//! kernel written in Aver and Lean therefore judge the same definition, and
//! neither keeps a reading of `?` of its own (Lean used to write such bodies
//! as a `do` block). The VM, wasm-gc and Rust backends never call this: they
//! lower `?` to their own early return (`MirExpr::Try`), so what runs does
//! not change. Lean expands after effect lifting, on a body whose effects are
//! already explicit oracle calls, so pulling a `?` ahead of its neighbours
//! cannot reorder an effect; steps read pure functions only. `?` in `verify`
//! cases and law statements is not touched: there is no function there to
//! return from.
//!
//! A `?` inside a larger expression (`f(a?, b?)`) is first pulled out into a
//! binding of its own, innermost first and then left to right, which is the
//! order the VM evaluates operands in (`vm::compiler::mir`: a call's
//! arguments, an operator's sides and a record's fields as written), so the
//! first error the run returns is the one the expansion returns. A `?` in a
//! match arm stays in that arm, the only place it runs. Generated names start
//! with `__`, which source names cannot.

use crate::ast::{Expr, FnBody, MatchArm, Pattern, Shared, Spanned, Stmt};
use crate::codegen::expr_walk;

/// `body` with every `?` (and `?!`) written as a `match`, or `None` when it
/// has none.
pub(crate) fn expand(body: &FnBody) -> Option<FnBody> {
    if !body.stmts().iter().any(|s| has_try(value(s))) {
        return None;
    }
    Some(FnBody::Block(Expand(0).stmts(body.stmts())))
}

fn value(s: &Stmt) -> &Spanned<Expr> {
    let (Stmt::Binding(_, _, e) | Stmt::Expr(e)) = s;
    e
}

fn has_try(e: &Spanned<Expr>) -> bool {
    expr_walk::any(e, &mut |e| {
        matches!(
            e.node,
            Expr::ErrorProp(_) | Expr::IndependentProduct(_, true)
        )
    })
}

/// A `?` pulled out: `subject` is matched, `ok` binds its value.
struct Site {
    subject: Spanned<Expr>,
    ok: String,
    err: String,
}

/// The number of the next generated name.
struct Expand(u32);

impl Expand {
    fn stmts(&mut self, stmts: &[Stmt]) -> Vec<Stmt> {
        let mut out = Vec::new();
        for (i, stmt) in stmts.iter().enumerate() {
            if !has_try(value(stmt)) {
                out.push(stmt.clone());
                continue;
            }
            if i + 1 == stmts.len() {
                out.push(Stmt::Expr(self.tail(value(stmt).clone())));
                return out;
            }
            let mut sites = Vec::new();
            let mut rest = Vec::new();
            match stmt {
                // `x = e?` binds `x` in the arm itself.
                Stmt::Binding(
                    name,
                    _,
                    Spanned {
                        node: Expr::ErrorProp(inner),
                        ..
                    },
                ) => {
                    let mut inner = (**inner).clone();
                    self.lift(&mut inner, &mut sites, false);
                    let n = self.fresh();
                    sites.push(Site {
                        subject: inner,
                        ok: name.clone(),
                        err: format!("__qm_err_{n}"),
                    });
                }
                _ => {
                    let mut stmt = stmt.clone();
                    let (Stmt::Binding(_, _, e) | Stmt::Expr(e)) = &mut stmt;
                    self.lift(e, &mut sites, false);
                    rest.push(stmt);
                }
            }
            rest.extend(self.stmts(&stmts[i + 1..]));
            let line = value(stmt).line;
            let body = crate::types::checker::effect_lifting::stmts_to_let_chain(&rest, line);
            out.push(Stmt::Expr(wrap(sites, body)));
            return out;
        }
        out
    }

    /// `e`, whose value is the function's result, expanded.
    fn tail(&mut self, mut e: Spanned<Expr>) -> Spanned<Expr> {
        let mut sites = Vec::new();
        self.lift(&mut e, &mut sites, true);
        wrap(sites, e)
    }

    /// Pull every `?` that `e` evaluates whenever it is evaluated out into
    /// `sites`, in evaluation order. `tail`: the value of `e` is the
    /// function's result, so a match's arms can return an error themselves.
    fn lift(&mut self, e: &mut Spanned<Expr>, sites: &mut Vec<Site>, tail: bool) {
        if !has_try(e) {
            return;
        }
        match &mut e.node {
            Expr::ErrorProp(inner) => {
                let mut inner = (**inner).clone();
                self.lift(&mut inner, sites, false);
                e.node = self.site(inner, sites);
            }
            // `(a, b)?!`: each part as `?`, left to right.
            Expr::IndependentProduct(items, true) => {
                let items = items
                    .iter()
                    .map(|item| Spanned::new(Expr::ErrorProp(Shared::new(item.clone())), item.line))
                    .collect();
                e.node = Expr::Tuple(Shared::new(items));
                self.lift(e, sites, tail);
            }
            Expr::Match { subject, arms } if arms.iter().any(|arm| has_try(&arm.body)) => {
                self.lift(subject, sites, false);
                // Below the result, the match is made one that returns a
                // `Result`, and `?` is applied to that.
                let new_arms = arms
                    .iter()
                    .map(|arm| {
                        let body = (*arm.body).clone();
                        let body = if tail { body } else { ok(body) };
                        MatchArm::new(arm.pattern.clone(), self.tail(body))
                    })
                    .collect();
                *arms = Shared::new(new_arms);
                if !tail {
                    let matched = e.clone();
                    e.node = self.site(matched, sites);
                }
            }
            _ => expr_walk::for_each_child_mut(e, &mut |child| self.lift(child, sites, false)),
        }
    }

    /// Record `subject?` as a site; the name that stands for its value.
    fn site(&mut self, subject: Spanned<Expr>, sites: &mut Vec<Site>) -> Expr {
        let n = self.fresh();
        let ok = format!("__qm_value_{n}");
        sites.push(Site {
            subject,
            ok: ok.clone(),
            err: format!("__qm_err_{n}"),
        });
        Expr::Ident(ok)
    }

    fn fresh(&mut self) -> u32 {
        self.0 += 1;
        self.0 - 1
    }
}

fn ok(e: Spanned<Expr>) -> Spanned<Expr> {
    let line = e.line;
    Spanned::new(
        Expr::Constructor("Result.Ok".into(), Some(Shared::new(e))),
        line,
    )
}

/// `body` under the matches of `sites`, the first one outermost.
fn wrap(sites: Vec<Site>, body: Spanned<Expr>) -> Spanned<Expr> {
    sites.into_iter().rev().fold(body, |body, site| {
        let line = site.subject.line;
        let err = Spanned::new(Expr::Ident(site.err.clone()), line);
        let arms = vec![
            MatchArm::new(
                Pattern::Constructor("Result.Ok".into(), vec![site.ok]),
                body,
            ),
            MatchArm::new(
                Pattern::Constructor("Result.Err".into(), vec![site.err]),
                Spanned::new(
                    Expr::Constructor("Result.Err".into(), Some(Shared::new(err))),
                    line,
                ),
            ),
        ];
        Spanned::new(
            Expr::Match {
                subject: Shared::new(site.subject),
                arms: Shared::new(arms),
            },
            line,
        )
    })
}

#[cfg(test)]
mod tests {
    use crate::ast::{Stmt, TopLevel};

    /// The body of the one function in `src`, expanded, as source.
    fn expanded(src: &str) -> String {
        let items = crate::source::parse_source(src).expect("parses");
        let fd = items
            .into_iter()
            .find_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd),
                _ => None,
            })
            .expect("a function");
        let body = super::expand(&fd.body).expect("has `?`");
        let mut out = String::new();
        for stmt in body.stmts() {
            if let Stmt::Binding(name, _, _) = stmt {
                out.push_str(&format!("{name} = "));
            }
            crate::ast::unparse::write_expr_public(&mut out, super::value(stmt), 0)
                .expect("unparses");
            out.push('\n');
        }
        out
    }

    #[test]
    fn nested_questions_are_matched_innermost_first_then_left_to_right() {
        let got = expanded(
            "fn f(a: String, b: String) -> Result<Int, String>\n    Result.Ok(g(p(q(a)?)?, p(b)?))\n",
        );
        let q = got.find("match q(a)").expect("q(a) matched");
        let pq = got.find("match p(__qm_value_0)").expect("p of q matched");
        let pb = got.find("match p(b)").expect("p(b) matched");
        assert!(q < pq && pq < pb, "{got}");
        assert!(got.contains("Ok(g(__qm_value_1, __qm_value_2))"), "{got}");
        assert!(!got.contains('?'), "{got}");
    }

    #[test]
    fn a_question_in_an_arm_stays_in_the_arm() {
        let got = expanded(
            "fn f(c: Bool, a: String) -> Result<Int, String>\n    x = 1\n    match c\n        true -> Result.Ok(p(a)? + x)\n        false -> Result.Ok(x)\n",
        );
        assert!(got.starts_with("x = 1\nmatch c"), "{got}");
        assert!(!got.contains('?'), "{got}");
    }

    #[test]
    fn a_question_under_a_match_makes_the_match_return_a_result() {
        let got = expanded(
            "fn f(c: Bool, a: String) -> Result<Int, String>\n    x = match c\n        true -> p(a)?\n        false -> 0\n    Result.Ok(x)\n",
        );
        assert!(got.starts_with("match match c"), "{got}");
        assert!(got.contains("Result.Ok(0)"), "{got}");
        assert!(!got.contains('?'), "{got}");
    }

    #[test]
    fn a_binding_names_the_value_its_question_unwraps() {
        let got = expanded(
            "fn f(a: String) -> Result<Int, String>\n    x = p(a)?\n    y = x + 1\n    Result.Ok(y)\n",
        );
        assert!(got.starts_with("match p(a)"), "{got}");
        assert!(got.contains("Result.Ok(x) ->"), "{got}");
        assert!(
            got.contains("Result.Err(__qm_err_0) -> Result.Err(__qm_err_0)"),
            "{got}"
        );
    }
}
