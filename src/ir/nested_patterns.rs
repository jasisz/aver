//! Nested-pattern compilation: the front-door pass that turns every
//! `match` holding a nested constructor pattern (`Option.Some(0)`,
//! `Pair.Of(1, x)`) or a general list pattern (`[a, b, ..rest]`,
//! `[a, b]`, `[0, ..rest]`) into a tree of flat matches that every
//! backend and proof exporter already reads.
//!
//! The pass runs in [`crate::ir::pipeline::front`] after the program was
//! type-checked AS WRITTEN (so exhaustiveness, redundancy and type
//! errors speak about the patterns the user wrote) and before the
//! lowered program is checked again. Nothing below the front door ever
//! sees [`Pattern::ConstructorNested`] or [`Pattern::List`].
//!
//! The compilation is the classic clause-matrix one: pick the first
//! column the first row tests, switch on it with one flat match (one arm
//! per constructor or literal that appears, plus `_` when the arms do not
//! cover the type), specialise the rows for each arm and recurse. A row
//! whose remaining patterns are all binders is a leaf: its body, with the
//! row's binders renamed to the values they matched. First-match order is
//! kept because rows are never reordered, so the tree takes exactly the
//! arm the written match takes. Bodies are cloned into every leaf that
//! reaches them; a matched value is never computed twice (the subject is
//! evaluated once, sub-values are bound by the flat patterns).
//!
//! An arm that reaches no leaf can never be taken: it is reported as
//! unreachable, which covers the cases the pairwise check in
//! `types::checker::exhaustiveness` cannot see.

use std::collections::{HashMap, HashSet};

use crate::ast::{Expr, FnBody, Literal, MatchArm, Pattern, Spanned, Stmt, TopLevel, VerifyKind};
use crate::types::checker::TypeError;

/// Leaves one match may expand to before the pass refuses it. The
/// clause-matrix expansion is exponential only on adversarial pattern
/// sets; a real program stays far below this.
const MAX_LEAVES: usize = 4096;

/// True when some `match` in `items` uses a nested constructor pattern
/// or a general list pattern.
pub fn has_nested_patterns(items: &[TopLevel]) -> bool {
    let mut found = false;
    for_each_root_expr(items, &mut |expr| {
        if !found {
            found = expr_has_nested(expr);
        }
    });
    found
}

/// The `yield` functions of `items` that use a nested pattern: the
/// `yield` lowering reads the program as written, so these are refused
/// until it learns the nested forms.
pub fn nested_patterns_in_yield_fns(items: &[TopLevel]) -> Vec<TypeError> {
    items
        .iter()
        .filter_map(|item| match item {
            TopLevel::FnDef(fd) if crate::yield_lowering::is_yield_fn(fd) => {
                let has = fd.body.stmts().iter().any(|stmt| match stmt {
                    Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_has_nested(expr),
                });
                has.then(|| TypeError {
                    message: format!(
                        "nested constructor and list patterns are not supported inside the \
                         `yield` function '{}' yet; move the match into a helper function",
                        fd.name
                    ),
                    line: fd.line,
                    col: 0,
                    origin: None,
                    secondary: None,
                })
            }
            _ => None,
        })
        .collect()
}

/// Compile every match holding a nested pattern into flat matches.
/// `families` maps a constructor spelled in a pattern to the variant
/// names of its sum type (from the check of the written program).
/// Returns the arms that can never be taken.
pub fn lower_nested_patterns(
    items: &mut [TopLevel],
    families: &HashMap<String, Vec<String>>,
) -> Vec<TypeError> {
    let mut errors = Vec::new();
    for item in items.iter_mut() {
        match item {
            TopLevel::FnDef(fd) => {
                let has = fd.body.stmts().iter().any(|stmt| match stmt {
                    Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => expr_has_nested(expr),
                });
                if !has {
                    continue;
                }
                let mut lowering = Lowering::new(families, &mut errors);
                let FnBody::Block(stmts) = std::sync::Arc::make_mut(&mut fd.body);
                for stmt in stmts {
                    match stmt {
                        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => lowering.rewrite(expr),
                    }
                }
            }
            TopLevel::Stmt(Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) => {
                if expr_has_nested(expr) {
                    Lowering::new(families, &mut errors).rewrite(expr);
                }
            }
            TopLevel::Verify(vb) => {
                let mut lowering = Lowering::new(families, &mut errors);
                for (lhs, rhs) in &mut vb.cases {
                    lowering.rewrite_if_nested(lhs);
                    lowering.rewrite_if_nested(rhs);
                }
                for givens in &mut vb.case_givens {
                    for (_, expr) in givens {
                        lowering.rewrite_if_nested(expr);
                    }
                }
                if let VerifyKind::Law(law) = &mut vb.kind {
                    lowering.rewrite_if_nested(&mut law.lhs);
                    lowering.rewrite_if_nested(&mut law.rhs);
                    if let Some(when) = &mut law.when {
                        lowering.rewrite_if_nested(when);
                    }
                    for expr in law.because.iter_mut().chain(law.sample_guards.iter_mut()) {
                        lowering.rewrite_if_nested(expr);
                    }
                }
            }
            _ => {}
        }
    }
    errors
}

fn for_each_root_expr(items: &[TopLevel], f: &mut impl FnMut(&Spanned<Expr>)) {
    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                for stmt in fd.body.stmts() {
                    match stmt {
                        Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => f(expr),
                    }
                }
            }
            TopLevel::Stmt(Stmt::Binding(_, _, expr) | Stmt::Expr(expr)) => f(expr),
            TopLevel::Verify(vb) => {
                for (lhs, rhs) in &vb.cases {
                    f(lhs);
                    f(rhs);
                }
                for givens in &vb.case_givens {
                    for (_, expr) in givens {
                        f(expr);
                    }
                }
                if let VerifyKind::Law(law) = &vb.kind {
                    f(&law.lhs);
                    f(&law.rhs);
                    if let Some(when) = &law.when {
                        f(when);
                    }
                    law.because
                        .iter()
                        .chain(law.sample_guards.iter())
                        .for_each(&mut *f);
                }
            }
            _ => {}
        }
    }
}

fn expr_has_nested(expr: &Spanned<Expr>) -> bool {
    crate::codegen::expr_walk::any(expr, &mut |e| match &e.node {
        Expr::Match { arms, .. } => arms.iter().any(|arm| arm.pattern.has_nested_form()),
        _ => false,
    })
}

/// A pattern of the clause matrix: the written pattern with binders
/// split from wildcards and list patterns spelled as cons chains.
#[derive(Debug, Clone)]
enum Pat {
    Wild,
    Bind(String),
    Lit(Literal),
    Tuple(Vec<Pat>),
    Ctor(String, Vec<Pat>),
    Nil,
    Cons(Box<Pat>, Box<Pat>),
}

impl Pat {
    fn from_ast(pattern: &Pattern) -> Pat {
        match pattern {
            Pattern::Wildcard => Pat::Wild,
            Pattern::Ident(name) => Pat::binder(name),
            Pattern::Literal(lit) => Pat::Lit(lit.clone()),
            Pattern::EmptyList => Pat::Nil,
            Pattern::Cons(head, tail) => {
                Pat::Cons(Box::new(Pat::binder(head)), Box::new(Pat::binder(tail)))
            }
            Pattern::Tuple(items) => Pat::Tuple(items.iter().map(Pat::from_ast).collect()),
            Pattern::Constructor(name, binders) => Pat::Ctor(
                name.clone(),
                binders.iter().map(|b| Pat::binder(b)).collect(),
            ),
            Pattern::ConstructorNested(name, fields) => {
                Pat::Ctor(name.clone(), fields.iter().map(Pat::from_ast).collect())
            }
            Pattern::List { items, rest } => {
                let tail = rest.as_deref().map(Pat::binder).unwrap_or(Pat::Nil);
                items.iter().rev().fold(tail, |tail, item| {
                    Pat::Cons(Box::new(Pat::from_ast(item)), Box::new(tail))
                })
            }
        }
    }

    fn binder(name: &str) -> Pat {
        if name == "_" {
            Pat::Wild
        } else {
            Pat::Bind(name.to_string())
        }
    }

    fn is_irrefutable(&self) -> bool {
        matches!(self, Pat::Wild | Pat::Bind(_))
    }

    fn push_binders<'a>(&'a self, out: &mut Vec<&'a str>) {
        match self {
            Pat::Wild | Pat::Lit(_) | Pat::Nil => {}
            Pat::Bind(name) => out.push(name),
            Pat::Tuple(items) | Pat::Ctor(_, items) => {
                items.iter().for_each(|item| item.push_binders(out))
            }
            Pat::Cons(head, tail) => {
                head.push_binders(out);
                tail.push_binders(out);
            }
        }
    }
}

/// Where the value a column tests lives.
#[derive(Debug, Clone)]
enum Occ {
    /// A variable in scope (a parameter, a local, or a binder a flat
    /// pattern introduced).
    Var(String),
    /// The match subject itself, when it is not a variable and no row
    /// binds it whole: it is evaluated exactly once, by the root switch.
    Subject(Box<Spanned<Expr>>),
    /// A field no row looks at.
    Unused,
}

#[derive(Debug, Clone)]
struct Row {
    pats: Vec<Pat>,
    /// `(user binder, variable holding its value)` for binders already
    /// consumed by a switch above.
    renames: Vec<(String, String)>,
    arm: usize,
}

impl Row {
    fn binders_outside(&self, column: usize) -> Vec<&str> {
        let mut out = Vec::new();
        for (index, pat) in self.pats.iter().enumerate() {
            if index != column {
                pat.push_binders(&mut out);
            }
        }
        out
    }
}

/// Constructor families of one switch.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    Tuple,
    Ctor,
    List,
    Bool,
    Lit,
}

fn kind_of(pat: &Pat) -> Kind {
    match pat {
        Pat::Tuple(_) => Kind::Tuple,
        Pat::Ctor(_, _) => Kind::Ctor,
        Pat::Nil | Pat::Cons(_, _) => Kind::List,
        Pat::Lit(Literal::Bool(_)) => Kind::Bool,
        Pat::Lit(_) => Kind::Lit,
        Pat::Wild | Pat::Bind(_) => {
            unreachable!("a switch column is chosen on a refutable pattern")
        }
    }
}

/// `Module.Type.Variant` and `Type.Variant` name the same constructor:
/// compare the last two segments, as the exhaustiveness check does.
fn ctor_key(name: &str) -> (&str, &str) {
    let mut parts = name.rsplit('.');
    let variant = parts.next().unwrap_or(name);
    let owner = parts.next().unwrap_or("");
    (owner, variant)
}

/// One case of a switch: what the flat arm tests, and how many
/// sub-values it binds.
#[derive(Debug, Clone)]
enum Case {
    Tuple(usize),
    Ctor(String, usize),
    Nil,
    Cons,
    Lit(Literal),
}

impl Case {
    fn arity(&self) -> usize {
        match self {
            Case::Tuple(n) | Case::Ctor(_, n) => *n,
            Case::Cons => 2,
            Case::Nil | Case::Lit(_) => 0,
        }
    }

    /// The sub-patterns `pat` contributes under this case, or `None`
    /// when `pat` is a different refutable case.
    fn specialize(&self, pat: &Pat) -> Option<Vec<Pat>> {
        match (self, pat) {
            (_, Pat::Wild | Pat::Bind(_)) => Some(vec![Pat::Wild; self.arity()]),
            (Case::Tuple(n), Pat::Tuple(items)) if items.len() == *n => Some(items.clone()),
            (Case::Ctor(name, n), Pat::Ctor(other, args))
                if ctor_key(name) == ctor_key(other) && args.len() == *n =>
            {
                Some(args.clone())
            }
            (Case::Nil, Pat::Nil) => Some(Vec::new()),
            (Case::Cons, Pat::Cons(head, tail)) => Some(vec![(**head).clone(), (**tail).clone()]),
            (Case::Lit(a), Pat::Lit(b)) if a == b => Some(Vec::new()),
            _ => None,
        }
    }

    fn same_as(&self, pat: &Pat) -> bool {
        !pat.is_irrefutable() && self.specialize(pat).is_some()
    }

    fn of(pat: &Pat) -> Case {
        match pat {
            Pat::Tuple(items) => Case::Tuple(items.len()),
            Pat::Ctor(name, args) => Case::Ctor(name.clone(), args.len()),
            Pat::Nil => Case::Nil,
            Pat::Cons(_, _) => Case::Cons,
            Pat::Lit(lit) => Case::Lit(lit.clone()),
            Pat::Wild | Pat::Bind(_) => unreachable!("cases are read off refutable patterns"),
        }
    }

    /// The flat pattern of this case's arm, binding `fields`.
    fn flat_pattern(&self, fields: &[String]) -> Pattern {
        match self {
            Case::Tuple(_) => Pattern::Tuple(
                fields
                    .iter()
                    .map(|name| {
                        if name == "_" {
                            Pattern::Wildcard
                        } else {
                            Pattern::Ident(name.clone())
                        }
                    })
                    .collect(),
            ),
            Case::Ctor(name, _) => Pattern::Constructor(name.clone(), fields.to_vec()),
            Case::Nil => Pattern::EmptyList,
            Case::Cons => Pattern::Cons(fields[0].clone(), fields[1].clone()),
            Case::Lit(lit) => Pattern::Literal(lit.clone()),
        }
    }
}

struct Lowering<'a> {
    families: &'a HashMap<String, Vec<String>>,
    errors: &'a mut Vec<TypeError>,
    fresh: usize,
}

/// Per-match state: the written arm bodies, which arms some leaf
/// reached, and how many leaves were built.
struct MatchState<'b> {
    bodies: &'b [Spanned<Expr>],
    reached: Vec<bool>,
    leaves: usize,
    line: usize,
}

impl<'a> Lowering<'a> {
    fn new(families: &'a HashMap<String, Vec<String>>, errors: &'a mut Vec<TypeError>) -> Self {
        Self {
            families,
            errors,
            fresh: 0,
        }
    }

    fn rewrite_if_nested(&mut self, expr: &mut Spanned<Expr>) {
        if expr_has_nested(expr) {
            self.rewrite(expr);
        }
    }

    /// Post-order: inner matches are compiled first, so the arm bodies a
    /// match clones are already flat.
    fn rewrite(&mut self, expr: &mut Spanned<Expr>) {
        crate::codegen::expr_walk::for_each_child_mut(expr, &mut |child| self.rewrite(child));
        let Expr::Match { subject, arms } = &expr.node else {
            return;
        };
        if !arms.iter().any(|arm| arm.pattern.has_nested_form()) {
            return;
        }
        let line = expr.line;
        if let Some(compiled) = self.compile_match(subject, arms, line) {
            expr.node = compiled.node;
        }
    }

    fn fresh_name(&mut self) -> String {
        let name = format!("__pat{}", self.fresh);
        self.fresh += 1;
        name
    }

    fn compile_match(
        &mut self,
        subject: &Spanned<Expr>,
        arms: &[MatchArm],
        line: usize,
    ) -> Option<Spanned<Expr>> {
        let bodies: Vec<Spanned<Expr>> = arms.iter().map(|arm| (*arm.body).clone()).collect();
        let rows: Vec<Row> = arms
            .iter()
            .enumerate()
            .map(|(arm, a)| Row {
                pats: vec![Pat::from_ast(&a.pattern)],
                renames: Vec::new(),
                arm,
            })
            .collect();
        let mut state = MatchState {
            bodies: &bodies,
            reached: vec![false; arms.len()],
            leaves: 0,
            line,
        };

        // The root value: a variable is used as it is; any other subject
        // is either tested once by the root switch, or — when some arm
        // binds it whole — bound first by a one-arm match.
        let root_binders: Vec<&str> = rows
            .iter()
            .filter_map(|row| match &row.pats[0] {
                Pat::Bind(name) => Some(name.as_str()),
                _ => None,
            })
            .collect();
        let compiled = match &subject.node {
            Expr::Ident(name) => self.compile(&[Occ::Var(name.clone())], rows, &mut state),
            _ if root_binders.is_empty() => {
                self.compile(&[Occ::Subject(Box::new(subject.clone()))], rows, &mut state)
            }
            _ => {
                let name = self.fresh_name();
                let inner = self.compile(&[Occ::Var(name.clone())], rows, &mut state);
                Spanned::new(
                    Expr::Match {
                        subject: Box::new(subject.clone()),
                        arms: vec![MatchArm::new(Pattern::Ident(name), inner)],
                    },
                    line,
                )
            }
        };

        if state.leaves > MAX_LEAVES {
            self.errors.push(error_at(
                line,
                format!(
                    "this match expands to more than {MAX_LEAVES} cases once its nested patterns are compiled; split it into smaller matches"
                ),
            ));
            return None;
        }
        for (index, reached) in state.reached.iter().enumerate() {
            if !reached {
                self.errors.push(error_at(
                    arms[index].body.line.max(line),
                    format!(
                        "Unreachable match arm: no value reaches pattern {} — the arms above it already match everything it matches",
                        crate::ast::unparse::pattern_to_source(&arms[index].pattern)
                    ),
                ));
            }
        }
        Some(compiled)
    }

    fn compile(
        &mut self,
        occs: &[Occ],
        rows: Vec<Row>,
        state: &mut MatchState<'_>,
    ) -> Spanned<Expr> {
        let line = state.line;
        let first = &rows[0];
        let Some(column) = first.pats.iter().position(|pat| !pat.is_irrefutable()) else {
            return self.leaf(occs, first, state);
        };
        if state.leaves > MAX_LEAVES {
            // Already refused; stop expanding.
            return self.leaf(occs, first, state);
        }
        let occ = occs[column].clone();
        let kind = kind_of(&first.pats[column]);

        // The cases this column tests, in order of first appearance.
        let mut cases: Vec<Case> = Vec::new();
        for row in &rows {
            let pat = &row.pats[column];
            if pat.is_irrefutable() || cases.iter().any(|case| case.same_as(pat)) {
                continue;
            }
            cases.push(Case::of(pat));
        }
        let complete = match kind {
            Kind::Tuple => true,
            Kind::List => cases.len() == 2,
            Kind::Bool => cases.len() == 2,
            Kind::Lit => false,
            Kind::Ctor => self.ctor_cases_complete(&cases),
        };

        let mut out_arms = Vec::new();
        for case in &cases {
            let spec: Vec<(Row, Vec<Pat>)> = rows
                .iter()
                .filter_map(|row| {
                    let fields = case.specialize(&row.pats[column])?;
                    Some((self.consume(row, column, &occ), fields))
                })
                .collect();
            let (names, field_occs) = self.name_fields(&spec, state);
            let mut sub_occs: Vec<Occ> = Vec::with_capacity(occs.len() - 1 + field_occs.len());
            sub_occs.extend(field_occs);
            sub_occs.extend(
                occs.iter()
                    .enumerate()
                    .filter(|(index, _)| *index != column)
                    .map(|(_, occ)| occ.clone()),
            );
            let sub_rows: Vec<Row> = spec
                .into_iter()
                .map(|(mut row, fields)| {
                    let rest: Vec<Pat> = row
                        .pats
                        .drain(..)
                        .enumerate()
                        .filter(|(index, _)| *index != column)
                        .map(|(_, pat)| pat)
                        .collect();
                    row.pats = fields;
                    row.pats.extend(rest);
                    row
                })
                .collect();
            let body = self.compile(&sub_occs, sub_rows, state);
            out_arms.push(MatchArm::new(case.flat_pattern(&names), body));
        }
        if !complete {
            let default_rows: Vec<Row> = rows
                .iter()
                .filter(|row| row.pats[column].is_irrefutable())
                .map(|row| {
                    let mut row = self.consume(row, column, &occ);
                    row.pats.remove(column);
                    row
                })
                .collect();
            // No row for the values no case names: the written match is
            // not exhaustive there. The check of the written program has
            // already refused that, except where it stops looking (deep
            // recursive types); leaving the arm out makes the check of
            // the lowered program report it instead of guessing.
            if !default_rows.is_empty() {
                let sub_occs: Vec<Occ> = occs
                    .iter()
                    .enumerate()
                    .filter(|(index, _)| *index != column)
                    .map(|(_, occ)| occ.clone())
                    .collect();
                let body = self.compile(&sub_occs, default_rows, state);
                out_arms.push(MatchArm::new(Pattern::Wildcard, body));
            }
        }

        let subject = match occ {
            Occ::Var(name) => Spanned::new(Expr::Ident(name), line),
            Occ::Subject(expr) => *expr,
            Occ::Unused => unreachable!("an unused field is never tested"),
        };
        Spanned::new(
            Expr::Match {
                subject: Box::new(subject),
                arms: out_arms,
            },
            line,
        )
    }

    /// The row with the binder at `column` (if any) recorded as a rename
    /// to the variable holding that column's value.
    fn consume(&self, row: &Row, column: usize, occ: &Occ) -> Row {
        let mut row = row.clone();
        if let Pat::Bind(name) = &row.pats[column] {
            match occ {
                Occ::Var(var) => {
                    if name != var {
                        row.renames.push((name.clone(), var.clone()));
                    }
                }
                // The root is bound first whenever an arm binds it whole,
                // and a binder always makes a field a variable.
                Occ::Subject(_) | Occ::Unused => {
                    unreachable!("a bound column always has a variable")
                }
            }
        }
        row.pats[column] = Pat::Wild;
        row
    }

    fn ctor_cases_complete(&self, cases: &[Case]) -> bool {
        let Some(Case::Ctor(first, _)) = cases.first() else {
            return false;
        };
        let (owner, _) = ctor_key(first);
        let variants: Vec<String> = match owner {
            "Option" => vec!["Some".to_string(), "None".to_string()],
            "Result" => vec!["Ok".to_string(), "Err".to_string()],
            _ => match self.families.get(first) {
                Some(variants) => variants.clone(),
                None => return false,
            },
        };
        let present: HashSet<&str> = cases
            .iter()
            .filter_map(|case| match case {
                Case::Ctor(name, _) => Some(ctor_key(name).1),
                _ => None,
            })
            .collect();
        variants
            .iter()
            .all(|variant| present.contains(variant.as_str()))
    }

    /// Names for the sub-values a case binds. A field no row looks at is
    /// `_`; a field every binding row binds under the same name `x` is
    /// bound as `x` itself (when that cannot capture anything); any
    /// other field gets a fresh `__patN`.
    fn name_fields(
        &mut self,
        spec: &[(Row, Vec<Pat>)],
        state: &MatchState<'_>,
    ) -> (Vec<String>, Vec<Occ>) {
        let arity = spec.first().map(|(_, fields)| fields.len()).unwrap_or(0);
        let mut names = Vec::with_capacity(arity);
        let mut occs = Vec::with_capacity(arity);
        for index in 0..arity {
            let column: Vec<&Pat> = spec.iter().map(|(_, fields)| &fields[index]).collect();
            if column.iter().all(|pat| matches!(pat, Pat::Wild)) {
                names.push("_".to_string());
                occs.push(Occ::Unused);
                continue;
            }
            let binders: HashSet<&str> = column
                .iter()
                .filter_map(|pat| match pat {
                    Pat::Bind(name) => Some(name.as_str()),
                    _ => None,
                })
                .collect();
            let reuse = match binders.iter().next() {
                Some(name) if binders.len() == 1 => {
                    let name = *name;
                    let clash = spec.iter().any(|(row, fields)| {
                        let binds_here = matches!(&fields[index], Pat::Bind(n) if n == name);
                        let mut elsewhere = row.binders_outside(usize::MAX);
                        for (other, field) in fields.iter().enumerate() {
                            if other != index || !binds_here {
                                field.push_binders(&mut elsewhere);
                            }
                        }
                        elsewhere.contains(&name)
                            || row.renames.iter().any(|(user, _)| user == name)
                            || (!binds_here && mentions(&state.bodies[row.arm], name))
                    });
                    (!clash).then(|| name.to_string())
                }
                _ => None,
            };
            let name = reuse.unwrap_or_else(|| self.fresh_name());
            names.push(name.clone());
            occs.push(Occ::Var(name));
        }
        (names, occs)
    }

    fn leaf(&mut self, occs: &[Occ], row: &Row, state: &mut MatchState<'_>) -> Spanned<Expr> {
        state.leaves += 1;
        state.reached[row.arm] = true;
        let mut renames: HashMap<String, String> = row.renames.iter().cloned().collect();
        for (pat, occ) in row.pats.iter().zip(occs) {
            if let (Pat::Bind(name), Occ::Var(var)) = (pat, occ)
                && name != var
            {
                renames.insert(name.clone(), var.clone());
            }
        }
        let body = &state.bodies[row.arm];
        if renames.is_empty() {
            return body.clone();
        }
        crate::ast_rewrite::rewrite_idents_scoped(body, |name| {
            renames
                .get(name)
                .map(|var| Spanned::new(Expr::Ident(var.clone()), body.line))
        })
    }
}

/// Whether `name` appears as an identifier anywhere in `expr` (binder
/// scoping ignored — the safe direction for the capture check).
fn mentions(expr: &Spanned<Expr>, name: &str) -> bool {
    crate::codegen::expr_walk::any(expr, &mut |e| match &e.node {
        Expr::Ident(n) => n == name,
        Expr::TailCall(call) => call.target == name,
        _ => false,
    })
}

fn error_at(line: usize, message: String) -> TypeError {
    TypeError {
        message,
        line,
        col: 0,
        origin: None,
        secondary: None,
    }
}

#[cfg(test)]
mod tests;
