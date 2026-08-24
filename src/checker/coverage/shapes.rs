use std::collections::{BTreeSet, HashMap};

use crate::ast::{Expr, FnDef, Literal, Pattern, Spanned, Stmt, TopLevel};

use super::super::constructor_tag_from_expr;

/// An outer value shape that `verify-coverage` can state without evaluating
/// user code. Payloads are deliberately absent: the lint only asks which arm
/// a value occupies.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum OuterShape {
    Constructor(String),
    Bool(bool),
    EmptyList,
    NonEmptyList,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct ShapeSummary {
    pub(super) shapes: BTreeSet<OuterShape>,
    /// False when some return path goes through an expression whose outer
    /// shape the static checker cannot know. A partial set must never become
    /// a denominator: that would recreate the false-positive this analysis
    /// exists to remove.
    pub(super) complete: bool,
}

impl Default for ShapeSummary {
    fn default() -> Self {
        Self {
            shapes: BTreeSet::new(),
            complete: true,
        }
    }
}

impl ShapeSummary {
    fn exact(shape: OuterShape) -> Self {
        Self {
            shapes: BTreeSet::from([shape]),
            complete: true,
        }
    }

    fn unknown() -> Self {
        Self {
            shapes: BTreeSet::new(),
            complete: false,
        }
    }

    fn join(&mut self, other: Self) {
        self.shapes.extend(other.shapes);
        self.complete &= other.complete;
    }
}

/// A finite, static outer-shape analysis for local functions.
///
/// Function calls are solved to a fixed point. That makes mutually recursive
/// return paths ordinary edges in the analysis instead of a special case:
/// constructors from every base arm flow around the SCC until the set stops
/// growing. Calls outside this file make the summary incomplete, so callers
/// decline to make a coverage claim rather than guessing.
pub(in crate::checker) struct ShapeAnalysis {
    summaries: HashMap<String, ShapeSummary>,
}

impl ShapeAnalysis {
    pub(in crate::checker) fn new(items: &[TopLevel]) -> Self {
        let functions: HashMap<&str, &FnDef> = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(function) => Some((function.name.as_str(), function)),
                _ => None,
            })
            .collect();
        let mut summaries: HashMap<String, ShapeSummary> = functions
            .keys()
            .map(|name| ((*name).to_string(), ShapeSummary::default()))
            .collect();

        loop {
            let mut next = summaries.clone();
            for (name, function) in &functions {
                next.insert((*name).to_string(), summarize_body(function, &summaries));
            }
            if next == summaries {
                break;
            }
            summaries = next;
        }

        Self { summaries }
    }

    pub(super) fn function(&self, name: &str) -> Option<&ShapeSummary> {
        self.summaries.get(name)
    }

    /// Resolve one verify expression only when it has exactly one statically
    /// knowable outer shape. A helper that always returns `Result.Err`, or
    /// always builds a non-empty list, is creditable. A helper whose result
    /// depends on its arguments is not.
    pub(super) fn exact_expr_shape(&self, expr: &Spanned<Expr>) -> Option<OuterShape> {
        let summary = summarize_expr(expr, &self.summaries, &HashMap::new());
        if !summary.complete || summary.shapes.len() != 1 {
            return None;
        }
        summary.shapes.into_iter().next()
    }
}

fn summarize_body(function: &FnDef, functions: &HashMap<String, ShapeSummary>) -> ShapeSummary {
    let mut bindings = HashMap::new();
    let Some((tail, prefix)) = function.body.stmts().split_last() else {
        return ShapeSummary::unknown();
    };

    for statement in prefix {
        if let Stmt::Binding(name, _, value) = statement {
            let summary = summarize_expr(value, functions, &bindings);
            bindings.insert(name.clone(), summary);
        }
    }

    match tail {
        Stmt::Expr(expr) => summarize_expr(expr, functions, &bindings),
        Stmt::Binding(_, _, _) => ShapeSummary::unknown(),
    }
}

fn summarize_expr(
    expr: &Spanned<Expr>,
    functions: &HashMap<String, ShapeSummary>,
    bindings: &HashMap<String, ShapeSummary>,
) -> ShapeSummary {
    if let Some(tag) = constructor_tag_from_expr(expr) {
        return ShapeSummary::exact(OuterShape::Constructor(tag));
    }

    match &expr.node {
        Expr::Literal(Literal::Bool(value)) => ShapeSummary::exact(OuterShape::Bool(*value)),
        Expr::List(items) if items.is_empty() => ShapeSummary::exact(OuterShape::EmptyList),
        Expr::List(_) => ShapeSummary::exact(OuterShape::NonEmptyList),
        Expr::Ident(name) | Expr::Resolved { name, .. } => bindings
            .get(name)
            .cloned()
            .unwrap_or_else(ShapeSummary::unknown),
        Expr::FnCall(callee, _) => local_callee_name(callee)
            .and_then(|name| functions.get(name))
            .cloned()
            .unwrap_or_else(ShapeSummary::unknown),
        Expr::TailCall(call) => functions
            .get(&call.target)
            .cloned()
            .unwrap_or_else(ShapeSummary::unknown),
        Expr::Match { arms, .. } => {
            let mut result = ShapeSummary {
                shapes: BTreeSet::new(),
                complete: true,
            };
            for arm in arms {
                let mut arm_bindings = bindings.clone();
                forget_pattern_bindings(&arm.pattern, &mut arm_bindings);
                result.join(summarize_expr(&arm.body, functions, &arm_bindings));
            }
            result
        }
        _ => ShapeSummary::unknown(),
    }
}

fn local_callee_name(expr: &Spanned<Expr>) -> Option<&str> {
    match &expr.node {
        Expr::Ident(name) | Expr::Resolved { name, .. } => Some(name),
        _ => None,
    }
}

fn forget_pattern_bindings(pattern: &Pattern, bindings: &mut HashMap<String, ShapeSummary>) {
    match pattern {
        Pattern::Ident(name) => {
            bindings.remove(name);
        }
        Pattern::Cons(head, tail) => {
            bindings.remove(head);
            bindings.remove(tail);
        }
        Pattern::Tuple(items) => {
            for item in items {
                forget_pattern_bindings(item, bindings);
            }
        }
        Pattern::Constructor(_, names) => {
            for name in names {
                bindings.remove(name);
            }
        }
        Pattern::Wildcard | Pattern::Literal(_) | Pattern::EmptyList => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn parse(source: &str) -> Vec<TopLevel> {
        let tokens = Lexer::new(source).tokenize().expect("lex failed");
        Parser::new(tokens).parse().expect("parse failed")
    }

    fn analyze(source: &str) -> ShapeAnalysis {
        ShapeAnalysis::new(&parse(source))
    }

    #[test]
    fn mutual_recursion_reaches_a_fixed_point() {
        let mut items = parse(
            r#"
type Finding
    Left
    Right

fn left(flag: Bool) -> Finding
    match flag
        true -> Finding.Left
        false -> right(flag)

fn right(flag: Bool) -> Finding
    match flag
        true -> Finding.Right
        false -> left(flag)
"#,
        );
        crate::tco::transform_program(&mut items);
        let analysis = ShapeAnalysis::new(&items);
        let expected = BTreeSet::from([
            OuterShape::Constructor("Finding.Left".to_string()),
            OuterShape::Constructor("Finding.Right".to_string()),
        ]);
        assert_eq!(analysis.function("left").unwrap().shapes, expected);
        assert_eq!(analysis.function("right").unwrap().shapes, expected);
    }

    #[test]
    fn an_external_return_path_makes_the_summary_incomplete() {
        let analysis = analyze(
            r#"
type Finding
    Known
    Other

fn classify(flag: Bool) -> Finding
    match flag
        true -> Finding.Known
        false -> externalFinding()
"#,
        );
        let summary = analysis.function("classify").unwrap();
        assert!(!summary.complete);
        assert!(
            summary
                .shapes
                .contains(&OuterShape::Constructor("Finding.Known".to_string()))
        );
    }
}
