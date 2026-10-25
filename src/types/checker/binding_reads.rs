//! What the value of a module-level binding may read.
//!
//! The VM computes a module's bindings before `main`, in the order the module
//! writes them, so a value can read only the bindings written before it. A
//! direct read of a later binding, or of the binding itself, is already an
//! unknown name where the value is checked. This pass follows the functions
//! the value calls or hands on as values, through any number of them and
//! through the later bindings they read, and refuses a value that reaches
//! itself or a binding written after it. A read in a branch that never runs
//! counts the same.

use std::collections::{HashMap, HashSet, VecDeque};

use super::{TypeChecker, TypeErrorSpan};
use crate::ast::{Expr, Spanned, Stmt, StrPart, TopLevel};

/// A binding or a function of the module: what a value can reach.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
enum Node<'a> {
    Binding(&'a str),
    Fn(&'a str),
}

impl<'a> Node<'a> {
    fn name(self) -> &'a str {
        match self {
            Node::Binding(name) | Node::Fn(name) => name,
        }
    }
}

/// Collects the module's bindings and functions a piece of code reads, each
/// with the line it is read on.
struct Reads<'a, 'n> {
    bindings: &'n HashMap<&'a str, usize>,
    fns: &'n HashSet<&'a str>,
    own_module: Option<&'a str>,
    out: Vec<(Node<'a>, usize)>,
}

impl<'a> Reads<'a, '_> {
    fn name(&mut self, name: &'a str, line: usize, bound: &[&str]) {
        if bound.contains(&name) {
            return;
        }
        if let Some((name, _)) = self.bindings.get_key_value(name) {
            self.out.push((Node::Binding(name), line));
        } else if let Some(name) = self.fns.get(name) {
            self.out.push((Node::Fn(name), line));
        }
    }

    fn expr(&mut self, expr: &'a Spanned<Expr>, bound: &mut Vec<&'a str>) {
        match &expr.node {
            Expr::Ident(name) => self.name(name, expr.line, bound),
            Expr::Attr(obj, field) => match &obj.node {
                Expr::Ident(module) if Some(module.as_str()) == self.own_module => {
                    self.name(field, expr.line, &[])
                }
                _ => self.expr(obj, bound),
            },
            Expr::FnCall(callee, args) => {
                self.expr(callee, bound);
                args.iter().for_each(|arg| self.expr(arg, bound));
            }
            Expr::TailCall(call) => {
                self.name(&call.target, expr.line, bound);
                call.args.iter().for_each(|arg| self.expr(arg, bound));
            }
            Expr::BinOp(_, left, right) => {
                self.expr(left, bound);
                self.expr(right, bound);
            }
            Expr::Neg(inner) | Expr::ErrorProp(inner) => self.expr(inner, bound),
            Expr::Constructor(_, inner) => inner.iter().for_each(|e| self.expr(e, bound)),
            Expr::Match { subject, arms, .. } => {
                self.expr(subject, bound);
                for arm in arms {
                    let depth = bound.len();
                    bound.extend(arm.pattern.binder_names());
                    self.expr(&arm.body, bound);
                    bound.truncate(depth);
                }
            }
            Expr::List(items) | Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                items.iter().for_each(|item| self.expr(item, bound));
            }
            Expr::MapLiteral(entries) => {
                for (key, value) in entries {
                    self.expr(key, bound);
                    self.expr(value, bound);
                }
            }
            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let StrPart::Parsed(e) = part {
                        self.expr(e, bound);
                    }
                }
            }
            Expr::RecordCreate { fields, .. } => {
                fields.iter().for_each(|(_, e)| self.expr(e, bound));
            }
            Expr::RecordUpdate { base, updates, .. } => {
                self.expr(base, bound);
                updates.iter().for_each(|(_, e)| self.expr(e, bound));
            }
            Expr::Literal(_) | Expr::Resolved { .. } => {}
        }
    }
}

impl TypeChecker {
    /// Refuses a module-level binding whose value reaches itself, or a
    /// binding written after it, through the functions it calls.
    pub(super) fn check_binding_reads(&mut self, items: &[TopLevel]) {
        let order: Vec<(&str, &Spanned<Expr>)> = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::Stmt(Stmt::Binding(name, _, value)) => Some((name.as_str(), value)),
                _ => None,
            })
            .collect();
        if order.is_empty() {
            return;
        }
        let bindings: HashMap<&str, usize> = order
            .iter()
            .enumerate()
            .map(|(at, (name, _))| (*name, at))
            .collect();
        let fns: HashSet<&str> = items
            .iter()
            .filter_map(|item| match item {
                TopLevel::FnDef(fd) => Some(fd.name.as_str()),
                _ => None,
            })
            .collect();
        let mut reads = Reads {
            bindings: &bindings,
            fns: &fns,
            own_module: crate::visibility::module_decl(items).map(|m| m.name.as_str()),
            out: Vec::new(),
        };
        let mut graph: HashMap<Node, Vec<(Node, usize)>> = HashMap::new();
        for (name, value) in &order {
            reads.expr(value, &mut Vec::new());
            graph.insert(Node::Binding(name), std::mem::take(&mut reads.out));
        }
        for item in items {
            let TopLevel::FnDef(fd) = item else { continue };
            let mut bound: Vec<&str> = fd.params.iter().map(|(p, _)| p.as_str()).collect();
            for stmt in fd.body.stmts() {
                match stmt {
                    Stmt::Binding(local, _, value) => {
                        reads.expr(value, &mut bound);
                        bound.push(local);
                    }
                    Stmt::Expr(value) => reads.expr(value, &mut bound),
                }
            }
            graph.insert(Node::Fn(&fd.name), std::mem::take(&mut reads.out));
        }

        for (at, (name, value)) in order.iter().enumerate() {
            let start = Node::Binding(name);
            // Breadth first, so the path reported is a shortest one. A read
            // straight from the value is already refused where the value is
            // checked when it is not of an earlier binding.
            let mut came_from: HashMap<Node, (Node, usize)> = HashMap::new();
            let mut queue: VecDeque<Node> = VecDeque::new();
            for &(next, line) in &graph[&start] {
                if matches!(next, Node::Fn(_)) && !came_from.contains_key(&next) {
                    came_from.insert(next, (start, line));
                    queue.push_back(next);
                }
            }
            let mut later: Option<Node> = None;
            let mut cycle: Option<(Node, usize)> = None;
            'search: while let Some(node) = queue.pop_front() {
                for &(next, line) in &graph[&node] {
                    if let Node::Binding(read) = next {
                        if read == *name {
                            cycle = Some((node, line));
                            break 'search;
                        }
                        if bindings[read] < at {
                            continue;
                        }
                    }
                    if !came_from.contains_key(&next) {
                        came_from.insert(next, (node, line));
                        if later.is_none() && matches!(next, Node::Binding(_)) {
                            later = Some(next);
                        }
                        queue.push_back(next);
                    }
                }
            }
            let path_to = |last: Node| {
                let mut path = vec![last.name()];
                let mut node = last;
                while let Some(&(from, _)) = came_from.get(&node) {
                    path.push(from.name());
                    node = from;
                }
                path.reverse();
                path.join(" -> ")
            };
            let (message, read, line) = match (cycle, later) {
                (Some((reader, line)), _) => (
                    format!(
                        "Module-level binding '{name}' reads itself while it is computed: {} -> {name}. A binding's value may read only the bindings written before it, also through the functions it calls",
                        path_to(reader)
                    ),
                    *name,
                    line,
                ),
                (None, Some(later)) => {
                    let read = later.name();
                    (
                        format!(
                            "Module-level binding '{name}' reads '{read}' before it is computed: {}, and '{read}' is written after '{name}'. Write '{read}' above '{name}'",
                            path_to(later)
                        ),
                        read,
                        came_from[&later].1,
                    )
                }
                (None, None) => continue,
            };
            self.error_at_line(value.line, message);
            if let Some(error) = self.errors.last_mut() {
                error.secondary = Some(TypeErrorSpan {
                    line,
                    col: 0,
                    label: format!("'{read}' is read here"),
                });
            }
        }
    }
}
