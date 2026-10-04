//! Empty collection literals handed to a generic collection builtin.
//!
//! `[]`, `{}` and `Option.None` carry nothing that fixes their element
//! type, and a node's type is stamped once. A call such as `List.len([])`
//! used to leave its `[]` stamped `List<T>`, which the VM ignores but a
//! backend with one representation per element type cannot lower. Here the
//! call fixes the type before the literal is inferred: from the call's
//! other arguments, then from the type its context expects, and a type the
//! call's result does not carry — so nothing outside the call can observe
//! it — settles on `Int`. A type the result does carry is left open unless
//! the context supplied one, exactly as before.

use super::expr::is_bare_none_expr;
use super::*;

/// A position no argument has fixed yet. No signature names a variable so.
const HOLE: &str = "?";

fn hole() -> Type {
    Type::Var(HOLE.to_string())
}

/// The shape of a literal with no element type of its own.
fn open_literal_shape(expr: &Expr) -> Option<Type> {
    match expr {
        Expr::List(items) if items.is_empty() => Some(Type::List(Box::new(hole()))),
        Expr::MapLiteral(entries) if entries.is_empty() => {
            Some(Type::Map(Box::new(hole()), Box::new(hole())))
        }
        _ if is_bare_none_expr(expr) => Some(Type::Option(Box::new(hole()))),
        _ => None,
    }
}

/// The generic collection builtins instantiated here. `List.prepend` and
/// `Map.set` read their element type off an argument of their own and have
/// recognisers that run first.
fn is_open_call_name(name: &str) -> bool {
    (name.starts_with("List.") || name.starts_with("Map.") || name.starts_with("Vector."))
        && !matches!(name, "List.prepend" | "Map.set")
}

/// `ty` with every type variable turned into a hole.
fn to_shape(ty: &Type) -> Type {
    TypeChecker::instantiate_all_vars(ty, &hole())
}

/// `bound` with its open positions filled from `shape`; where both are
/// fixed, `bound` stands and the ordinary argument check reports a clash.
fn refine(bound: &Type, shape: &Type) -> Type {
    let go = |b: &Type, s: &Type| Box::new(refine(b, s));
    match (bound, shape) {
        (Type::Var(_), _) => shape.clone(),
        (Type::List(b), Type::List(s)) => Type::List(go(b, s)),
        (Type::Option(b), Type::Option(s)) => Type::Option(go(b, s)),
        (Type::Vector(b), Type::Vector(s)) => Type::Vector(go(b, s)),
        (Type::Map(bk, bv), Type::Map(sk, sv)) => Type::Map(go(bk, sk), go(bv, sv)),
        (Type::Result(bo, be), Type::Result(so, se)) => Type::Result(go(bo, so), go(be, se)),
        (Type::Tuple(bs), Type::Tuple(ss)) if bs.len() == ss.len() => {
            Type::Tuple(bs.iter().zip(ss).map(|(b, s)| refine(b, s)).collect())
        }
        _ => bound.clone(),
    }
}

/// Bind the signature variables of `param` to what `shape` fixes.
fn bind_shape(param: &Type, shape: &Type, subst: &mut HashMap<String, Type>) {
    match (param, shape) {
        (_, Type::Var(name)) if name == HOLE => {}
        (Type::Var(name), _) => {
            let merged = match subst.get(name) {
                Some(bound) => refine(bound, shape),
                None => shape.clone(),
            };
            subst.insert(name.clone(), merged);
        }
        (Type::List(p), Type::List(s))
        | (Type::Option(p), Type::Option(s))
        | (Type::Vector(p), Type::Vector(s)) => bind_shape(p, s, subst),
        (Type::Map(pk, pv), Type::Map(sk, sv)) | (Type::Result(pk, pv), Type::Result(sk, sv)) => {
            bind_shape(pk, sk, subst);
            bind_shape(pv, sv, subst);
        }
        (Type::Tuple(ps), Type::Tuple(ss)) if ps.len() == ss.len() => {
            for (p, s) in ps.iter().zip(ss) {
                bind_shape(p, s, subst);
            }
        }
        _ => {}
    }
}

fn collect_vars(ty: &Type, out: &mut HashSet<String>) {
    match ty {
        Type::Var(name) => {
            out.insert(name.clone());
        }
        Type::List(inner) | Type::Option(inner) | Type::Vector(inner) => collect_vars(inner, out),
        Type::Map(a, b) | Type::Result(a, b) => {
            collect_vars(a, out);
            collect_vars(b, out);
        }
        Type::Tuple(items) => items.iter().for_each(|item| collect_vars(item, out)),
        Type::Fn(params, ret, _) => {
            params.iter().for_each(|p| collect_vars(p, out));
            collect_vars(ret, out);
        }
        _ => {}
    }
}

impl TypeChecker {
    pub(super) fn is_open_call(&self, expr: &Expr) -> bool {
        matches!(expr, Expr::FnCall(callee, args)
            if Self::callee_key(&callee.node).is_some_and(|name| is_open_call_name(&name))
                && args.iter().any(|arg| self.is_open(&arg.node)))
    }

    fn is_open(&self, expr: &Expr) -> bool {
        open_literal_shape(expr).is_some() || self.is_open_call(expr)
    }

    /// Infer a generic collection builtin call with an open argument; see
    /// the module doc. `None` when the call has no open argument, so the
    /// caller's ordinary path stands. `expected` is what the context wants
    /// of the result (it may still have holes); `default_all` says the
    /// result itself is invisible outside an enclosing call, so every type
    /// left open may settle.
    pub(super) fn infer_open_collection_call(
        &mut self,
        name: &str,
        args: &[Spanned<Expr>],
        expected: Option<&Type>,
        default_all: bool,
    ) -> Option<Type> {
        if !is_open_call_name(name) || !args.iter().any(|arg| self.is_open(&arg.node)) {
            return None;
        }
        let sig = self.find_fn_sig(name)?.clone();
        if sig.params.len() != args.len() {
            return None;
        }
        let defaults_ok = default_all || expected.is_some_and(type_is_fully_concrete);
        let mut escaping = HashSet::new();
        if !defaults_ok {
            collect_vars(&sig.ret, &mut escaping);
        }
        let mut subst = HashMap::new();
        let mut arg_types: Vec<Option<Type>> = vec![None; args.len()];
        for (slot, (arg, param)) in arg_types.iter_mut().zip(args.iter().zip(&sig.params)) {
            if !self.is_open(&arg.node) {
                let ty = self.infer_type(arg);
                self.match_with(&ty, param, &mut subst);
                *slot = Some(ty);
            }
        }
        if let Some(expected) = expected {
            bind_shape(&sig.ret, &to_shape(expected), &mut subst);
        }
        for (arg, param) in args.iter().zip(&sig.params) {
            if let Some(shape) = open_literal_shape(&arg.node) {
                bind_shape(param, &shape, &mut subst);
            }
        }
        // A parameter may settle when every variable it mentions is either
        // fixed already or invisible outside this call.
        let may_settle = |param: &Type, subst: &HashMap<String, Type>| {
            let mut vars = HashSet::new();
            collect_vars(param, &mut vars);
            vars.iter().all(|var| {
                !escaping.contains(var) || subst.get(var).is_some_and(type_is_fully_concrete)
            })
        };
        for (slot, (arg, param)) in arg_types.iter_mut().zip(args.iter().zip(&sig.params)) {
            if slot.is_none() && self.is_open_call(&arg.node) {
                let ty = if may_settle(param, &subst) {
                    let partial = to_shape(&Self::instantiate_type(param, &subst));
                    self.infer_open_call_node(arg, &partial)
                } else {
                    self.infer_type(arg)
                };
                bind_shape(param, &to_shape(&ty), &mut subst);
                *slot = Some(ty);
            }
        }
        for (slot, (arg, param)) in arg_types.iter_mut().zip(args.iter().zip(&sig.params)) {
            if slot.is_none() {
                let ty = if may_settle(param, &subst) {
                    let settled = Self::instantiate_all_vars(
                        &Self::instantiate_type(param, &subst),
                        &Type::Int,
                    );
                    self.infer_type_with_expected(arg, Some(&settled))
                } else {
                    self.infer_type(arg)
                };
                *slot = Some(ty);
            }
        }
        let arg_types: Vec<Type> = arg_types.into_iter().flatten().collect();
        if let Some(ty) = self.infer_list_call_type(name, &arg_types) {
            return Some(ty);
        }
        if let Some(ty) = self.infer_map_call_type(name, &arg_types) {
            return Some(ty);
        }
        self.infer_vector_call_type(name, args, &arg_types)
    }

    /// The left side of `left == right` when neither side has a type of
    /// its own: equality does not show the element type, so whatever the
    /// right side's shape leaves open settles. The caller hands the result
    /// to the right side as its expected type. `None` when either side is
    /// not open; the caller's ordinary order then stands.
    pub(super) fn infer_open_equality_side(
        &mut self,
        left: &Spanned<Expr>,
        right: &Expr,
    ) -> Option<Type> {
        if !self.is_open(&left.node) || !self.is_open(right) {
            return None;
        }
        let partial = open_literal_shape(right).unwrap_or_else(hole);
        Some(match open_literal_shape(&left.node) {
            Some(shape) => {
                let settled = Self::instantiate_all_vars(&refine(&shape, &partial), &Type::Int);
                self.infer_type_with_expected(left, Some(&settled))
            }
            None => self.infer_open_call_node(left, &partial),
        })
    }

    /// An open call in argument position, inferred against what its
    /// enclosing call has fixed so far; whatever is still open settles.
    fn infer_open_call_node(&mut self, expr: &Spanned<Expr>, partial: &Type) -> Type {
        if let Expr::FnCall(callee, args) = &expr.node
            && let Some(name) = Self::callee_key(&callee.node)
            && let Some(ty) = self.infer_open_collection_call(&name, args, Some(partial), true)
        {
            let ty = self.canonicalize_named(ty);
            expr.set_ty(ty.clone());
            return ty;
        }
        self.infer_type(expr)
    }
}
