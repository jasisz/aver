//! Type-driven bottom-up expression generator.
//!
//! Existing strategies are syntactic — they pick an AST node and edit
//! it locally. The dominant outcome is a typecheck reject, which is
//! still useful coverage but barely touches the codegen / verify
//! layers behind it. This module flips polarity: build expressions
//! that typecheck **by construction**, so the resulting mutator
//! strategies route the input through the post-typecheck pipeline
//! (resolver, tco, codegen, vm/wasm-gc execution) instead of bouncing
//! at the front gate.
//!
//! Scope-collection is best-effort: we re-derive the function-signature
//! registry and sum/record-type definitions directly from the AST so
//! the generator never needs to invoke the host typechecker. Anything
//! we can't parse out of the source-level type annotation is treated
//! as "give up on this site"; the calling strategy returns `false` and
//! AFL falls back to byte havoc, exactly the same contract as every
//! other strategy.

use std::collections::HashMap;

use aver::ast::{BinOp, Expr, FnBody, FnDef, Literal, Spanned, Stmt, TopLevel, TypeDef, TypeVariant};
use rand::Rng;
use rand::seq::IndexedRandom;

/// Maximum nesting depth for generated expressions. At this depth we
/// stop recurring through fn calls / constructors and fall back to
/// the cheapest base case (literal or in-scope local). Five is enough
/// to thread a Result around two fn calls or build a 3-deep tuple,
/// which covers the shapes the existing corpus stresses.
const MAX_DEPTH: u32 = 5;

/// Subset of Aver's `Type` enum that we need for generation. We
/// re-derive it from source-level annotations rather than reusing
/// `aver::ast::Type` because the source string is what the parser
/// produces; mapping it back to a structural type before the
/// typechecker has run would duplicate parser logic.
#[derive(Debug, Clone, PartialEq)]
pub enum GenType {
    Int,
    Float,
    Str,
    Bool,
    Unit,
    Result(Box<GenType>, Box<GenType>),
    Option(Box<GenType>),
    List(Box<GenType>),
    Tuple(Vec<GenType>),
    /// Any other capitalized identifier we treat as a user-defined
    /// type and resolve against the sum/record tables in `Scope`. If
    /// the lookup misses, generation bails out.
    Named(String),
}

/// Minimal signature of a function we can call from a generated body.
/// Only pure (`effects: []`) functions land here — calling an
/// effectful peer from inside a body the strategy is about to splice
/// would require copying the effect row too, which is more friction
/// than it's worth for the generation contract.
#[derive(Debug, Clone)]
pub struct FnSig {
    pub name: String,
    pub params: Vec<GenType>,
    pub return_type: GenType,
}

/// Scope visible to the generator: locals (params of the containing
/// fn), pure fn signatures in the module, and user-defined types.
#[derive(Debug, Default)]
pub struct Scope {
    pub locals: Vec<(String, GenType)>,
    pub fn_sigs: Vec<FnSig>,
    pub sums: HashMap<String, Vec<TypeVariant>>,
    pub records: HashMap<String, Vec<(String, GenType)>>,
}

/// Parse a source-level type annotation (`"Result<Int, String>"`)
/// into a structural `GenType`. Returns `None` for shapes we don't
/// handle (function-typed params, generic Map keys, …) — callers map
/// `None` to "bail out, skip this site".
pub fn parse_type(s: &str) -> Option<GenType> {
    let s = s.trim();
    match s {
        "Int" => Some(GenType::Int),
        "Float" => Some(GenType::Float),
        "String" => Some(GenType::Str),
        "Bool" => Some(GenType::Bool),
        "Unit" => Some(GenType::Unit),
        _ => {
            if let Some(inner) = strip_generic(s, "Result") {
                let parts = split_top_commas(&inner);
                if parts.len() == 2 {
                    let ok = parse_type(&parts[0])?;
                    let err = parse_type(&parts[1])?;
                    return Some(GenType::Result(Box::new(ok), Box::new(err)));
                }
                return None;
            }
            if let Some(inner) = strip_generic(s, "Option") {
                return Some(GenType::Option(Box::new(parse_type(&inner)?)));
            }
            if let Some(inner) = strip_generic(s, "List") {
                return Some(GenType::List(Box::new(parse_type(&inner)?)));
            }
            if let Some(inner) = s.strip_prefix('(').and_then(|x| x.strip_suffix(')')) {
                let parts = split_top_commas(inner);
                if parts.len() >= 2 {
                    let mut elems = Vec::with_capacity(parts.len());
                    for p in parts {
                        elems.push(parse_type(&p)?);
                    }
                    return Some(GenType::Tuple(elems));
                }
                return None;
            }
            if is_bare_named(s) {
                return Some(GenType::Named(s.to_string()));
            }
            None
        }
    }
}

fn strip_generic(s: &str, head: &str) -> Option<String> {
    let prefix = format!("{head}<");
    let inner = s.strip_prefix(&prefix)?;
    let inner = inner.strip_suffix('>')?;
    Some(inner.to_string())
}

fn split_top_commas(s: &str) -> Vec<String> {
    let mut out = Vec::new();
    let mut depth: i32 = 0;
    let mut cur = String::new();
    for c in s.chars() {
        match c {
            '<' | '(' | '[' => {
                depth += 1;
                cur.push(c);
            }
            '>' | ')' | ']' => {
                depth -= 1;
                cur.push(c);
            }
            ',' if depth == 0 => {
                out.push(cur.trim().to_string());
                cur.clear();
            }
            _ => cur.push(c),
        }
    }
    if !cur.trim().is_empty() {
        out.push(cur.trim().to_string());
    }
    out
}

fn is_bare_named(s: &str) -> bool {
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_ascii_uppercase() {
        return false;
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == '.')
}

/// Walk the AST and collect everything the generator needs to know
/// about the current module: in-scope locals, callable pure fns,
/// type-def shapes. `skip_fn` excludes one fn from `fn_sigs` (used
/// by `replace-fn-body` to avoid an infinite self-recursive body).
pub fn build_scope(
    items: &[TopLevel],
    skip_fn: Option<&str>,
    locals: &[(String, String)],
) -> Scope {
    let mut fn_sigs = Vec::new();
    let mut sums = HashMap::new();
    let mut records: HashMap<String, Vec<(String, GenType)>> = HashMap::new();

    for item in items {
        match item {
            TopLevel::FnDef(fd) => {
                if Some(fd.name.as_str()) == skip_fn {
                    continue;
                }
                if !fd.effects.is_empty() {
                    continue;
                }
                let Some(rt) = parse_type(&fd.return_type) else {
                    continue;
                };
                let mut params = Vec::with_capacity(fd.params.len());
                let mut ok = true;
                for (_, ty) in &fd.params {
                    match parse_type(ty) {
                        Some(t) => params.push(t),
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                if !ok {
                    continue;
                }
                fn_sigs.push(FnSig {
                    name: fd.name.clone(),
                    params,
                    return_type: rt,
                });
            }
            TopLevel::TypeDef(TypeDef::Sum { name, variants, .. }) => {
                sums.insert(name.clone(), variants.clone());
            }
            TopLevel::TypeDef(TypeDef::Product { name, fields, .. }) => {
                let mut parsed = Vec::with_capacity(fields.len());
                let mut ok = true;
                for (fname, fty) in fields {
                    match parse_type(fty) {
                        Some(t) => parsed.push((fname.clone(), t)),
                        None => {
                            ok = false;
                            break;
                        }
                    }
                }
                if ok {
                    records.insert(name.clone(), parsed);
                }
            }
            _ => {}
        }
    }

    let mut local_typed = Vec::new();
    for (name, ty) in locals {
        if let Some(t) = parse_type(ty) {
            local_typed.push((name.clone(), t));
        }
    }

    Scope {
        locals: local_typed,
        fn_sigs,
        sums,
        records,
    }
}

/// Random-walk generator. Top-level entry: produce a `Spanned<Expr>`
/// of `target`. Returns `None` if no well-typed expression can be
/// produced (typically a `Named(_)` we have no definition for).
pub fn generate<R: Rng>(
    target: &GenType,
    scope: &Scope,
    depth: u32,
    rng: &mut R,
) -> Option<Spanned<Expr>> {
    let at_max = depth >= MAX_DEPTH;

    let local_matches: Vec<&(String, GenType)> = scope
        .locals
        .iter()
        .filter(|(_, t)| t == target)
        .collect();

    // At max depth, take a local if available, otherwise fall through
    // to the literal-emitting base case. Skip fn calls and recursive
    // constructors so generation always terminates.
    if at_max {
        if let Some((name, _)) = local_matches.choose(rng) {
            return Some(Spanned::bare(Expr::Ident(name.clone())));
        }
        return generate_base(target, scope, depth, rng);
    }

    let sig_matches: Vec<&FnSig> = scope
        .fn_sigs
        .iter()
        .filter(|s| &s.return_type == target)
        .collect();

    // Weighted choice: locals + fn calls compete with the base case
    // (literal / constructor). The base case is always in the pool so
    // primitives don't always hit the same fn-call path.
    let mut weights: Vec<u8> = vec![0]; // base
    if !local_matches.is_empty() {
        weights.push(1);
        weights.push(1); // bump locals so we use bound names often
    }
    if !sig_matches.is_empty() {
        weights.push(2);
    }
    if matches!(target, GenType::Bool) {
        weights.push(3); // comparison
    }
    if matches!(target, GenType::Int) {
        weights.push(4); // arithmetic
    }

    let &choice = weights.choose(rng).unwrap_or(&0);
    match choice {
        1 => {
            let (name, _) = local_matches.choose(rng)?;
            Some(Spanned::bare(Expr::Ident(name.clone())))
        }
        2 => {
            let sig = sig_matches.choose(rng)?;
            let mut args = Vec::with_capacity(sig.params.len());
            for p in &sig.params {
                args.push(generate(p, scope, depth + 1, rng)?);
            }
            let callee = Spanned::bare(Expr::Ident(sig.name.clone()));
            Some(Spanned::bare(Expr::FnCall(Box::new(callee), args)))
        }
        3 => generate_comparison(scope, depth, rng).or_else(|| generate_base(target, scope, depth, rng)),
        4 => generate_int_arith(scope, depth, rng).or_else(|| generate_base(target, scope, depth, rng)),
        _ => generate_base(target, scope, depth, rng),
    }
}

fn generate_comparison<R: Rng>(
    scope: &Scope,
    depth: u32,
    rng: &mut R,
) -> Option<Spanned<Expr>> {
    // Compare two Ints — simplest shape that's always well-typed.
    let lhs = generate(&GenType::Int, scope, depth + 1, rng)?;
    let rhs = generate(&GenType::Int, scope, depth + 1, rng)?;
    let op = *[BinOp::Eq, BinOp::Neq, BinOp::Lt, BinOp::Gt, BinOp::Lte, BinOp::Gte]
        .choose(rng)
        .unwrap();
    Some(Spanned::bare(Expr::BinOp(
        op,
        Box::new(lhs),
        Box::new(rhs),
    )))
}

fn generate_int_arith<R: Rng>(
    scope: &Scope,
    depth: u32,
    rng: &mut R,
) -> Option<Spanned<Expr>> {
    let lhs = generate(&GenType::Int, scope, depth + 1, rng)?;
    let rhs = generate(&GenType::Int, scope, depth + 1, rng)?;
    let op = *[BinOp::Add, BinOp::Sub, BinOp::Mul].choose(rng).unwrap();
    Some(Spanned::bare(Expr::BinOp(
        op,
        Box::new(lhs),
        Box::new(rhs),
    )))
}

fn generate_base<R: Rng>(
    target: &GenType,
    scope: &Scope,
    depth: u32,
    rng: &mut R,
) -> Option<Spanned<Expr>> {
    match target {
        GenType::Int => Some(Spanned::bare(Expr::Literal(Literal::Int(
            rng.random_range(-32..32),
        )))),
        GenType::Float => Some(Spanned::bare(Expr::Literal(Literal::Float(
            (rng.random_range(-100..100) as f64) / 10.0,
        )))),
        GenType::Str => Some(Spanned::bare(Expr::Literal(Literal::Str(
            "fuzz".to_string(),
        )))),
        GenType::Bool => {
            let b = rng.random_bool(0.5);
            Some(Spanned::bare(Expr::Literal(Literal::Bool(b))))
        }
        GenType::Unit => Some(Spanned::bare(Expr::Literal(Literal::Unit))),
        GenType::Result(ok, err) => {
            // Bias toward Ok so a chain of nested Results keeps
            // generation finite (Err on a primitive is the shorter
            // path, so we let it appear ~30% of the time too).
            if rng.random_bool(0.7) {
                let arg = generate(ok, scope, depth + 1, rng)?;
                Some(constructor_call("Result", "Ok", vec![arg]))
            } else {
                let arg = generate(err, scope, depth + 1, rng)?;
                Some(constructor_call("Result", "Err", vec![arg]))
            }
        }
        GenType::Option(inner) => {
            if rng.random_bool(0.7) {
                let arg = generate(inner, scope, depth + 1, rng)?;
                Some(constructor_call("Option", "Some", vec![arg]))
            } else {
                // Option.None is field access, no call parens. Match
                // what the parser produces for the bare constructor.
                Some(Spanned::bare(Expr::Attr(
                    Box::new(Spanned::bare(Expr::Ident("Option".to_string()))),
                    "None".to_string(),
                )))
            }
        }
        GenType::List(elem) => {
            let n = rng.random_range(0..3);
            let mut items = Vec::with_capacity(n);
            for _ in 0..n {
                items.push(generate(elem, scope, depth + 1, rng)?);
            }
            Some(Spanned::bare(Expr::List(items)))
        }
        GenType::Tuple(elems) => {
            let mut items = Vec::with_capacity(elems.len());
            for e in elems {
                items.push(generate(e, scope, depth + 1, rng)?);
            }
            Some(Spanned::bare(Expr::Tuple(items)))
        }
        GenType::Named(name) => {
            if let Some(variants) = scope.sums.get(name).cloned() {
                let v = variants.choose(rng)?.clone();
                if v.fields.is_empty() {
                    // `Shape.Point` shape — bare attribute access, no
                    // parens. Otherwise the parser sees an empty arg
                    // list which is legal but verbose.
                    return Some(Spanned::bare(Expr::Attr(
                        Box::new(Spanned::bare(Expr::Ident(name.clone()))),
                        v.name,
                    )));
                }
                let mut args = Vec::with_capacity(v.fields.len());
                for fty in &v.fields {
                    let t = parse_type(fty)?;
                    args.push(generate(&t, scope, depth + 1, rng)?);
                }
                Some(constructor_call(name, &v.name, args))
            } else if let Some(fields) = scope.records.get(name).cloned() {
                let mut record_fields = Vec::with_capacity(fields.len());
                for (fname, fty) in fields {
                    let val = generate(&fty, scope, depth + 1, rng)?;
                    record_fields.push((fname, val));
                }
                Some(Spanned::bare(Expr::RecordCreate {
                    type_name: name.clone(),
                    fields: record_fields,
                }))
            } else {
                None
            }
        }
    }
}

/// Build `Namespace.Variant(arg1, arg2, ...)` as the FnCall(Attr, …)
/// shape the parser produces for the same source. Using the parser's
/// canonical shape keeps mutator → unparse → re-parse stable: every
/// downstream pass already handles this form.
fn constructor_call(
    namespace: &str,
    variant: &str,
    args: Vec<Spanned<Expr>>,
) -> Spanned<Expr> {
    let callee = Spanned::bare(Expr::Attr(
        Box::new(Spanned::bare(Expr::Ident(namespace.to_string()))),
        variant.to_string(),
    ));
    Spanned::bare(Expr::FnCall(Box::new(callee), args))
}

/// Replace `fd.body` with a freshly-generated expression of
/// `fd.return_type`. Mutates `fd` in place via Arc::make_mut to keep
/// the rest of the AST sharing intact.
pub fn replace_body<R: Rng>(rng: &mut R, items: &[TopLevel], fd: &mut FnDef) -> bool {
    let Some(target) = parse_type(&fd.return_type) else {
        return false;
    };
    let scope = build_scope(items, Some(&fd.name), &fd.params);
    let Some(expr) = generate(&target, &scope, 0, rng) else {
        return false;
    };
    let new_body = FnBody::Block(vec![Stmt::Expr(expr)]);
    fd.body = std::sync::Arc::new(new_body);
    // Resolution metadata is stale after the body swap; the resolver
    // re-runs from the parser entry so a `None` here is correct.
    fd.resolution = None;
    true
}

/// Build a fresh `FnDef` with a random pure signature and a generated
/// body of the right return type. `existing_names` is consulted to
/// avoid colliding with an already-declared fn (the parser silently
/// shadows duplicates, so the mutation would still parse — but the
/// reader can't see why the second fn never gets called).
pub fn synthesize_fn<R: Rng>(
    rng: &mut R,
    items: &[TopLevel],
    existing_names: &[String],
) -> Option<FnDef> {
    let return_types: [(&str, GenType); 3] = [
        ("Int", GenType::Int),
        ("Bool", GenType::Bool),
        ("Unit", GenType::Unit),
    ];
    let param_types: [(&str, &str); 2] = [("Int", "Int"), ("Bool", "Bool")];

    let (rt_name, rt_gen) = return_types.choose(rng).cloned()?;
    let param_count = rng.random_range(0..3);
    let param_names = ["x", "y", "z"];
    let mut params: Vec<(String, String)> = Vec::with_capacity(param_count);
    for i in 0..param_count {
        let &(_, ty_name) = param_types.choose(rng)?;
        params.push((param_names[i].to_string(), ty_name.to_string()));
    }

    let mut name_idx = 0u32;
    let new_name = loop {
        let candidate = format!("fuzz_g{name_idx}");
        if !existing_names.iter().any(|n| n == &candidate) {
            break candidate;
        }
        name_idx += 1;
        if name_idx > 256 {
            return None;
        }
    };

    let scope = build_scope(items, None, &params);
    let body_expr = generate(&rt_gen, &scope, 0, rng)?;
    let body = FnBody::Block(vec![Stmt::Expr(body_expr)]);

    Some(FnDef {
        name: new_name,
        line: 0,
        params,
        return_type: rt_name.to_string(),
        effects: Vec::new(),
        desc: None,
        body: std::sync::Arc::new(body),
        resolution: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_primitives() {
        assert_eq!(parse_type("Int"), Some(GenType::Int));
        assert_eq!(parse_type("  String "), Some(GenType::Str));
        assert_eq!(parse_type("Bool"), Some(GenType::Bool));
    }

    #[test]
    fn parse_generics() {
        assert_eq!(
            parse_type("Result<Int, String>"),
            Some(GenType::Result(
                Box::new(GenType::Int),
                Box::new(GenType::Str)
            ))
        );
        assert_eq!(
            parse_type("List<Bool>"),
            Some(GenType::List(Box::new(GenType::Bool)))
        );
    }

    #[test]
    fn parse_tuple() {
        assert_eq!(
            parse_type("(Int, String)"),
            Some(GenType::Tuple(vec![GenType::Int, GenType::Str]))
        );
    }

    #[test]
    fn parse_named() {
        assert_eq!(parse_type("Shape"), Some(GenType::Named("Shape".to_string())));
    }

    #[test]
    fn split_top_commas_basic() {
        assert_eq!(split_top_commas("Int, String"), vec!["Int", "String"]);
        assert_eq!(
            split_top_commas("Result<Int, String>, Int"),
            vec!["Result<Int, String>", "Int"]
        );
    }
}
