use std::collections::{HashMap, HashSet};

use super::*;

const EXHAUSTIVENESS_MAX_DEPTH: usize = 64;

/// Maximum depth for recursive Named types in exhaustiveness checking.
/// Recursive sum types (e.g. Expr with ExprAdd(Expr, Expr)) cause exponential
/// branching. After this many nested expansions of the same Named type,
/// we assume coverage and stop exploring deeper.
const RECURSIVE_TYPE_MAX_DEPTH: usize = 2;

#[derive(Debug, Clone, PartialEq)]
enum CoverPat {
    Wild,
    Lit(Literal),
    EmptyList,
    Cons(Box<CoverPat>, Box<CoverPat>),
    Tuple(Vec<CoverPat>),
    Constructor(String, Vec<CoverPat>),
}

#[derive(Debug, Clone)]
enum CtorTag {
    Bool(bool),
    ResultOk,
    ResultErr,
    OptionSome,
    OptionNone,
    ListEmpty,
    ListCons,
    Tuple,
    Named(String), // fully-qualified constructor name
}

#[derive(Debug, Clone)]
struct CtorSpec {
    tag: CtorTag,
    arg_types: Vec<Type>,
}

impl TypeChecker {
    /// Walk arms forward; if an earlier arm's normalized pattern subsumes a
    /// later one, the later arm can never fire (Aver match is first-arm-wins).
    /// Emits a type error naming the earlier covering arm so the user can pick
    /// which one to delete.
    pub(super) fn check_match_redundancy(&mut self, arms: &[crate::ast::MatchArm]) {
        let pats: Vec<CoverPat> = arms.iter().map(|a| normalize_pattern(&a.pattern)).collect();
        for i in 1..pats.len() {
            for j in 0..i {
                if pattern_subsumes(&pats[j], &pats[i]) {
                    let line = arms[i].body.line.max(1);
                    let earlier_line = arms[j].body.line.max(1);
                    self.error_at_line(
                        line,
                        format!(
                            "Unreachable match arm: pattern {} is already covered by an earlier arm at line {}",
                            format_cover_pattern(&pats[i]),
                            earlier_line,
                        ),
                    );
                    break;
                }
            }
        }
    }

    /// Check whether a match expression covers all possible values of the subject type.
    /// Emits a type error if any cases are missing.
    pub(super) fn check_match_exhaustiveness(
        &mut self,
        subject_ty: &Type,
        arms: &[crate::ast::MatchArm],
        line: usize,
    ) {
        // Preserve historical behavior: these domains are not exhaustiveness-checked.
        match subject_ty {
            Type::Map(_, _) | Type::Fn(_, _, _) | Type::Unit | Type::Var(_) => {
                return;
            }
            Type::Named { name, .. } if !self.has_variants_for(name) => return, // records
            _ => {}
        }

        let rows: Vec<Vec<CoverPat>> = arms
            .iter()
            .map(|arm| vec![normalize_pattern(&arm.pattern)])
            .collect();
        let mut seen = HashSet::new();
        let mut type_depth = HashMap::new();
        if let Some(witness_vec) = self.find_uncovered_vector(
            std::slice::from_ref(subject_ty),
            &rows,
            &mut seen,
            0,
            &mut type_depth,
        ) {
            let witness_msg = if let Some(first) = witness_vec.first() {
                if is_catch_all_witness(first) {
                    "missing catch-all (_) pattern".to_string()
                } else if matches!(first, CoverPat::Cons(_, _)) {
                    "missing pattern [h, ..t]".to_string()
                } else {
                    format!("missing pattern {}", format_cover_pattern(first))
                }
            } else {
                "missing catch-all (_) pattern".to_string()
            };
            self.error_at_line(line, format!("Non-exhaustive match: {}", witness_msg));
        }
    }

    fn find_uncovered_vector(
        &self,
        types: &[Type],
        rows: &[Vec<CoverPat>],
        seen: &mut HashSet<String>,
        depth: usize,
        type_depth: &mut HashMap<String, usize>,
    ) -> Option<Vec<CoverPat>> {
        if types.is_empty() {
            return if rows.is_empty() { Some(vec![]) } else { None };
        }
        if depth >= EXHAUSTIVENESS_MAX_DEPTH {
            return None;
        }

        let key = state_key(types, rows);
        // Recursive types (List / recursive sum) can re-enter the same state.
        // A repeated state here means we've reached a fixed point for coverage.
        if !seen.insert(key.clone()) {
            return None;
        }

        let head_ty = &types[0];
        let tail_tys = &types[1..];

        // Track how deeply we've expanded each Named type to prevent
        // exponential branching on recursive sum types.
        let named_key = if let Type::Named { name, .. } = head_ty {
            let d = type_depth.entry(name.clone()).or_insert(0);
            *d += 1;
            if *d > RECURSIVE_TYPE_MAX_DEPTH {
                *d -= 1;
                seen.remove(&key);
                return None; // assume covered at this depth
            }
            Some(name.clone())
        } else {
            None
        };

        let out = if let Some(ctors) = self.constructors_for_type(head_ty) {
            let mut missing = None;
            for ctor in ctors {
                let specialized = specialize_rows_for_ctor(rows, &ctor);
                let mut sub_types = ctor.arg_types.clone();
                sub_types.extend_from_slice(tail_tys);

                if let Some(mut sub_witness) = self.find_uncovered_vector(
                    &sub_types,
                    &specialized,
                    seen,
                    depth + 1,
                    type_depth,
                ) {
                    let arg_count = ctor.arg_types.len();
                    let args = sub_witness.drain(..arg_count).collect::<Vec<_>>();
                    let head_pat = build_witness_head(&ctor, args);
                    let mut full = vec![head_pat];
                    full.extend(sub_witness);
                    missing = Some(full);
                    break;
                }
            }
            missing
        } else {
            let default_rows = default_matrix(rows);
            if let Some(mut tail_witness) =
                self.find_uncovered_vector(tail_tys, &default_rows, seen, depth + 1, type_depth)
            {
                let mut full = vec![CoverPat::Wild];
                full.append(&mut tail_witness);
                Some(full)
            } else {
                None
            }
        };

        // Restore Named type depth counter.
        if let Some(name) = named_key
            && let Some(d) = type_depth.get_mut(&name)
        {
            *d -= 1;
        }

        // Only remove the key when a witness was found (Some).
        // When out is None (covered), keep the key in `seen` so sibling
        // constructors with identical (types, rows) return immediately
        // instead of re-entering the same exponentially-branching search.
        if out.is_some() {
            seen.remove(&key);
        }
        out
    }

    fn constructors_for_type(&self, ty: &Type) -> Option<Vec<CtorSpec>> {
        match ty {
            Type::Bool => Some(vec![
                CtorSpec {
                    tag: CtorTag::Bool(true),
                    arg_types: vec![],
                },
                CtorSpec {
                    tag: CtorTag::Bool(false),
                    arg_types: vec![],
                },
            ]),
            Type::Result(ok_ty, err_ty) => Some(vec![
                CtorSpec {
                    tag: CtorTag::ResultOk,
                    arg_types: vec![*ok_ty.clone()],
                },
                CtorSpec {
                    tag: CtorTag::ResultErr,
                    arg_types: vec![*err_ty.clone()],
                },
            ]),
            Type::Option(inner) => Some(vec![
                CtorSpec {
                    tag: CtorTag::OptionSome,
                    arg_types: vec![*inner.clone()],
                },
                CtorSpec {
                    tag: CtorTag::OptionNone,
                    arg_types: vec![],
                },
            ]),
            Type::List(elem) => Some(vec![
                CtorSpec {
                    tag: CtorTag::ListEmpty,
                    arg_types: vec![],
                },
                CtorSpec {
                    tag: CtorTag::ListCons,
                    arg_types: vec![*elem.clone(), Type::List(elem.clone())],
                },
            ]),
            Type::Tuple(items) => Some(vec![CtorSpec {
                tag: CtorTag::Tuple,
                arg_types: items.clone(),
            }]),
            Type::Vector(_) => None, // Vector is not exhaustively matchable
            Type::Named { name, .. } => {
                let variants = self.variants_for(name)?;
                let mut out = Vec::new();
                for variant in variants {
                    out.push(CtorSpec {
                        tag: CtorTag::Named(crate::visibility::member_key(name, variant)),
                        arg_types: self.named_variant_arg_types(name, variant),
                    });
                }
                Some(out)
            }
            // Infinite / unenumerated domains.
            Type::Int
            | Type::Float
            | Type::Str
            | Type::Map(_, _)
            | Type::Fn(_, _, _)
            | Type::Unit
            | Type::Invalid
            | Type::Var(_) => None,
        }
    }

    fn named_variant_arg_types(&self, type_name: &str, variant: &str) -> Vec<Type> {
        let key = crate::visibility::member_key(type_name, variant);
        if let Some(sig) = self.find_fn_sig(&key) {
            return sig.params.clone();
        }
        // Zero-arg constructors are values, not fn_sigs entries.
        Vec::new()
    }
}

fn pattern_subsumes(earlier: &CoverPat, current: &CoverPat) -> bool {
    match (earlier, current) {
        (CoverPat::Wild, _) => true,
        (CoverPat::Lit(a), CoverPat::Lit(b)) => a == b,
        (CoverPat::EmptyList, CoverPat::EmptyList) => true,
        (CoverPat::Cons(h1, t1), CoverPat::Cons(h2, t2)) => {
            pattern_subsumes(h1, h2) && pattern_subsumes(t1, t2)
        }
        (CoverPat::Tuple(a), CoverPat::Tuple(b)) if a.len() == b.len() => {
            a.iter().zip(b).all(|(x, y)| pattern_subsumes(x, y))
        }
        (CoverPat::Constructor(n1, a1), CoverPat::Constructor(n2, a2))
            if ctor_name_matches(n2, n1) && a1.len() == a2.len() =>
        {
            a1.iter().zip(a2).all(|(x, y)| pattern_subsumes(x, y))
        }
        _ => false,
    }
}

fn normalize_pattern(pattern: &Pattern) -> CoverPat {
    match pattern {
        Pattern::Wildcard | Pattern::Ident(_) => CoverPat::Wild,
        Pattern::Literal(lit) => CoverPat::Lit(lit.clone()),
        Pattern::EmptyList => CoverPat::EmptyList,
        Pattern::Cons(_, _) => CoverPat::Cons(Box::new(CoverPat::Wild), Box::new(CoverPat::Wild)),
        Pattern::Tuple(items) => CoverPat::Tuple(items.iter().map(normalize_pattern).collect()),
        Pattern::Constructor(name, bindings) => {
            CoverPat::Constructor(name.clone(), vec![CoverPat::Wild; bindings.len()])
        }
    }
}

fn specialize_rows_for_ctor(rows: &[Vec<CoverPat>], ctor: &CtorSpec) -> Vec<Vec<CoverPat>> {
    let mut out = Vec::new();
    for row in rows {
        if row.is_empty() {
            continue;
        }
        if let Some(mut head_args) = specialize_head_pattern(&row[0], ctor) {
            head_args.extend_from_slice(&row[1..]);
            out.push(head_args);
        }
    }
    out
}

fn specialize_head_pattern(pat: &CoverPat, ctor: &CtorSpec) -> Option<Vec<CoverPat>> {
    if matches!(pat, CoverPat::Wild) {
        return Some(vec![CoverPat::Wild; ctor.arg_types.len()]);
    }

    match (&ctor.tag, pat) {
        (CtorTag::Bool(expected), CoverPat::Lit(Literal::Bool(actual))) if expected == actual => {
            Some(vec![])
        }
        (CtorTag::ResultOk, CoverPat::Constructor(name, args))
            if ctor_name_matches(name, "Result.Ok") && args.len() == 1 =>
        {
            Some(args.clone())
        }
        (CtorTag::ResultErr, CoverPat::Constructor(name, args))
            if ctor_name_matches(name, "Result.Err") && args.len() == 1 =>
        {
            Some(args.clone())
        }
        (CtorTag::OptionSome, CoverPat::Constructor(name, args))
            if ctor_name_matches(name, "Option.Some") && args.len() == 1 =>
        {
            Some(args.clone())
        }
        (CtorTag::OptionNone, CoverPat::Constructor(name, args))
            if ctor_name_matches(name, "Option.None") && args.is_empty() =>
        {
            Some(vec![])
        }
        (CtorTag::ListEmpty, CoverPat::EmptyList) => Some(vec![]),
        (CtorTag::ListCons, CoverPat::Cons(head, tail)) => {
            Some(vec![(**head).clone(), (**tail).clone()])
        }
        (CtorTag::Tuple, CoverPat::Tuple(items)) if items.len() == ctor.arg_types.len() => {
            Some(items.clone())
        }
        (CtorTag::Named(expected), CoverPat::Constructor(name, args))
            if ctor_name_matches(name, expected) && args.len() == ctor.arg_types.len() =>
        {
            Some(args.clone())
        }
        _ => None,
    }
}

fn default_matrix(rows: &[Vec<CoverPat>]) -> Vec<Vec<CoverPat>> {
    rows.iter()
        .filter_map(|row| {
            if row.first().is_some_and(|p| matches!(p, CoverPat::Wild)) {
                Some(row[1..].to_vec())
            } else {
                None
            }
        })
        .collect()
}

fn build_witness_head(ctor: &CtorSpec, args: Vec<CoverPat>) -> CoverPat {
    match &ctor.tag {
        CtorTag::Bool(v) => CoverPat::Lit(Literal::Bool(*v)),
        CtorTag::ResultOk => CoverPat::Constructor("Result.Ok".to_string(), args),
        CtorTag::ResultErr => CoverPat::Constructor("Result.Err".to_string(), args),
        CtorTag::OptionSome => CoverPat::Constructor("Option.Some".to_string(), args),
        CtorTag::OptionNone => CoverPat::Constructor("Option.None".to_string(), vec![]),
        CtorTag::ListEmpty => CoverPat::EmptyList,
        CtorTag::ListCons => {
            let head = args.first().cloned().unwrap_or(CoverPat::Wild);
            let tail = args.get(1).cloned().unwrap_or(CoverPat::Wild);
            CoverPat::Cons(Box::new(head), Box::new(tail))
        }
        CtorTag::Tuple => CoverPat::Tuple(args),
        CtorTag::Named(name) => CoverPat::Constructor(name.clone(), args),
    }
}

fn ctor_name_matches(pattern_name: &str, expected_full: &str) -> bool {
    fn split_tail(name: &str) -> Option<(&str, &str)> {
        let mut parts = name.rsplit('.');
        let variant = parts.next()?;
        let type_name = parts.next()?;
        Some((type_name, variant))
    }

    if pattern_name == expected_full {
        return true;
    }

    match (split_tail(pattern_name), split_tail(expected_full)) {
        (Some((pat_type, pat_variant)), Some((exp_type, exp_variant))) => {
            pat_type == exp_type && pat_variant == exp_variant
        }
        _ => false,
    }
}

fn format_cover_pattern(pat: &CoverPat) -> String {
    match pat {
        CoverPat::Wild => "_".to_string(),
        CoverPat::Lit(Literal::Int(i)) => i.to_string(),
        CoverPat::Lit(Literal::BigInt(s)) => s.clone(),
        CoverPat::Lit(Literal::Float(f)) => f.to_string(),
        CoverPat::Lit(Literal::Str(s)) => format!("{:?}", s),
        CoverPat::Lit(Literal::Bool(b)) => b.to_string(),
        CoverPat::Lit(Literal::Unit) => "Unit".to_string(),
        CoverPat::EmptyList => "[]".to_string(),
        CoverPat::Cons(head, tail) => {
            format!(
                "[{}, ..{}]",
                format_cover_pattern(head),
                format_cover_pattern(tail)
            )
        }
        CoverPat::Tuple(items) => {
            let parts = items.iter().map(format_cover_pattern).collect::<Vec<_>>();
            format!("({})", parts.join(", "))
        }
        CoverPat::Constructor(name, args) => {
            if args.is_empty() {
                name.clone()
            } else {
                let parts = args.iter().map(format_cover_pattern).collect::<Vec<_>>();
                format!("{}({})", name, parts.join(", "))
            }
        }
    }
}

fn is_catch_all_witness(pat: &CoverPat) -> bool {
    match pat {
        CoverPat::Wild => true,
        CoverPat::Tuple(items) => items.iter().all(is_catch_all_witness),
        _ => false,
    }
}

fn state_key(types: &[Type], rows: &[Vec<CoverPat>]) -> String {
    let ts = types
        .iter()
        .map(Type::display)
        .collect::<Vec<_>>()
        .join("|");
    let rs = rows
        .iter()
        .map(|row| row.iter().map(pattern_sig).collect::<Vec<_>>().join(","))
        .collect::<Vec<_>>()
        .join(";");
    format!("{}#{}", ts, rs)
}

fn pattern_sig(pat: &CoverPat) -> String {
    match pat {
        CoverPat::Wild => "_".to_string(),
        CoverPat::Lit(Literal::Int(i)) => format!("i{}", i),
        // Distinct prefix from `i{}` so a big literal never collides with an
        // i64 literal that happens to share digits in the dedup state key.
        CoverPat::Lit(Literal::BigInt(s)) => format!("big{}", s),
        CoverPat::Lit(Literal::Float(f)) => format!("f{}", f),
        CoverPat::Lit(Literal::Str(s)) => format!("s{:?}", s),
        CoverPat::Lit(Literal::Bool(b)) => format!("b{}", b),
        CoverPat::Lit(Literal::Unit) => "u".to_string(),
        CoverPat::EmptyList => "[]".to_string(),
        CoverPat::Cons(h, t) => format!("[{},..{}]", pattern_sig(h), pattern_sig(t)),
        CoverPat::Tuple(items) => {
            let parts = items.iter().map(pattern_sig).collect::<Vec<_>>();
            format!("({})", parts.join(","))
        }
        CoverPat::Constructor(name, args) => {
            let parts = args.iter().map(pattern_sig).collect::<Vec<_>>();
            format!("{}({})", name, parts.join(","))
        }
    }
}
