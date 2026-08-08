//! Proof-derived packed layouts for structural refinement carriers.
//!
//! Scalar refinement carriers already feed their proven interval into the
//! carrier-`i64` representation pass. This module extends the same idea to a
//! canonical structural invariant:
//!
//! ```text
//! record Octets { values: List<Int> }
//! fn allOctets(xs: List<Int>) -> Bool
//!     match xs
//!         [] -> true
//!         [head, ..tail] -> match 0 <= head <= 255
//!             true -> allOctets(tail)
//!             false -> false
//! fn fromList(xs: List<Int>) -> Result<Octets, String>
//!     match allOctets(xs)
//!         true -> Result.Ok(Octets(values = xs))
//!         false -> Result.Err(...)
//! ```
//!
//! The recognizer proves an interval for *every element* of the carrier and
//! chooses the smallest lossless integer storage. It is deliberately keyed by
//! the refinement shape, never by a standard-library type name: `Bytes` is the
//! first consumer, not a compiler special case.

use super::*;
use crate::ast::{Pattern, Type};
use crate::ir::interval::{Bound, Interval};

/// Smallest integer storage that can represent every inhabitant of a proven
/// element interval. Signedness controls the load extension; wasm packed
/// storage itself is just `i8` / `i16`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PackedIntElement {
    U8,
    I8,
    U16,
    I16,
    U32,
    I32,
    I64,
}

impl PackedIntElement {
    /// Pick the narrowest lossless storage. Open or wider-than-i64 intervals
    /// decline so codegen keeps the ordinary structural carrier.
    pub fn for_interval(interval: Interval) -> Option<Self> {
        let (Bound::Finite(lo), Bound::Finite(hi)) = (interval.lo, interval.hi) else {
            return None;
        };
        if lo >= 0 && hi <= u8::MAX as i128 {
            Some(Self::U8)
        } else if lo >= i8::MIN as i128 && hi <= i8::MAX as i128 {
            Some(Self::I8)
        } else if lo >= 0 && hi <= u16::MAX as i128 {
            Some(Self::U16)
        } else if lo >= i16::MIN as i128 && hi <= i16::MAX as i128 {
            Some(Self::I16)
        } else if lo >= 0 && hi <= u32::MAX as i128 {
            Some(Self::U32)
        } else if lo >= i32::MIN as i128 && hi <= i32::MAX as i128 {
            Some(Self::I32)
        } else if lo >= i64::MIN as i128 && hi <= i64::MAX as i128 {
            Some(Self::I64)
        } else {
            None
        }
    }
}

/// Backend-neutral representation fact for one opaque `List<Int>` carrier.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PackedSequenceLayout {
    pub element_interval: Interval,
    pub element: PackedIntElement,
}

/// Derive packed layouts for every canonical opaque `List<Int>` refinement in
/// scope. Unknown predicate shapes are omitted (fail-closed).
pub fn packed_sequence_layout_table(
    inputs: &ProofLowerInputs<'_>,
) -> HashMap<String, PackedSequenceLayout> {
    let mut table = HashMap::new();
    let mut ambiguous_names = HashSet::new();

    let entry_typedefs = inputs.entry_items.iter().filter_map(|item| match item {
        TopLevel::TypeDef(td) => Some((None::<&str>, td)),
        _ => None,
    });
    let module_typedefs = inputs.dep_modules.iter().flat_map(|module| {
        module
            .type_defs
            .iter()
            .map(move |td| (Some(module.prefix.as_str()), td))
    });

    for (scope, td) in entry_typedefs.chain(module_typedefs) {
        let TypeDef::Product { name, fields, .. } = td else {
            continue;
        };
        if ambiguous_names.contains(name) {
            continue;
        }
        if fields.len() != 1 {
            continue;
        }
        let Some(info) = crate::codegen::common::refinement_info_for_in_scope(name, inputs, scope)
        else {
            continue;
        };
        if !matches!(
            crate::types::parse_type_str(info.carrier_type),
            Type::List(inner) if *inner == Type::Int
        ) {
            continue;
        }
        let Some(element_interval) = element_interval_from_refinement(info, inputs, scope) else {
            continue;
        };
        let Some(element) = PackedIntElement::for_interval(element_interval) else {
            continue;
        };
        let layout = PackedSequenceLayout {
            element_interval,
            element,
        };
        // The existing scalar table is bare-name keyed too. A collision must
        // never widen a fact: retain a layout only when both scopes derive the
        // exact same interval/storage; otherwise decline the ambiguous name.
        match table.get(name) {
            None => {
                table.insert(name.clone(), layout);
            }
            Some(existing) if *existing == layout => {}
            Some(_) => {
                table.remove(name);
                ambiguous_names.insert(name.clone());
            }
        }
    }

    table
}

fn element_interval_from_refinement(
    info: crate::codegen::common::RefinementInfo<'_>,
    inputs: &ProofLowerInputs<'_>,
    scope: Option<&str>,
) -> Option<Interval> {
    element_interval_from_predicate(
        info.predicate,
        info.param_name,
        &inputs.pure_fns_in_scope(scope),
        &|expr| inputs.resolve_expr(expr, scope),
    )
}

/// Derive the per-element interval of a canonical `List<Int>` refinement
/// from the smart constructor's predicate call, the same-scope function
/// pool, and a resolver for the per-element predicate expression.
///
/// Split out of [`element_interval_from_refinement`] so the analysis tier
/// (`crate::analysis::literal_refinement`, which drives the literal
/// discharge in the typechecker and the HIR resolver) derives the element
/// bound through the SAME recognizer the packed layout uses, instead of
/// re-deriving — or worse, hardcoding — a range. Both callers therefore
/// agree by construction: a value the discharge admits is a value the
/// packed layout can store.
pub(crate) fn element_interval_from_predicate(
    predicate: &Spanned<Expr>,
    param_name: &str,
    scope_fns: &[&FnDef],
    resolve: &dyn Fn(&Spanned<Expr>) -> Spanned<crate::ir::hir::ResolvedExpr>,
) -> Option<Interval> {
    let (predicate_fn, predicate_args) = call_target_and_args(&predicate.node)?;
    if predicate_args.len() != 1 || !expr_is_ident(&predicate_args[0], param_name) {
        return None;
    }
    let helper = scope_fns
        .iter()
        .copied()
        .find(|fd| same_callee_name(&fd.name, &predicate_fn))?;
    recursive_list_element_predicate(helper).and_then(|(element_name, element_predicate)| {
        let predicate = Predicate {
            free_vars: vec![(element_name, QuantifierType::Plain("Int".to_string()))],
            expr: resolve(element_predicate),
        };
        let (interval, known) = crate::ir::interval::interval_of_invariant(&predicate);
        known.then_some(interval)
    })
}

/// Recognize a total recursive `all` over `List<Int>` and return the cons-head
/// binder plus its per-element predicate.
fn recursive_list_element_predicate(fd: &FnDef) -> Option<(String, &Spanned<Expr>)> {
    if fd.params.len() != 1 || fd.params[0].1.replace(' ', "") != "List<Int>" {
        return None;
    }
    let param = fd.params[0].0.as_str();
    let [crate::ast::Stmt::Expr(body)] = fd.body.stmts() else {
        return None;
    };
    let Expr::Match { subject, arms } = &body.node else {
        return None;
    };
    if !expr_is_ident(subject, param) || arms.len() != 2 {
        return None;
    }

    let empty_ok = arms
        .iter()
        .any(|arm| matches!(arm.pattern, Pattern::EmptyList) && expr_is_bool(&arm.body, true));
    if !empty_ok {
        return None;
    }
    let cons = arms.iter().find_map(|arm| match &arm.pattern {
        Pattern::Cons(head, tail) => Some((head.as_str(), tail.as_str(), arm.body.as_ref())),
        _ => None,
    })?;
    let (head, tail, cons_body) = cons;

    // Direct form: Bool.and(P(head), all(tail)).
    if let Some((callee, args)) = call_target_and_args(&cons_body.node)
        && callee == "Bool.and"
        && args.len() == 2
    {
        if is_recursive_call(&args[1].node, &fd.name, tail) {
            return Some((head.to_string(), &args[0]));
        }
        if is_recursive_call(&args[0].node, &fd.name, tail) {
            return Some((head.to_string(), &args[1]));
        }
    }

    // Short-circuit form used by stdlib/bytes.av:
    // match P(head) { true -> all(tail), false -> false }.
    let Expr::Match {
        subject: element_predicate,
        arms: predicate_arms,
    } = &cons_body.node
    else {
        return None;
    };
    if predicate_arms.len() != 2 {
        return None;
    }
    let true_recurses = predicate_arms.iter().any(|arm| {
        matches!(arm.pattern, Pattern::Literal(Literal::Bool(true)))
            && is_recursive_call(&arm.body.node, &fd.name, tail)
    });
    let false_rejects = predicate_arms.iter().any(|arm| {
        matches!(arm.pattern, Pattern::Literal(Literal::Bool(false)))
            && expr_is_bool(&arm.body, false)
    });
    (true_recurses && false_rejects).then(|| (head.to_string(), element_predicate.as_ref()))
}

fn call_target_and_args(expr: &Expr) -> Option<(String, &[Spanned<Expr>])> {
    match expr {
        Expr::FnCall(callee, args) => crate::codegen::common::expr_to_dotted_name(&callee.node)
            .map(|name| (name, args.as_slice())),
        Expr::TailCall(call) => Some((call.target.clone(), call.args.as_slice())),
        _ => None,
    }
}

fn is_recursive_call(expr: &Expr, fn_name: &str, arg_name: &str) -> bool {
    let Some((callee, args)) = call_target_and_args(expr) else {
        return false;
    };
    same_callee_name(fn_name, &callee) && args.len() == 1 && expr_is_ident(&args[0], arg_name)
}

/// Callee-name comparison for the recognizer's helper lookup and
/// recursive-call check. On the production path the table is computed
/// post-flatten, where every declared fn name and every (rewritten)
/// same-scope call-site name is dot-free — exact equality is the whole
/// contract. A bare-suffix fallback (`rsplit('.')`) used to live here;
/// it was dead on dot-free names (the suffix IS the name) but would go
/// live on a pre-flatten walk, where it could match a same-named helper
/// from a DIFFERENT module and key a layout on the wrong module's
/// predicate. A dotted callee (`SomeDep.helper`) now simply declines
/// the recognizer — fail-closed, the carrier stays unpacked.
fn same_callee_name(declared: &str, called: &str) -> bool {
    declared == called
}

fn expr_is_ident(expr: &Spanned<Expr>, name: &str) -> bool {
    matches!(
        &expr.node,
        Expr::Ident(found) | Expr::Resolved { name: found, .. } if found == name
    )
}

fn expr_is_bool(expr: &Spanned<Expr>, value: bool) -> bool {
    matches!(&expr.node, Expr::Literal(Literal::Bool(found)) if *found == value)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::parse_source;

    fn layouts_for(src: &str) -> HashMap<String, PackedSequenceLayout> {
        let mut items = parse_source(src).expect("parse");
        let result = crate::ir::pipeline::run(
            &mut items,
            crate::ir::pipeline::PipelineConfig {
                typecheck: Some(crate::ir::pipeline::TypecheckMode::Full { base_dir: None }),
                ..Default::default()
            },
        );
        let tc = result.typecheck.expect("typecheck requested");
        assert!(tc.errors.is_empty(), "typecheck errors: {:?}", tc.errors);
        let prefixes = HashSet::new();
        let recursive = HashSet::new();
        let inputs = ProofLowerInputs {
            entry_items: &items,
            dep_modules: &[],
            module_prefixes: &prefixes,
            recursive_fns: &recursive,
            symbol_table: &result.symbol_table,
            program_shape: None,
        };
        packed_sequence_layout_table(&inputs)
    }

    const OCTETS: &str = r#"
module Octets
    intent = "generic structural refinement"
    effects []

record Octets
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn fromList(xs: List<Int>) -> Result<Octets, String>
    match allInRange(xs)
        true -> Result.Ok(Octets(values = xs))
        false -> Result.Err("oob")
"#;

    #[test]
    fn derives_u8_without_a_bytes_name_special_case() {
        let layouts = layouts_for(OCTETS);
        assert_eq!(
            layouts.get("Octets"),
            Some(&PackedSequenceLayout {
                element_interval: Interval::between(0, 255),
                element: PackedIntElement::U8,
            })
        );
        assert!(!OCTETS.contains("record Bytes"));
    }

    #[test]
    fn derives_u8_for_the_real_standard_library_bytes_module() {
        let layouts = layouts_for(include_str!("../../../stdlib/bytes.av"));
        assert_eq!(
            layouts.get("Bytes"),
            Some(&PackedSequenceLayout {
                element_interval: Interval::between(0, 255),
                element: PackedIntElement::U8,
            })
        );
    }

    #[test]
    fn derives_signed_i8_for_negative_elements() {
        let src = OCTETS
            .replace("record Octets", "record Samples")
            .replace("Result<Octets", "Result<Samples")
            .replace("Octets(values", "Samples(values")
            .replace("head >= 0", "head >= -128")
            .replace("head <= 255", "head <= 127");
        let layouts = layouts_for(&src);
        assert_eq!(
            layouts.get("Samples").map(|layout| layout.element),
            Some(PackedIntElement::I8)
        );
    }

    #[test]
    fn declines_a_non_recursive_list_predicate() {
        let src = OCTETS.replace("true -> allInRange(tail)", "true -> true");
        assert!(layouts_for(&src).is_empty());
    }
}
