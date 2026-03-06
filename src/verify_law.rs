use std::collections::HashMap;

use crate::ast::{Expr, MatchArm, StrPart, VerifyLaw};
use crate::types::Type;

pub type FnSigMap = HashMap<String, (Vec<Type>, Type, Vec<String>)>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NamedLawFunction {
    pub name: String,
    pub is_pure: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyLawSpecRef {
    pub spec_fn_name: String,
}

pub fn named_law_function(law: &VerifyLaw, fn_sigs: &FnSigMap) -> Option<NamedLawFunction> {
    let (_, _, effects) = fn_sigs.get(&law.name)?;
    Some(NamedLawFunction {
        name: law.name.clone(),
        is_pure: effects.is_empty(),
    })
}

pub fn declared_spec_ref(law: &VerifyLaw, fn_sigs: &FnSigMap) -> Option<VerifyLawSpecRef> {
    let named = named_law_function(law, fn_sigs)?;
    named.is_pure.then_some(VerifyLawSpecRef {
        spec_fn_name: named.name,
    })
}

pub fn law_spec_ref(law: &VerifyLaw, fn_sigs: &FnSigMap) -> Option<VerifyLawSpecRef> {
    let spec = declared_spec_ref(law, fn_sigs)?;
    law_calls_function(law, &spec.spec_fn_name).then_some(spec)
}

pub fn canonical_spec_ref(
    fn_name: &str,
    law: &VerifyLaw,
    fn_sigs: &FnSigMap,
) -> Option<VerifyLawSpecRef> {
    let spec = law_spec_ref(law, fn_sigs)?;
    canonical_spec_shape(fn_name, law, &spec.spec_fn_name).then_some(spec)
}

pub fn law_calls_function(law: &VerifyLaw, fn_name: &str) -> bool {
    expr_calls_function(&law.lhs, fn_name) || expr_calls_function(&law.rhs, fn_name)
}

pub fn canonical_spec_shape(fn_name: &str, law: &VerifyLaw, spec_fn_name: &str) -> bool {
    let try_side = |impl_side: &Expr, spec_side: &Expr| -> bool {
        let Some((impl_callee, impl_args)) = direct_call(impl_side) else {
            return false;
        };
        let Some((spec_callee, spec_args)) = direct_call(spec_side) else {
            return false;
        };
        impl_callee == fn_name && spec_callee == spec_fn_name && impl_args == spec_args
    };

    try_side(&law.lhs, &law.rhs) || try_side(&law.rhs, &law.lhs)
}

fn expr_calls_function(expr: &Expr, fn_name: &str) -> bool {
    match expr {
        Expr::FnCall(callee, args) => {
            expr_is_function_name(callee, fn_name)
                || expr_calls_function(callee, fn_name)
                || args.iter().any(|arg| expr_calls_function(arg, fn_name))
        }
        Expr::Attr(obj, _) => expr_calls_function(obj, fn_name),
        Expr::BinOp(_, left, right) => {
            expr_calls_function(left, fn_name) || expr_calls_function(right, fn_name)
        }
        Expr::Match { subject, arms, .. } => {
            expr_calls_function(subject, fn_name)
                || arms.iter().any(|arm| match_arm_calls_function(arm, fn_name))
        }
        Expr::Constructor(_, Some(inner)) => expr_calls_function(inner, fn_name),
        Expr::ErrorProp(inner) => expr_calls_function(inner, fn_name),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            StrPart::Literal(_) => false,
            StrPart::Parsed(expr) => expr_calls_function(expr, fn_name),
        }),
        Expr::List(items) | Expr::Tuple(items) => {
            items.iter().any(|item| expr_calls_function(item, fn_name))
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(key, value)| expr_calls_function(key, fn_name) || expr_calls_function(value, fn_name)),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, expr)| expr_calls_function(expr, fn_name)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_calls_function(base, fn_name)
                || updates
                    .iter()
                    .any(|(_, expr)| expr_calls_function(expr, fn_name))
        }
        Expr::TailCall(boxed) => {
            boxed.0 == fn_name
                || boxed
                    .1
                    .iter()
                    .any(|arg| expr_calls_function(arg, fn_name))
        }
        Expr::Literal(_)
        | Expr::Ident(_)
        | Expr::Resolved(_)
        | Expr::Constructor(_, None) => false,
    }
}

fn match_arm_calls_function(arm: &MatchArm, fn_name: &str) -> bool {
    expr_calls_function(&arm.body, fn_name)
}

fn expr_is_function_name(expr: &Expr, fn_name: &str) -> bool {
    matches!(expr, Expr::Ident(name) if name == fn_name)
}

fn direct_call(expr: &Expr) -> Option<(&str, &[Expr])> {
    let Expr::FnCall(callee, args) = expr else {
        return None;
    };
    let Expr::Ident(name) = callee.as_ref() else {
        return None;
    };
    Some((name.as_str(), args.as_slice()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Literal, VerifyGiven, VerifyGivenDomain};

    fn int_sig() -> (Vec<Type>, Type, Vec<String>) {
        (vec![Type::Int], Type::Int, vec![])
    }

    fn law(lhs: Expr, rhs: Expr, name: &str) -> VerifyLaw {
        VerifyLaw {
            name: name.to_string(),
            givens: vec![VerifyGiven {
                name: "x".to_string(),
                type_name: "Int".to_string(),
                domain: VerifyGivenDomain::Explicit(vec![Expr::Literal(Literal::Int(1))]),
            }],
            lhs,
            rhs,
        }
    }

    #[test]
    fn pure_named_law_function_becomes_declared_spec_ref() {
        let mut fn_sigs = FnSigMap::new();
        fn_sigs.insert("fooSpec".to_string(), int_sig());

        let verify_law = law(
            Expr::FnCall(
                Box::new(Expr::Ident("foo".to_string())),
                vec![Expr::Ident("x".to_string())],
            ),
            Expr::FnCall(
                Box::new(Expr::Ident("fooSpec".to_string())),
                vec![Expr::Ident("x".to_string())],
            ),
            "fooSpec",
        );

        assert_eq!(
            declared_spec_ref(&verify_law, &fn_sigs),
            Some(VerifyLawSpecRef {
                spec_fn_name: "fooSpec".to_string()
            })
        );
        assert_eq!(law_spec_ref(&verify_law, &fn_sigs), declared_spec_ref(&verify_law, &fn_sigs));
        assert_eq!(
            canonical_spec_ref("foo", &verify_law, &fn_sigs),
            declared_spec_ref(&verify_law, &fn_sigs)
        );
    }

    #[test]
    fn effectful_named_law_function_is_not_a_spec_ref() {
        let mut fn_sigs = FnSigMap::new();
        fn_sigs.insert(
            "fooSpec".to_string(),
            (vec![Type::Int], Type::Int, vec!["Console.print".to_string()]),
        );

        let verify_law = law(Expr::Ident("x".to_string()), Expr::Ident("x".to_string()), "fooSpec");

        assert!(declared_spec_ref(&verify_law, &fn_sigs).is_none());
        assert_eq!(
            named_law_function(&verify_law, &fn_sigs),
            Some(NamedLawFunction {
                name: "fooSpec".to_string(),
                is_pure: false
            })
        );
    }

    #[test]
    fn canonical_spec_ref_requires_call_to_named_function() {
        let mut fn_sigs = FnSigMap::new();
        fn_sigs.insert("fooSpec".to_string(), int_sig());

        let verify_law = law(Expr::Ident("x".to_string()), Expr::Ident("x".to_string()), "fooSpec");

        assert!(declared_spec_ref(&verify_law, &fn_sigs).is_some());
        assert!(law_spec_ref(&verify_law, &fn_sigs).is_none());
        assert!(!law_calls_function(&verify_law, "fooSpec"));
    }

    #[test]
    fn canonical_spec_ref_requires_same_arguments_on_both_sides() {
        let mut fn_sigs = FnSigMap::new();
        fn_sigs.insert("fooSpec".to_string(), int_sig());

        let verify_law = law(
            Expr::FnCall(
                Box::new(Expr::Ident("foo".to_string())),
                vec![Expr::Ident("x".to_string())],
            ),
            Expr::FnCall(
                Box::new(Expr::Ident("fooSpec".to_string())),
                vec![Expr::Literal(Literal::Int(5)), Expr::Ident("x".to_string())],
            ),
            "fooSpec",
        );

        assert!(law_spec_ref(&verify_law, &fn_sigs).is_some());
        assert!(canonical_spec_ref("foo", &verify_law, &fn_sigs).is_none());
        assert!(!canonical_spec_shape("foo", &verify_law, "fooSpec"));
    }
}
