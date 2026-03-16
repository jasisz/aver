use std::collections::{BTreeSet, HashMap, HashSet};

use crate::ast::{
    Expr, FnDef, MatchArm, Stmt, StrPart, TopLevel, VerifyBlock, VerifyKind, VerifyLaw,
};
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingHelperLawHint {
    pub line: usize,
    pub fn_name: String,
    pub law_name: String,
    pub missing_helpers: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ContextualHelperLawHint {
    pub line: usize,
    pub fn_name: String,
    pub law_name: String,
    pub missing_helpers: Vec<String>,
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

pub fn collect_missing_helper_law_hints(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
) -> Vec<MissingHelperLawHint> {
    let fn_defs = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some((fd.name.clone(), fd))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();
    let verified_law_functions = items
        .iter()
        .filter_map(|item| {
            let TopLevel::Verify(vb) = item else {
                return None;
            };
            let VerifyKind::Law(law) = &vb.kind else {
                return None;
            };
            let mut covered = BTreeSet::new();
            covered.insert(vb.fn_name.clone());
            collect_direct_pure_user_calls(&law.lhs, &fn_defs, fn_sigs, &mut covered);
            collect_direct_pure_user_calls(&law.rhs, &fn_defs, fn_sigs, &mut covered);
            Some(covered)
        })
        .flatten()
        .collect::<HashSet<_>>();

    items
        .iter()
        .filter_map(|item| {
            let TopLevel::Verify(vb) = item else {
                return None;
            };
            let VerifyKind::Law(law) = &vb.kind else {
                return None;
            };
            missing_helper_law_hint_for_block(vb, law, &fn_defs, &verified_law_functions, fn_sigs)
        })
        .collect()
}

pub fn missing_helper_law_message(hint: &MissingHelperLawHint) -> String {
    format!(
        "verify law '{}.{}' uses helper functions without their own verify law: {}; add layered `verify ... law ...` blocks for those helpers before expecting a universal auto-proof",
        hint.fn_name,
        hint.law_name,
        hint.missing_helpers.join(", ")
    )
}

pub fn collect_contextual_helper_law_hints(
    items: &[TopLevel],
    fn_sigs: &FnSigMap,
) -> Vec<ContextualHelperLawHint> {
    let fn_defs = items
        .iter()
        .filter_map(|item| {
            if let TopLevel::FnDef(fd) = item {
                Some((fd.name.clone(), fd))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();
    let contextual_law_targets = items
        .iter()
        .filter_map(|item| {
            let TopLevel::Verify(vb) = item else {
                return None;
            };
            let VerifyKind::Law(law) = &vb.kind else {
                return None;
            };
            top_level_direct_pure_call_in_law(law, &fn_defs, fn_sigs)
        })
        .collect::<HashSet<_>>();

    items
        .iter()
        .filter_map(|item| {
            let TopLevel::Verify(vb) = item else {
                return None;
            };
            let VerifyKind::Law(law) = &vb.kind else {
                return None;
            };
            contextual_helper_law_hint_for_block(
                vb,
                law,
                &fn_defs,
                &contextual_law_targets,
                fn_sigs,
            )
        })
        .collect()
}

pub fn contextual_helper_law_message(hint: &ContextualHelperLawHint) -> String {
    format!(
        "verify law '{}.{}' still lacks analogous `verify ... law ...` coverage for contextual helpers: {}; universal auto-proof will likely stop at those helper boundaries",
        hint.fn_name,
        hint.law_name,
        hint.missing_helpers.join(", ")
    )
}

fn missing_helper_law_hint_for_block(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    fn_defs: &HashMap<String, &FnDef>,
    verified_law_functions: &HashSet<String>,
    fn_sigs: &FnSigMap,
) -> Option<MissingHelperLawHint> {
    if law.when.is_none() || law.givens.len() != 1 {
        return None;
    }

    let root_calls = direct_pure_user_calls_in_law(law, fn_defs, fn_sigs);
    if root_calls.is_empty() {
        return None;
    }

    let mut missing_helpers = BTreeSet::new();
    for root in root_calls {
        for helper in frontier_helper_calls(&root, fn_defs, fn_sigs) {
            if helper != vb.fn_name && !verified_law_functions.contains(&helper) {
                missing_helpers.insert(helper);
            }
        }
    }

    if missing_helpers.is_empty() {
        return None;
    }

    Some(MissingHelperLawHint {
        line: vb.line,
        fn_name: vb.fn_name.clone(),
        law_name: law.name.clone(),
        missing_helpers: missing_helpers.into_iter().collect(),
    })
}

fn contextual_helper_law_hint_for_block(
    vb: &VerifyBlock,
    law: &VerifyLaw,
    fn_defs: &HashMap<String, &FnDef>,
    contextual_law_targets: &HashSet<String>,
    fn_sigs: &FnSigMap,
) -> Option<ContextualHelperLawHint> {
    let parser_name = contextual_roundtrip_parser_name(law, fn_defs, fn_sigs)?;
    let root_parser_name = wrapper_dispatch_root(&parser_name, fn_defs, fn_sigs)
        .unwrap_or_else(|| parser_name.clone());
    if root_parser_name != parser_name {
        return None;
    }

    let missing_helpers = frontier_helper_calls(&root_parser_name, fn_defs, fn_sigs)
        .into_iter()
        .filter(|helper| helper != &vb.fn_name && !contextual_law_targets.contains(helper))
        .collect::<BTreeSet<_>>();

    if missing_helpers.is_empty() {
        return None;
    }

    Some(ContextualHelperLawHint {
        line: vb.line,
        fn_name: vb.fn_name.clone(),
        law_name: law.name.clone(),
        missing_helpers: missing_helpers.into_iter().collect(),
    })
}

fn direct_pure_user_calls_in_law(
    law: &VerifyLaw,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> BTreeSet<String> {
    let mut out = BTreeSet::new();
    collect_direct_pure_user_calls(&law.lhs, fn_defs, fn_sigs, &mut out);
    collect_direct_pure_user_calls(&law.rhs, fn_defs, fn_sigs, &mut out);
    out
}

fn top_level_direct_pure_call_in_law(
    law: &VerifyLaw,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> Option<String> {
    direct_pure_user_call_name(&law.lhs, fn_defs, fn_sigs)
        .or_else(|| direct_pure_user_call_name(&law.rhs, fn_defs, fn_sigs))
}

fn contextual_roundtrip_parser_name(
    law: &VerifyLaw,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> Option<String> {
    let given = law.givens.first()?;
    detect_roundtrip_layers(law, &given.name, fn_defs, fn_sigs).map(|(parser_name, _)| parser_name)
}

fn frontier_helper_calls(
    root_name: &str,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> BTreeSet<String> {
    let mut current =
        wrapper_dispatch_root(root_name, fn_defs, fn_sigs).unwrap_or_else(|| root_name.to_string());
    let mut visited = BTreeSet::new();

    for _ in 0..2 {
        if !visited.insert(current.clone()) {
            break;
        }
        let direct = direct_pure_fn_callees_matching_return(&current, fn_defs, fn_sigs);
        if direct.is_empty() {
            return BTreeSet::new();
        }
        if direct.len() == 1 {
            current = direct.iter().next().cloned().unwrap_or_default();
            continue;
        }
        return direct;
    }

    direct_pure_fn_callees_matching_return(&current, fn_defs, fn_sigs)
}

fn wrapper_dispatch_root(
    fn_name: &str,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> Option<String> {
    let fd = fn_defs.get(fn_name)?;
    let tail = fd.body.tail_expr()?;
    match tail {
        Expr::Match { subject, .. } => direct_pure_user_call_name(subject, fn_defs, fn_sigs),
        Expr::FnCall(_, _) => direct_pure_user_call_name(tail, fn_defs, fn_sigs),
        _ => None,
    }
}

fn direct_pure_fn_callees_matching_return(
    fn_name: &str,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> BTreeSet<String> {
    let Some((_, return_type, _)) = fn_sigs.get(fn_name) else {
        return BTreeSet::new();
    };
    let Some(fd) = fn_defs.get(fn_name) else {
        return BTreeSet::new();
    };

    let mut direct = BTreeSet::new();
    for stmt in fd.body.stmts() {
        match stmt {
            Stmt::Expr(expr) | Stmt::Binding(_, _, expr) => {
                collect_direct_pure_user_calls(expr, fn_defs, fn_sigs, &mut direct);
            }
        }
    }
    direct
        .into_iter()
        .filter(|callee| {
            callee != fn_name
                && fn_sigs.get(callee).is_some_and(|(_, callee_ret, effects)| {
                    effects.is_empty() && callee_ret == return_type
                })
        })
        .collect()
}

fn collect_direct_pure_user_calls(
    expr: &Expr,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
    out: &mut BTreeSet<String>,
) {
    match expr {
        Expr::FnCall(callee, args) => {
            if let Some(name) = direct_pure_user_call_name(expr, fn_defs, fn_sigs) {
                out.insert(name);
            }
            collect_direct_pure_user_calls(callee, fn_defs, fn_sigs, out);
            for arg in args {
                collect_direct_pure_user_calls(arg, fn_defs, fn_sigs, out);
            }
        }
        Expr::Attr(obj, _) => collect_direct_pure_user_calls(obj, fn_defs, fn_sigs, out),
        Expr::BinOp(_, left, right) => {
            collect_direct_pure_user_calls(left, fn_defs, fn_sigs, out);
            collect_direct_pure_user_calls(right, fn_defs, fn_sigs, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_direct_pure_user_calls(subject, fn_defs, fn_sigs, out);
            for arm in arms {
                collect_direct_pure_user_calls(&arm.body, fn_defs, fn_sigs, out);
            }
        }
        Expr::Constructor(_, Some(inner)) | Expr::ErrorProp(inner) => {
            collect_direct_pure_user_calls(inner, fn_defs, fn_sigs, out);
        }
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_direct_pure_user_calls(inner, fn_defs, fn_sigs, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_direct_pure_user_calls(item, fn_defs, fn_sigs, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_direct_pure_user_calls(key, fn_defs, fn_sigs, out);
                collect_direct_pure_user_calls(value, fn_defs, fn_sigs, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_direct_pure_user_calls(value, fn_defs, fn_sigs, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_direct_pure_user_calls(base, fn_defs, fn_sigs, out);
            for (_, value) in updates {
                collect_direct_pure_user_calls(value, fn_defs, fn_sigs, out);
            }
        }
        Expr::TailCall(boxed) => {
            let (target, args) = boxed.as_ref();
            if fn_defs.contains_key(target)
                && fn_sigs
                    .get(target)
                    .is_some_and(|(_, _, effects)| effects.is_empty())
            {
                out.insert(target.clone());
            }
            for arg in args {
                collect_direct_pure_user_calls(arg, fn_defs, fn_sigs, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) | Expr::Constructor(_, None) => {}
    }
}

fn direct_pure_user_call_name(
    expr: &Expr,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> Option<String> {
    let Expr::FnCall(callee, _) = expr else {
        return None;
    };
    let name = dotted_name(callee)?;
    if !fn_defs.contains_key(&name) {
        return None;
    }
    fn_sigs
        .get(&name)
        .is_some_and(|(_, _, effects)| effects.is_empty())
        .then_some(name)
}

fn dotted_name(expr: &Expr) -> Option<String> {
    match expr {
        Expr::Ident(name) => Some(name.clone()),
        Expr::Attr(base, field) => {
            let mut prefix = dotted_name(base)?;
            prefix.push('.');
            prefix.push_str(field);
            Some(prefix)
        }
        _ => None,
    }
}

fn detect_roundtrip_layers(
    law: &VerifyLaw,
    given_name: &str,
    fn_defs: &HashMap<String, &FnDef>,
    fn_sigs: &FnSigMap,
) -> Option<(String, String)> {
    if law.givens.len() != 1 {
        return None;
    }

    fn detect_roundtrip_side(
        expr: &Expr,
        given_name: &str,
        fn_defs: &HashMap<String, &FnDef>,
        fn_sigs: &FnSigMap,
    ) -> Option<(String, String)> {
        let Expr::FnCall(parser_callee, parser_args) = expr else {
            return None;
        };
        if parser_args.is_empty() {
            return None;
        }
        let (serializer_callee, serializer_args) =
            extract_roundtrip_serializer_call(&parser_args[0], given_name)?;
        if !serializer_args
            .iter()
            .any(|arg| matches_ident(arg, given_name))
        {
            return None;
        }
        if serializer_args
            .iter()
            .filter(|arg| expr_mentions_ident(arg, given_name))
            .any(|arg| !matches_ident(arg, given_name))
        {
            return None;
        }
        if parser_args[1..]
            .iter()
            .any(|arg| expr_mentions_ident(arg, given_name))
        {
            return None;
        }

        let parser_name = dotted_name(parser_callee)?;
        let serializer_name = dotted_name(serializer_callee)?;
        if !fn_defs.contains_key(&parser_name) || !fn_defs.contains_key(&serializer_name) {
            return None;
        }
        if !fn_sigs
            .get(&parser_name)
            .is_some_and(|(_, _, effects)| effects.is_empty())
        {
            return None;
        }
        if !fn_sigs
            .get(&serializer_name)
            .is_some_and(|(_, _, effects)| effects.is_empty())
        {
            return None;
        }
        Some((parser_name, serializer_name))
    }

    detect_roundtrip_side(&law.lhs, given_name, fn_defs, fn_sigs)
        .or_else(|| detect_roundtrip_side(&law.rhs, given_name, fn_defs, fn_sigs))
}

fn extract_roundtrip_serializer_call<'a>(
    expr: &'a Expr,
    given_name: &str,
) -> Option<(&'a Expr, &'a [Expr])> {
    let mut candidates = Vec::new();
    collect_roundtrip_serializer_calls(expr, given_name, &mut candidates);
    if candidates.len() != 1 {
        return None;
    }
    let (callee, args) = candidates.pop()?;
    if expr_mentions_ident(expr, given_name)
        && args
            .iter()
            .filter(|arg| expr_mentions_ident(arg, given_name))
            .all(|arg| matches_ident(arg, given_name))
    {
        Some((callee, args))
    } else {
        None
    }
}

fn collect_roundtrip_serializer_calls<'a>(
    expr: &'a Expr,
    given_name: &str,
    out: &mut Vec<(&'a Expr, &'a [Expr])>,
) {
    match expr {
        Expr::FnCall(callee, args) => {
            if args.iter().any(|arg| matches_ident(arg, given_name))
                && args
                    .iter()
                    .filter(|arg| expr_mentions_ident(arg, given_name))
                    .all(|arg| matches_ident(arg, given_name))
            {
                out.push((callee.as_ref(), args.as_slice()));
            }
            collect_roundtrip_serializer_calls(callee, given_name, out);
            for arg in args {
                collect_roundtrip_serializer_calls(arg, given_name, out);
            }
        }
        Expr::Attr(base, _) => collect_roundtrip_serializer_calls(base, given_name, out),
        Expr::BinOp(_, left, right) => {
            collect_roundtrip_serializer_calls(left, given_name, out);
            collect_roundtrip_serializer_calls(right, given_name, out);
        }
        Expr::Match { subject, arms, .. } => {
            collect_roundtrip_serializer_calls(subject, given_name, out);
            for arm in arms {
                collect_roundtrip_serializer_calls(&arm.body, given_name, out);
            }
        }
        Expr::Constructor(_, inner) => {
            if let Some(inner) = inner {
                collect_roundtrip_serializer_calls(inner, given_name, out);
            }
        }
        Expr::ErrorProp(inner) => collect_roundtrip_serializer_calls(inner, given_name, out),
        Expr::InterpolatedStr(parts) => {
            for part in parts {
                if let StrPart::Parsed(inner) = part {
                    collect_roundtrip_serializer_calls(inner, given_name, out);
                }
            }
        }
        Expr::List(items) | Expr::Tuple(items) => {
            for item in items {
                collect_roundtrip_serializer_calls(item, given_name, out);
            }
        }
        Expr::MapLiteral(entries) => {
            for (key, value) in entries {
                collect_roundtrip_serializer_calls(key, given_name, out);
                collect_roundtrip_serializer_calls(value, given_name, out);
            }
        }
        Expr::RecordCreate { fields, .. } => {
            for (_, value) in fields {
                collect_roundtrip_serializer_calls(value, given_name, out);
            }
        }
        Expr::RecordUpdate { base, updates, .. } => {
            collect_roundtrip_serializer_calls(base, given_name, out);
            for (_, value) in updates {
                collect_roundtrip_serializer_calls(value, given_name, out);
            }
        }
        Expr::TailCall(call) => {
            for arg in &call.1 {
                collect_roundtrip_serializer_calls(arg, given_name, out);
            }
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
    }
}

fn matches_ident(expr: &Expr, name: &str) -> bool {
    matches!(expr, Expr::Ident(current) if current == name)
}

fn expr_mentions_ident(expr: &Expr, name: &str) -> bool {
    match expr {
        Expr::Ident(current) => current == name,
        Expr::Attr(base, _) => expr_mentions_ident(base, name),
        Expr::FnCall(callee, args) => {
            expr_mentions_ident(callee, name)
                || args.iter().any(|arg| expr_mentions_ident(arg, name))
        }
        Expr::BinOp(_, left, right) => {
            expr_mentions_ident(left, name) || expr_mentions_ident(right, name)
        }
        Expr::Match { subject, arms, .. } => {
            expr_mentions_ident(subject, name)
                || arms.iter().any(|arm| expr_mentions_ident(&arm.body, name))
        }
        Expr::Constructor(_, inner) => inner
            .as_deref()
            .is_some_and(|inner| expr_mentions_ident(inner, name)),
        Expr::ErrorProp(inner) => expr_mentions_ident(inner, name),
        Expr::InterpolatedStr(parts) => parts.iter().any(|part| match part {
            StrPart::Literal(_) => false,
            StrPart::Parsed(inner) => expr_mentions_ident(inner, name),
        }),
        Expr::List(items) | Expr::Tuple(items) => {
            items.iter().any(|item| expr_mentions_ident(item, name))
        }
        Expr::MapLiteral(entries) => entries
            .iter()
            .any(|(key, value)| expr_mentions_ident(key, name) || expr_mentions_ident(value, name)),
        Expr::RecordCreate { fields, .. } => fields
            .iter()
            .any(|(_, value)| expr_mentions_ident(value, name)),
        Expr::RecordUpdate { base, updates, .. } => {
            expr_mentions_ident(base, name)
                || updates
                    .iter()
                    .any(|(_, value)| expr_mentions_ident(value, name))
        }
        Expr::TailCall(call) => call.1.iter().any(|arg| expr_mentions_ident(arg, name)),
        Expr::Literal(_) | Expr::Resolved(_) => false,
    }
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
                || arms
                    .iter()
                    .any(|arm| match_arm_calls_function(arm, fn_name))
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
        Expr::MapLiteral(entries) => entries.iter().any(|(key, value)| {
            expr_calls_function(key, fn_name) || expr_calls_function(value, fn_name)
        }),
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
            boxed.0 == fn_name || boxed.1.iter().any(|arg| expr_calls_function(arg, fn_name))
        }
        Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) | Expr::Constructor(_, None) => false,
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
            when: None,
            lhs,
            rhs,
            sample_guards: vec![],
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
        assert_eq!(
            law_spec_ref(&verify_law, &fn_sigs),
            declared_spec_ref(&verify_law, &fn_sigs)
        );
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
            (
                vec![Type::Int],
                Type::Int,
                vec!["Console.print".to_string()],
            ),
        );

        let verify_law = law(
            Expr::Ident("x".to_string()),
            Expr::Ident("x".to_string()),
            "fooSpec",
        );

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

        let verify_law = law(
            Expr::Ident("x".to_string()),
            Expr::Ident("x".to_string()),
            "fooSpec",
        );

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
