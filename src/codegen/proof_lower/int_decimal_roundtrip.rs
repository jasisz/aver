//! `IntDecimalRoundtrip` strategy detector.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;

/// Detector for [`crate::ir::ProofStrategy::IntDecimalRoundtrip`] — the
/// canonical decimal-Int parse/serialize roundtrip
/// (`examples/data/json.av` `parseNumber.fromIntRoundtrip`).
///
/// Law shape: no `when`, exactly one `given n: Int`, lhs
/// `parse(ser(C(n)), 0)`, rhs `Ok(C(n), String.len(ser(C(n))))`. Fn
/// shape, validated transitively and EXACTLY (every gate is load-
/// bearing for the Lean emission's fixed proof skeleton — see the
/// variant doc):
///
/// ```text
/// parse(s, start)   = match charAt(s, start) { None -> _,
///                       Some(c) -> match c { "-" -> neg(s, start+1, start),
///                                            "0" -> scan(s, start+1, start, true),
///                                            _   -> pos(s, start, c) } }
/// pos(s, start, c)  = match P(c) { true -> scan(s, start+1, start, false), false -> _ }
/// neg(s, p, start)  = match charAt(s, p) { None -> _,
///                       Some(c) -> match c { "0" -> scan(s, p+1, start, true),
///                                            _   -> sign(s, p, start, c) } }
/// sign(s, p, start, c) = match P(c) { true -> scan(s, p+1, start, false), false -> _ }
/// scan              = recognized string-pos scanner (predicate P,
///                     pins [carried, false], string-pos fuel contract,
///                     exit finish(s, start, p, false))
/// finish(s, a, e, asFloat) = num = String.slice(s, a, e);
///                     match asFloat { true -> _, false -> fint(num, e) }
/// fint(num, p)      = match Int.fromString(num) { Result.Ok(v) -> Ok(C(v), p), _ -> _ }
/// ser               = match … { C(x) -> String.fromInt(x), … }
/// ```
///
/// A shape that deviates anywhere pins nothing (falls through to the
/// prelude-simp rung / `BackendDispatch`) — the conservative gate is
/// what lets the emission cite the scanner's synthesized
/// `<fn>__fuel_scan` lemma knowing it exists.
pub(super) fn detect_int_decimal_roundtrip(
    law: &crate::ast::VerifyLaw,
    fn_name: &str,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<crate::ir::ProofStrategy> {
    use crate::ast::{Expr, Literal, Pattern, Stmt};

    if law.when.is_some() || law.givens.len() != 1 || law.givens[0].type_name != "Int" {
        return None;
    }
    let given = law.givens[0].name.as_str();
    // The Lean emission's fixed skeleton binds these names; a colliding
    // given would be shadowed mid-proof.
    const SKELETON_RESERVED: &[&str] = &[
        "m", "d", "ds", "x", "hx", "hm", "hnd", "hsl", "hch", "hch0", "hch1", "hlen", "hmk",
        "hds10", "hdigits", "hfuel", "harm", "heq", "hdisp1", "hts", "hfin", "h0", "h1", "h2",
        "hlen0", "hslice",
    ];
    if SKELETON_RESERVED.contains(&given) {
        return None;
    }

    fn ident_of(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Ident(n) | Expr::Resolved { name: n, .. } => Some(n.as_str()),
            _ => None,
        }
    }
    fn call_of(e: &Spanned<Expr>) -> Option<(String, &[Spanned<Expr>])> {
        match &e.node {
            Expr::FnCall(callee, args) => {
                Some((expr_to_dotted_name(&callee.node)?, args.as_slice()))
            }
            Expr::TailCall(data) => Some((data.target.clone(), data.args.as_slice())),
            _ => None,
        }
    }
    // Constructor application: `Type.Variant(args…)` parses as a FnCall
    // on a dotted uppercase-leaf name; resolved bodies may carry
    // `Expr::Constructor`.
    fn ctor_of(e: &Spanned<Expr>) -> Option<(String, Vec<&Spanned<Expr>>)> {
        match &e.node {
            Expr::FnCall(callee, args) => {
                let name = expr_to_dotted_name(&callee.node)?;
                let leaf = name.rsplit('.').next()?;
                if !leaf.chars().next().is_some_and(|c| c.is_uppercase()) {
                    return None;
                }
                Some((name, args.iter().collect()))
            }
            Expr::Constructor(name, payload) => {
                let args: Vec<&Spanned<Expr>> = match payload.as_deref() {
                    None => Vec::new(),
                    Some(Spanned {
                        node: Expr::Tuple(items),
                        ..
                    }) => items.iter().collect(),
                    Some(single) => vec![single],
                };
                Some((name.clone(), args))
            }
            _ => None,
        }
    }
    fn is_ident(e: &Spanned<Expr>, name: &str) -> bool {
        ident_of(e) == Some(name)
    }
    fn is_plus_one(e: &Spanned<Expr>, name: &str) -> bool {
        matches!(&e.node, Expr::BinOp(crate::ast::BinOp::Add, l, r)
            if ident_of(l) == Some(name)
                && matches!(&r.node, Expr::Literal(Literal::Int(1))))
    }
    let resolve_user_fn = |name: &str| -> Option<&FnDef> {
        let fd = inputs.find_fn_def_by_call_name(name)?;
        (fd.effects.is_empty() && fd.name != "main").then_some(fd)
    };
    fn single_match(fd: &FnDef) -> Option<(&Spanned<Expr>, &[crate::ast::MatchArm])> {
        let [Stmt::Expr(body)] = fd.body.stmts() else {
            return None;
        };
        let Expr::Match { subject, arms } = &body.node else {
            return None;
        };
        Some((subject, arms.as_slice()))
    }
    /// (Some-binder name, Some arm) of a 2-arm charAt(p0, p1) match.
    fn charat_match(fd: &FnDef) -> Option<(String, &crate::ast::MatchArm)> {
        let (subject, arms) = single_match(fd)?;
        let (callee, args) = call_of(subject)?;
        if callee != "String.charAt"
            || args.len() != 2
            || !is_ident(&args[0], &fd.params[0].0)
            || !is_ident(&args[1], &fd.params[1].0)
            || arms.len() != 2
        {
            return None;
        }
        arms.iter()
            .any(|a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.None" && b.is_empty()))
            .then_some(())?;
        let some_arm = arms.iter().find(
            |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.Some" && b.len() == 1),
        )?;
        let Pattern::Constructor(_, binders) = &some_arm.pattern else {
            return None;
        };
        Some((binders[0].clone(), some_arm))
    }
    /// `match P(c) { true -> scan(s, pos+1, start, false), false -> _ }`
    /// dispatcher tail shared by `pos_fn` and `sign_fn`. Returns
    /// (predicate name, scanner name).
    fn digit_dispatch(
        fd: &FnDef,
        s_param: &str,
        pos_param: &str,
        start_param: &str,
        c_param: &str,
    ) -> Option<(String, String)> {
        let (subject, arms) = single_match(fd)?;
        let (pred, pred_args) = call_of(subject)?;
        if pred.contains('.') || pred_args.len() != 1 || !is_ident(&pred_args[0], c_param) {
            return None;
        }
        if arms.len() != 2
            || !arms
                .iter()
                .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))))
        {
            return None;
        }
        let true_arm = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))))?;
        let (scan, scan_args) = call_of(&true_arm.body)?;
        (scan_args.len() == 4
            && is_ident(&scan_args[0], s_param)
            && is_plus_one(&scan_args[1], pos_param)
            && is_ident(&scan_args[2], start_param)
            && matches!(&scan_args[3].node, Expr::Literal(Literal::Bool(false))))
        .then_some((pred, scan))
    }

    // ---- Law shape -------------------------------------------------
    let (lhs_callee, lhs_args) = call_of(&law.lhs)?;
    if lhs_callee.rsplit('.').next()? != fn_name || lhs_args.len() != 2 {
        return None;
    }
    let ser_arg = &lhs_args[0];
    if !matches!(&lhs_args[1].node, Expr::Literal(Literal::Int(0))) {
        return None;
    }
    let (ser_name, ser_args) = call_of(ser_arg)?;
    if ser_args.len() != 1 {
        return None;
    }
    let ctor_expr = &ser_args[0];
    let (ctor_name, ctor_args) = ctor_of(ctor_expr)?;
    if ctor_args.len() != 1 || !is_ident(ctor_args[0], given) {
        return None;
    }
    // The constructor's variant must carry exactly one Int field — the
    // serializer's ADT measure is then constant on `C(n)` for free `n`
    // (fuel computes; `ser(C(n)) = String.fromInt n` can be `rfl`).
    {
        let (type_name, variant_name) = ctor_name.rsplit_once('.')?;
        let Some(crate::ast::TypeDef::Sum { variants, .. }) = inputs.find_type_def(type_name)
        else {
            return None;
        };
        variants
            .iter()
            .any(|v| v.name == variant_name && v.fields.len() == 1 && v.fields[0].trim() == "Int")
            .then_some(())?;
    }
    let (ok_name, rhs_args) = ctor_of(&law.rhs)?;
    if rhs_args.len() != 2 || rhs_args[0].node != ctor_expr.node {
        return None;
    }
    let (len_callee, len_args) = call_of(rhs_args[1])?;
    if len_callee != "String.len" || len_args.len() != 1 || len_args[0].node != ser_arg.node {
        return None;
    }

    // ---- Serializer: has the `C(x) -> String.fromInt(x)` arm --------
    let ser_fd = resolve_user_fn(&ser_name)?;
    {
        let (_, arms) = single_match(ser_fd)?;
        arms.iter()
            .any(|a| {
                let Pattern::Constructor(n, binders) = &a.pattern else {
                    return false;
                };
                if n != &ctor_name || binders.len() != 1 {
                    return false;
                }
                call_of(&a.body).is_some_and(|(callee, args)| {
                    callee == "String.fromInt" && args.len() == 1 && is_ident(&args[0], &binders[0])
                })
            })
            .then_some(())?;
    }

    // ---- Parser: head-char dispatch ---------------------------------
    let parse_fd = resolve_user_fn(fn_name)?;
    if parse_fd.params.len() != 2
        || parse_fd.params[0].1 != "String"
        || parse_fd.params[1].1 != "Int"
    {
        return None;
    }
    let (p_s, p_start) = (parse_fd.params[0].0.as_str(), parse_fd.params[1].0.as_str());
    let (c_name, some_arm) = charat_match(parse_fd)?;
    let Expr::Match {
        subject: c_subject,
        arms: c_arms,
    } = &some_arm.body.node
    else {
        return None;
    };
    if !is_ident(c_subject, &c_name) || c_arms.len() != 3 {
        return None;
    }
    // Arm order is load-bearing: the emission's `split` bullets follow
    // the textual arm order of the emitted Lean match.
    if !matches!(&c_arms[0].pattern, Pattern::Literal(Literal::Str(s)) if s == "-")
        || !matches!(&c_arms[1].pattern, Pattern::Literal(Literal::Str(s)) if s == "0")
        || !matches!(&c_arms[2].pattern, Pattern::Wildcard)
    {
        return None;
    }
    let (neg_name, neg_args) = call_of(&c_arms[0].body)?;
    (neg_args.len() == 3
        && is_ident(&neg_args[0], p_s)
        && is_plus_one(&neg_args[1], p_start)
        && is_ident(&neg_args[2], p_start))
    .then_some(())?;
    let (scan_zero, zero_args) = call_of(&c_arms[1].body)?;
    (zero_args.len() == 4
        && is_ident(&zero_args[0], p_s)
        && is_plus_one(&zero_args[1], p_start)
        && is_ident(&zero_args[2], p_start)
        && matches!(&zero_args[3].node, Expr::Literal(Literal::Bool(true))))
    .then_some(())?;
    let (pos_name, pos_args) = call_of(&c_arms[2].body)?;
    (pos_args.len() == 3
        && is_ident(&pos_args[0], p_s)
        && is_ident(&pos_args[1], p_start)
        && is_ident(&pos_args[2], &c_name))
    .then_some(())?;

    // ---- pos_fn / neg_fn / sign_fn ----------------------------------
    let pos_fd = resolve_user_fn(&pos_name)?;
    if pos_fd.params.len() != 3 {
        return None;
    }
    let (pred_a, scan_a) = digit_dispatch(
        pos_fd,
        &pos_fd.params[0].0,
        &pos_fd.params[1].0,
        &pos_fd.params[1].0,
        &pos_fd.params[2].0,
    )?;

    let neg_fd = resolve_user_fn(&neg_name)?;
    if neg_fd.params.len() != 3 {
        return None;
    }
    let (n_s, n_pos, n_start) = (
        neg_fd.params[0].0.as_str(),
        neg_fd.params[1].0.as_str(),
        neg_fd.params[2].0.as_str(),
    );
    let (c2_name, neg_some_arm) = charat_match(neg_fd)?;
    let Expr::Match {
        subject: c2_subject,
        arms: c2_arms,
    } = &neg_some_arm.body.node
    else {
        return None;
    };
    if !is_ident(c2_subject, &c2_name) || c2_arms.len() != 2 {
        return None;
    }
    if !matches!(&c2_arms[0].pattern, Pattern::Literal(Literal::Str(s)) if s == "0")
        || !matches!(&c2_arms[1].pattern, Pattern::Wildcard)
    {
        return None;
    }
    let (scan_b, nz_args) = call_of(&c2_arms[0].body)?;
    (nz_args.len() == 4
        && is_ident(&nz_args[0], n_s)
        && is_plus_one(&nz_args[1], n_pos)
        && is_ident(&nz_args[2], n_start)
        && matches!(&nz_args[3].node, Expr::Literal(Literal::Bool(true))))
    .then_some(())?;
    let (sign_name, sign_args) = call_of(&c2_arms[1].body)?;
    (sign_args.len() == 4
        && is_ident(&sign_args[0], n_s)
        && is_ident(&sign_args[1], n_pos)
        && is_ident(&sign_args[2], n_start)
        && is_ident(&sign_args[3], &c2_name))
    .then_some(())?;

    let sign_fd = resolve_user_fn(&sign_name)?;
    if sign_fd.params.len() != 4 {
        return None;
    }
    let (pred_b, scan_c) = digit_dispatch(
        sign_fd,
        &sign_fd.params[0].0,
        &sign_fd.params[1].0,
        &sign_fd.params[2].0,
        &sign_fd.params[3].0,
    )?;

    // One scanner, one predicate, everywhere.
    if pred_a != pred_b || scan_a != scan_b || scan_a != scan_c || scan_a != scan_zero {
        return None;
    }
    let scanner_name = scan_a;
    let pred_name = pred_a;

    // ---- Scanner: recognized shape + string-pos fuel contract -------
    let scan_fd = resolve_user_fn(&scanner_name)?;
    let shape = crate::codegen::proof_recognize::detect_string_pos_scan(scan_fd)?;
    if shape.predicate_fn != pred_name || shape.param_pins != vec![None, Some(false)] {
        return None;
    }
    let pred_fd = resolve_user_fn(&pred_name)?;
    if !crate::codegen::proof_recognize::scan_predicate_fn_ok(pred_fd) {
        return None;
    }
    let scan_key = match inputs.fn_owning_scope(scan_fd) {
        Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &scan_fd.name),
        None => crate::ir::FnKey::entry(&scan_fd.name),
    };
    let scan_contract = inputs
        .symbol_table
        .fn_id_of(&scan_key)
        .and_then(|id| fn_contracts.get(&id))?;
    if !matches!(
        scan_contract.recursion,
        Some(crate::ir::RecursionContract::Fuel {
            fuel_metric: crate::ir::FuelMetric::StringLenMinusPos { .. },
        })
    ) {
        return None;
    }
    // Exit: `finish(s, start, pos, false)`.
    let (finish_name, exit_args) = call_of(&shape.exit_expr)?;
    (exit_args.len() == 4
        && is_ident(&exit_args[0], &scan_fd.params[0].0)
        && is_ident(&exit_args[1], &scan_fd.params[2].0)
        && is_ident(&exit_args[2], &scan_fd.params[1].0)
        && matches!(&exit_args[3].node, Expr::Literal(Literal::Bool(false))))
    .then_some(())?;

    // ---- finish_fn: slice + asFloat dispatch -------------------------
    let finish_fd = resolve_user_fn(&finish_name)?;
    if finish_fd.params.len() != 4 {
        return None;
    }
    let [
        Stmt::Binding(num_name, _, slice_expr),
        Stmt::Expr(finish_match),
    ] = finish_fd.body.stmts()
    else {
        return None;
    };
    {
        let (slice_callee, slice_args) = call_of(slice_expr)?;
        (slice_callee == "String.slice"
            && slice_args.len() == 3
            && is_ident(&slice_args[0], &finish_fd.params[0].0)
            && is_ident(&slice_args[1], &finish_fd.params[1].0)
            && is_ident(&slice_args[2], &finish_fd.params[2].0))
        .then_some(())?;
    }
    let Expr::Match {
        subject: float_subject,
        arms: float_arms,
    } = &finish_match.node
    else {
        return None;
    };
    if !is_ident(float_subject, &finish_fd.params[3].0) || float_arms.len() != 2 {
        return None;
    }
    float_arms
        .iter()
        .any(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))))
        .then_some(())?;
    let int_arm = float_arms
        .iter()
        .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))))?;
    let (finish_int_name, fi_args) = call_of(&int_arm.body)?;
    (fi_args.len() == 2
        && is_ident(&fi_args[0], num_name)
        && is_ident(&fi_args[1], &finish_fd.params[2].0))
    .then_some(())?;

    // ---- finish_int_fn: Int.fromString leaf rebuilding the law's rhs --
    let fint_fd = resolve_user_fn(&finish_int_name)?;
    if fint_fd.params.len() != 2 {
        return None;
    }
    {
        let (subject, arms) = single_match(fint_fd)?;
        let (callee, args) = call_of(subject)?;
        (callee == "Int.fromString" && args.len() == 1 && is_ident(&args[0], &fint_fd.params[0].0))
            .then_some(())?;
        let ok_arm = arms.iter().find(
            |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Result.Ok" && b.len() == 1),
        )?;
        let Pattern::Constructor(_, ok_binders) = &ok_arm.pattern else {
            return None;
        };
        let (arm_ok_name, arm_args) = ctor_of(&ok_arm.body)?;
        (arm_ok_name == ok_name
            && arm_args.len() == 2
            && is_ident(arm_args[1], &fint_fd.params[1].0))
        .then_some(())?;
        let (arm_ctor, arm_ctor_args) = ctor_of(arm_args[0])?;
        (arm_ctor == ctor_name
            && arm_ctor_args.len() == 1
            && is_ident(arm_ctor_args[0], &ok_binders[0]))
        .then_some(())?;
    }

    Some(crate::ir::ProofStrategy::IntDecimalRoundtrip {
        parse_fn: fn_name.to_string(),
        neg_fn: neg_name,
        pos_fn: pos_name,
        sign_fn: sign_name,
        scanner_fn: scanner_name,
        predicate_fn: pred_name,
        finish_fn: finish_name,
        finish_int_fn: finish_int_name,
        serializer_fn: ser_name,
    })
}
