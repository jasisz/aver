//! `StringEscapeRoundtrip` strategy detector.
//!
//! Split from `proof_lower.rs` — see the module docs in [`super`].

use super::*;
use crate::ir::{EscapePairSpec, StringEscapeRoundtripPin};

/// Detector for [`crate::ir::ProofStrategy::StringEscapeRoundtrip`] —
/// the canonical escaped-string parse/serialize roundtrip
/// (`examples/data/json.av` `escapeJsonString.parseStringRoundtrip` /
/// `parseStringChunk.escapedStringRoundtrip`).
///
/// Law shape: no `when`, exactly one `given s: String`, lhs
/// `entry(<open> + escape(s) + <terminator>, 1)` (or the scanner
/// entered directly with `pos = segmentStart = 1, chunks = []`), rhs
/// `Ok(StrCtor(s), String.len(escape(s)) + 2)`. Fn shapes, validated
/// transitively and EXACTLY (every gate is load-bearing for the Lean
/// emission's suffix-invariant proof skeleton — see the variant doc):
///
/// ```text
/// entry(s, pos)      = scan(s, pos, pos, [])              [wrapper laws only]
/// scan(s, p, st, ch) = match charAt(s, p) { None -> _,
///                        Some(c) -> match c { TERM -> finish(s, p+1, st, ch),
///                                             ESC  -> escape(s, p+1, p, st, ch),
///                                             _    -> validate(s, p, st, ch, c) } }
/// finish(s, n, st, ch)  = seg = slice(s, st, n - 1); all = join(concat(ch, [seg]), "");
///                         Ok(StrCtor(all), n)
/// validate(s, p, st, ch, c) = match toCode(c) < T { true -> _, false -> scan(s, p+1, st, ch) }
/// escape(s, p, sp, st, ch)  = seg = slice(s, st, sp); base = concat(ch, [seg]);
///                         match charAt(s, p) { None -> _,
///                           Some(c) -> match c { LETTER_i -> scan(s, p+1, p+1, concat(base, [DEC_i])),
///                                                UNI -> unicode(s, p+1, p+1, base),
///                                                _ -> _ } }
/// unicode(s, p, e, ch)  = match readHex(s, p, 0, 0) { None -> _,
///                           Some(cp) -> codepoint(s, p+4, e, ch, cp) }
/// codepoint(…, cp)   = match hi(cp) { true -> _, false ->
///                        match lo(cp) { true -> _, false -> apply(s, p, e, ch, cp) } }
/// apply(s, p, e, ch, cp) = match String.fromCodePoint(cp) { None -> _,
///                           Some(c2) -> scan(s, p, p, concat(ch, [c2])) }
/// readHex(s, p, a, n)   = match n == 4 { true -> Some(a), false ->
///                        match charAt(s, p) { None -> _, Some(c) ->
///                          match hexVal(c) { None -> _,
///                            Some(v) -> readHex(s, p+1, a*16+v, n+1) } } }
/// hi(cp) = match cp >= HI { true -> _, false -> false }   (same for lo / LO)
/// producer(s)        = fold(String.chars(s), "")
/// fold(cs, acc)      = match cs { [] -> acc, [c, ..r] -> fold(r, acc + cls(c)) }
/// cls(c)             = match c { DEC_i -> "<ESC><LETTER_i>", …, _ -> ctl(c) }
/// ctl(c)             = code = toCode(c); match code == K_j { true -> "<ESC><LETTER_j>",
///                        false -> … match code < T { true -> cce(c, code), false -> c } }
/// cce(c, code)       = match Bytes.fromList([code]) {
///                        Ok(bs) -> "<ESC><UNI>00" + Bytes.toHex(bs), Err(_) -> c }
/// ```
///
/// Alignment gates (each rules out a shape whose synthesized proof
/// could not close, so the pin declines instead — the conservatism
/// contract): every producer pair has a consumer escape arm decoding
/// it back; the terminator and the escape introducer are themselves
/// classifier pairs with codes >= the control threshold; control
/// ladder codes sit below the threshold and never duplicate a
/// classifier pair; the threshold matches between producer and
/// validator and stays <= 256 (the `Bytes.fromList` octet range, since a
/// `"00" + two hex digits` payload encodes exactly one octet, and below the
/// surrogate guards' bounds); the control-escape prefix is exactly
/// `[ESC, UNI, '0', '0']` (4 chars, so a hex escape is consumed by
/// 1 escape hop + 4 `readHex` digits); the scanner SCC members carry
/// `Fuel { Lex }` contracts (the fuel mutual the step lemmas
/// unfold); `readHex` is separately fueled on its count climbing to
/// the literal bound 4.
pub(super) fn detect_string_escape_roundtrip(
    law: &crate::ast::VerifyLaw,
    inputs: &ProofLowerInputs,
    fn_contracts: &std::collections::HashMap<crate::ir::FnId, crate::ir::FnContract>,
) -> Option<crate::ir::ProofStrategy> {
    use crate::ast::{Expr, Literal, Pattern, Stmt};

    if law.when.is_some() || law.givens.len() != 1 || law.givens[0].type_name != "String" {
        return None;
    }
    let given = law.givens[0].name.as_str();
    // The Lean emission's skeleton binds these names in the law proof
    // body; a colliding given would be shadowed mid-proof.
    const SKELETON_RESERVED: &[&str] = &["hdata", "hval", "hpos", "cs"];
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
    fn is_int_lit(e: &Spanned<Expr>, v: i64) -> bool {
        matches!(&e.node, Expr::Literal(Literal::Int(n)) if *n == v)
    }
    fn is_first_code_point_or_zero(e: &Spanned<Expr>, text_name: &str) -> bool {
        let Some((callee, args)) = call_of(e) else {
            return false;
        };
        if callee != "Option.withDefault" || args.len() != 2 || !is_int_lit(&args[1], 0) {
            return false;
        }
        let Some((inner, inner_args)) = call_of(&args[0]) else {
            return false;
        };
        inner == "String.firstCodePoint"
            && inner_args.len() == 1
            && is_ident(&inner_args[0], text_name)
    }
    fn is_plus_lit(e: &Spanned<Expr>, name: &str, v: i64) -> bool {
        matches!(&e.node, Expr::BinOp(crate::ast::BinOp::Add, l, r)
            if ident_of(l) == Some(name) && is_int_lit(r, v))
    }
    fn str_lit(e: &Spanned<Expr>) -> Option<&str> {
        match &e.node {
            Expr::Literal(Literal::Str(s)) => Some(s.as_str()),
            _ => None,
        }
    }
    fn single_char(s: &str) -> Option<char> {
        let mut it = s.chars();
        let c = it.next()?;
        it.next().is_none().then_some(c)
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
    fn match_of(e: &Spanned<Expr>) -> Option<(&Spanned<Expr>, &[crate::ast::MatchArm])> {
        let Expr::Match { subject, arms } = &e.node else {
            return None;
        };
        Some((subject, arms.as_slice()))
    }
    /// (Some-binder name, Some arm) of a 2-arm `charAt(p_s, p_pos)` match.
    fn charat_match<'b>(
        subject: &Spanned<Expr>,
        arms: &'b [crate::ast::MatchArm],
        s_param: &str,
        pos_param: &str,
    ) -> Option<(String, &'b crate::ast::MatchArm)> {
        let (callee, args) = call_of(subject)?;
        if callee != "String.charAt"
            || args.len() != 2
            || !is_ident(&args[0], s_param)
            || !is_ident(&args[1], pos_param)
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
    /// Bool-subject 2-arm match: returns (true-arm body, false-arm body).
    fn bool_match(arms: &[crate::ast::MatchArm]) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
        if arms.len() != 2 {
            return None;
        }
        let t = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(true))))?;
        let f = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::Literal(Literal::Bool(false))))?;
        Some((&t.body, &f.body))
    }
    /// `List.concat(list_expr, [item_expr])` — returns (list, item).
    fn list_concat_snoc(e: &Spanned<Expr>) -> Option<(&Spanned<Expr>, &Spanned<Expr>)> {
        let (callee, args) = call_of(e)?;
        if callee != "List.concat" || args.len() != 2 {
            return None;
        }
        let Expr::List(items) = &args[1].node else {
            return None;
        };
        if items.len() != 1 {
            return None;
        }
        Some((&args[0], &items[0]))
    }
    /// Param type strings must match exactly (the synthesized lemma
    /// statements quote the fns at these types).
    fn param_types(fd: &FnDef, expected: &[&str]) -> bool {
        fd.params.len() == expected.len()
            && fd
                .params
                .iter()
                .zip(expected)
                .all(|((_, ty), want)| ty.trim() == *want)
    }

    // ---- Law shape -------------------------------------------------
    let (lhs_callee, lhs_args) = call_of(&law.lhs)?;
    // SUBJECT = (OPEN + producer(given)) + TERM, left-assoc string `+`.
    let subject = lhs_args.first()?;
    let Expr::BinOp(crate::ast::BinOp::Add, open_prod, term_lit) = &subject.node else {
        return None;
    };
    let term_str = str_lit(term_lit)?;
    let terminator = single_char(term_str)?;
    let Expr::BinOp(crate::ast::BinOp::Add, open_lit, prod_call) = &open_prod.node else {
        return None;
    };
    // The opening char is never read (parsing starts at pos 1) but its
    // length is position-arithmetic load-bearing: exactly 1 char.
    single_char(str_lit(open_lit)?)?;
    let (producer_name, prod_args) = call_of(prod_call)?;
    if prod_args.len() != 1 || !is_ident(&prod_args[0], given) {
        return None;
    }
    // rhs: Ok(StrCtor(given), String.len(producer(given)) + 2)
    let (ok_ctor, rhs_args) = ctor_of(&law.rhs)?;
    if rhs_args.len() != 2 {
        return None;
    }
    let (str_ctor, str_args) = ctor_of(rhs_args[0])?;
    if str_args.len() != 1 || !is_ident(str_args[0], given) {
        return None;
    }
    {
        let Expr::BinOp(crate::ast::BinOp::Add, len_call, two) = &rhs_args[1].node else {
            return None;
        };
        if !is_int_lit(two, 2) {
            return None;
        }
        let (len_callee, len_args) = call_of(len_call)?;
        if len_callee != "String.len" || len_args.len() != 1 || len_args[0].node != prod_call.node {
            return None;
        }
    }

    // ---- Entry: wrapper `entry(s, 1)` or scanner `scan(s, 1, 1, [])` --
    let entry_fd = resolve_user_fn(&lhs_callee)?;
    let scan_name: String = if lhs_args.len() == 2 {
        // Wrapper form. Args: (SUBJECT, 1); body: scan(s, pos, pos, []).
        if !is_int_lit(&lhs_args[1], 1) || !param_types(entry_fd, &["String", "Int"]) {
            return None;
        }
        let [Stmt::Expr(body)] = entry_fd.body.stmts() else {
            return None;
        };
        let (scan, scan_args) = call_of(body)?;
        (scan_args.len() == 4
            && is_ident(&scan_args[0], &entry_fd.params[0].0)
            && is_ident(&scan_args[1], &entry_fd.params[1].0)
            && is_ident(&scan_args[2], &entry_fd.params[1].0)
            && matches!(&scan_args[3].node, Expr::List(items) if items.is_empty()))
        .then_some(())?;
        scan
    } else if lhs_args.len() == 4 {
        // Direct scanner entry. Args: (SUBJECT, 1, 1, []).
        (is_int_lit(&lhs_args[1], 1)
            && is_int_lit(&lhs_args[2], 1)
            && matches!(&lhs_args[3].node, Expr::List(items) if items.is_empty()))
        .then_some(())?;
        lhs_callee.clone()
    } else {
        return None;
    };

    // ---- Scanner: charAt dispatch over { TERM, ESC, default } -------
    let scan_fd = resolve_user_fn(&scan_name)?;
    if !param_types(scan_fd, &["String", "Int", "Int", "List<String>"]) {
        return None;
    }
    let (sc_s, sc_pos, sc_st, sc_ch) = (
        scan_fd.params[0].0.as_str(),
        scan_fd.params[1].0.as_str(),
        scan_fd.params[2].0.as_str(),
        scan_fd.params[3].0.as_str(),
    );
    let (scan_subject, scan_arms) = single_match(scan_fd)?;
    let (c_name, some_arm) = charat_match(scan_subject, scan_arms, sc_s, sc_pos)?;
    let (c_subject, c_arms) = match_of(&some_arm.body)?;
    if !is_ident(c_subject, &c_name) || c_arms.len() != 3 {
        return None;
    }
    if !matches!(&c_arms[2].pattern, Pattern::Wildcard) {
        return None;
    }
    // The two literal arms: finish (4-arg, `pos + 1`) vs escape
    // (5-arg, `pos + 1, pos`) — identified by body shape, either order.
    let mut finish_name: Option<String> = None;
    let mut escape_name: Option<String> = None;
    let mut term_arm_char: Option<char> = None;
    let mut esc_arm_char: Option<char> = None;
    for arm in &c_arms[0..2] {
        let Pattern::Literal(Literal::Str(lit)) = &arm.pattern else {
            return None;
        };
        let ch = single_char(lit)?;
        let (callee, args) = call_of(&arm.body)?;
        if args.len() == 4
            && is_ident(&args[0], sc_s)
            && is_plus_lit(&args[1], sc_pos, 1)
            && is_ident(&args[2], sc_st)
            && is_ident(&args[3], sc_ch)
        {
            (finish_name.is_none()).then_some(())?;
            finish_name = Some(callee);
            term_arm_char = Some(ch);
        } else if args.len() == 5
            && is_ident(&args[0], sc_s)
            && is_plus_lit(&args[1], sc_pos, 1)
            && is_ident(&args[2], sc_pos)
            && is_ident(&args[3], sc_st)
            && is_ident(&args[4], sc_ch)
        {
            (escape_name.is_none()).then_some(())?;
            escape_name = Some(callee);
            esc_arm_char = Some(ch);
        } else {
            return None;
        }
    }
    let finish_name = finish_name?;
    let escape_name = escape_name?;
    if term_arm_char? != terminator {
        return None;
    }
    let escape_char = esc_arm_char?;
    if escape_char == terminator {
        return None;
    }
    // Default arm: validate(s, pos, st, ch, c).
    let validate_name = {
        let (callee, args) = call_of(&c_arms[2].body)?;
        (args.len() == 5
            && is_ident(&args[0], sc_s)
            && is_ident(&args[1], sc_pos)
            && is_ident(&args[2], sc_st)
            && is_ident(&args[3], sc_ch)
            && is_ident(&args[4], &c_name))
        .then_some(())?;
        callee
    };

    // ---- finish: slice + join + Ok ctor matching the law's rhs ------
    let finish_fd = resolve_user_fn(&finish_name)?;
    if !param_types(finish_fd, &["String", "Int", "Int", "List<String>"]) {
        return None;
    }
    {
        let (f_s, f_next, f_st, f_ch) = (
            finish_fd.params[0].0.as_str(),
            finish_fd.params[1].0.as_str(),
            finish_fd.params[2].0.as_str(),
            finish_fd.params[3].0.as_str(),
        );
        let [
            Stmt::Binding(seg_name, _, slice_expr),
            Stmt::Binding(all_name, _, join_expr),
            Stmt::Expr(ok_expr),
        ] = finish_fd.body.stmts()
        else {
            return None;
        };
        {
            let (callee, args) = call_of(slice_expr)?;
            (callee == "String.slice"
                && args.len() == 3
                && is_ident(&args[0], f_s)
                && is_ident(&args[1], f_st)
                && matches!(&args[2].node, Expr::BinOp(crate::ast::BinOp::Sub, l, r)
                    if ident_of(l) == Some(f_next) && is_int_lit(r, 1)))
            .then_some(())?;
        }
        {
            let (callee, args) = call_of(join_expr)?;
            (callee == "String.join" && args.len() == 2 && str_lit(&args[1]) == Some(""))
                .then_some(())?;
            let (list, item) = list_concat_snoc(&args[0])?;
            (is_ident(list, f_ch) && is_ident(item, seg_name)).then_some(())?;
        }
        {
            let (arm_ok, ok_args) = ctor_of(ok_expr)?;
            (arm_ok == ok_ctor && ok_args.len() == 2 && is_ident(ok_args[1], f_next))
                .then_some(())?;
            let (arm_str, str_args) = ctor_of(ok_args[0])?;
            (arm_str == str_ctor && str_args.len() == 1 && is_ident(str_args[0], all_name))
                .then_some(())?;
        }
    }

    // ---- validate: control threshold + scan continuation -------------
    let validate_fd = resolve_user_fn(&validate_name)?;
    if !param_types(
        validate_fd,
        &["String", "Int", "Int", "List<String>", "String"],
    ) {
        return None;
    }
    let control_threshold = {
        let (v_s, v_pos, v_st, v_ch, v_c) = (
            validate_fd.params[0].0.as_str(),
            validate_fd.params[1].0.as_str(),
            validate_fd.params[2].0.as_str(),
            validate_fd.params[3].0.as_str(),
            validate_fd.params[4].0.as_str(),
        );
        let (subject, arms) = single_match(validate_fd)?;
        let Expr::BinOp(crate::ast::BinOp::Lt, code_call, t_lit) = &subject.node else {
            return None;
        };
        let Expr::Literal(Literal::Int(threshold)) = &t_lit.node else {
            return None;
        };
        is_first_code_point_or_zero(code_call, v_c).then_some(())?;
        let (_, false_body) = bool_match(arms)?;
        let (callee, args) = call_of(false_body)?;
        (callee == scan_name
            && args.len() == 4
            && is_ident(&args[0], v_s)
            && is_plus_lit(&args[1], v_pos, 1)
            && is_ident(&args[2], v_st)
            && is_ident(&args[3], v_ch))
        .then_some(())?;
        *threshold
    };
    // `Bytes.fromList` range on the producer side (`0 <= code <= 255` must
    // be omega-derivable from `code < threshold`).
    if !(1..=256).contains(&control_threshold) {
        return None;
    }

    // ---- escape dispatcher: slice + base + letter arms ----------------
    let escape_fd = resolve_user_fn(&escape_name)?;
    if !param_types(escape_fd, &["String", "Int", "Int", "Int", "List<String>"]) {
        return None;
    }
    // (letter, decoded string) consumer arms + the unicode hop.
    let mut consumer_pairs: Vec<(char, String)> = Vec::new();
    let mut unicode_hop: Option<(char, String)> = None;
    {
        let (e_s, e_pos, e_sp, e_st, e_ch) = (
            escape_fd.params[0].0.as_str(),
            escape_fd.params[1].0.as_str(),
            escape_fd.params[2].0.as_str(),
            escape_fd.params[3].0.as_str(),
            escape_fd.params[4].0.as_str(),
        );
        let [
            Stmt::Binding(seg_name, _, slice_expr),
            Stmt::Binding(base_name, _, base_expr),
            Stmt::Expr(dispatch),
        ] = escape_fd.body.stmts()
        else {
            return None;
        };
        {
            let (callee, args) = call_of(slice_expr)?;
            (callee == "String.slice"
                && args.len() == 3
                && is_ident(&args[0], e_s)
                && is_ident(&args[1], e_st)
                && is_ident(&args[2], e_sp))
            .then_some(())?;
        }
        {
            let (list, item) = list_concat_snoc(base_expr)?;
            (is_ident(list, e_ch) && is_ident(item, seg_name)).then_some(())?;
        }
        let (d_subject, d_arms) = match_of(dispatch)?;
        let (c2_name, some_arm) = charat_match(d_subject, d_arms, e_s, e_pos)?;
        let (l_subject, l_arms) = match_of(&some_arm.body)?;
        if !is_ident(l_subject, &c2_name) || l_arms.is_empty() {
            return None;
        }
        if !matches!(&l_arms.last()?.pattern, Pattern::Wildcard) {
            return None;
        }
        for arm in &l_arms[..l_arms.len() - 1] {
            let Pattern::Literal(Literal::Str(lit)) = &arm.pattern else {
                return None;
            };
            let letter = single_char(lit)?;
            let (callee, args) = call_of(&arm.body)?;
            if callee == scan_name {
                // scan(s, pos+1, pos+1, List.concat(base, [DEC]))
                (args.len() == 4
                    && is_ident(&args[0], e_s)
                    && is_plus_lit(&args[1], e_pos, 1)
                    && is_plus_lit(&args[2], e_pos, 1))
                .then_some(())?;
                let (list, item) = list_concat_snoc(&args[3])?;
                (is_ident(list, base_name)).then_some(())?;
                let decoded = str_lit(item)?;
                consumer_pairs.push((letter, decoded.to_string()));
            } else if args.len() == 4
                && is_ident(&args[0], e_s)
                && is_plus_lit(&args[1], e_pos, 1)
                && is_plus_lit(&args[2], e_pos, 1)
                && is_ident(&args[3], base_name)
            {
                // unicode(s, pos+1, pos+1, base) — at most one such arm.
                unicode_hop.is_none().then_some(())?;
                unicode_hop = Some((letter, callee));
            } else {
                return None;
            }
        }
    }
    let (unicode_letter, unicode_name) = unicode_hop?;

    // ---- unicode chain ------------------------------------------------
    let unicode_fd = resolve_user_fn(&unicode_name)?;
    if !param_types(unicode_fd, &["String", "Int", "Int", "List<String>"]) {
        return None;
    }
    let (read_hex_name, codepoint_name) = {
        let (u_s, u_pos, u_e, u_ch) = (
            unicode_fd.params[0].0.as_str(),
            unicode_fd.params[1].0.as_str(),
            unicode_fd.params[2].0.as_str(),
            unicode_fd.params[3].0.as_str(),
        );
        let (subject, arms) = single_match(unicode_fd)?;
        let (rh_name, rh_args) = call_of(subject)?;
        (rh_args.len() == 4
            && is_ident(&rh_args[0], u_s)
            && is_ident(&rh_args[1], u_pos)
            && is_int_lit(&rh_args[2], 0)
            && is_int_lit(&rh_args[3], 0))
        .then_some(())?;
        if arms.len() != 2 {
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
        let (cp_name, cp_args) = call_of(&some_arm.body)?;
        (cp_args.len() == 5
            && is_ident(&cp_args[0], u_s)
            && is_plus_lit(&cp_args[1], u_pos, 4)
            && is_ident(&cp_args[2], u_e)
            && is_ident(&cp_args[3], u_ch)
            && is_ident(&cp_args[4], &binders[0]))
        .then_some(())?;
        (rh_name, cp_name)
    };

    let codepoint_fd = resolve_user_fn(&codepoint_name)?;
    if !param_types(
        codepoint_fd,
        &["String", "Int", "Int", "List<String>", "Int"],
    ) {
        return None;
    }
    let (high_surrogate_name, low_surrogate_name, apply_name) = {
        let (p_s, p_pos, p_e, p_ch, p_cp) = (
            codepoint_fd.params[0].0.as_str(),
            codepoint_fd.params[1].0.as_str(),
            codepoint_fd.params[2].0.as_str(),
            codepoint_fd.params[3].0.as_str(),
            codepoint_fd.params[4].0.as_str(),
        );
        let (subject, arms) = single_match(codepoint_fd)?;
        let (hi_name, hi_args) = call_of(subject)?;
        (hi_args.len() == 1 && is_ident(&hi_args[0], p_cp)).then_some(())?;
        let (_, false_body) = bool_match(arms)?;
        let (lo_subject, lo_arms) = match_of(false_body)?;
        let (lo_name, lo_args) = call_of(lo_subject)?;
        (lo_args.len() == 1 && is_ident(&lo_args[0], p_cp)).then_some(())?;
        let (_, lo_false) = bool_match(lo_arms)?;
        let (app_name, app_args) = call_of(lo_false)?;
        (app_args.len() == 5
            && is_ident(&app_args[0], p_s)
            && is_ident(&app_args[1], p_pos)
            && is_ident(&app_args[2], p_e)
            && is_ident(&app_args[3], p_ch)
            && is_ident(&app_args[4], p_cp))
        .then_some(())?;
        (hi_name, lo_name, app_name)
    };

    let apply_fd = resolve_user_fn(&apply_name)?;
    if !param_types(apply_fd, &["String", "Int", "Int", "List<String>", "Int"]) {
        return None;
    }
    {
        let (a_s, a_pos, _a_e, a_ch, a_cp) = (
            apply_fd.params[0].0.as_str(),
            apply_fd.params[1].0.as_str(),
            apply_fd.params[2].0.as_str(),
            apply_fd.params[3].0.as_str(),
            apply_fd.params[4].0.as_str(),
        );
        let (subject, arms) = single_match(apply_fd)?;
        let (callee, args) = call_of(subject)?;
        (callee == "String.fromCodePoint" && args.len() == 1 && is_ident(&args[0], a_cp))
            .then_some(())?;
        if arms.len() != 2 {
            return None;
        }
        let some_arm = arms.iter().find(
            |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.Some" && b.len() == 1),
        )?;
        let Pattern::Constructor(_, binders) = &some_arm.pattern else {
            return None;
        };
        let (sc_callee, sc_args) = call_of(&some_arm.body)?;
        (sc_callee == scan_name
            && sc_args.len() == 4
            && is_ident(&sc_args[0], a_s)
            && is_ident(&sc_args[1], a_pos)
            && is_ident(&sc_args[2], a_pos))
        .then_some(())?;
        let (list, item) = list_concat_snoc(&sc_args[3])?;
        (is_ident(list, a_ch) && is_ident(item, &binders[0])).then_some(())?;
    }

    // Surrogate guards: `match cp >= MIN { true -> _, false -> false }`.
    let surrogate_min = |name: &str| -> Option<i64> {
        let fd = resolve_user_fn(name)?;
        if !param_types(fd, &["Int"]) || fd.return_type.trim() != "Bool" {
            return None;
        }
        let (subject, arms) = single_match(fd)?;
        let Expr::BinOp(crate::ast::BinOp::Gte, l, r) = &subject.node else {
            return None;
        };
        if !is_ident(l, &fd.params[0].0) {
            return None;
        }
        let Expr::Literal(Literal::Int(min)) = &r.node else {
            return None;
        };
        let (_, false_body) = bool_match(arms)?;
        matches!(&false_body.node, Expr::Literal(Literal::Bool(false))).then_some(*min)
    };
    let high_surrogate_min = surrogate_min(&high_surrogate_name)?;
    let low_surrogate_min = surrogate_min(&low_surrogate_name)?;
    // The decoded control codepoint (< threshold) must sit below both
    // guards so `high_false` / `low_false` close by omega.
    if control_threshold > high_surrogate_min || control_threshold > low_surrogate_min {
        return None;
    }

    // ---- readHex: count-to-4 hex accumulator --------------------------
    let read_hex_fd = resolve_user_fn(&read_hex_name)?;
    if !param_types(read_hex_fd, &["String", "Int", "Int", "Int"]) {
        return None;
    }
    let hex_val_name = {
        let (r_s, r_pos, r_acc, r_count) = (
            read_hex_fd.params[0].0.as_str(),
            read_hex_fd.params[1].0.as_str(),
            read_hex_fd.params[2].0.as_str(),
            read_hex_fd.params[3].0.as_str(),
        );
        let (subject, arms) = single_match(read_hex_fd)?;
        let Expr::BinOp(crate::ast::BinOp::Eq, l, r) = &subject.node else {
            return None;
        };
        (is_ident(l, r_count) && is_int_lit(r, 4)).then_some(())?;
        let (true_body, false_body) = bool_match(arms)?;
        {
            let (done_ctor, done_args) = ctor_of(true_body)?;
            (done_ctor == "Option.Some" && done_args.len() == 1 && is_ident(done_args[0], r_acc))
                .then_some(())?;
        }
        let (ca_subject, ca_arms) = match_of(false_body)?;
        let (c3_name, some_arm) = charat_match(ca_subject, ca_arms, r_s, r_pos)?;
        let (hv_subject, hv_arms) = match_of(&some_arm.body)?;
        let (hv_name, hv_args) = call_of(hv_subject)?;
        (hv_args.len() == 1 && is_ident(&hv_args[0], &c3_name)).then_some(())?;
        if hv_arms.len() != 2 {
            return None;
        }
        hv_arms
            .iter()
            .any(|a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.None" && b.is_empty()))
            .then_some(())?;
        let v_arm = hv_arms.iter().find(
            |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Option.Some" && b.len() == 1),
        )?;
        let Pattern::Constructor(_, v_binders) = &v_arm.pattern else {
            return None;
        };
        let (rec_name, rec_args) = call_of(&v_arm.body)?;
        (rec_name == read_hex_name
            && rec_args.len() == 4
            && is_ident(&rec_args[0], r_s)
            && is_plus_lit(&rec_args[1], r_pos, 1)
            && matches!(&rec_args[2].node, Expr::BinOp(crate::ast::BinOp::Add, ml, vr)
                if matches!(&ml.node, Expr::BinOp(crate::ast::BinOp::Mul, al, sixteen)
                    if ident_of(al) == Some(r_acc) && is_int_lit(sixteen, 16))
                    && ident_of(vr) == Some(&v_binders[0]))
            && is_plus_lit(&rec_args[3], r_count, 1))
        .then_some(())?;
        hv_name
    };
    let hex_val_fd = resolve_user_fn(&hex_val_name)?;
    if !param_types(hex_val_fd, &["String"]) || hex_val_fd.return_type.trim() != "Option<Int>" {
        return None;
    }

    // ---- Producer: wrapper + fold + classifier + control ladder -------
    let producer_fd = resolve_user_fn(&producer_name)?;
    if !param_types(producer_fd, &["String"]) || producer_fd.return_type.trim() != "String" {
        return None;
    }
    let fold_name = {
        let [Stmt::Expr(body)] = producer_fd.body.stmts() else {
            return None;
        };
        let (fold, fold_args) = call_of(body)?;
        (fold_args.len() == 2 && str_lit(&fold_args[1]) == Some("")).then_some(())?;
        let (chars_callee, chars_args) = call_of(&fold_args[0])?;
        (chars_callee == "String.chars"
            && chars_args.len() == 1
            && is_ident(&chars_args[0], &producer_fd.params[0].0))
        .then_some(())?;
        fold
    };
    let fold_fd = resolve_user_fn(&fold_name)?;
    if !param_types(fold_fd, &["List<String>", "String"]) || fold_fd.return_type.trim() != "String"
    {
        return None;
    }
    let classifier_name = {
        let (chars_p, acc_p) = (fold_fd.params[0].0.as_str(), fold_fd.params[1].0.as_str());
        let (subject, arms) = single_match(fold_fd)?;
        if !is_ident(subject, chars_p) || arms.len() != 2 {
            return None;
        }
        let nil_arm = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::EmptyList))?;
        is_ident(&nil_arm.body, acc_p).then_some(())?;
        let cons_arm = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::Cons(_, _)))?;
        let Pattern::Cons(h, t) = &cons_arm.pattern else {
            return None;
        };
        let (rec, rec_args) = call_of(&cons_arm.body)?;
        (rec == fold_name && rec_args.len() == 2 && is_ident(&rec_args[0], t)).then_some(())?;
        let Expr::BinOp(crate::ast::BinOp::Add, acc_side, cls_call) = &rec_args[1].node else {
            return None;
        };
        is_ident(acc_side, acc_p).then_some(())?;
        let (cls, cls_args) = call_of(cls_call)?;
        (cls_args.len() == 1 && is_ident(&cls_args[0], h)).then_some(())?;
        cls
    };
    let classifier_fd = resolve_user_fn(&classifier_name)?;
    if !param_types(classifier_fd, &["String"]) || classifier_fd.return_type.trim() != "String" {
        return None;
    }
    // Classifier literal arms → (decoded, letter) pairs, arm order kept
    // (the synthesized disequality bundles are indexed in arm order).
    let mut pairs: Vec<EscapePairSpec> = Vec::new();
    let control_name = {
        let (subject, arms) = single_match(classifier_fd)?;
        if !is_ident(subject, &classifier_fd.params[0].0) || arms.len() < 2 {
            return None;
        }
        if !matches!(&arms.last()?.pattern, Pattern::Wildcard) {
            return None;
        }
        for arm in &arms[..arms.len() - 1] {
            let Pattern::Literal(Literal::Str(lit)) = &arm.pattern else {
                return None;
            };
            let decoded = single_char(lit)?;
            let esc_out = str_lit(&arm.body)?;
            let mut out_chars = esc_out.chars();
            (out_chars.next() == Some(escape_char)).then_some(())?;
            let letter = out_chars.next()?;
            out_chars.next().is_none().then_some(())?;
            pairs.push(EscapePairSpec {
                decoded,
                letter,
                from_control_ladder: false,
            });
        }
        let (ctl, ctl_args) = call_of(&arms.last()?.body)?;
        (ctl_args.len() == 1 && is_ident(&ctl_args[0], &classifier_fd.params[0].0)).then_some(())?;
        ctl
    };
    let classifier_set: Vec<char> = pairs.iter().map(|p| p.decoded).collect();
    // Distinct decoded chars; the terminator and the escape introducer
    // must be classifier pairs with codes >= the threshold (the
    // printable/control branches derive their disequalities from
    // those facts).
    {
        let mut seen = classifier_set.clone();
        seen.sort_unstable();
        seen.dedup();
        if seen.len() != classifier_set.len() {
            return None;
        }
    }
    if !classifier_set.contains(&terminator) || !classifier_set.contains(&escape_char) {
        return None;
    }
    if ((terminator as i64) < control_threshold) || ((escape_char as i64) < control_threshold) {
        return None;
    }

    // Control classifier: `code = toCode(c)` + equality ladder + final
    // `< threshold` arm.
    let control_fd = resolve_user_fn(&control_name)?;
    if !param_types(control_fd, &["String"]) || control_fd.return_type.trim() != "String" {
        return None;
    }
    let control_escape_name = {
        let c_param = control_fd.params[0].0.as_str();
        let [Stmt::Binding(code_name, _, code_expr), Stmt::Expr(ladder)] = control_fd.body.stmts()
        else {
            return None;
        };
        is_first_code_point_or_zero(code_expr, c_param).then_some(())?;
        // Walk the equality ladder.
        let mut cursor = ladder;
        loop {
            let (subject, arms) = match_of(cursor)?;
            match &subject.node {
                Expr::BinOp(crate::ast::BinOp::Eq, l, r) => {
                    is_ident(l, code_name).then_some(())?;
                    let Expr::Literal(Literal::Int(code)) = &r.node else {
                        return None;
                    };
                    if *code < 0 || *code >= control_threshold {
                        return None;
                    }
                    let decoded = char::from_u32(*code as u32)?;
                    if classifier_set.contains(&decoded)
                        || pairs
                            .iter()
                            .any(|p| p.from_control_ladder && p.decoded == decoded)
                    {
                        return None;
                    }
                    let (true_body, false_body) = bool_match(arms)?;
                    let esc_out = str_lit(true_body)?;
                    let mut out_chars = esc_out.chars();
                    (out_chars.next() == Some(escape_char)).then_some(())?;
                    let letter = out_chars.next()?;
                    out_chars.next().is_none().then_some(())?;
                    pairs.push(EscapePairSpec {
                        decoded,
                        letter,
                        from_control_ladder: true,
                    });
                    cursor = false_body;
                }
                Expr::BinOp(crate::ast::BinOp::Lt, l, r) => {
                    // Final arm: `code < T -> cce(c, code), false -> c`.
                    (is_ident(l, code_name) && is_int_lit(r, control_threshold)).then_some(())?;
                    let (true_body, false_body) = bool_match(arms)?;
                    is_ident(false_body, c_param).then_some(())?;
                    let (cce, cce_args) = call_of(true_body)?;
                    (cce_args.len() == 2
                        && is_ident(&cce_args[0], c_param)
                        && is_ident(&cce_args[1], code_name))
                    .then_some(())?;
                    break cce;
                }
                _ => return None,
            }
        }
    };

    // Control escape: a singleton `Bytes.fromList` followed by `Bytes.toHex`,
    // plus the `[ESC, UNI, '0', '0']` prefix.
    let control_escape_fd = resolve_user_fn(&control_escape_name)?;
    if !param_types(control_escape_fd, &["String", "Int"])
        || control_escape_fd.return_type.trim() != "String"
    {
        return None;
    }
    {
        let (cc_c, cc_code) = (
            control_escape_fd.params[0].0.as_str(),
            control_escape_fd.params[1].0.as_str(),
        );
        let (subject, arms) = single_match(control_escape_fd)?;
        let (callee, args) = call_of(subject)?;
        (callee == "Bytes.fromList" && args.len() == 1).then_some(())?;
        let Expr::List(singleton) = &args[0].node else {
            return None;
        };
        (singleton.len() == 1 && is_ident(&singleton[0], cc_code)).then_some(())?;
        if arms.len() != 2 {
            return None;
        }
        let ok_arm = arms.iter().find(
            |a| matches!(&a.pattern, Pattern::Constructor(n, b) if n == "Result.Ok" && b.len() == 1),
        )?;
        let err_arm = arms
            .iter()
            .find(|a| matches!(&a.pattern, Pattern::Constructor(n, _) if n == "Result.Err"))?;
        is_ident(&err_arm.body, cc_c).then_some(())?;
        let Pattern::Constructor(_, ok_binders) = &ok_arm.pattern else {
            return None;
        };
        let Expr::BinOp(crate::ast::BinOp::Add, prefix, encoded) = &ok_arm.body.node else {
            return None;
        };
        let (to_hex, to_hex_args) = call_of(encoded)?;
        (to_hex == "Bytes.toHex"
            && to_hex_args.len() == 1
            && is_ident(&to_hex_args[0], &ok_binders[0]))
        .then_some(())?;
        let prefix_str = str_lit(prefix)?;
        let want: String = [escape_char, unicode_letter, '0', '0'].iter().collect();
        (prefix_str == want).then_some(())?;
    }

    // ---- Pair table alignment ------------------------------------------
    // Every producer pair must decode back through a consumer escape
    // arm; the unicode letter must not shadow a pair letter (the
    // consumer match dispatches first-match).
    for pair in &pairs {
        let decoded_str: String = pair.decoded.to_string();
        let hit = consumer_pairs
            .iter()
            .find(|(letter, _)| *letter == pair.letter);
        match hit {
            Some((_, dec)) if *dec == decoded_str => {}
            _ => return None,
        }
        if pair.letter == unicode_letter {
            return None;
        }
    }
    if pairs.is_empty() {
        return None;
    }

    // ---- Recursion contracts --------------------------------------------
    // The scanner SCC members the step lemmas unfold must be fuel-
    // emitted as one Lex mutual (the renderer separately probes the
    // emitted rank); readHex is separately fueled on count -> 4; the
    // producer fns and helpers must NOT be fuel mutual members.
    let contract_of = |fd: &FnDef| -> Option<&crate::ir::FnContract> {
        // **syntax-discovery-only** (epic #170 Phase 8 guardrail):
        // scope was just resolved via pointer-eq against dep modules —
        // the `None` arm is the correct entry-scope key by
        // construction (same shape as `fn_key_for_decl`).
        let key = match inputs.fn_owning_scope(fd) {
            Some(prefix) => crate::ir::FnKey::in_module(prefix.to_string(), &fd.name),
            None => crate::ir::FnKey::entry(&fd.name),
        };
        inputs
            .symbol_table
            .fn_id_of(&key)
            .and_then(|id| fn_contracts.get(&id))
    };
    for fd in [
        scan_fd,
        escape_fd,
        validate_fd,
        unicode_fd,
        codepoint_fd,
        apply_fd,
    ] {
        let contract = contract_of(fd)?;
        if !matches!(
            contract.recursion,
            Some(crate::ir::RecursionContract::Fuel {
                fuel_metric: crate::ir::FuelMetric::Lex { .. },
            })
        ) {
            return None;
        }
    }
    {
        let contract = contract_of(read_hex_fd)?;
        let Some(crate::ir::RecursionContract::Fuel {
            fuel_metric:
                crate::ir::FuelMetric::BoundMinusParamNatAbsPlusOne {
                    ref param,
                    ref bound,
                },
        }) = contract.recursion
        else {
            return None;
        };
        if *param != read_hex_fd.params[3].0 {
            return None;
        }
        // Bound must be the literal 4 (the readHex micro-run in the
        // control branch burns exactly 4 step ticks + 1 done tick).
        if !matches!(
            &bound.node,
            crate::ir::hir::ResolvedExpr::Literal(Literal::Int(4))
        ) {
            return None;
        }
    }
    // The producer fold/classifier/control/cce, the validator's hex
    // valuation and the surrogate guards must all stay outside the
    // fuel mutual (the skeleton unfolds them by equation lemma /
    // `decide`-evaluation). Non-scanner fns reached here are
    // non-recursive by shape except the fold; gate the fold's contract
    // to a non-Lex metric so it cannot be a fuel-mutual member.
    for name in [
        &fold_name,
        &classifier_name,
        &control_name,
        &control_escape_name,
        &hex_val_name,
        &high_surrogate_name,
        &low_surrogate_name,
    ] {
        let fd = resolve_user_fn(name)?;
        if let Some(contract) = contract_of(fd)
            && matches!(
                contract.recursion,
                Some(crate::ir::RecursionContract::Fuel {
                    fuel_metric: crate::ir::FuelMetric::Lex { .. },
                })
            )
        {
            return None;
        }
    }

    Some(crate::ir::ProofStrategy::StringEscapeRoundtrip(Box::new(
        StringEscapeRoundtripPin {
            scan_fn: scan_name,
            escape_fn: escape_name,
            validate_fn: validate_name,
            finish_fn: finish_name,
            unicode_fn: unicode_name,
            codepoint_fn: codepoint_name,
            apply_fn: apply_name,
            read_hex_fn: read_hex_name,
            hex_val_fn: hex_val_name,
            high_surrogate_fn: high_surrogate_name,
            low_surrogate_fn: low_surrogate_name,
            producer_fn: producer_name,
            fold_fn: fold_name,
            classifier_fn: classifier_name,
            control_fn: control_name,
            control_escape_fn: control_escape_name,
            ok_ctor,
            str_ctor,
            terminator,
            escape_char,
            unicode_letter,
            pairs,
            control_threshold,
            high_surrogate_min,
            low_surrogate_min,
        },
    )))
}
