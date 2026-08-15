//! Both passes' recognisers, on the shapes that decide them.

use super::*;
use crate::ast::{BinOp, FnBody, FnDef, Literal, Spanned, TailCallData};
use std::sync::Arc;

fn sp<T>(value: T) -> Spanned<T> {
    Spanned::new(value, 1)
}

fn ident(name: &str) -> Spanned<Expr> {
    sp(Expr::Ident(name.to_string()))
}

fn dotted(module: &str, member: &str) -> Spanned<Expr> {
    sp(Expr::Attr(Box::new(ident(module)), member.to_string()))
}

fn call(callee: Spanned<Expr>, args: Vec<Spanned<Expr>>) -> Spanned<Expr> {
    sp(Expr::FnCall(Box::new(callee), args))
}

/// Build a canonical buffer-build fn: takes (col: Int, acc: List<Int>),
/// matches col >= 10, true → reverse(acc), false → tail-call self
/// with prepend(col, acc).
fn canonical_builder(name: &str) -> FnDef {
    let true_body = call(dotted("List", "reverse"), vec![ident("acc")]);
    let prepend = call(dotted("List", "prepend"), vec![ident("col"), ident("acc")]);
    let false_body = sp(Expr::TailCall(Box::new(TailCallData {
        target: name.to_string(),
        args: vec![
            sp(Expr::BinOp(
                BinOp::Add,
                Box::new(ident("col")),
                Box::new(sp(Expr::Literal(Literal::Int(1)))),
            )),
            prepend,
        ],
    })));
    let match_expr = sp(Expr::Match {
        subject: Box::new(sp(Expr::BinOp(
            BinOp::Gte,
            Box::new(ident("col")),
            Box::new(sp(Expr::Literal(Literal::Int(10)))),
        ))),
        arms: vec![
            MatchArm {
                pattern: Pattern::Literal(Literal::Bool(true)),
                body: Box::new(true_body),
                binding_slots: std::sync::OnceLock::new(),
            },
            MatchArm {
                pattern: Pattern::Literal(Literal::Bool(false)),
                body: Box::new(false_body),
                binding_slots: std::sync::OnceLock::new(),
            },
        ],
    });
    FnDef {
        name: name.to_string(),
        line: 1,
        params: vec![
            ("col".to_string(), "Int".to_string()),
            ("acc".to_string(), "List<Int>".to_string()),
        ],
        return_type: "List<Int>".to_string(),
        effects: vec![],
        desc: None,
        body: Arc::new(FnBody::Block(vec![Stmt::Expr(match_expr)])),
        resolution: None,
    }
}

#[test]
fn matches_canonical_buffer_build() {
    let fd = canonical_builder("build");
    let info = compute_buffer_build_sinks(&[&fd]);
    let shape = info.get("build").expect("expected match");
    assert_eq!(shape.acc_param_idx, 1);
    assert_eq!(shape.acc_param_name, "acc");
}

#[test]
fn rejects_fn_without_list_param() {
    let mut fd = canonical_builder("build");
    // Strip the List<...> param.
    fd.params = vec![("col".to_string(), "Int".to_string())];
    let info = compute_buffer_build_sinks(&[&fd]);
    assert!(info.is_empty(), "fn without List param should not match");
}

#[test]
fn rejects_when_true_arm_isnt_reverse() {
    let mut fd = canonical_builder("build");
    // Replace true arm body with a different expression.
    if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body)
        && let Stmt::Expr(spanned) = &mut stmts[0]
        && let Expr::Match { arms, .. } = &mut spanned.node
    {
        *arms[0].body = ident("acc");
    }
    let info = compute_buffer_build_sinks(&[&fd]);
    assert!(
        info.is_empty(),
        "fn returning bare acc instead of reverse should not match"
    );
}

#[test]
fn rejects_when_false_arm_uses_append_not_prepend() {
    let mut fd = canonical_builder("build");
    // Swap List.prepend → List.append in the false arm tail call.
    if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body)
        && let Stmt::Expr(spanned) = &mut stmts[0]
        && let Expr::Match { arms, .. } = &mut spanned.node
    {
        let false_body = arms[1].body.as_mut();
        if let Expr::TailCall(data) = &mut false_body.node
            && let Expr::FnCall(callee, _) = &mut data.args[1].node
            && let Expr::Attr(_, attr) = &mut callee.node
        {
            *attr = "append".to_string();
        }
    }
    let info = compute_buffer_build_sinks(&[&fd]);
    assert!(
        info.is_empty(),
        "fn using List.append instead of prepend should not match"
    );
}

#[test]
fn rejects_tail_call_to_different_fn() {
    let mut fd = canonical_builder("build");
    if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body)
        && let Stmt::Expr(spanned) = &mut stmts[0]
        && let Expr::Match { arms, .. } = &mut spanned.node
    {
        let false_body = arms[1].body.as_mut();
        if let Expr::TailCall(data) = &mut false_body.node {
            data.target = "someone_else".to_string();
        }
    }
    let info = compute_buffer_build_sinks(&[&fd]);
    assert!(
        info.is_empty(),
        "fn whose recursive call targets a different name should not match"
    );
}

#[test]
fn rejects_match_with_non_bool_arms() {
    let mut fd = canonical_builder("build");
    if let FnBody::Block(stmts) = Arc::make_mut(&mut fd.body)
        && let Stmt::Expr(spanned) = &mut stmts[0]
        && let Expr::Match { arms, .. } = &mut spanned.node
    {
        arms[0].pattern = Pattern::Literal(Literal::Int(0));
    }
    let info = compute_buffer_build_sinks(&[&fd]);
    assert!(
        info.is_empty(),
        "match on non-bool patterns should not be detected as buffer-build"
    );
}

/// End-to-end: parse a small Aver source, run TCO, then detect.
/// The TCO transform is what produces `Expr::TailCall` nodes from
/// raw `Expr::FnCall` self-recursion; detection runs on the post-TCO
/// AST.
#[test]
fn detects_via_parser_after_tco() {
    let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))
"#;
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = crate::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    crate::ir::pipeline::tco(&mut items);
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let info = compute_buffer_build_sinks(&fns);
    let shape = info
        .get("build")
        .expect("expected end-to-end shape match for canonical builder");
    assert_eq!(shape.acc_param_idx, 1);
    assert_eq!(shape.acc_param_name, "acc");
}

/// End-to-end fusion-site detection: builder + caller `String.join`
/// site recognised, line recorded, sink name attached.
#[test]
fn finds_fusion_site_via_parser() {
    let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))

fn main() -> String
    String.join(build(5, []), ",")
"#;
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = crate::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    crate::ir::pipeline::tco(&mut items);
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let sinks = compute_buffer_build_sinks(&fns);
    let sites = find_fusion_sites(&fns, &sinks);
    assert_eq!(sites.len(), 1, "expected one fusion site, got {sites:?}");
    let site = &sites[0];
    assert_eq!(site.enclosing_fn, "main");
    assert_eq!(site.sink_fn, "build");
    assert!(site.line > 0, "expected real line info, got 0");
}

/// Caller passes the matched fn's result to a non-`String.join`
/// destination — should NOT register as a fusion site (no buffer
/// to write into).
#[test]
fn ignores_call_when_not_wrapped_in_string_join() {
    let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    build(5, [])
"#;
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = crate::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    crate::ir::pipeline::tco(&mut items);
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let sinks = compute_buffer_build_sinks(&fns);
    let sites = find_fusion_sites(&fns, &sinks);
    assert!(
        sites.is_empty(),
        "build called outside String.join must not be a fusion site, got {sites:?}"
    );
}

/// Counter-test: a recursive fn that returns `acc` directly (no
/// reverse) — semantically valid Aver, but its result order is
/// reversed relative to natural read order, so deforestation can't
/// safely rewrite to a forward-emit buffer loop without explicit
/// authorisation. Detector must reject it.
#[test]
fn rejects_via_parser_when_true_arm_returns_bare_acc() {
    let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> acc
        false -> build(n - 1, List.prepend(n, acc))
"#;
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = crate::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    crate::ir::pipeline::tco(&mut items);
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let info = compute_buffer_build_sinks(&fns);
    assert!(
        info.is_empty(),
        "fn returning bare acc must not be detected as a deforestation candidate"
    );
}

/// End-to-end synthesis: parse a small builder, run TCO, detect
/// it as a sink, then synthesize the buffered variant. Verify the
/// shape: name suffix, dropped acc param, added __buf/__sep
/// params, true arm returns __buf ident, false arm tail-calls
/// __buffered self with threaded buffer expression.
#[test]
fn synthesizes_buffered_variant_from_real_builder() {
    let src = r#"
fn build(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true  -> List.reverse(acc)
        false -> build(n - 1, List.prepend(n, acc))
"#;
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = crate::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    crate::ir::pipeline::tco(&mut items);
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let sinks = compute_buffer_build_sinks(&fns);
    assert!(sinks.contains_key("build"));
    let synthesized = synthesize_buffered_variants(&fns, &sinks);
    assert_eq!(
        synthesized.len(),
        1,
        "expected exactly one synthesized variant"
    );
    let bf = &synthesized[0];

    // Name + signature shape.
    assert_eq!(bf.name, "build__buffered");
    assert_eq!(bf.return_type, "Buffer");
    let param_names: Vec<&str> = bf.params.iter().map(|(n, _)| n.as_str()).collect();
    let param_types: Vec<&str> = bf.params.iter().map(|(_, t)| t.as_str()).collect();
    assert_eq!(param_names, vec!["n", "__buf", "__sep"]);
    assert_eq!(param_types, vec!["Int", "Buffer", "String"]);

    // Body: single Stmt::Expr holding a 2-arm match.
    let stmts = bf.body.stmts();
    assert_eq!(stmts.len(), 1);
    let match_expr = match &stmts[0] {
        Stmt::Expr(s) => match &s.node {
            Expr::Match { subject: _, arms } => arms,
            _ => panic!("body root must be a match"),
        },
        _ => panic!("body root must be Stmt::Expr"),
    };
    assert_eq!(match_expr.len(), 2);

    // True arm: body is `__buf` ident.
    let true_arm = match_expr
        .iter()
        .find(|a| matches!(a.pattern, Pattern::Literal(Literal::Bool(true))))
        .expect("true arm");
    match &true_arm.body.node {
        Expr::Ident(name) => assert_eq!(name, "__buf"),
        other => panic!("true arm should be Ident(__buf), got {other:?}"),
    }

    // False arm: tail-call to build__buffered with threaded buf.
    let false_arm = match_expr
        .iter()
        .find(|a| matches!(a.pattern, Pattern::Literal(Literal::Bool(false))))
        .expect("false arm");
    let tail_data = match &false_arm.body.node {
        Expr::TailCall(d) => d,
        other => panic!("false arm should be TailCall, got {other:?}"),
    };
    assert_eq!(tail_data.target, "build__buffered");
    // Args: [n - 1, threaded-buffer-expr, __sep_ident]. acc-pos
    // (was index 1 in original) is now the threaded buffer; sep
    // appended at end.
    assert_eq!(tail_data.args.len(), 3);
    // Arg 1 is the buffer-threading composition; verify it's
    // `__buf_append(__buf_append_sep_unless_first(__buf, __sep), n)`.
    let outer = match &tail_data.args[1].node {
        Expr::FnCall(callee, args) => {
            match &callee.node {
                Expr::Ident(name) => assert_eq!(name, "__buf_append"),
                _ => panic!("expected Ident callee"),
            }
            args
        }
        _ => panic!("expected outer __buf_append FnCall"),
    };
    assert_eq!(outer.len(), 2);
    // First arg of outer = inner sep-then-buf.
    match &outer[0].node {
        Expr::FnCall(callee, _) => match &callee.node {
            Expr::Ident(name) => assert_eq!(name, "__buf_append_sep_unless_first"),
            _ => panic!("expected Ident callee for inner intrinsic"),
        },
        _ => panic!("expected inner __buf_append_sep_unless_first FnCall"),
    }
    // Second arg of outer = original `n` (the prepend's element).
    match &outer[1].node {
        Expr::Ident(name) => assert_eq!(name, "n"),
        _ => panic!("expected `n` ident as elem"),
    }
    // Last tail-call arg = __sep ident.
    match &tail_data.args[2].node {
        Expr::Ident(name) => assert_eq!(name, "__sep"),
        _ => panic!("expected __sep ident as last arg"),
    }
}

#[test]
fn detects_acc_param_at_arbitrary_index() {
    // Builder where the List<T> param is first and the tail-call
    // body wires the prepend at the same index. Detection has to
    // pin the acc position to where the prepend actually lands —
    // an earlier loose `any` check would silently pass even on
    // mismatched param/arg orderings, then synthesis would fail
    // to extract the element expression. Keep the body and the
    // params consistent so we exercise the real path.
    let true_body = call(dotted("List", "reverse"), vec![ident("acc")]);
    let prepend = call(dotted("List", "prepend"), vec![ident("col"), ident("acc")]);
    // Tail call: build(prepend(col, acc), col + 1)
    // — acc-position arg is at index 0, col+1 at index 1.
    let false_body = sp(Expr::TailCall(Box::new(TailCallData {
        target: "build".to_string(),
        args: vec![
            prepend,
            sp(Expr::BinOp(
                BinOp::Add,
                Box::new(ident("col")),
                Box::new(sp(Expr::Literal(Literal::Int(1)))),
            )),
        ],
    })));
    let match_expr = sp(Expr::Match {
        subject: Box::new(sp(Expr::BinOp(
            BinOp::Gte,
            Box::new(ident("col")),
            Box::new(sp(Expr::Literal(Literal::Int(10)))),
        ))),
        arms: vec![
            MatchArm {
                pattern: Pattern::Literal(Literal::Bool(true)),
                body: Box::new(true_body),
                binding_slots: std::sync::OnceLock::new(),
            },
            MatchArm {
                pattern: Pattern::Literal(Literal::Bool(false)),
                body: Box::new(false_body),
                binding_slots: std::sync::OnceLock::new(),
            },
        ],
    });
    let fd = FnDef {
        name: "build".to_string(),
        line: 1,
        params: vec![
            ("acc".to_string(), "List<Int>".to_string()),
            ("col".to_string(), "Int".to_string()),
        ],
        return_type: "List<Int>".to_string(),
        effects: vec![],
        desc: None,
        body: Arc::new(FnBody::Block(vec![Stmt::Expr(match_expr)])),
        resolution: None,
    };
    let info = compute_buffer_build_sinks(&[&fd]);
    let shape = info.get("build").expect("expected match");
    assert_eq!(shape.acc_param_idx, 0);
    assert_eq!(shape.acc_param_name, "acc");
}

#[test]
fn rejects_loose_prepend_in_non_acc_position() {
    // Earlier the detector accepted a fn whose tail call had a
    // prepend in *some* arg, regardless of position. That let
    // detection promise a sink the synthesizer couldn't actually
    // build. Make sure the tightened predicate refuses this.
    let mut fd = canonical_builder("build");
    // Reorder tail-call args so prepend ends up at index 0 instead
    // of index 1 — but keep params [(col, Int), (acc, List<Int>)],
    // so acc-position is index 1, where there's now a `col + 1`
    // expression (no prepend). Detection should refuse.
    {
        let body = std::sync::Arc::make_mut(&mut fd.body);
        let FnBody::Block(stmts) = body;
        if let Stmt::Expr(spanned) = &mut stmts[0]
            && let Expr::Match { arms, .. } = &mut spanned.node
        {
            for arm in arms.iter_mut() {
                if matches!(arm.pattern, Pattern::Literal(Literal::Bool(false)))
                    && let Expr::TailCall(data) = &mut arm.body.node
                {
                    data.args.reverse();
                }
            }
        }
    }
    let info = compute_buffer_build_sinks(&[&fd]);
    assert!(
        !info.contains_key("build"),
        "loose-prepend (prepend not at acc-position) must not be detected"
    );
}

#[test]
fn skips_synth_when_no_rewriteable_call_site() {
    // A fn that matches the sink shape but whose only call site
    // doesn't fit the canonical fusion pattern (e.g. starts with a
    // non-empty initial accumulator, or the wrapper is an unrelated
    // function call rather than `String.join`) should NOT get a
    // synthesized `__buffered` variant. Generating one is bloat
    // and risks shadowing user fns.
    let sink = canonical_builder("build");
    // Dummy caller that uses `build` but not via `String.join(...)`.
    let caller = FnDef {
        name: "use_build".to_string(),
        line: 2,
        params: vec![],
        return_type: "List<Int>".to_string(),
        effects: vec![],
        desc: None,
        body: Arc::new(FnBody::Block(vec![Stmt::Expr(call(
            ident_expr("build"),
            vec![sp(Expr::Literal(Literal::Int(0))), sp(Expr::List(vec![]))],
        ))])),
        resolution: None,
    };
    let mut items = vec![
        crate::ast::TopLevel::FnDef(sink),
        crate::ast::TopLevel::FnDef(caller),
    ];
    let initial_count = items.len();
    let report = run_buffer_build_pass(&mut items);
    assert_eq!(report.rewrites, 0, "no fusion sites — no rewriteable call");
    assert_eq!(
        report.synthesized.len(),
        0,
        "no synth — nothing to fuse against"
    );
    assert_eq!(items.len(), initial_count, "no buffered variant appended");
}

#[test]
fn external_reverse_pattern_round_trips() {
    // `match list { [] -> acc; [h, ..t] -> recurse(t, prepend(_, acc)) }`
    // sink + `String.join(List.reverse(<sink>(args, [])), sep)` call
    // site should detect, synth, and rewrite as a single fusion.
    let nil_body = ident("acc");
    let prepend = call(dotted("List", "prepend"), vec![ident("h"), ident("acc")]);
    let cons_body = sp(Expr::TailCall(Box::new(TailCallData {
        target: "build".to_string(),
        args: vec![ident("t"), prepend],
    })));
    let match_expr = sp(Expr::Match {
        subject: Box::new(ident("xs")),
        arms: vec![
            MatchArm {
                pattern: Pattern::EmptyList,
                body: Box::new(nil_body),
                binding_slots: std::sync::OnceLock::new(),
            },
            MatchArm {
                pattern: Pattern::Cons("h".to_string(), "t".to_string()),
                body: Box::new(cons_body),
                binding_slots: std::sync::OnceLock::new(),
            },
        ],
    });
    let sink = FnDef {
        name: "build".to_string(),
        line: 1,
        params: vec![
            ("xs".to_string(), "List<Int>".to_string()),
            ("acc".to_string(), "List<String>".to_string()),
        ],
        return_type: "List<String>".to_string(),
        effects: vec![],
        desc: None,
        body: Arc::new(FnBody::Block(vec![Stmt::Expr(match_expr)])),
        resolution: None,
    };
    let info = compute_buffer_build_sinks(&[&sink]);
    let shape = info
        .get("build")
        .expect("external-reverse sink should be detected");
    assert_eq!(shape.kind, BufferBuildKind::ExternalReverse);
    assert_eq!(shape.acc_param_idx, 1);

    // Caller: `String.join(List.reverse(build(xs, [])), "\n")`
    let join_call = call(
        dotted("String", "join"),
        vec![
            call(
                dotted("List", "reverse"),
                vec![call(
                    ident_expr("build"),
                    vec![ident("xs"), sp(Expr::List(vec![]))],
                )],
            ),
            sp(Expr::Literal(Literal::Str("\n".to_string()))),
        ],
    );
    let caller = FnDef {
        name: "render".to_string(),
        line: 2,
        params: vec![("xs".to_string(), "List<Int>".to_string())],
        return_type: "String".to_string(),
        effects: vec![],
        desc: None,
        body: Arc::new(FnBody::Block(vec![Stmt::Expr(join_call)])),
        resolution: None,
    };

    let mut items = vec![
        crate::ast::TopLevel::FnDef(sink),
        crate::ast::TopLevel::FnDef(caller),
    ];
    let report = run_buffer_build_pass(&mut items);
    assert_eq!(
        report.rewrites, 1,
        "external-reverse pattern should be one fusion site"
    );
    assert_eq!(
        report.synthesized.len(),
        1,
        "exactly one buffered variant for the used sink"
    );

    // The synthesized variant should be appended.
    let synth_present = items.iter().any(|it| match it {
        crate::ast::TopLevel::FnDef(fd) => fd.name == "build__buffered",
        _ => false,
    });
    assert!(synth_present, "build__buffered must be appended");
}

/// Parse + TCO a snippet the way the real pipeline does, so the
/// list-driven tests below match on the same AST the compiler sees.
fn parse_and_tco(src: &str) -> Vec<crate::ast::TopLevel> {
    let mut lexer = crate::lexer::Lexer::new(src);
    let tokens = lexer.tokenize().expect("lex");
    let mut parser = crate::parser::Parser::new(tokens);
    let mut items = parser.parse().expect("parse");
    crate::ir::pipeline::tco(&mut items);
    items
}

fn sinks_of(items: &[crate::ast::TopLevel]) -> HashMap<String, BufferBuildShape> {
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    compute_buffer_build_sinks(&fns)
}

fn fn_named<'a>(items: &'a [crate::ast::TopLevel], name: &str) -> &'a FnDef {
    items
        .iter()
        .find_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) if fd.name == name => Some(fd),
            _ => None,
        })
        .unwrap_or_else(|| panic!("fn {name} not found"))
}

/// The fourth quadrant: list-driven loop with the reverse in the
/// base case, consumed by a plain `String.join(<sink>(xs, []), sep)`.
/// This is Aver's own `Bytes.hexParts` / `Bytes.toHex` pair copied
/// verbatim except for the module wrapper — before the recogniser
/// learned this shape, the standard library's own `toHex` missed the
/// pass Aver ships.
#[test]
fn list_driven_internal_reverse_fuses_the_hex_parts_shape() {
    let mut items = parse_and_tco(
        r#"
fn byteToHex(value: Int) -> String
    String.fromInt(value)

fn hexParts(values: List<Int>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> hexParts(tail, List.prepend(byteToHex(head), acc))

fn toHex(values: List<Int>) -> String
    String.join(hexParts(values, []), "")
"#,
    );
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    let sinks = compute_buffer_build_sinks(&fns);
    let shape = sinks
        .get("hexParts")
        .expect("list-driven internal-reverse sink should be detected");
    assert_eq!(shape.kind, BufferBuildKind::ListInternalReverse);
    assert_eq!(shape.acc_param_idx, 1);
    assert_eq!(shape.acc_param_name, "acc");
    drop(fns);

    let report = run_buffer_build_pass(&mut items);
    assert_eq!(report.rewrites, 1, "toHex should be one fusion site");
    assert_eq!(report.synthesized, vec!["hexParts__buffered".to_string()]);

    // The call site now allocates a buffer, runs the buffered
    // variant, and finalizes — no intermediate List<String>.
    let to_hex = fn_named(&items, "toHex");
    let body = match &to_hex.body.stmts()[0] {
        Stmt::Expr(s) => &s.node,
        other => panic!("expected expression body, got {other:?}"),
    };
    let finalize_args = match body {
        Expr::FnCall(callee, args) => {
            assert!(
                matches!(&callee.node, Expr::Ident(n) if n == "__buf_finalize"),
                "expected __buf_finalize wrapper, got {:?}",
                callee.node
            );
            args
        }
        other => panic!("expected __buf_finalize call, got {other:?}"),
    };
    match &finalize_args[0].node {
        Expr::FnCall(callee, args) => {
            assert!(
                matches!(&callee.node, Expr::Ident(n) if n == "hexParts__buffered"),
                "expected buffered variant call, got {:?}",
                callee.node
            );
            // `values`, the fresh buffer, and the separator — the
            // accumulator argument is gone.
            assert_eq!(args.len(), 3);
        }
        other => panic!("expected hexParts__buffered call, got {other:?}"),
    }

    // The buffered variant keeps the list-driven arms and returns
    // the buffer straight out of the `[]` case.
    let buffered = fn_named(&items, "hexParts__buffered");
    assert_eq!(buffered.return_type, "Buffer");
    let arms = match &buffered.body.stmts()[0] {
        Stmt::Expr(s) => match &s.node {
            Expr::Match { arms, .. } => arms,
            other => panic!("expected match body, got {other:?}"),
        },
        other => panic!("expected expression body, got {other:?}"),
    };
    let nil_arm = arms
        .iter()
        .find(|a| matches!(a.pattern, Pattern::EmptyList))
        .expect("nil arm");
    assert!(
        matches!(&nil_arm.body.node, Expr::Ident(n) if n == "__buf"),
        "nil arm must return the buffer, got {:?}",
        nil_arm.body.node
    );
    let cons_arm = arms
        .iter()
        .find(|a| matches!(a.pattern, Pattern::Cons(_, _)))
        .expect("cons arm");
    assert!(
        matches!(&cons_arm.body.node, Expr::TailCall(d) if d.target == "hexParts__buffered"),
        "cons arm must tail-call the buffered variant, got {:?}",
        cons_arm.body.node
    );
}

/// Conservatism: the same sink consumed through a call site that
/// ALSO reverses would come out backwards after the rewrite (the
/// sink's own reverse is what the buffer replaces). The call site
/// must be left alone.
#[test]
fn list_driven_internal_reverse_refuses_a_reversing_call_site() {
    let mut items = parse_and_tco(
        r#"
fn hexParts(values: List<Int>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> hexParts(tail, List.prepend(String.fromInt(head), acc))

fn toHex(values: List<Int>) -> String
    String.join(List.reverse(hexParts(values, [])), "")
"#,
    );
    let report = run_buffer_build_pass(&mut items);
    assert_eq!(
        report.rewrites, 0,
        "a doubly-reversing call site must not be fused"
    );
    assert!(report.synthesized.is_empty());
}

/// Conservatism: a list-driven loop whose base case reverses the
/// INPUT rather than the accumulator is a different function. It
/// must stay unrecognised (and therefore unfused) rather than be
/// normalised into the shape we know.
#[test]
fn list_driven_base_arm_reversing_another_binding_is_not_a_sink() {
    let items = parse_and_tco(
        r#"
fn build(values: List<Int>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(values)
        [head, ..tail] -> build(tail, List.prepend(String.fromInt(head), acc))
"#,
    );
    let fns: Vec<&FnDef> = items
        .iter()
        .filter_map(|it| match it {
            crate::ast::TopLevel::FnDef(fd) => Some(fd),
            _ => None,
        })
        .collect();
    assert!(
        compute_buffer_build_sinks(&fns).is_empty(),
        "reversing a binding other than the accumulator must not match"
    );
}

fn ident_expr(name: &str) -> Spanned<Expr> {
    sp(Expr::Ident(name.to_string()))
}

/// The buffered variant has no accumulator parameter, so every
/// mention of the accumulator the rewrite keeps would be a free
/// name. A loop that asks what it has collected so far — here
/// `List.contains(acc, head)`, marking repeated values — is exactly
/// that: the element expression is copied verbatim into the buffer
/// append. The recogniser has to decline before the rewrite gets a
/// chance to drop the parameter out from under it.
#[test]
fn list_driven_internal_reverse_declines_when_the_element_reads_the_accumulator() {
    let items = parse_and_tco(
        r#"
fn tag(value: String, seen: Bool) -> String
    match seen
        true -> "dup"
        false -> value

fn parts(values: List<String>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> parts(tail, List.prepend(tag(head, List.contains(acc, head)), acc))
"#,
    );
    assert!(
        sinks_of(&items).is_empty(),
        "an element expression that reads the accumulator must not be fused"
    );
}

/// Same hole, external-reverse quadrant: the base arm hands back a
/// bare `acc` and the caller reverses. Shipped before this pass
/// learned the fourth quadrant, so the guard has to cover it too.
#[test]
fn external_reverse_declines_when_the_element_reads_the_accumulator() {
    let items = parse_and_tco(
        r#"
fn tag(value: String, seen: Bool) -> String
    match seen
        true -> "dup"
        false -> value

fn markInto(values: List<String>, acc: List<String>) -> List<String>
    match values
        [] -> acc
        [head, ..tail] -> markInto(tail, List.prepend(tag(head, List.contains(acc, head)), acc))
"#,
    );
    assert!(
        sinks_of(&items).is_empty(),
        "the external-reverse quadrant must decline the same escape"
    );
}

/// Same hole, Bool-driven quadrant: the loop numbers each element by
/// asking how many the accumulator already holds.
#[test]
fn bool_driven_internal_reverse_declines_when_the_element_reads_the_accumulator() {
    let items = parse_and_tco(
        r#"
fn countdown(n: Int, acc: List<String>) -> List<String>
    match n <= 0
        true -> List.reverse(acc)
        false -> countdown(n - 1, List.prepend(String.fromInt(List.len(acc)), acc))
"#,
    );
    assert!(
        sinks_of(&items).is_empty(),
        "the Bool-driven quadrant must decline the same escape"
    );
}

/// The accumulator can also escape through a tail-call argument that
/// is not the accumulator's own: those arguments are copied into the
/// buffered variant's call verbatim.
#[test]
fn declines_when_a_sibling_tail_call_argument_reads_the_accumulator() {
    let items = parse_and_tco(
        r#"
fn build(seen: Int, values: List<String>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> build(List.len(acc), tail, List.prepend(head, acc))
"#,
    );
    assert!(
        sinks_of(&items).is_empty(),
        "a sibling tail-call argument reading the accumulator must not be fused"
    );
}

/// And through the match subject, which the rewrite clones as-is
/// onto the buffered variant's own match.
#[test]
fn declines_when_the_match_subject_reads_the_accumulator() {
    let items = parse_and_tco(
        r#"
fn build(word: String, acc: List<String>) -> List<String>
    match List.len(acc) >= 3
        true -> List.reverse(acc)
        false -> build(word, List.prepend(word, acc))
"#,
    );
    assert!(
        sinks_of(&items).is_empty(),
        "a match subject reading the accumulator must not be fused"
    );
}

/// Counting references is not enough on its own: the one permitted
/// read has to be the accumulator. A cons pattern that binds the
/// head under the accumulator's own name makes the prepend thread
/// that head instead, which is a different loop with a different
/// answer — and the rewrite, which keeps the pattern and drops the
/// parameter, would not preserve it.
#[test]
fn declines_when_the_cons_pattern_shadows_the_accumulator() {
    let items = parse_and_tco(
        r#"
fn f(values: List<List<String>>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [acc, ..tail] -> f(tail, List.prepend("x", acc))
"#,
    );
    assert!(
        sinks_of(&items).is_empty(),
        "a pattern binding that shadows the accumulator must not be fused"
    );
}

/// The guard counts references, so a loop that mentions the
/// accumulator only where the rewrite replaces it keeps fusing —
/// including one whose element expression is itself a call taking
/// several arguments.
#[test]
fn a_single_prepend_tail_reference_still_fuses() {
    let items = parse_and_tco(
        r#"
fn label(value: Int, width: Int) -> String
    String.fromInt(value + width)

fn rows(values: List<Int>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> rows(tail, List.prepend(label(head, 2), acc))
"#,
    );
    let sinks = sinks_of(&items);
    let shape = sinks
        .get("rows")
        .expect("the accumulator is only read as the prepend tail, so this still fuses");
    assert_eq!(shape.kind, BufferBuildKind::ListInternalReverse);
}

// ── list build ──────────────────────────────────────────────────

/// Run the pass the way the pipeline does and hand back what it
/// produced along with the items it produced them in.
fn list_build(src: &str) -> (Vec<crate::ast::TopLevel>, ListBuildPassReport) {
    let mut items = parse_and_tco(src);
    let report = run_list_build_pass(&mut items);
    (items, report)
}

/// Render a fn back out so a test can assert on the shape the pass
/// produced rather than on a tree walk of its own. The IR dump
/// rather than the unparser, because a tail call is what the pass
/// rewrites and only the dump prints one.
fn body_source(items: &[crate::ast::TopLevel], name: &str) -> String {
    let one = vec![crate::ast::TopLevel::FnDef(fn_named(items, name).clone())];
    crate::ir::dump::dump_items(&one, None)
}

/// The canonical collecting loop: prepend into the accumulator,
/// reverse in the base case, hand the list back.
#[test]
fn a_collecting_loop_appends_instead_of_prepending() {
    let (items, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    collect(5, [])
"#,
    );
    assert_eq!(report.rewrites, 1);
    assert_eq!(report.synthesized, vec!["collect__collected".to_string()]);
    let body = body_source(&items, "collect__collected");
    assert!(
        body.contains("__lst_push(acc, n)"),
        "the prepend becomes an append: {body}"
    );
    assert!(
        body.contains("__lst_finalize(acc)") && !body.contains("List.reverse"),
        "the reverse is what the builder makes unnecessary: {body}"
    );
    let caller = body_source(&items, "main");
    assert!(
        caller.contains("collect__collected") && caller.contains("__lst_new"),
        "the call site starts a builder instead of an empty list: {caller}"
    );
}

/// The order test the whole rewrite rests on. `prepend` then
/// `reverse` yields traversal order, so pushes in loop order have to
/// reproduce it — asserted as the sequence of element expressions
/// the rewritten loop appends, in the order it appends them, against
/// the sequence the original prepends.
///
/// The argument is easy to get backwards, which is exactly why it is
/// pinned here and again as a running program in
/// `rust_codegen_differential`.
#[test]
fn the_builder_appends_in_the_order_the_loop_prepends() {
    let (items, _) = list_build(
        r#"
fn walk(values: List<Int>, acc: List<Int>) -> List<Int>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> walk(tail, List.prepend(head + 1, acc))

fn main() -> List<Int>
    walk([1, 2, 3], [])
"#,
    );
    let original = body_source(&items, "walk");
    let collected = body_source(&items, "walk__collected");
    // Same element expression, same position in the same arm: the
    // rewrite moved WHERE the element goes, never WHEN it is
    // computed. That is what makes "pushes in loop order" the same
    // sequence as "prepends in loop order".
    assert!(
        original.contains("List.prepend(head + 1, acc)"),
        "the loop this is about: {original}"
    );
    assert!(
        collected.contains("__lst_push(acc, head + 1)"),
        "the same element, appended where it was prepended: {collected}"
    );
    assert_eq!(
        collected.matches("__lst_push").count(),
        1,
        "one append per step, not two: {collected}"
    );
}

/// The parser shape: the recursion is four matches deep, the exits
/// that do not return the accumulator do not mention it at all, and
/// the one that does is wrapped in a constructor. This is
/// `Bytes.parseHexChars` with the hexadecimal taken out.
#[test]
fn a_nested_parser_loop_with_error_exits_still_collects() {
    let (items, report) = list_build(
        r#"
fn parse(chars: List<String>, acc: List<Int>) -> Result<List<Int>, String>
    match chars
        [] -> Result.Ok(List.reverse(acc))
        [high, ..afterHigh] -> match afterHigh
            [] -> Result.Err("odd length")
            [low, ..rest] -> match String.len(high) == 1
                true -> parse(rest, List.prepend(String.len(low), acc))
                false -> Result.Err("bad digit '{high}'")

fn main() -> Result<List<Int>, String>
    parse(["a", "b"], [])
"#,
    );
    assert_eq!(report.declined, Default::default(), "{report:?}");
    assert_eq!(report.rewrites, 1);
    let body = body_source(&items, "parse__collected");
    assert!(
        body.contains("Result.Ok(__lst_finalize(acc))"),
        "the constructor around the exit is not in the way: {body}"
    );
    assert!(
        body.contains("Result.Err(\"odd length\")"),
        "an exit that never mentions the accumulator is left alone: {body}"
    );
}

/// The other spelling: no reverse in the loop, the caller reverses.
/// Only a call site wearing that reverse may be moved — the builder
/// produces the forward list, so a bare call asked for the elements
/// backwards and has to keep getting them.
#[test]
fn a_caller_reversed_loop_moves_only_the_reversing_call_site() {
    let (items, report) = list_build(
        r#"
fn markInto(values: List<Int>, acc: List<Int>) -> List<Int>
    match values
        [] -> acc
        [head, ..tail] -> markInto(tail, List.prepend(head, acc))

fn forwards(values: List<Int>) -> List<Int>
    List.reverse(markInto(values, []))

fn backwards(values: List<Int>) -> List<Int>
    markInto(values, [])
"#,
    );
    assert_eq!(
        report.rewrites, 1,
        "only the reversing call site: {report:?}"
    );
    let forwards = body_source(&items, "forwards");
    assert!(
        forwards.contains("markInto__collected") && !forwards.contains("List.reverse"),
        "the caller's reverse is what the builder replaces: {forwards}"
    );
    let backwards = body_source(&items, "backwards");
    assert!(
        backwards.contains("markInto(values, [])"),
        "a caller that wanted the elements backwards still gets them: {backwards}"
    );
}

/// A non-empty starting accumulator is elements the builder would
/// never hold, and the rewrite has nowhere to put them.
#[test]
fn a_call_site_that_starts_with_elements_is_left_alone() {
    let (items, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    collect(5, [99])
"#,
    );
    assert_eq!(report.rewrites, 0);
    assert!(
        report.synthesized.is_empty(),
        "a variant nobody calls is not left in the program: {report:?}"
    );
    assert!(
        body_source(&items, "main").contains("collect(5, [99])"),
        "the call keeps the loop it named"
    );
}

/// A loop that measures what it has collected is a different loop.
#[test]
fn a_loop_that_reads_the_accumulator_twice_declines() {
    let (_, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(List.len(acc), acc))

fn main() -> List<Int>
    collect(5, [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::AccEscapes.reason())
    );
    assert_eq!(report.rewrites, 0);
}

/// The subject is evaluated before whichever arm runs, so a read
/// there and a read in an arm are two reads on one path.
#[test]
fn a_loop_that_matches_on_its_accumulator_declines() {
    let (_, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match List.len(acc) >= 3
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    collect(5, [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::AccEscapes.reason())
    );
}

/// One exit reverses and another does not. One call-site spelling
/// cannot be right for both, and picking a winner by source order is
/// not something this rewrite can say it does.
#[test]
fn a_loop_whose_exits_disagree_about_the_reverse_declines() {
    let (_, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> match n == 7
            true -> acc
            false -> collect(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    collect(5, [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::MixedFinish.reason())
    );
}

/// A pattern that re-binds the accumulator's name makes every read
/// underneath it a different value. The occurs-check counts names,
/// and here the name lies — the same trap the joined builders above
/// guard against.
#[test]
fn a_pattern_that_shadows_the_accumulator_declines() {
    let (_, report) = list_build(
        r#"
fn collect(values: List<List<Int>>, acc: List<Int>) -> List<Int>
    match values
        [] -> List.reverse(acc)
        [acc, ..tail] -> collect(tail, List.prepend(1, acc))

fn main() -> List<Int>
    collect([[1]], [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::AccShadowed.reason())
    );
}

/// A second self-call that restarts the fold with a fresh list. Only
/// the recognised one would be moved, and the other would call the
/// list spelling from inside a variant holding a builder.
#[test]
fn a_self_call_that_does_not_append_declines() {
    let (_, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> match n == 3
            true -> collect(n - 1, [])
            false -> collect(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    collect(5, [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::SelfCallShape.reason())
    );
}

/// A binder named `__lst_push` shadows the intrinsic for everything
/// underneath it, and a leading `__` is not reserved — the program
/// below compiles and runs. A binder is not a read, so no traversal
/// that looks at identifiers can see one; the guard asks the same
/// collector chars fusion asks.
#[test]
fn a_program_that_binds_a_builder_name_declines() {
    let (_, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> List<Int>
    __lst_push = 3
    collect(__lst_push, [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::NameTaken.reason())
    );
}

/// So does a fn already called `<loop>__collected`.
#[test]
fn a_taken_variant_name_declines() {
    let (_, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn collect__collected(n: Int, acc: List<Int>) -> List<Int>
    acc

fn main() -> List<Int>
    collect(5, [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::NameTaken.reason())
    );
}

/// A loop whose accumulator is never handed back builds nothing.
#[test]
fn a_loop_that_never_returns_its_accumulator_declines() {
    let (_, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> Int
    match n <= 0
        true -> 0
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> Int
    collect(5, [])
"#,
    );
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::NoFinish.reason())
    );
}

/// A loop that binds something before its match keeps the binding.
/// The variant is the fn it was built from with one statement
/// rewritten, so dropping the earlier ones would leave the names
/// they bind free — a compile error, or a silent read of a
/// top-level binding that happens to share one.
#[test]
fn a_loop_with_a_binding_before_its_match_keeps_it() {
    let (items, report) = list_build(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    step = 3
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n * step, acc))

fn main() -> List<Int>
    collect(5, [])
"#,
    );
    assert_eq!(report.rewrites, 1);
    let body = body_source(&items, "collect__collected");
    assert!(
        body.contains("step = 3"),
        "the binding the loop set up must travel with it: {body}"
    );
    assert!(
        body.contains("__lst_push(acc, n * step)"),
        "and the element that reads it: {body}"
    );
}

/// A joined builder is fused by the pass above, which leaves no
/// `<sink>(…, [])` call site behind — so the two passes cannot both
/// claim the same loop, and the one that removes the list entirely
/// wins.
#[test]
fn a_joined_builder_is_left_to_the_buffer_pass() {
    let mut items = parse_and_tco(
        r#"
fn parts(values: List<Int>, acc: List<String>) -> List<String>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> parts(tail, List.prepend(String.fromInt(head), acc))

fn render(values: List<Int>) -> String
    String.join(parts(values, []), "")
"#,
    );
    let buffered = run_buffer_build_pass(&mut items);
    assert_eq!(buffered.rewrites, 1, "the joined shape is the other pass's");
    let collected = run_list_build_pass(&mut items);
    assert_eq!(
        collected.rewrites, 0,
        "no `parts(values, [])` call site is left to move: {collected:?}"
    );
}

// === The byte sink ====================================================

/// The standard library's `fromList` family, copied word for word. The
/// byte-sink retarget verifies its consumer STRUCTURALLY against the
/// embedded module — spanned equality ignores lines — so an exact copy
/// is the way a self-contained test earns the retarget, and a copy
/// that changes a word is the way it earns the decline.
const FROM_LIST_COPY: &str = r#"
record Bytes
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    ? "Return true when every integer in the list is an octet."
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn firstOutOfRange(xs: List<Int>) -> Int
    ? "Return the first non-octet value; -1 when every value is an octet."
    match xs
        [] -> -1
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> firstOutOfRange(tail)
            false -> head

fn firstOutOfRangeIndex(xs: List<Int>) -> Int
    ? "Return the index of the first non-octet value; the length when every value is an octet."
    match xs
        [] -> 0
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> 1 + firstOutOfRangeIndex(tail)
            false -> 0

fn fromList(xs: List<Int>) -> Result<Bytes, String>
    ? "Validate raw integers and construct a byte sequence."
    match allInRange(xs)
        true -> Result.Ok(Bytes(values = xs))
        false -> Result.Err("byte {firstOutOfRange(xs)} at index {firstOutOfRangeIndex(xs)} is outside 0..=255")
"#;

fn list_build_with_copy(src: &str) -> (Vec<crate::ast::TopLevel>, ListBuildPassReport) {
    let source = format!("{FROM_LIST_COPY}\n{src}");
    let mut items = parse_and_tco(&source);
    let report = run_list_build_pass(&mut items);
    (items, report)
}

/// The direct spelling: a bare-list loop applied straight to
/// `fromList`. The builder becomes the byte builder, the exits answer
/// what the pair answered, and the `fromList` call is gone.
#[test]
fn a_collected_list_consumed_only_by_from_list_collects_bytes() {
    let (items, report) = list_build_with_copy(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> Result<Bytes, String>
    fromList(collect(5, []))
"#,
    );
    assert_eq!(report.byte_retargets, 1, "{report:?}");
    assert_eq!(report.byte_fns, vec!["collect__collected".to_string()]);
    assert_eq!(report.byte_declined, Default::default(), "{report:?}");
    let body = body_source(&items, "collect__collected");
    assert!(
        body.contains("__byt_push(acc, n)")
            && body.contains("__byt_finalize(acc)")
            && body.contains("Bytes(values = __byt_vals)"),
        "the variant pushes bytes and wraps the record itself: {body}"
    );
    assert!(
        !body.contains("__lst_"),
        "no list builder is left in the variant: {body}"
    );
    let main = body_source(&items, "main");
    assert!(
        main.contains("collect__collected(5, __byt_new(0))") && !main.contains("fromList"),
        "the caller starts the byte builder and the second walk is gone: {main}"
    );
}

/// The binding spelling — the standard library's own `fromHex` shape:
/// the loop answers a `Result`, the consumer unwraps it with `?` and
/// hands the list to `fromList` as its answer. Both statements fuse
/// into the one retargeted call.
#[test]
fn a_bound_collected_result_consumed_only_by_from_list_collects_bytes() {
    let (items, report) = list_build_with_copy(
        r#"
fn tripled(values: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    match values
        [] -> Result.Ok(List.reverse(acc))
        [head, ..tail] -> match head == 0
            true -> Result.Err("zero is not a sample")
            false -> tripled(tail, List.prepend(head * 3, acc))

fn toBytes(values: List<Int>) -> Result<Bytes, String>
    collected = tripled(values, [])?
    fromList(collected)
"#,
    );
    assert_eq!(report.byte_retargets, 1, "{report:?}");
    assert_eq!(report.byte_fns, vec!["tripled__collected".to_string()]);
    let variant = body_source(&items, "tripled__collected");
    assert!(
        variant.contains("__byt_push(acc, head * 3)")
            && variant.contains("Result.Err(\"zero is not a sample\")"),
        "the pushes retarget and the parse error keeps its exit: {variant}"
    );
    let consumer = body_source(&items, "toBytes");
    assert!(
        consumer.contains("tripled__collected(values, __byt_new(0))")
            && !consumer.contains("fromList")
            && !consumer.contains("collected ="),
        "the binding and the fromList call fuse into the one answer: {consumer}"
    );
}

/// The collected result is read once more on the way to `fromList` —
/// the consumer-side occurs-check of the family.
#[test]
fn a_collected_result_read_twice_declines_the_byte_retarget() {
    let (items, report) = list_build_with_copy(
        r#"
fn tripled(values: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    match values
        [] -> Result.Ok(List.reverse(acc))
        [head, ..tail] -> tripled(tail, List.prepend(head * 3, acc))

fn toBytes(values: List<Int>) -> Result<Bytes, String>
    collected = tripled(values, [])?
    total = List.len(collected)
    fromList(collected)
"#,
    );
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("tripled__collected").copied(),
        Some(ByteSinkDecline::NotTheAnswer.reason()),
        "{report:?}"
    );
    let variant = body_source(&items, "tripled__collected");
    assert!(
        variant.contains("__lst_push") && !variant.contains("__byt_"),
        "the variant keeps its list: {variant}"
    );
}

/// A binder arm re-uses the collected result's name, so the reads the
/// occurs-check counts are not all the same value.
#[test]
fn a_pattern_that_shadows_the_collected_result_declines_the_byte_retarget() {
    let (_, report) = list_build_with_copy(
        r#"
fn tripled(values: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    match values
        [] -> Result.Ok(List.reverse(acc))
        [head, ..tail] -> tripled(tail, List.prepend(head * 3, acc))

fn toBytes(values: List<Int>, n: Int) -> Result<Bytes, String>
    probe = match n
        collected -> collected
    collected = tripled(values, [])?
    fromList(collected)
"#,
    );
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("tripled__collected").copied(),
        Some(ByteSinkDecline::SecondReader.reason()),
        "{report:?}"
    );
}

/// `fromList` fed through a `?` somewhere other than the consumer's
/// answer: the `?` returns parse errors from the fn early, and fused
/// they would become the call's value — so the position is the shape.
#[test]
fn a_from_list_away_from_the_answer_declines_the_byte_retarget() {
    let (_, report) = list_build_with_copy(
        r#"
fn tripled(values: List<Int>, acc: List<Int>) -> Result<List<Int>, String>
    match values
        [] -> Result.Ok(List.reverse(acc))
        [head, ..tail] -> tripled(tail, List.prepend(head * 3, acc))

fn toBytes(values: List<Int>) -> Result<Int, String>
    packed = fromList(tripled(values, [])?)
    match packed
        Result.Ok(bytes) -> Result.Ok(List.len(bytes.values))
        Result.Err(message) -> Result.Err(message)
"#,
    );
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("tripled__collected").copied(),
        Some(ByteSinkDecline::NotTheAnswer.reason()),
        "{report:?}"
    );
}

/// The consumer is a `fromList` in name only — one word of the message
/// differs from the standard library's. The retarget bakes in the
/// library's semantics, so anything else keeps its second walk.
#[test]
fn a_from_list_that_is_not_the_stdlib_one_declines_the_byte_retarget() {
    let source = r#"
record Bytes
    values: List<Int>

fn allInRange(xs: List<Int>) -> Bool
    match xs
        [] -> true
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> allInRange(tail)
            false -> false

fn firstOutOfRange(xs: List<Int>) -> Int
    match xs
        [] -> -1
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> firstOutOfRange(tail)
            false -> head

fn firstOutOfRangeIndex(xs: List<Int>) -> Int
    match xs
        [] -> 0
        [head, ..tail] -> match Bool.and(head >= 0, head <= 255)
            true -> 1 + firstOutOfRangeIndex(tail)
            false -> 0

fn fromList(xs: List<Int>) -> Result<Bytes, String>
    match allInRange(xs)
        true -> Result.Ok(Bytes(values = xs))
        false -> Result.Err("value {firstOutOfRange(xs)} at index {firstOutOfRangeIndex(xs)} is outside 0..=255")

fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> Result<Bytes, String>
    fromList(collect(5, []))
"#;
    let mut items = parse_and_tco(source);
    let report = run_list_build_pass(&mut items);
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("collect__collected").copied(),
        Some(ByteSinkDecline::ConsumerShape.reason()),
        "{report:?}"
    );
}

/// The consumer binds `fromList` itself, so the call underneath reads
/// the binder, not the verified module fn.
#[test]
fn a_shadowed_from_list_declines_the_byte_retarget() {
    let (_, report) = list_build_with_copy(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> Result<Bytes, String>
    fromList = 3
    fromList(collect(5, []))
"#,
    );
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("collect__collected").copied(),
        Some(ByteSinkDecline::ConsumerShape.reason()),
        "{report:?}"
    );
}

/// An exit the accumulator never reaches hands back a list of its own.
/// `fromList` would have validated those elements too, and the byte
/// builder never saw them — the kind decision declines.
#[test]
fn an_exit_the_accumulator_never_reaches_declines_the_byte_retarget() {
    let (items, report) = list_build_with_copy(
        r#"
fn collect(values: List<Int>, acc: List<Int>) -> List<Int>
    match values
        [] -> List.reverse(acc)
        [head, ..tail] -> match head == 0
            true -> [7, 999]
            false -> collect(tail, List.prepend(head, acc))

fn main() -> Result<Bytes, String>
    fromList(collect([1, 2], []))
"#,
    );
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("collect__collected").copied(),
        Some(ByteSinkDecline::ExitShape.reason()),
        "{report:?}"
    );
    let main = body_source(&items, "main");
    assert!(
        main.contains("fromList(collect__collected("),
        "the consumer keeps its validation walk: {main}"
    );
}

/// One caller feeds `fromList`, another wants the list itself — a
/// single variant cannot answer both.
#[test]
fn a_second_caller_that_wants_the_list_declines_the_byte_retarget() {
    let (_, report) = list_build_with_copy(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n, acc))

fn main() -> Result<Bytes, String>
    total = List.len(collect(3, []))
    fromList(collect(total, []))
"#,
    );
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("collect__collected").copied(),
        Some(ByteSinkDecline::MixedConsumers.reason()),
        "{report:?}"
    );
}

/// A loop that collects something other than `List<Int>` cannot be
/// collecting octets. Unreachable through a type-checked consumer;
/// pinned so the decision never rests on the typechecker having run.
#[test]
fn a_loop_that_does_not_collect_ints_declines_the_byte_retarget() {
    let (_, report) = list_build_with_copy(
        r#"
fn names(n: Int, acc: List<String>) -> List<String>
    match n <= 0
        true -> List.reverse(acc)
        false -> names(n - 1, List.prepend("x", acc))

fn main() -> Result<Bytes, String>
    fromList(names(3, []))
"#,
    );
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.byte_declined.get("names__collected").copied(),
        Some(ByteSinkDecline::ElemShape.reason()),
        "{report:?}"
    );
}

/// A program that binds into the `__byt_` namespace takes the whole
/// pass away, exactly as a `__lst_` binder always has: the retarget is
/// a stage of this pass and both namespaces are emitted by name.
#[test]
fn a_program_that_binds_a_byte_builder_name_declines_the_whole_pass() {
    let (_, report) = list_build_with_copy(
        r#"
fn collect(n: Int, acc: List<Int>) -> List<Int>
    __byt_probe = 3
    match n <= 0
        true -> List.reverse(acc)
        false -> collect(n - 1, List.prepend(n + __byt_probe, acc))

fn main() -> Result<Bytes, String>
    fromList(collect(5, []))
"#,
    );
    assert_eq!(report.rewrites, 0, "{report:?}");
    assert_eq!(report.byte_retargets, 0, "{report:?}");
    assert_eq!(
        report.declined.get("collect").copied(),
        Some(ListBuildDecline::NameTaken.reason()),
        "{report:?}"
    );
}
