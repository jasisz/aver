//! A trace equality from an empty cursor is a corollary of its stateful form.
//! Distinct quantified cursor fields are important for functional induction:
//! generalizing two literal zero arguments can identify otherwise independent
//! answer and resumption counters. The stronger obligation is ordinary Aver.
use super::*;

pub(in crate::yield_lowering) fn strengthen_laws(
    items: &mut Vec<TopLevel>,
    protocols: &[ProcessProtocol],
) -> Vec<TopLevel> {
    let mut occupied: std::collections::HashSet<_> = items
        .iter()
        .filter_map(|item| match item {
            TopLevel::Verify(block) => match &block.kind {
                VerifyKind::Law(law) => Some((block.fn_name.clone(), law.name.clone())),
                _ => None,
            },
            _ => None,
        })
        .collect();
    let mut generated = Vec::new();
    let mut output = Vec::new();
    for mut item in items.drain(..) {
        if let TopLevel::Verify(block) = &mut item
            && let Some(protocol) = protocols.iter().find(|p| {
                p.trace.is_some() && block.fn_name == format!("__{}SourceTrace", p.fn_name)
            })
            && let VerifyKind::Law(law) = &block.kind
            && law.because.is_empty()
            && standard_equality(
                law,
                &block.fn_name,
                &format!("__{}ProtocolTrace", protocol.fn_name),
            )
        {
            let source = block.fn_name.clone();
            let target = format!("__{}ProtocolTrace", protocol.fn_name);
            let mut auxiliary = block.clone();
            auxiliary.fn_name.push_str("From");
            let VerifyKind::Law(aux) = &mut auxiliary.kind else {
                unreachable!()
            };
            while !occupied.insert((auxiliary.fn_name.clone(), aux.name.clone())) {
                aux.name.push('_');
            }
            let types = [
                "Int".to_string(),
                format!("List<__{}TraceEvent>", build::capitalize(&protocol.fn_name)),
                "Int".into(),
            ];
            let samples = [
                Spanned::new(Expr::Literal(Literal::Int(0)), block.line),
                Spanned::new(Expr::List(vec![]), block.line),
                Spanned::new(Expr::Literal(Literal::Int(0)), block.line),
            ];
            let mut names = Vec::new();
            for (base, (ty, sample)) in [
                "__traceLawPosition",
                "__traceLawEvents",
                "__traceLawConsumed",
            ]
            .into_iter()
            .zip(types.into_iter().zip(samples.iter()))
            {
                let mut name = base.to_string();
                while aux.givens.iter().any(|g| g.name == name) {
                    name.push('_');
                }
                aux.givens.push(VerifyGiven {
                    name: name.clone(),
                    type_name: ty,
                    domain: VerifyGivenDomain::Explicit(vec![sample.clone()]),
                });
                names.push(name);
            }
            let variables: Vec<_> = names
                .iter()
                .map(|name| build::ident(name, block.line))
                .collect();
            for expression in [&mut aux.lhs, &mut aux.rhs] {
                from_cursor(expression, &source, &target, &variables);
            }
            for (left, right) in &mut auxiliary.cases {
                from_cursor(left, &source, &target, &samples);
                from_cursor(right, &source, &target, &samples);
            }
            for givens in &mut auxiliary.case_givens {
                givens.extend(names.iter().cloned().zip(samples.iter().cloned()));
            }
            let dependency = format!("{}.{}", auxiliary.fn_name, aux.name);
            let VerifyKind::Law(original) = &mut block.kind else {
                unreachable!()
            };
            original.using.get_or_insert_with(Vec::new).push(dependency);
            let auxiliary = TopLevel::Verify(auxiliary);
            generated.push(auxiliary.clone());
            output.push(auxiliary);
        }
        output.push(item);
    }
    *items = output;
    generated
}

fn standard_equality(law: &VerifyLaw, source: &str, target: &str) -> bool {
    let (left, right) = match &law.lhs.node {
        Expr::BinOp(BinOp::Eq, left, right)
            if matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) =>
        {
            (left.as_ref(), right.as_ref())
        }
        _ => (&law.lhs, &law.rhs),
    };
    let (Expr::FnCall(a, x), Expr::FnCall(b, y)) = (&left.node, &right.node) else {
        return false;
    };
    build::dotted_name(a).as_deref() == Some(source)
        && build::dotted_name(b).as_deref() == Some(target)
        && x.len() == y.len()
        && x.iter()
            .zip(y)
            .all(|(a, b)| matches!(a.node, Expr::Ident(_)) && a.node == b.node)
}

fn from_cursor(expr: &mut Spanned<Expr>, source: &str, target: &str, cursor: &[Spanned<Expr>]) {
    if let Expr::FnCall(callee, args) = &mut expr.node
        && let Some(name) = build::dotted_name(callee)
        && (name == source || name == target)
    {
        **callee = build::ident(&format!("{name}From"), callee.line);
        args.extend(cursor.iter().cloned());
        return;
    }
    crate::codegen::expr_walk::for_each_child_mut(expr, &mut |child| {
        from_cursor(child, source, target, cursor)
    });
}
