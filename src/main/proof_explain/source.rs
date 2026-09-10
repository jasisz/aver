//! Source snapshots for proof diagnostics. No generated expression is presented
//! as Aver: goals and assumptions always come from the checked source AST.

use aver::ast::{Expr, Literal, Spanned, TopLevel, VerifyBlock, VerifyKind, VerifyLaw};
use aver::codegen::CodegenContext;
use std::collections::BTreeMap;

#[derive(Clone)]
pub(super) struct Law {
    pub id: String,
    pub scope: Option<String>,
    pub file: String,
    pub line: usize,
    pub function: String,
    pub body: VerifyLaw,
    pub untranslate: aver::codegen::lean::untranslate::UntranslateCtx,
    pub emitted_module: String,
    pub dafny_file: String,
}

#[derive(Default)]
pub(crate) struct Catalog {
    pub(super) laws: BTreeMap<String, Law>,
    pub(super) entry_file: String,
}

impl Catalog {
    pub(crate) fn new(ctx: &CodegenContext, file: &str, root: &str) -> Self {
        let mut catalog = Self {
            entry_file: file.to_string(),
            ..Self::default()
        };
        for item in &ctx.items {
            if let TopLevel::Verify(block) = item {
                catalog.add(block, None, file);
            }
        }
        for module in &ctx.modules {
            let file = aver::source::resolve_module_source(&module.prefix, root)
                .ok()
                .flatten()
                .map(|source| source.path.to_string_lossy().into_owned())
                .unwrap_or_else(|| module.prefix.clone());
            for block in &module.verify_blocks {
                catalog.add(block, Some(&module.prefix), &file);
            }
        }
        for law in catalog.laws.values_mut() {
            law.dafny_file = law.scope.as_ref().map_or_else(
                || format!("{}.dfy", aver::codegen::common::entry_basename(ctx)),
                |scope| format!("{}.dfy", scope.replace('.', "/")),
            );
            law.emitted_module =
                aver::codegen::lean::citation_probe::module_name(ctx, law.scope.as_deref());
            law.untranslate = aver::codegen::lean::untranslate::context_for_law(
                ctx,
                law.scope.as_deref(),
                &law.body,
            );
        }
        catalog
    }

    pub(crate) fn accepts_residual_suggestion(&self, identity: &str) -> bool {
        self.laws
            .get(identity)
            .is_some_and(|law| law.body.because.is_empty())
    }

    pub(super) fn add(&mut self, block: &VerifyBlock, scope: Option<&str>, file: &str) {
        let VerifyKind::Law(body) = &block.kind else {
            return;
        };
        let local = format!("{}.{}", block.fn_name, body.name);
        let id = qualify(scope, &local);
        self.laws.insert(
            id.clone(),
            Law {
                id,
                scope: scope.map(str::to_string),
                file: file.to_string(),
                line: block.line,
                function: qualify(scope, &block.fn_name),
                body: *body.clone(),
                untranslate: Default::default(),
                emitted_module: scope.unwrap_or_default().to_string(),
                dafny_file: String::new(),
            },
        );
    }

    pub(super) fn claim(&self, identity: &str) -> Option<(&Law, Option<usize>)> {
        if let Some(law) = self.laws.get(identity) {
            return Some((law, None));
        }
        let (parent, step) = identity.rsplit_once('.')?;
        let law = self.laws.get(parent)?;
        if step == "implication" {
            return Some((law, Some(law.body.because.len())));
        }
        let index = step
            .strip_prefix("because")?
            .parse::<usize>()
            .ok()?
            .checked_sub(1)?;
        (index < law.body.because.len()).then_some((law, Some(index)))
    }

    pub(super) fn citation(&self, owner: &Law, name: &str) -> Option<&Law> {
        self.laws
            .get(&qualify(owner.scope.as_deref(), name))
            .or_else(|| self.laws.get(name))
    }
}

fn qualify(scope: Option<&str>, name: &str) -> String {
    scope.map_or_else(|| name.to_string(), |scope| format!("{scope}.{name}"))
}

pub(super) fn expression(expr: &Spanned<Expr>) -> String {
    super::display::expression(expr)
}

pub(super) fn assertion(law: &VerifyLaw) -> String {
    if matches!(law.rhs.node, Expr::Literal(Literal::Bool(true))) {
        format!("{} holds", expression(&law.lhs))
    } else {
        format!("{} => {}", expression(&law.lhs), expression(&law.rhs))
    }
}

pub(super) fn conditions(expr: &Spanned<Expr>) -> Vec<&Spanned<Expr>> {
    if let Expr::FnCall(callee, args) = &expr.node
        && aver::checker::expr_to_str(callee) == "Bool.and"
        && args.len() == 2
    {
        conditions(&args[0])
            .into_iter()
            .chain(conditions(&args[1]))
            .collect()
    } else {
        vec![expr]
    }
}

/// Instantiate only a direct, unambiguous call pattern in the cited assertion.
/// This is source substitution, not an assertion that the backend applied this
/// law or checked its premises. Repeated/structured binders are left schematic.
pub(super) fn bindings(
    owner: &Law,
    cited: &Law,
    goal: Option<&Spanned<Expr>>,
) -> Option<BTreeMap<String, Spanned<Expr>>> {
    let goal = goal?;
    let Expr::FnCall(actual_callee, actual) = &goal.node else {
        return None;
    };
    let actual_name = aver::checker::expr_to_str(actual_callee);
    let actual_name = if actual_name.contains('.') {
        actual_name
    } else {
        qualify(owner.scope.as_deref(), &actual_name)
    };
    if actual_name != cited.function {
        return None;
    }
    let Expr::FnCall(pattern_callee, patterns) = &cited.body.lhs.node else {
        return None;
    };
    let pattern_name = aver::checker::expr_to_str(pattern_callee);
    let pattern_name = if pattern_name.contains('.') {
        pattern_name
    } else {
        qualify(cited.scope.as_deref(), &pattern_name)
    };
    if pattern_name != cited.function {
        return None;
    }
    if patterns.len() != actual.len() {
        return None;
    }
    // The shared identifier rewriter respects shadowing but does not rename
    // binders that could capture identifiers from a substituted argument.
    // Keep these uncommon assertion forms schematic rather than misreporting
    // an instantiated requirement.
    let mut has_binder = false;
    for expr in cited
        .body
        .when
        .iter()
        .chain([&cited.body.lhs, &cited.body.rhs])
    {
        aver::codegen::expr_walk::walk(expr, &mut |expr| {
            has_binder |= matches!(expr.node, Expr::Match { .. });
        });
    }
    if has_binder {
        return None;
    }
    let mut substitutions = BTreeMap::new();
    for (pattern, value) in patterns.iter().zip(actual) {
        let (Expr::Ident(name) | Expr::Resolved { name, .. }) = &pattern.node else {
            return None;
        };
        if !cited.body.givens.iter().any(|given| given.name == *name)
            || substitutions.insert(name.clone(), value.clone()).is_some()
        {
            return None;
        }
    }
    // Leaving a quantified variable unsubstituted could accidentally give it
    // the meaning of a same-named variable in the calling law.
    if substitutions.len() != cited.body.givens.len() {
        return None;
    }
    Some(substitutions)
}

pub(super) fn substitute(
    expr: &Spanned<Expr>,
    bindings: &BTreeMap<String, Spanned<Expr>>,
) -> Spanned<Expr> {
    aver::ast_rewrite::rewrite_idents_scoped(expr, |name| bindings.get(name).cloned())
}
