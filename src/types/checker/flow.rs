use super::*;

impl TypeChecker {
    fn with_verify_law_givens<T>(
        &mut self,
        givens: &[crate::ast::VerifyGiven],
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let prev_locals = self.locals.clone();
        for given in givens {
            // Oracle v1: `given rnd: Random.int = [...]` — the type annotation
            // is a classified effect method reference, not a type. Substitute
            // the effect's oracle signature (branch-indexed for generative,
            // capability reader for snapshot). Output effects have no oracle
            // and are rejected with a clear message.
            if let Some(c) = super::effect_classification::classify(&given.type_name) {
                match super::effect_classification::oracle_signature(&given.type_name) {
                    Some(sig) => {
                        self.locals.insert(given.name.clone(), sig);
                    }
                    None => {
                        self.error(format!(
                            "given '{}': effect '{}' is output-only (dimension {:?}) \
                             and has no oracle — output effects are asserted about via \
                             the trace API, not bound via `given`",
                            given.name, given.type_name, c.dimension
                        ));
                    }
                }
                continue;
            }
            match parse_type_str_strict(&given.type_name) {
                Ok(ty) => {
                    self.locals.insert(given.name.clone(), ty);
                }
                Err(unknown) => {
                    self.error(format!(
                        "Unknown type '{}' in verify law given '{}'",
                        unknown, given.name
                    ));
                }
            }
        }
        let out = f(self);
        self.locals = prev_locals;
        out
    }

    fn verify_case_calls_target(expr: &Spanned<Expr>, fn_name: &str) -> bool {
        match &expr.node {
            Expr::FnCall(callee, args) => {
                Self::callee_is_verify_target(&callee.node, fn_name)
                    || Self::verify_case_calls_target(callee, fn_name)
                    || args
                        .iter()
                        .any(|arg| Self::verify_case_calls_target(arg, fn_name))
            }
            Expr::BinOp(_, left_expr, right_expr) => {
                Self::verify_case_calls_target(left_expr, fn_name)
                    || Self::verify_case_calls_target(right_expr, fn_name)
            }
            Expr::Neg(inner) => Self::verify_case_calls_target(inner, fn_name),
            Expr::Match { subject, arms, .. } => {
                Self::verify_case_calls_target(subject, fn_name)
                    || arms
                        .iter()
                        .any(|arm| Self::verify_case_calls_target(&arm.body, fn_name))
            }
            Expr::Constructor(_, Some(inner)) => Self::verify_case_calls_target(inner, fn_name),
            Expr::ErrorProp(inner) => Self::verify_case_calls_target(inner, fn_name),
            Expr::List(elems) => elems
                .iter()
                .any(|elem| Self::verify_case_calls_target(elem, fn_name)),
            Expr::Tuple(items) | Expr::IndependentProduct(items, _) => items
                .iter()
                .any(|item| Self::verify_case_calls_target(item, fn_name)),
            Expr::MapLiteral(entries) => entries.iter().any(|(k, v)| {
                Self::verify_case_calls_target(k, fn_name)
                    || Self::verify_case_calls_target(v, fn_name)
            }),
            Expr::Attr(obj, _) => Self::verify_case_calls_target(obj, fn_name),
            Expr::RecordCreate { fields, .. } => fields
                .iter()
                .any(|(_, expr)| Self::verify_case_calls_target(expr, fn_name)),
            Expr::RecordUpdate { base, updates, .. } => {
                Self::verify_case_calls_target(base, fn_name)
                    || updates
                        .iter()
                        .any(|(_, expr)| Self::verify_case_calls_target(expr, fn_name))
            }
            Expr::TailCall(boxed) => {
                boxed.target == fn_name
                    || boxed
                        .args
                        .iter()
                        .any(|arg| Self::verify_case_calls_target(arg, fn_name))
            }
            Expr::Literal(_)
            | Expr::Ident(_)
            | Expr::InterpolatedStr(_)
            | Expr::Resolved { .. }
            | Expr::Constructor(_, None) => false,
        }
    }

    fn callee_is_verify_target(callee: &Expr, fn_name: &str) -> bool {
        matches!(callee, Expr::Ident(name) if name == fn_name)
    }

    pub(super) fn check_fn(&mut self, f: &FnDef) {
        self.current_fn_line = Some(f.line);
        // Start with globals and overlay parameter bindings.
        self.locals = self.globals.clone();
        if let Some(sig) = self.find_fn_sig(&f.name).cloned() {
            for ((param_name, _), param_type) in f.params.iter().zip(sig.params.iter()) {
                self.locals.insert(param_name.clone(), param_type.clone());
            }

            let declared_ret = sig.ret.clone();
            let declared_effects = sig.effects.clone();

            self.current_fn_ret = Some(declared_ret.clone());

            // Clear unused-binding tracking for this function.
            self.used_names.clear();
            self.fn_bindings.clear();

            let last_type = self.check_stmts(f.body.stmts(), &f.name, &declared_effects);
            if !self.compatible(&last_type, &declared_ret) {
                // Find line of the last expression in body for secondary span.
                let body_last_line = f.body.stmts().last().map(|stmt| match stmt {
                    Stmt::Expr(e) => e.line,
                    Stmt::Binding(_, _, e) => e.line,
                });
                let secondary = body_last_line.map(|line| TypeErrorSpan {
                    line,
                    col: 0,
                    label: format!("returns {}", last_type.display()),
                });
                self.errors.push(TypeError {
                    message: format!(
                        "Function '{}': body returns {} but declared return type is {}",
                        f.name,
                        last_type.display(),
                        declared_ret.display()
                    ),
                    line: f.line,
                    col: 0,
                    secondary,
                });
            }

            // Detect unused bindings (skip names starting with '_').
            for (binding_name, binding_line) in &self.fn_bindings {
                if !binding_name.starts_with('_') && !self.used_names.contains(binding_name) {
                    self.unused_warnings.push((
                        binding_name.clone(),
                        f.name.clone(),
                        *binding_line,
                    ));
                }
            }

            self.current_fn_ret = None;
            self.current_fn_line = None;
        }
    }

    pub(super) fn check_top_level_stmts(&mut self, items: &[TopLevel]) {
        self.locals.clear();
        let no_effects: Vec<String> = vec![];
        for item in items {
            if let TopLevel::Stmt(stmt) = item {
                match stmt {
                    Stmt::Binding(name, type_ann, expr) => {
                        if self.locals.contains_key(name) {
                            self.error(format!("'{}' is already defined", name));
                        } else {
                            if matches!(expr.node, Expr::List(ref elems) if elems.is_empty())
                                && type_ann.is_none()
                            {
                                self.error(format!(
                                    "Binding '{}' to empty list literal is not allowed — immutable empty collection is dead code",
                                    name
                                ));
                            }
                            let inferred = self.infer_type(expr);
                            // Per `docs/language.md:228`: top-level fns are
                            // first-class *as call arguments*
                            // (`HttpServer.listen(port, handler)`) but not as
                            // local bindings or standalone refs. Every
                            // backend treats `h = <fn>` as unimplemented —
                            // VM has no slot, wasm-gc emit reaches the
                            // backend with "not implemented yet — bare Ident
                            // reached emitter", `<namespace>.<method>`
                            // (`Vector.set`) used to panic codegen entirely
                            // before Iron 0.21. Lift the rejection into
                            // typecheck so the same message lands on every
                            // target.
                            if self.type_contains_fn(&inferred) {
                                self.error(format!(
                                    "Binding '{}' to a fn reference is not supported. Aver allows top-level fns as first-class values only in call-argument position (e.g. `HttpServer.listen(port, {})`). For local use, call it: `{} = <fn>(...)`.",
                                    name, name, name
                                ));
                            }
                            let ty = if let Some(ann_src) = type_ann {
                                match crate::types::parse_type_str_strict(ann_src) {
                                    Ok(annotated) => {
                                        let annotated = self.canonicalize_named(annotated);
                                        let ctx = format!("Binding '{}' annotation", name);
                                        self.report_ambiguous_named(&annotated, expr.line, &ctx);
                                        self.reject_fn_in_type(&annotated, false, expr.line, &ctx);
                                        if !self.compatible(&inferred, &annotated) {
                                            self.error(format!(
                                                "Binding '{}': expression has type {}, annotation says {}",
                                                name, inferred.display(), annotated.display()
                                            ));
                                        }
                                        annotated
                                    }
                                    Err(unknown) => {
                                        self.error(format!(
                                            "Unknown type '{}' in binding annotation",
                                            unknown
                                        ));
                                        inferred
                                    }
                                }
                            } else {
                                inferred
                            };
                            self.check_effects_in_expr(expr, "<top-level>", &no_effects);
                            self.locals.insert(name.clone(), ty);
                        }
                    }
                    Stmt::Expr(expr) => {
                        let _ = self.infer_type(expr);
                        self.check_effects_in_expr(expr, "<top-level>", &no_effects);
                    }
                }
            }
        }
        self.globals = self.locals.clone();
    }

    pub(super) fn check_verify_blocks(&mut self, items: &[TopLevel]) {
        // Allow `?` in verify cases: treat each case as if inside a Result-returning
        // function so ErrorProp type-checks. At runtime, `?` hitting Err means
        // "test failed" rather than error propagation.
        let prev_ret = self.current_fn_ret.take();
        self.current_fn_ret = Some(Type::Result(
            Box::new(Type::Var("VerifyOk".to_string())),
            Box::new(Type::Var("VerifyErr".to_string())),
        ));
        // Oracle v1: identify recursive functions once. A `verify fn trace law`
        // targeting an effectful recursive function is rejected because the
        // caller_fn filter used to scope fn.trace cannot distinguish the
        // outermost invocation from recursive self-calls. Result-only laws
        // for the same function remain fully supported.
        let recursive_fns = crate::call_graph::find_recursive_fns(items);
        for item in items {
            if let TopLevel::Verify(vb) = item {
                self.current_fn_line = Some(vb.line);
                if vb.cases.is_empty() {
                    self.error(format!(
                        "Verify block '{}' must contain at least one case",
                        vb.fn_name
                    ));
                    continue;
                }
                // Oracle v1: classify the verified function's effects to
                // decide whether this verify block is in the proof subset.
                let fn_effects: Vec<String> = self
                    .find_fn_sig(&vb.fn_name)
                    .map(|sig| sig.effects.clone())
                    .unwrap_or_default();
                let classified_effects: Vec<String> = fn_effects
                    .iter()
                    .filter(|e| super::effect_classification::is_classified(e.as_str()))
                    .cloned()
                    .collect();
                let unclassified_effects: Vec<String> = fn_effects
                    .iter()
                    .filter(|e| !super::effect_classification::is_classified(e.as_str()))
                    .cloned()
                    .collect();

                // Rejection 1: trace-aware law on a recursive effectful function.
                if vb.trace && recursive_fns.contains(&vb.fn_name) && !classified_effects.is_empty()
                {
                    self.error_at_line(
                        vb.line,
                        format!(
                            "verify '{fn_name} trace {kind}' targets a recursive effectful function \
                             (effects: {effects}). Trace-aware laws on effectful recursion are not \
                             supported in Oracle v1 — the caller_fn filter that scopes fn.trace to \
                             direct emissions cannot distinguish the outermost invocation from \
                             recursive self-calls. Drop the 'trace' keyword to use a result-only \
                             law, or refactor the effect-emitting work into a non-recursive helper \
                             and verify the helper's trace separately.",
                            fn_name = vb.fn_name,
                            kind = match &vb.kind {
                                crate::ast::VerifyKind::Law(law) => format!("law {}", law.name),
                                crate::ast::VerifyKind::Cases => String::new(),
                            },
                            effects = classified_effects.join(", "),
                        ),
                    );
                }

                // Rejection 2: verify on a function using effects outside
                // the Oracle v1 proof subset (stateful / interactive /
                // higher-order-callback). Applies to any law and to
                // cases-form `verify fn trace` — trace-aware assertions
                // on unclassified effects can't be lifted or emulated.
                let enforces_proof_subset =
                    matches!(vb.kind, crate::ast::VerifyKind::Law(_)) || vb.trace;
                if !unclassified_effects.is_empty() && enforces_proof_subset {
                    let kind_label = match &vb.kind {
                        crate::ast::VerifyKind::Law(_) => "verify law",
                        crate::ast::VerifyKind::Cases if vb.trace => "verify trace",
                        crate::ast::VerifyKind::Cases => "verify",
                    };
                    self.error_at_line(
                        vb.line,
                        format!(
                            "{kind_label} '{fn_name}' uses effect(s) outside Oracle v1's proof subset: \
                             {effects}. These effects are ambient state, protocol/session state, modal \
                             terminal state, or higher-order callbacks and cannot be lifted to pure form. \
                             Use 'aver record' / 'aver replay' for deterministic reproduction. \
                             Oracle v1's classified effects: Args.get, Env.get, Console.readLine, \
                             Random.int/.float, Time.now/.unixMs/.sleep, Disk.readText/.exists/.listDir/\
                             .writeText/.appendText/.delete/.deleteDir/.makeDir, Http.get/.head/.delete/\
                             .post/.put/.patch, Tcp.send/.ping, Console.print/.error/.warn, \
                             Terminal.clear/.moveTo/.print/.readKey/.hideCursor/.showCursor/.flush.",
                            kind_label = kind_label,
                            fn_name = vb.fn_name,
                            effects = unclassified_effects.join(", "),
                        ),
                    );
                }
                // Rejection 2b: duplicate `given` bindings for the same
                // effect method. Lifted fns carry one oracle param per
                // unique effect, so two givens with the same effect
                // `type_name` have no sensible mapping — the lifter's
                // `oracles_map` keys by effect name and the second
                // given silently overwrites the first. The emitted
                // theorem still quantifies both binding names and
                // asserts `stubA(...) = stubB(...)`, which is false
                // for any two distinct stubs. Only classifies as
                // duplicate when `type_name` names a built-in effect
                // (e.g. `Random.int`) — plain-type givens like
                // `given a: Int = [1]` and `given b: Int = [2]` are
                // independent domain bindings, not oracle stubs.
                {
                    let mut given_effects: std::collections::HashMap<&str, Vec<&str>> =
                        std::collections::HashMap::new();
                    let givens: Box<dyn Iterator<Item = &crate::ast::VerifyGiven>> = match &vb.kind
                    {
                        crate::ast::VerifyKind::Law(law) => Box::new(law.givens.iter()),
                        crate::ast::VerifyKind::Cases => Box::new(vb.cases_givens.iter()),
                    };
                    for given in givens {
                        if super::effect_classification::classify(&given.type_name).is_some() {
                            given_effects
                                .entry(given.type_name.as_str())
                                .or_default()
                                .push(given.name.as_str());
                        }
                    }
                    for (effect, names) in &given_effects {
                        if names.len() > 1 {
                            self.error_at_line(
                                vb.line,
                                format!(
                                    "verify '{fn_name}' has {count} `given` bindings for the same \
                                     effect '{effect}': {names}. Each effect method has one oracle \
                                     parameter in the lifted form, so a second stub has no slot to \
                                     bind to. To test multiple stub behaviours, use a multi-value \
                                     domain: `given {first}: {effect} = [stub1, stub2, ...]`, which \
                                     expands into a separate sample theorem per stub.",
                                    fn_name = vb.fn_name,
                                    count = names.len(),
                                    effect = effect,
                                    names = names.join(", "),
                                    first = names[0],
                                ),
                            );
                        }
                    }
                }

                // Rejection 3: under `verify fn trace`, every generative
                // (or generative+output) effect the fn uses must have a
                // `given` binding. Without a stub, each verify run would
                // dispatch the real effect and the law would check
                // against non-deterministic values — the failure looks
                // spooky (`expected: 4 actual: 5`) because the user
                // didn't realise Random.int was live. A loud rejection
                // at check time points straight at the fix.
                if vb.trace {
                    use super::effect_classification::{EffectDimension, classify};
                    let given_names: std::collections::HashSet<&str> = match &vb.kind {
                        crate::ast::VerifyKind::Law(law) => {
                            law.givens.iter().map(|g| g.type_name.as_str()).collect()
                        }
                        crate::ast::VerifyKind::Cases => vb
                            .cases_givens
                            .iter()
                            .map(|g| g.type_name.as_str())
                            .collect(),
                    };
                    let needs_stub: Vec<String> = fn_effects
                        .iter()
                        .filter_map(|e| {
                            let c = classify(e)?;
                            matches!(
                                c.dimension,
                                EffectDimension::Generative | EffectDimension::GenerativeOutput
                            )
                            .then(|| e.clone())
                        })
                        .filter(|e| !given_names.contains(e.as_str()))
                        .collect();
                    // Rejection 4: each `given <name>: <Effect.method> = [vals]`
                    // must bind values whose inferred type matches the
                    // oracle signature for that effect. Catches the
                    // common footgun of authoring a snapshot stub with
                    // a leading (BranchPath, Int) (copy-paste from a
                    // generative stub) — at runtime those stubs silently
                    // ignore extra params and produce bogus values.
                    let givens_iter: Box<dyn Iterator<Item = &crate::ast::VerifyGiven>> =
                        match &vb.kind {
                            crate::ast::VerifyKind::Law(law) => Box::new(law.givens.iter()),
                            crate::ast::VerifyKind::Cases => Box::new(vb.cases_givens.iter()),
                        };
                    for given in givens_iter {
                        let Some(expected) =
                            super::effect_classification::oracle_signature(&given.type_name)
                        else {
                            continue;
                        };
                        if let crate::ast::VerifyGivenDomain::Explicit(vals) = &given.domain {
                            for v in vals {
                                let actual = self.infer_type(v);
                                if !self.compatible(&actual, &expected) {
                                    self.error_at_line(
                                        v.line,
                                        format!(
                                            "given '{name}: {effect}' expects a stub of type {exp}, \
                                             got {act}. Snapshot effects take no BranchPath / counter; \
                                             generative effects take a leading (BranchPath, Int).",
                                            name = given.name,
                                            effect = given.type_name,
                                            exp = expected.display(),
                                            act = actual.display(),
                                        ),
                                    );
                                }
                            }
                        }
                    }

                    if !needs_stub.is_empty() {
                        self.error_at_line(
                            vb.line,
                            format!(
                                "verify trace '{fn_name}' needs a `given` stub for each generative \
                                 effect the fn uses; missing: {missing}. Without stubs the verify \
                                 run dispatches the real effect (e.g. a live random value) and \
                                 assertions compare against non-deterministic output. Add e.g. \
                                 `given name: {first} = [stubFn]`, where stubFn has signature \
                                 `(BranchPath, Int, args...) -> T`.",
                                fn_name = vb.fn_name,
                                missing = needs_stub.join(", "),
                                first = needs_stub[0],
                            ),
                        );
                    }
                }

                // Inherit effects from the tested function so verify blocks
                // can call effectful functions without declaring effects.
                let inherited_effects: Vec<String> = self
                    .find_fn_sig(&vb.fn_name)
                    .map(|sig| sig.effects.clone())
                    .unwrap_or_default();
                let caller = format!("<verify:{}>", vb.fn_name);
                if let crate::ast::VerifyKind::Law(law) = &vb.kind {
                    self.with_verify_law_givens(&law.givens, |checker| {
                        if let Some(when_expr) = &law.when {
                            let when_ty = checker.infer_type(when_expr);
                            if !checker.compatible(&when_ty, &Type::Bool) {
                                checker.error_at_line(
                                    vb.line,
                                    format!(
                                        "Verify law '{}.{}' when condition must have type Bool, got {}",
                                        vb.fn_name,
                                        law.name,
                                        when_ty.display()
                                    ),
                                );
                            }
                            checker.check_effects_in_expr(when_expr, &caller, &inherited_effects);
                        }
                    });
                    if law.when.is_some() && law.sample_guards.len() != vb.cases.len() {
                        self.error_at_line(
                            vb.line,
                            format!(
                                "Verify law '{}.{}' internal guard expansion mismatch: {} guards for {} cases",
                                vb.fn_name,
                                law.name,
                                law.sample_guards.len(),
                                vb.cases.len()
                            ),
                        );
                    }
                }
                // Oracle v1: `.result` / `.trace.*` projections are
                // typechecker-valid only inside verify-trace cases.
                // Flip the flag for the whole case loop so LHS / RHS
                // inference sees it; reset after. Applies to both
                // cases-form `verify fn trace` and law-form (where
                // the law body can use `.result` on `fn_name()` RHS).
                let prev_in_verify_trace = self.in_verify_trace_context;
                self.in_verify_trace_context =
                    vb.trace || matches!(vb.kind, crate::ast::VerifyKind::Law(_));
                for (idx, (left, right)) in vb.cases.iter().enumerate() {
                    // Use case-specific line if available, fall back to block line
                    let case_line = vb
                        .case_spans
                        .get(idx)
                        .map(|s| s.line)
                        .filter(|l| *l > 0)
                        .unwrap_or(vb.line);
                    self.current_fn_line = Some(case_line);
                    if matches!(vb.kind, crate::ast::VerifyKind::Cases)
                        && !Self::verify_case_calls_target(left, &vb.fn_name)
                    {
                        self.error_at_line(
                            case_line,
                            format!(
                                "Verify block '{}' case #{} must call '{}' on the left side",
                                vb.fn_name,
                                idx + 1,
                                vb.fn_name
                            ),
                        );
                    }
                    let _ = self.infer_type(left);
                    self.check_effects_in_expr(left, &caller, &inherited_effects);
                    let _ = self.infer_type(right);
                    self.check_effects_in_expr(right, &caller, &inherited_effects);
                    if let crate::ast::VerifyKind::Law(law) = &vb.kind
                        && let Some(sample_guard) = law.sample_guards.get(idx)
                    {
                        let guard_ty = self.infer_type(sample_guard);
                        if !self.compatible(&guard_ty, &Type::Bool) {
                            self.error_at_line(
                                vb.line,
                                format!(
                                    "Verify law '{}.{}' when-expanded case #{} must have type Bool, got {}",
                                    vb.fn_name,
                                    law.name,
                                    idx + 1,
                                    guard_ty.display()
                                ),
                            );
                        }
                    }
                }
                self.in_verify_trace_context = prev_in_verify_trace;
            }
        }
        self.current_fn_line = None;
        self.current_fn_ret = prev_ret;
    }

    pub(super) fn check_stmts(
        &mut self,
        stmts: &[Stmt],
        fn_name: &str,
        caller_effects: &[String],
    ) -> Type {
        let mut last = Type::Unit;
        for stmt in stmts {
            match stmt {
                Stmt::Binding(name, type_ann, expr) => {
                    if self.locals.contains_key(name) {
                        self.error(format!("'{}' is already defined in '{}'", name, fn_name));
                    } else {
                        if matches!(expr.node, Expr::List(ref elems) if elems.is_empty())
                            && type_ann.is_none()
                        {
                            self.error(format!(
                                "Binding '{}' to empty list literal is not allowed — immutable empty collection is dead code",
                                name
                            ));
                        }
                        // Bidirectional: if the binding is annotated, parse
                        // the annotation first and pass it as the expected
                        // type so generic constructor RHS (`Map.empty()`,
                        // `Option.None`, `[]`) picks up T from the
                        // annotation rather than stamping `Unknown`.
                        let parsed_ann = type_ann
                            .as_ref()
                            .and_then(|src| crate::types::parse_type_str_strict(src).ok())
                            .map(|ty| self.canonicalize_named(ty));
                        let inferred = self.infer_type_with_expected(expr, parsed_ann.as_ref());
                        // Mirror the top-level rejection above — fn refs
                        // aren't supported as local bindings, period. See
                        // `flow.rs:175` for the rationale.
                        if self.type_contains_fn(&inferred) {
                            self.error(format!(
                                "Binding '{}' to a fn reference is not supported. Aver allows top-level fns as first-class values only in call-argument position (e.g. `HttpServer.listen(port, {})`). For local use, call it: `{} = <fn>(...)`.",
                                name, name, name
                            ));
                        }
                        let ty = if let Some(ann_src) = type_ann {
                            match crate::types::parse_type_str_strict(ann_src) {
                                Ok(annotated) => {
                                    let annotated = self.canonicalize_named(annotated);
                                    let ctx = format!("Binding '{}' annotation", name);
                                    self.report_ambiguous_named(&annotated, expr.line, &ctx);
                                    self.reject_fn_in_type(&annotated, false, expr.line, &ctx);
                                    if !self.compatible(&inferred, &annotated) {
                                        self.error(format!(
                                            "Binding '{}': expression has type {}, annotation says {}",
                                            name, inferred.display(), annotated.display()
                                        ));
                                    }
                                    annotated
                                }
                                Err(unknown) => {
                                    self.error(format!(
                                        "Unknown type '{}' in binding annotation",
                                        unknown
                                    ));
                                    inferred
                                }
                            }
                        } else {
                            inferred
                        };
                        self.check_effects_in_expr(expr, fn_name, caller_effects);
                        self.locals.insert(name.clone(), ty);
                        // Track binding for unused detection.
                        let line = if expr.line > 0 {
                            expr.line
                        } else {
                            self.current_fn_line.unwrap_or(1)
                        };
                        self.fn_bindings.push((name.clone(), line));
                    }
                    last = Type::Unit;
                }
                Stmt::Expr(expr) => {
                    // Bidirectional: pass fn return type as expected so
                    // generic constructors in tail position (last stmt of
                    // body) pick up T. Match arms inside also propagate
                    // via current_fn_ret already.
                    let expected = self.current_fn_ret.clone();
                    last = self.infer_type_with_expected(expr, expected.as_ref());
                    self.check_effects_in_expr(expr, fn_name, caller_effects);
                }
            }
        }
        last
    }

    // -----------------------------------------------------------------------
    // Effect propagation: ERROR (not warning) if callee has effect caller lacks
    // -----------------------------------------------------------------------
    pub(super) fn callee_key(fn_expr: &Expr) -> Option<String> {
        Self::attr_key(fn_expr)
    }

    pub(super) fn callable_effects(&self, fn_expr: &Expr) -> Option<(String, Vec<String>)> {
        if let Some(callee_name) = Self::callee_key(fn_expr)
            && let Some(callee_sig) = self.find_fn_sig(&callee_name)
        {
            return Some((callee_name, callee_sig.effects.clone()));
        }
        if let Expr::Ident(name) = fn_expr
            && let Some(ty) = self.binding_type(name)
            && let Type::Fn(_, _, effects) = ty
        {
            return Some((name.clone(), effects));
        }
        None
    }

    pub(super) fn check_effects_in_expr(
        &mut self,
        expr: &Spanned<Expr>,
        caller_name: &str,
        caller_effects: &[String],
    ) {
        match &expr.node {
            Expr::FnCall(fn_expr, args) => {
                if let Some((callee_name, effects)) = self.callable_effects(&fn_expr.node) {
                    let err_line = if expr.line > 0 {
                        expr.line
                    } else {
                        self.current_fn_line.unwrap_or(1)
                    };
                    for effect in &effects {
                        if !self.caller_has_effect(caller_effects, effect) {
                            self.error_at_line(err_line, format!(
                                "Function '{}' calls '{}' which has effect '{}', but '{}' does not declare it",
                                caller_name, callee_name, effect, caller_name
                            ));
                        }
                    }
                }
                self.check_effects_in_expr(fn_expr, caller_name, caller_effects);
                for arg in args {
                    self.check_effects_in_expr(arg, caller_name, caller_effects);
                }
            }
            Expr::BinOp(_, left, right) => {
                self.check_effects_in_expr(left, caller_name, caller_effects);
                self.check_effects_in_expr(right, caller_name, caller_effects);
            }
            Expr::Neg(inner) => self.check_effects_in_expr(inner, caller_name, caller_effects),
            Expr::Match { subject, arms, .. } => {
                self.check_effects_in_expr(subject, caller_name, caller_effects);
                for arm in arms {
                    self.check_effects_in_expr(&arm.body, caller_name, caller_effects);
                }
            }
            Expr::Constructor(_, Some(inner)) => {
                self.check_effects_in_expr(inner, caller_name, caller_effects);
            }
            Expr::ErrorProp(inner) => {
                self.check_effects_in_expr(inner, caller_name, caller_effects);
            }
            Expr::List(elems) => {
                for elem in elems {
                    self.check_effects_in_expr(elem, caller_name, caller_effects);
                }
            }
            Expr::Tuple(items) | Expr::IndependentProduct(items, _) => {
                for item in items {
                    self.check_effects_in_expr(item, caller_name, caller_effects);
                }
            }
            Expr::MapLiteral(entries) => {
                for (key, value) in entries {
                    self.check_effects_in_expr(key, caller_name, caller_effects);
                    self.check_effects_in_expr(value, caller_name, caller_effects);
                }
            }
            Expr::Attr(obj, _) => {
                self.check_effects_in_expr(obj, caller_name, caller_effects);
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, expr) in fields {
                    self.check_effects_in_expr(expr, caller_name, caller_effects);
                }
            }
            Expr::RecordUpdate { base, updates, .. } => {
                self.check_effects_in_expr(base, caller_name, caller_effects);
                for (_, expr) in updates {
                    self.check_effects_in_expr(expr, caller_name, caller_effects);
                }
            }
            Expr::TailCall(boxed) => {
                for arg in &boxed.args {
                    self.check_effects_in_expr(arg, caller_name, caller_effects);
                }
            }
            Expr::InterpolatedStr(parts) => {
                // `"x = {fn_call()}"` — interpolated call sites must
                // propagate their effects to the enclosing fn. Without
                // this, `Console.print("{roll()}")` type-checks even
                // though main lacks Random.int; the VM then crashes at
                // runtime when roll() actually emits Random.int.
                for part in parts {
                    if let crate::ast::StrPart::Parsed(inner) = part {
                        self.check_effects_in_expr(inner, caller_name, caller_effects);
                    }
                }
            }
            _ => {}
        }
    }
}
