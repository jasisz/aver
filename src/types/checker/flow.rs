use super::*;

impl TypeChecker {
    fn with_verify_law_givens<T>(
        &mut self,
        givens: &[crate::ast::VerifyGiven],
        f: impl FnOnce(&mut Self) -> T,
    ) -> T {
        let prev_locals = self.locals.clone();
        for given in givens {
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
            Expr::Tuple(items) => items
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
                boxed.0 == fn_name
                    || boxed
                        .1
                        .iter()
                        .any(|arg| Self::verify_case_calls_target(arg, fn_name))
            }
            Expr::Literal(_)
            | Expr::Ident(_)
            | Expr::InterpolatedStr(_)
            | Expr::Resolved(_)
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
        if let Some(sig) = self.fn_sigs.get(&f.name).cloned() {
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
            if !Self::constraint_compatible(&last_type, &declared_ret) {
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
                            let ty = if let Some(ann_src) = type_ann {
                                match crate::types::parse_type_str_strict(ann_src) {
                                    Ok(annotated) => {
                                        if !Self::constraint_compatible(&inferred, &annotated) {
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
        let no_effects: Vec<String> = vec![];
        // Allow `?` in verify cases: treat each case as if inside a Result-returning
        // function so ErrorProp type-checks. At runtime, `?` hitting Err means
        // "test failed" rather than error propagation.
        let prev_ret = self.current_fn_ret.take();
        self.current_fn_ret = Some(Type::Result(
            Box::new(Type::Unknown),
            Box::new(Type::Unknown),
        ));
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
                let caller = format!("<verify:{}>", vb.fn_name);
                if let crate::ast::VerifyKind::Law(law) = &vb.kind {
                    self.with_verify_law_givens(&law.givens, |checker| {
                        if let Some(when_expr) = &law.when {
                            let when_ty = checker.infer_type(when_expr);
                            if !Self::constraint_compatible(&when_ty, &Type::Bool) {
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
                            checker.check_effects_in_expr(when_expr, &caller, &no_effects);
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
                    self.check_effects_in_expr(left, &caller, &no_effects);
                    let _ = self.infer_type(right);
                    self.check_effects_in_expr(right, &caller, &no_effects);
                    if let crate::ast::VerifyKind::Law(law) = &vb.kind
                        && let Some(sample_guard) = law.sample_guards.get(idx)
                    {
                        let guard_ty = self.infer_type(sample_guard);
                        if !Self::constraint_compatible(&guard_ty, &Type::Bool) {
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
                        let inferred = self.infer_type(expr);
                        let ty = if let Some(ann_src) = type_ann {
                            match crate::types::parse_type_str_strict(ann_src) {
                                Ok(annotated) => {
                                    if !Self::constraint_compatible(&inferred, &annotated) {
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
                    last = self.infer_type(expr);
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
            && let Some(callee_sig) = self.fn_sigs.get(&callee_name)
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
            Expr::Tuple(items) => {
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
                for arg in &boxed.1 {
                    self.check_effects_in_expr(arg, caller_name, caller_effects);
                }
            }
            _ => {}
        }
    }
}
