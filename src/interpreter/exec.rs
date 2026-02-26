use super::*;

impl Interpreter {
    pub fn exec_items(&mut self, items: &[TopLevel]) -> Result<Value, RuntimeError> {
        for item in items {
            match item {
                TopLevel::FnDef(fd) => self.exec_fn_def(fd)?,
                TopLevel::Module(_) => {}
                TopLevel::Verify(_) => {}
                TopLevel::Decision(_) => {}
                TopLevel::TypeDef(td) => self.register_type_def(td),
                TopLevel::EffectSet { name, effects } => {
                    self.register_effect_set(name.clone(), effects.clone());
                }
                TopLevel::Stmt(s) => {
                    self.exec_stmt(s)?;
                }
            }
        }
        Ok(Value::Unit)
    }

    /// Register a user-defined type: sum type variants and record constructors.
    pub fn register_type_def(&mut self, td: &TypeDef) {
        match td {
            TypeDef::Sum {
                name: type_name,
                variants,
            } => {
                let mut members = HashMap::new();
                for variant in variants {
                    if variant.fields.is_empty() {
                        // Zero-arg variant: stored directly as a Value
                        members.insert(
                            variant.name.clone(),
                            Value::Variant {
                                type_name: type_name.clone(),
                                variant: variant.name.clone(),
                                fields: vec![],
                            },
                        );
                    } else {
                        // Constructor function
                        members.insert(
                            variant.name.clone(),
                            Value::Builtin(format!("__ctor:{}:{}", type_name, variant.name)),
                        );
                    }
                }
                self.define(
                    type_name.clone(),
                    Value::Namespace {
                        name: type_name.clone(),
                        members,
                    },
                );
            }
            TypeDef::Product { name, fields } => {
                // Product types are constructed via Expr::RecordCreate.
                // Keep declaration field order so runtime records are canonicalized
                // and positional record matches stay stable.
                let schema = fields.iter().map(|(field, _)| field.clone()).collect();
                self.record_schemas.insert(name.clone(), schema);
            }
        }
    }

    pub fn exec_fn_def(&mut self, fd: &FnDef) -> Result<(), RuntimeError> {
        let val = Value::Fn {
            name: fd.name.clone(),
            params: fd.params.clone(),
            effects: self.expand_effects(&fd.effects),
            body: Rc::clone(&fd.body),
            resolution: fd.resolution.clone(),
            memo_eligible: self.memo_fns.contains(&fd.name),
            home_globals: None,
        };
        self.define(fd.name.clone(), val);
        Ok(())
    }

    pub fn exec_stmt(&mut self, stmt: &Stmt) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                let val = self.eval_expr(expr)?;
                self.define(name.clone(), val);
                Ok(Value::Unit)
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
        }
    }

    pub fn exec_body(&mut self, stmts: &[Stmt]) -> Result<Value, RuntimeError> {
        let mut last = Value::Unit;
        for stmt in stmts {
            last = self.exec_stmt(stmt)?;
        }
        Ok(last)
    }

    /// Execute a block body inside a resolved (Slots-based) function.
    /// Val/Var/Assign use slot indices when available.
    pub(super) fn exec_body_resolved(
        &mut self,
        stmts: &[Stmt],
        local_slots: &HashMap<String, u16>,
    ) -> Result<Value, RuntimeError> {
        let mut last = Value::Unit;
        for stmt in stmts {
            last = self.exec_stmt_resolved(stmt, local_slots)?;
        }
        Ok(last)
    }

    pub(super) fn exec_stmt_resolved(
        &mut self,
        stmt: &Stmt,
        local_slots: &HashMap<String, u16>,
    ) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::Binding(name, _, expr) => {
                let val = self.eval_expr(expr)?;
                if let Some(&slot) = local_slots.get(name) {
                    self.define_slot(slot, val);
                } else {
                    self.define(name.clone(), val);
                }
                Ok(Value::Unit)
            }
            Stmt::Expr(expr) => self.eval_expr(expr),
        }
    }

    pub fn run_file(&mut self, source: &str) -> Result<Value, RuntimeError> {
        let mut items = parse_source(source).map_err(RuntimeError::Error)?;
        crate::resolver::resolve_program(&mut items);

        // First pass: register all top-level definitions
        for item in &items {
            match item {
                TopLevel::FnDef(fd) => self.exec_fn_def(fd)?,
                TopLevel::Stmt(s) => {
                    self.exec_stmt(s)?;
                }
                _ => {}
            }
        }

        // Second pass: run main() if it exists
        let main_fn = self.lookup("main");
        if let Ok(fn_val) = main_fn {
            let allowed = Self::callable_declared_effects(&fn_val);
            self.call_value_with_effects_pub(fn_val, vec![], "<main>", allowed)?;
        }

        Ok(Value::Unit)
    }
}
