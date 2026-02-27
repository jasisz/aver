use super::*;

impl Interpreter {
    pub fn new() -> Self {
        let mut global = HashMap::new();

        console::register(&mut global);
        http::register(&mut global);
        http_server::register(&mut global);
        disk::register(&mut global);
        tcp::register(&mut global);
        int::register(&mut global);
        float::register(&mut global);
        string::register(&mut global);
        list::register(&mut global);
        map::register(&mut global);
        char::register(&mut global);
        byte::register(&mut global);

        // Result and Option namespaces — constructors for Ok/Err/Some/None
        {
            let mut members = HashMap::new();
            members.insert(
                "Ok".to_string(),
                Value::Builtin("__ctor:Result.Ok".to_string()),
            );
            members.insert(
                "Err".to_string(),
                Value::Builtin("__ctor:Result.Err".to_string()),
            );
            global.insert(
                "Result".to_string(),
                Value::Namespace {
                    name: "Result".to_string(),
                    members,
                },
            );
        }
        {
            let mut members = HashMap::new();
            members.insert(
                "Some".to_string(),
                Value::Builtin("__ctor:Option.Some".to_string()),
            );
            members.insert("None".to_string(), Value::None);
            global.insert(
                "Option".to_string(),
                Value::Namespace {
                    name: "Option".to_string(),
                    members,
                },
            );
        }

        let rc_global = global
            .into_iter()
            .map(|(k, v)| (k, Rc::new(v)))
            .collect::<HashMap<_, _>>();

        let mut record_schemas = HashMap::new();
        record_schemas.insert(
            "HttpResponse".to_string(),
            vec![
                "status".to_string(),
                "body".to_string(),
                "headers".to_string(),
            ],
        );
        record_schemas.insert(
            "HttpRequest".to_string(),
            vec![
                "method".to_string(),
                "path".to_string(),
                "body".to_string(),
                "headers".to_string(),
            ],
        );
        record_schemas.insert(
            "Header".to_string(),
            vec!["name".to_string(), "value".to_string()],
        );
        record_schemas.insert(
            "Tcp.Connection".to_string(),
            vec!["id".to_string(), "host".to_string(), "port".to_string()],
        );

        Interpreter {
            env: vec![EnvFrame::Owned(rc_global)],
            module_cache: HashMap::new(),
            record_schemas,
            call_stack: Vec::new(),
            effect_aliases: HashMap::new(),
            active_local_slots: None,
            memo_fns: HashSet::new(),
            memo_cache: HashMap::new(),
            execution_mode: ExecutionMode::Normal,
            recorded_effects: Vec::new(),
            replay_effects: Vec::new(),
            replay_pos: 0,
            validate_replay_args: false,
        }
    }

    pub fn execution_mode(&self) -> ExecutionMode {
        self.execution_mode
    }

    pub fn set_execution_mode_normal(&mut self) {
        self.execution_mode = ExecutionMode::Normal;
        self.recorded_effects.clear();
        self.replay_effects.clear();
        self.replay_pos = 0;
        self.validate_replay_args = false;
    }

    pub fn start_recording(&mut self) {
        self.execution_mode = ExecutionMode::Record;
        self.recorded_effects.clear();
        self.replay_effects.clear();
        self.replay_pos = 0;
        self.validate_replay_args = false;
    }

    pub fn start_replay(&mut self, effects: Vec<EffectRecord>, validate_args: bool) {
        self.execution_mode = ExecutionMode::Replay;
        self.replay_effects = effects;
        self.replay_pos = 0;
        self.validate_replay_args = validate_args;
        self.recorded_effects.clear();
    }

    pub fn take_recorded_effects(&mut self) -> Vec<EffectRecord> {
        std::mem::take(&mut self.recorded_effects)
    }

    pub fn replay_progress(&self) -> (usize, usize) {
        (self.replay_pos, self.replay_effects.len())
    }

    pub fn ensure_replay_consumed(&self) -> Result<(), RuntimeError> {
        if self.execution_mode == ExecutionMode::Replay
            && self.replay_pos < self.replay_effects.len()
        {
            return Err(RuntimeError::ReplayUnconsumed {
                remaining: self.replay_effects.len() - self.replay_pos,
            });
        }
        Ok(())
    }

    /// Mark a set of function names as eligible for auto-memoization.
    pub fn enable_memo(&mut self, fns: HashSet<String>) {
        self.memo_fns = fns;
    }

    /// Register a named effect set alias.
    pub fn register_effect_set(&mut self, name: String, effects: Vec<String>) {
        self.effect_aliases.insert(name, effects);
    }

    /// Expand effect names one level: aliases → concrete effect names.
    pub(super) fn expand_effects(&self, effects: &[String]) -> Vec<String> {
        let mut result = Vec::new();
        for e in effects {
            if let Some(expanded) = self.effect_aliases.get(e) {
                result.extend(expanded.iter().cloned());
            } else {
                result.push(e.clone());
            }
        }
        result
    }

    // -------------------------------------------------------------------------
    // Environment management
    // -------------------------------------------------------------------------
    pub(super) fn push_env(&mut self, frame: EnvFrame) {
        self.env.push(frame);
    }

    pub(super) fn pop_env(&mut self) {
        if self.env.len() > 1 {
            self.env.pop();
        }
    }

    pub(super) fn last_owned_scope_mut(
        &mut self,
    ) -> Result<&mut HashMap<String, Rc<Value>>, RuntimeError> {
        let frame = self
            .env
            .last_mut()
            .ok_or_else(|| RuntimeError::Error("No active scope".to_string()))?;
        match frame {
            EnvFrame::Owned(scope) => Ok(scope),
            EnvFrame::Shared(_) | EnvFrame::Slots(_) => Err(RuntimeError::Error(
                "Cannot define name in non-owned frame".to_string(),
            )),
        }
    }

    pub(super) fn lookup_rc(&self, name: &str) -> Result<&Rc<Value>, RuntimeError> {
        for frame in self.env.iter().rev() {
            let found = match frame {
                EnvFrame::Owned(scope) => scope.get(name),
                EnvFrame::Shared(scope) => scope.get(name),
                // Slots frames are indexed by slot, not by name — skip in name-based lookup
                EnvFrame::Slots(_) => None,
            };
            if let Some(v) = found {
                return Ok(v);
            }
        }
        Err(RuntimeError::Error(format!(
            "Undefined variable: '{}'",
            name
        )))
    }

    pub(super) fn global_scope_clone(&self) -> Result<HashMap<String, Rc<Value>>, RuntimeError> {
        let frame = self
            .env
            .first()
            .ok_or_else(|| RuntimeError::Error("No global scope".to_string()))?;
        match frame {
            EnvFrame::Owned(scope) => Ok(scope.clone()),
            EnvFrame::Shared(scope) => Ok((**scope).clone()),
            EnvFrame::Slots(_) => Err(RuntimeError::Error(
                "Invalid global scope frame: Slots".to_string(),
            )),
        }
    }

    pub fn lookup(&self, name: &str) -> Result<Value, RuntimeError> {
        self.lookup_rc(name).map(|rc| (**rc).clone())
    }

    pub fn define(&mut self, name: String, val: Value) {
        if let Ok(scope) = self.last_owned_scope_mut() {
            scope.insert(name, Rc::new(val));
        }
    }

    /// O(1) slot-based variable lookup for resolved function bodies.
    pub(super) fn lookup_slot(&self, slot: u16) -> Result<Value, RuntimeError> {
        let idx = self.env.len() - 1;
        match &self.env[idx] {
            EnvFrame::Slots(v) => Ok(v[slot as usize].as_ref().clone()),
            _ => {
                // Fallback — shouldn't happen if resolver is correct
                Err(RuntimeError::Error(
                    "Resolved lookup on non-Slots frame".to_string(),
                ))
            }
        }
    }

    /// Define a value in the current Slots frame at the given slot index.
    pub(super) fn define_slot(&mut self, slot: u16, val: Value) {
        let idx = self.env.len() - 1;
        if let EnvFrame::Slots(v) = &mut self.env[idx] {
            v[slot as usize] = Rc::new(val);
        }
    }

    pub fn define_module_path(&mut self, path: &str, val: Value) -> Result<(), RuntimeError> {
        let parts: Vec<&str> = path.split('.').filter(|s| !s.is_empty()).collect();
        if parts.is_empty() {
            return Err(RuntimeError::Error("Empty module path".to_string()));
        }
        if parts.len() == 1 {
            self.define(parts[0].to_string(), val);
            return Ok(());
        }

        let scope = self.last_owned_scope_mut()?;
        let head = parts[0];
        let tail = &parts[1..];

        if let Some(rc_existing) = scope.remove(head) {
            let existing = Rc::try_unwrap(rc_existing).unwrap_or_else(|rc| (*rc).clone());
            match existing {
                Value::Namespace { name, mut members } => {
                    Self::insert_namespace_path(&mut members, tail, val)?;
                    scope.insert(
                        head.to_string(),
                        Rc::new(Value::Namespace { name, members }),
                    );
                    Ok(())
                }
                _ => Err(RuntimeError::Error(format!(
                    "Cannot mount module '{}': '{}' is not a namespace",
                    parts.join("."),
                    head
                ))),
            }
        } else {
            let mut members = HashMap::new();
            Self::insert_namespace_path(&mut members, tail, val)?;
            scope.insert(
                head.to_string(),
                Rc::new(Value::Namespace {
                    name: head.to_string(),
                    members,
                }),
            );
            Ok(())
        }
    }

    pub(super) fn insert_namespace_path(
        scope: &mut HashMap<String, Value>,
        parts: &[&str],
        val: Value,
    ) -> Result<(), RuntimeError> {
        if parts.len() == 1 {
            scope.insert(parts[0].to_string(), val);
            return Ok(());
        }

        let head = parts[0];
        let tail = &parts[1..];

        if let Some(existing) = scope.remove(head) {
            match existing {
                Value::Namespace { name, mut members } => {
                    Self::insert_namespace_path(&mut members, tail, val)?;
                    scope.insert(head.to_string(), Value::Namespace { name, members });
                    Ok(())
                }
                _ => Err(RuntimeError::Error(format!(
                    "Cannot mount module '{}': '{}' is not a namespace",
                    parts.join("."),
                    head
                ))),
            }
        } else {
            let mut members = HashMap::new();
            Self::insert_namespace_path(&mut members, tail, val)?;
            scope.insert(
                head.to_string(),
                Value::Namespace {
                    name: head.to_string(),
                    members,
                },
            );
            Ok(())
        }
    }

    pub(super) fn module_cache_key(path: &Path) -> String {
        canonicalize_path(path).to_string_lossy().to_string()
    }

    pub(super) fn module_decl(items: &[TopLevel]) -> Option<&Module> {
        items.iter().find_map(|item| {
            if let TopLevel::Module(m) = item {
                Some(m)
            } else {
                None
            }
        })
    }

    pub(super) fn exposed_set(items: &[TopLevel]) -> Option<HashSet<String>> {
        Self::module_decl(items).and_then(|m| {
            if m.exposes.is_empty() {
                None
            } else {
                Some(m.exposes.iter().cloned().collect())
            }
        })
    }

    pub(super) fn cycle_display(loading: &[String], next: &str) -> String {
        let mut chain = loading
            .iter()
            .map(|key| {
                Path::new(key)
                    .file_stem()
                    .and_then(|s| s.to_str())
                    .unwrap_or(key)
                    .to_string()
            })
            .collect::<Vec<_>>();
        chain.push(
            Path::new(next)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(next)
                .to_string(),
        );
        chain.join(" -> ")
    }

    pub fn load_module(
        &mut self,
        name: &str,
        base_dir: &str,
        loading: &mut Vec<String>,
        loading_set: &mut HashSet<String>,
    ) -> Result<Value, RuntimeError> {
        let path = find_module_file(name, base_dir).ok_or_else(|| {
            RuntimeError::Error(format!("Module '{}' not found in '{}'", name, base_dir))
        })?;
        let cache_key = Self::module_cache_key(&path);

        if let Some(cached) = self.module_cache.get(&cache_key) {
            return Ok(cached.clone());
        }

        if loading_set.contains(&cache_key) {
            return Err(RuntimeError::Error(format!(
                "Circular import: {}",
                Self::cycle_display(loading, &cache_key)
            )));
        }

        loading.push(cache_key.clone());
        loading_set.insert(cache_key.clone());
        let result = (|| -> Result<Value, RuntimeError> {
            let src = std::fs::read_to_string(&path).map_err(|e| {
                RuntimeError::Error(format!("Cannot read '{}': {}", path.display(), e))
            })?;
            let mut items = parse_source(&src).map_err(|e| {
                RuntimeError::Error(format!("Parse error in '{}': {}", path.display(), e))
            })?;
            crate::resolver::resolve_program(&mut items);

            if let Some(module) = Self::module_decl(&items) {
                let expected = name.rsplit('.').next().unwrap_or(name);
                if module.name != expected {
                    return Err(RuntimeError::Error(format!(
                        "Module name mismatch: expected '{}' (from '{}'), found '{}' in '{}'",
                        expected,
                        name,
                        module.name,
                        path.display()
                    )));
                }
            }

            let mut sub = Interpreter::new();

            if let Some(module) = Self::module_decl(&items) {
                for dep_name in &module.depends {
                    let dep_ns = self.load_module(dep_name, base_dir, loading, loading_set)?;
                    sub.define_module_path(dep_name, dep_ns)?;
                }
            }

            for item in &items {
                if let TopLevel::EffectSet { name, effects } = item {
                    sub.register_effect_set(name.clone(), effects.clone());
                }
            }
            for item in &items {
                if let TopLevel::TypeDef(td) = item {
                    sub.register_type_def(td);
                }
            }
            for item in &items {
                if let TopLevel::FnDef(fd) = item {
                    sub.exec_fn_def(fd)?;
                }
            }
            let module_globals = Rc::new(sub.global_scope_clone()?);

            let exposed = Self::exposed_set(&items);
            let mut members = HashMap::new();
            for item in &items {
                if let TopLevel::FnDef(fd) = item {
                    let include = match &exposed {
                        Some(set) => set.contains(&fd.name),
                        None => !fd.name.starts_with('_'),
                    };
                    if include {
                        let mut val = sub.lookup(&fd.name).map_err(|_| {
                            RuntimeError::Error(format!("Failed to export '{}.{}'", name, fd.name))
                        })?;
                        if let Value::Fn { home_globals, .. } = &mut val {
                            *home_globals = Some(Rc::clone(&module_globals));
                        }
                        members.insert(fd.name.clone(), val);
                    }
                }
            }

            Ok(Value::Namespace {
                name: name.to_string(),
                members,
            })
        })();
        loading.pop();
        loading_set.remove(&cache_key);

        match result {
            Ok(ns) => {
                self.module_cache.insert(cache_key, ns.clone());
                Ok(ns)
            }
            Err(e) => Err(e),
        }
    }
}
