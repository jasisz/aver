use super::*;

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let mut global = HashMap::new();

        args::register(&mut global);
        console::register(&mut global);
        http::register(&mut global);
        http_server::register(&mut global);
        disk::register(&mut global);
        env::register(&mut global);
        random::register(&mut global);
        tcp::register(&mut global);
        #[cfg(feature = "terminal")]
        terminal::register(&mut global);
        time::register(&mut global);
        bool::register(&mut global);
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
            for (name, builtin_name) in result::extra_members() {
                members.insert(name.to_string(), Value::Builtin(builtin_name));
            }
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
            for (name, builtin_name) in option::extra_members() {
                members.insert(name.to_string(), Value::Builtin(builtin_name));
            }
            global.insert(
                "Option".to_string(),
                Value::Namespace {
                    name: "Option".to_string(),
                    members,
                },
            );
        }

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
        #[cfg(feature = "terminal")]
        record_schemas.insert(
            "Terminal.Size".to_string(),
            vec!["width".to_string(), "height".to_string()],
        );

        Interpreter {
            env: vec![EnvFrame::Owned(global)],
            env_base: 1,
            module_cache: HashMap::new(),
            record_schemas,
            call_stack: Vec::new(),
            active_local_slots: None,
            memo_fns: HashSet::new(),
            memo_cache: HashMap::new(),
            execution_mode: ExecutionMode::Normal,
            recorded_effects: Vec::new(),
            replay_effects: Vec::new(),
            replay_pos: 0,
            validate_replay_args: false,
            recording_sink: None,
            verify_match_coverage: None,
            runtime_policy: None,
            cli_args: Vec::new(),
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

    pub fn configure_recording_sink(&mut self, cfg: RecordingConfig) {
        self.recording_sink = Some(RecordingSink {
            path: cfg.path,
            request_id: cfg.request_id,
            timestamp: cfg.timestamp,
            program_file: cfg.program_file,
            module_root: cfg.module_root,
            entry_fn: cfg.entry_fn,
            input: cfg.input,
        });
    }

    pub fn recording_sink_path(&self) -> Option<std::path::PathBuf> {
        self.recording_sink.as_ref().map(|s| s.path.clone())
    }

    pub fn persist_recording_snapshot(&self, output: RecordedOutcome) -> Result<(), RuntimeError> {
        let Some(sink) = &self.recording_sink else {
            return Ok(());
        };

        let recording = SessionRecording {
            schema_version: 1,
            request_id: sink.request_id.clone(),
            timestamp: sink.timestamp.clone(),
            program_file: sink.program_file.clone(),
            module_root: sink.module_root.clone(),
            entry_fn: sink.entry_fn.clone(),
            input: sink.input.clone(),
            effects: self.recorded_effects.clone(),
            output,
        };

        let json = session_recording_to_string_pretty(&recording);
        std::fs::write(&sink.path, json).map_err(|e| {
            RuntimeError::Error(format!(
                "Cannot write recording '{}': {}",
                sink.path.display(),
                e
            ))
        })?;
        Ok(())
    }

    /// Mark a set of function names as eligible for auto-memoization.
    pub fn enable_memo(&mut self, fns: HashSet<String>) {
        self.memo_fns = fns;
    }

    /// Register a named effect set alias.
    /// Set the runtime policy from an `aver.toml` configuration.
    pub fn set_runtime_policy(&mut self, config: crate::config::ProjectConfig) {
        self.runtime_policy = Some(config);
    }

    /// Set command-line arguments available via `Args.get()`.
    pub fn set_cli_args(&mut self, args: Vec<String>) {
        self.cli_args = args;
    }

    /// Check whether a builtin call is permitted by the runtime policy.
    /// Skipped in Replay mode (deterministic playback).
    pub(super) fn check_runtime_policy(
        &self,
        name: &str,
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        if self.execution_mode == ExecutionMode::Replay {
            return Ok(());
        }
        let Some(policy) = &self.runtime_policy else {
            return Ok(());
        };

        if name.starts_with("Http.") {
            if let Some(Value::Str(url)) = args.first() {
                policy
                    .check_http_host(name, url)
                    .map_err(RuntimeError::Error)?;
            }
        } else if name.starts_with("Disk.") {
            if let Some(Value::Str(path)) = args.first() {
                policy
                    .check_disk_path(name, path)
                    .map_err(RuntimeError::Error)?;
            }
        } else if name.starts_with("Env.")
            && let Some(Value::Str(key)) = args.first()
        {
            policy
                .check_env_key(name, key)
                .map_err(RuntimeError::Error)?;
        }

        Ok(())
    }

    pub fn start_verify_match_coverage(&mut self, fn_name: &str) {
        let Ok(fn_val) = self.lookup(fn_name) else {
            self.verify_match_coverage = None;
            return;
        };
        let Value::Fn(function) = fn_val else {
            self.verify_match_coverage = None;
            return;
        };

        let mut expected = std::collections::BTreeMap::new();
        Self::collect_match_sites_from_fn_body(function.body.as_ref(), &mut expected);
        if expected.is_empty() {
            self.verify_match_coverage = None;
            return;
        }

        self.verify_match_coverage = Some(VerifyMatchCoverageTracker {
            target_fn: fn_name.to_string(),
            expected_arms: expected,
            visited_arms: HashMap::new(),
        });
    }

    pub fn finish_verify_match_coverage(&mut self) -> Vec<VerifyMatchCoverageMiss> {
        let Some(tracker) = self.verify_match_coverage.take() else {
            return vec![];
        };

        let mut misses = Vec::new();
        for ((line, arm_count), expected_total) in tracker.expected_arms {
            let visited = tracker.visited_arms.get(&(line, arm_count));
            let mut missing = Vec::new();
            for arm_idx in 0..expected_total {
                let covered = visited.is_some_and(|set| set.contains(&arm_idx));
                if !covered {
                    missing.push(arm_idx);
                }
            }
            if !missing.is_empty() {
                misses.push(VerifyMatchCoverageMiss {
                    line,
                    total_arms: expected_total,
                    missing_arms: missing,
                });
            }
        }
        misses
    }

    pub(super) fn note_verify_match_arm(&mut self, line: usize, arm_count: usize, arm_idx: usize) {
        let Some(tracker) = self.verify_match_coverage.as_mut() else {
            return;
        };
        let Some(frame) = self.call_stack.last() else {
            return;
        };
        if frame.name.as_str() != tracker.target_fn {
            return;
        }
        let key = (line, arm_count);
        if !tracker.expected_arms.contains_key(&key) {
            return;
        }
        tracker.visited_arms.entry(key).or_default().insert(arm_idx);
    }

    fn collect_match_sites_from_fn_body(
        body: &FnBody,
        out: &mut std::collections::BTreeMap<MatchSiteKey, usize>,
    ) {
        for stmt in body.stmts() {
            Self::collect_match_sites_from_stmt(stmt, out);
        }
    }

    fn collect_match_sites_from_stmt(
        stmt: &Stmt,
        out: &mut std::collections::BTreeMap<MatchSiteKey, usize>,
    ) {
        match stmt {
            Stmt::Binding(_, _, expr) | Stmt::Expr(expr) => {
                Self::collect_match_sites_from_expr(expr, out);
            }
        }
    }

    fn collect_match_sites_from_expr(
        expr: &Expr,
        out: &mut std::collections::BTreeMap<MatchSiteKey, usize>,
    ) {
        match expr {
            Expr::Match {
                subject,
                arms,
                line,
            } => {
                out.insert((*line, arms.len()), arms.len());
                Self::collect_match_sites_from_expr(subject, out);
                for arm in arms {
                    Self::collect_match_sites_from_expr(&arm.body, out);
                }
            }
            Expr::FnCall(fn_expr, args) => {
                Self::collect_match_sites_from_expr(fn_expr, out);
                for arg in args {
                    Self::collect_match_sites_from_expr(arg, out);
                }
            }
            Expr::BinOp(_, left, right) => {
                Self::collect_match_sites_from_expr(left, out);
                Self::collect_match_sites_from_expr(right, out);
            }
            Expr::Attr(obj, _) | Expr::ErrorProp(obj) => {
                Self::collect_match_sites_from_expr(obj, out);
            }
            Expr::Constructor(_, maybe_arg) => {
                if let Some(arg) = maybe_arg {
                    Self::collect_match_sites_from_expr(arg, out);
                }
            }
            Expr::InterpolatedStr(parts) => {
                for part in parts {
                    if let StrPart::Parsed(expr) = part {
                        Self::collect_match_sites_from_expr(expr, out);
                    }
                }
            }
            Expr::List(items) | Expr::Tuple(items) => {
                for item in items {
                    Self::collect_match_sites_from_expr(item, out);
                }
            }
            Expr::MapLiteral(entries) => {
                for (key, value) in entries {
                    Self::collect_match_sites_from_expr(key, out);
                    Self::collect_match_sites_from_expr(value, out);
                }
            }
            Expr::RecordCreate { fields, .. } => {
                for (_, expr) in fields {
                    Self::collect_match_sites_from_expr(expr, out);
                }
            }
            Expr::RecordUpdate { base, updates, .. } => {
                Self::collect_match_sites_from_expr(base, out);
                for (_, expr) in updates {
                    Self::collect_match_sites_from_expr(expr, out);
                }
            }
            Expr::TailCall(boxed) => {
                for arg in &boxed.1 {
                    Self::collect_match_sites_from_expr(arg, out);
                }
            }
            Expr::Literal(_) | Expr::Ident(_) | Expr::Resolved(_) => {}
        }
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
    ) -> Result<&mut HashMap<String, Value>, RuntimeError> {
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

    pub(super) fn lookup_ref(&self, name: &str) -> Result<&Value, RuntimeError> {
        // Current function's frames first (env_base..), then global (env[0]).
        // Caller frames in env[1..env_base] are invisible — skipped entirely.
        let env = &self.env;
        let mut i = env.len();
        let base = self.env_base;
        while i > base {
            i -= 1;
            match &env[i] {
                EnvFrame::Owned(scope) => {
                    if let Some(v) = scope.get(name) {
                        return Ok(v);
                    }
                }
                EnvFrame::Shared(scope) => {
                    if let Some(v) = scope.get(name) {
                        return Ok(v);
                    }
                }
                EnvFrame::Slots(_) => {}
            }
        }
        // Global scope
        match &env[0] {
            EnvFrame::Owned(scope) => scope.get(name),
            EnvFrame::Shared(scope) => scope.get(name),
            EnvFrame::Slots(_) => None,
        }
        .ok_or_else(|| RuntimeError::Error(format!("Undefined variable: '{}'", name)))
    }

    pub(super) fn global_scope_clone(&self) -> Result<HashMap<String, Value>, RuntimeError> {
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
        self.lookup_ref(name).cloned()
    }

    pub fn define(&mut self, name: String, val: Value) {
        if let Ok(scope) = self.last_owned_scope_mut() {
            scope.insert(name, val);
        }
    }

    fn alias_exposed_type_namespaces(&mut self, module_val: &Value) {
        let Value::Namespace { members, .. } = module_val else {
            return;
        };
        for (name, member) in members {
            if matches!(member, Value::Namespace { .. }) {
                self.define(name.clone(), member.clone());
            }
        }
    }

    /// O(1) slot-based variable lookup for resolved function bodies.
    pub(super) fn lookup_slot(&self, slot: u16) -> Result<Value, RuntimeError> {
        let idx = self.env.len() - 1;
        match &self.env[idx] {
            EnvFrame::Slots(v) => Ok(v[slot as usize].clone()),
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
            v[slot as usize] = val;
        }
    }

    pub fn define_module_path(&mut self, path: &str, val: Value) -> Result<(), RuntimeError> {
        let alias_source = val.clone();
        let parts: Vec<&str> = path.split('.').filter(|s| !s.is_empty()).collect();
        if parts.is_empty() {
            return Err(RuntimeError::Error("Empty module path".to_string()));
        }
        if parts.len() == 1 {
            self.define(parts[0].to_string(), val);
            self.alias_exposed_type_namespaces(&alias_source);
            return Ok(());
        }

        let head = parts[0];
        let tail = &parts[1..];

        let result = {
            let scope = self.last_owned_scope_mut()?;
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
        };

        if result.is_ok() {
            self.alias_exposed_type_namespaces(&alias_source);
        }
        result
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
            require_module_declaration(&items, &path.to_string_lossy())
                .map_err(RuntimeError::Error)?;
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
            let opaque_set: HashSet<String> = Self::module_decl(&items)
                .map(|m| m.exposes_opaque.iter().cloned().collect())
                .unwrap_or_default();
            let mut members = HashMap::new();
            for item in &items {
                match item {
                    TopLevel::FnDef(fd) => {
                        let include = match &exposed {
                            Some(set) => set.contains(&fd.name),
                            None => !fd.name.starts_with('_'),
                        };
                        if include {
                            let mut val = sub.lookup(&fd.name).map_err(|_| {
                                RuntimeError::Error(format!(
                                    "Failed to export '{}.{}'",
                                    name, fd.name
                                ))
                            })?;
                            if let Value::Fn(function) = &mut val {
                                Rc::make_mut(function).home_globals =
                                    Some(Rc::clone(&module_globals));
                            }
                            members.insert(fd.name.clone(), val);
                        }
                    }
                    TopLevel::TypeDef(TypeDef::Sum {
                        name: type_name, ..
                    }) => {
                        // Skip opaque sum types — do not expose constructors.
                        if opaque_set.contains(type_name) {
                            continue;
                        }
                        let include = match &exposed {
                            Some(set) => set.contains(type_name),
                            None => !type_name.starts_with('_'),
                        };
                        if include {
                            let val = sub.lookup(type_name).map_err(|_| {
                                RuntimeError::Error(format!(
                                    "Failed to export '{}.{}'",
                                    name, type_name
                                ))
                            })?;
                            members.insert(type_name.clone(), val);
                        }
                    }
                    _ => {}
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
