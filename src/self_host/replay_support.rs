pub mod aver_replay {
    use std::cell::RefCell;
    use std::collections::{BTreeMap, BTreeSet};
    use std::hash::Hash;
    use std::path::{Path, PathBuf};

    use crate::IntoAverStr;
    use serde::{Deserialize, Serialize};
    use serde_json::Value as ReplayJson;

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    #[serde(tag = "kind")]
    pub enum RecordedOutcome {
        #[serde(rename = "value")]
        Value { value: ReplayJson },
        #[serde(rename = "runtime_error")]
        RuntimeError { message: String },
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    pub struct EffectRecord {
        pub seq: u32,
        #[serde(rename = "type")]
        pub effect_type: String,
        pub args: Vec<ReplayJson>,
        pub outcome: RecordedOutcome,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub group_id: Option<u32>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub branch_path: Option<String>,
        #[serde(default, skip_serializing_if = "Option::is_none")]
        pub effect_occurrence: Option<u32>,
    }

    #[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
    pub struct CapabilityProvenance {
        pub capability: String,
        pub contract_hash: String,
        pub model_hash: String,
        pub provider: String,
        pub fingerprint: String,
    }

    #[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
    pub struct SessionRecording {
        pub schema_version: u32,
        pub request_id: String,
        pub timestamp: String,
        pub program_file: String,
        pub module_root: String,
        pub entry_fn: String,
        pub input: ReplayJson,
        #[serde(default, skip_serializing_if = "Vec::is_empty")]
        pub capabilities: Vec<CapabilityProvenance>,
        pub effects: Vec<EffectRecord>,
        pub output: RecordedOutcome,
    }

    pub trait ReplayValue: Sized {
        fn to_replay_json(&self) -> ReplayJson;
        fn from_replay_json(value: &ReplayJson) -> Result<Self, String>;
    }

    pub trait ReplayKey {
        fn replay_string_key(&self) -> Option<String>;
    }

    impl ReplayKey for aver_rt::AverStr {
        fn replay_string_key(&self) -> Option<String> {
            Some(self.to_string())
        }
    }

    impl ReplayKey for aver_rt::AverInt {
        fn replay_string_key(&self) -> Option<String> {
            // Int keys retain their numeric replay shape.  Returning None
            // selects the existing `$map` array-of-pairs encoding instead of
            // coercing the key into a JSON object property that could not be
            // decoded by `ReplayValue for AverInt`.
            None
        }
    }

    impl ReplayValue for () {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Null
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            match value {
                ReplayJson::Null => Ok(()),
                _ => Err("expected null".to_string()),
            }
        }
    }

    impl ReplayValue for bool {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Bool(*self)
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value.as_bool().ok_or_else(|| "expected bool".to_string())
        }
    }

    impl ReplayValue for i64 {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Number((*self).into())
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value.as_i64().ok_or_else(|| "expected int".to_string())
        }
    }

    impl ReplayValue for aver_rt::AverInt {
        fn to_replay_json(&self) -> ReplayJson {
            // Mirror the VM's replay encoding (`src/replay/json.rs`): an
            // `Int` serializes as a JSON Number via `to_i64`, erroring on a
            // value outside the 64-bit JSON range. Replay only captures
            // effect arg / result values, which stay in i64 range.
            let n = self
                .to_i64()
                .expect("cannot serialize an integer outside the 64-bit JSON range");
            ReplayJson::Number(n.into())
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value
                .as_i64()
                .map(aver_rt::AverInt::from_i64)
                .ok_or_else(|| "expected int".to_string())
        }
    }

    impl ReplayValue for f64 {
        fn to_replay_json(&self) -> ReplayJson {
            let number =
                serde_json::Number::from_f64(*self).expect("replay cannot encode non-finite float");
            ReplayJson::Number(number)
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value.as_f64().ok_or_else(|| "expected float".to_string())
        }
    }

    impl ReplayValue for aver_rt::AverStr {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::String(self.to_string())
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            value
                .as_str()
                .map(aver_rt::AverStr::from)
                .ok_or_else(|| "expected string".to_string())
        }
    }

    impl<T: ReplayValue> ReplayValue for std::sync::Arc<T> {
        fn to_replay_json(&self) -> ReplayJson {
            (**self).to_replay_json()
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            Ok(std::sync::Arc::new(T::from_replay_json(value)?))
        }
    }

    impl<T: ReplayValue> ReplayValue for Option<T> {
        fn to_replay_json(&self) -> ReplayJson {
            match self {
                Some(value) => wrap_marker("$some", value.to_replay_json()),
                None => wrap_marker("$none", ReplayJson::Bool(true)),
            }
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            if let Some(payload) = marker_payload(value, "$some") {
                return Ok(Some(T::from_replay_json(payload)?));
            }
            if marker_payload(value, "$none").is_some() {
                return Ok(None);
            }
            Err("expected Option replay marker".to_string())
        }
    }

    impl<T: ReplayValue, E: ReplayValue> ReplayValue for Result<T, E> {
        fn to_replay_json(&self) -> ReplayJson {
            match self {
                Ok(value) => wrap_marker("$ok", value.to_replay_json()),
                Err(value) => wrap_marker("$err", value.to_replay_json()),
            }
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            if let Some(payload) = marker_payload(value, "$ok") {
                return Ok(Ok(T::from_replay_json(payload)?));
            }
            if let Some(payload) = marker_payload(value, "$err") {
                return Ok(Err(E::from_replay_json(payload)?));
            }
            Err("expected Result replay marker".to_string())
        }
    }

    impl<T: ReplayValue> ReplayValue for aver_rt::AverList<T> {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Array(self.iter().map(ReplayValue::to_replay_json).collect())
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            let arr = expect_array(value, "list")?;
            let mut items = Vec::with_capacity(arr.len());
            for item in arr {
                items.push(T::from_replay_json(item)?);
            }
            Ok(aver_rt::AverList::from_vec(items))
        }
    }

    impl ReplayValue for aver_rt::AverIntList {
        fn to_replay_json(&self) -> ReplayJson {
            ReplayJson::Array(
                self.iter_cloned()
                    .map(|value| ReplayValue::to_replay_json(&value))
                    .collect(),
            )
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            let arr = expect_array(value, "list")?;
            let mut items = Vec::with_capacity(arr.len());
            for item in arr {
                items.push(aver_rt::AverInt::from_replay_json(item)?);
            }
            Ok(aver_rt::AverIntList::from_vec(items))
        }
    }

    impl<T: ReplayValue + Clone> ReplayValue for aver_rt::AverVector<T> {
        fn to_replay_json(&self) -> ReplayJson {
            wrap_marker(
                "$vector",
                ReplayJson::Array(self.iter().map(ReplayValue::to_replay_json).collect()),
            )
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            let payload = expect_marker(value, "$vector")?;
            let arr = expect_array(payload, "$vector")?;
            let mut items = Vec::with_capacity(arr.len());
            for item in arr {
                items.push(T::from_replay_json(item)?);
            }
            Ok(aver_rt::AverVector::from_vec(items))
        }
    }

    impl<K, V> ReplayValue for aver_rt::AverMap<K, V>
    where
        K: ReplayValue + ReplayKey + Eq + Hash + Clone,
        V: ReplayValue + Clone,
    {
        fn to_replay_json(&self) -> ReplayJson {
            if self
                .iter()
                .all(|(key, _)| key.replay_string_key().is_some())
            {
                let mut obj = serde_json::Map::new();
                for (key, value) in self.iter() {
                    let key_str = key.replay_string_key().expect("checked above");
                    obj.insert(key_str, value.to_replay_json());
                }
                ReplayJson::Object(obj)
            } else {
                let pairs = self
                    .iter()
                    .map(|(key, value)| {
                        ReplayJson::Array(vec![key.to_replay_json(), value.to_replay_json()])
                    })
                    .collect();
                wrap_marker("$map", ReplayJson::Array(pairs))
            }
        }

        fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
            match value {
                ReplayJson::Object(obj) => {
                    let mut map = aver_rt::AverMap::new();
                    for (key, value) in obj {
                        map = map.insert_owned(
                            K::from_replay_json(&ReplayJson::String(key.clone()))?,
                            V::from_replay_json(value)?,
                        );
                    }
                    Ok(map)
                }
                _ => {
                    let payload = expect_marker(value, "$map")?;
                    let arr = expect_array(payload, "$map")?;
                    let mut map = aver_rt::AverMap::new();
                    for (idx, pair) in arr.iter().enumerate() {
                        let pair_arr = expect_array(pair, &format!("$map[{idx}]"))?;
                        if pair_arr.len() != 2 {
                            return Err(format!("$map[{idx}] must be a 2-element array"));
                        }
                        map = map.insert_owned(
                            K::from_replay_json(&pair_arr[0])?,
                            V::from_replay_json(&pair_arr[1])?,
                        );
                    }
                    Ok(map)
                }
            }
        }
    }

    macro_rules! impl_tuple_replay {
        ($($name:ident : $idx:tt),+) => {
            impl<$($name: ReplayValue),+> ReplayValue for ($($name,)+) {
                fn to_replay_json(&self) -> ReplayJson {
                    wrap_marker(
                        "$tuple",
                        ReplayJson::Array(vec![$(self.$idx.to_replay_json(),)+]),
                    )
                }

                fn from_replay_json(value: &ReplayJson) -> Result<Self, String> {
                    let payload = expect_marker(value, "$tuple")?;
                    let arr = expect_array(payload, "$tuple")?;
                    Ok((
                        $(
                            <$name as ReplayValue>::from_replay_json(
                                arr.get($idx).ok_or_else(|| format!("$tuple missing item {}", $idx))?,
                            )?,
                        )+
                    ))
                }
            }
        };
    }

    impl_tuple_replay!(A: 0, B: 1);
    impl_tuple_replay!(A: 0, B: 1, C: 2);
    impl_tuple_replay!(A: 0, B: 1, C: 2, D: 3);
    impl_tuple_replay!(A: 0, B: 1, C: 2, D: 3, E: 4);
    impl_tuple_replay!(A: 0, B: 1, C: 2, D: 3, E: 4, F: 5);

    #[derive(Clone)]
    enum ScopeMode {
        Normal,
        Record {
            path: PathBuf,
            session: SessionRecording,
        },
        Replay {
            session: SessionRecording,
            position: usize,
            check_args: bool,
        },
    }

    #[derive(Clone)]
    struct ActiveScope {
        mode: ScopeMode,
        guest_args: Option<aver_rt::AverList<crate::AverStr>>,
        runtime_policy: Option<RuntimePolicy>,
        independence_mode_cancel: bool,
        group_stack: Vec<u32>,
        branch_stack: Vec<u32>,
        effect_count_stack: Vec<u32>,
        next_group_id: u32,
        resource_tokens: std::sync::Arc<std::sync::Mutex<ResourceTokenState>>,
    }

    #[derive(Default)]
    struct ResourceTokenState {
        next_trace: u64,
        live: BTreeMap<(u64, String, u64, u64), u64>,
    }

    #[derive(Clone)]
    enum ScopeState {
        Inactive,
        Active(ActiveScope),
    }

    thread_local! {
        static SCOPE_STATE: RefCell<ScopeState> = const { RefCell::new(ScopeState::Inactive) };
    }

    #[derive(Clone)]
    pub struct ParallelScopeContext(Option<ActiveScope>);

    pub fn entry_input(args: Vec<ReplayJson>) -> ReplayJson {
        match args.len() {
            0 => ReplayJson::Null,
            1 => args.into_iter().next().expect("single input"),
            _ => ReplayJson::Array(args),
        }
    }

    pub fn with_guest_scope<T, F>(entry_fn: &str, input: ReplayJson, run: F) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        with_guest_scope_args_inner(entry_fn, input, None, run)
    }

    pub fn with_guest_scope_result<T, E, F>(
        entry_fn: &str,
        input: ReplayJson,
        run: F,
    ) -> Result<T, E>
    where
        T: ReplayValue,
        E: aver_rt::AverDisplay,
        F: FnOnce() -> Result<T, E>,
    {
        with_guest_scope_result_args_inner(entry_fn, input, None, run)
    }

    pub fn with_guest_scope_args<T, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: aver_rt::AverList<crate::AverStr>,
        run: F,
    ) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        with_guest_scope_args_inner(entry_fn, input, Some(guest_args), run)
    }

    pub fn with_guest_scope_args_result<T, E, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: aver_rt::AverList<crate::AverStr>,
        run: F,
    ) -> Result<T, E>
    where
        T: ReplayValue,
        E: aver_rt::AverDisplay,
        F: FnOnce() -> Result<T, E>,
    {
        with_guest_scope_result_args_inner(entry_fn, input, Some(guest_args), run)
    }

    fn with_guest_scope_args_inner<T, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: Option<aver_rt::AverList<crate::AverStr>>,
        run: F,
    ) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        if scope_is_active() {
            return run();
        }

        let mode = load_scope_mode(entry_fn, input.clone());
        activate_scope(mode.clone(), guest_args);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
        match result {
            Ok(value) => {
                finish_scope_success(&value);
                clear_scope();
                value
            }
            Err(payload) => {
                let panic_message = panic_payload_to_string(&payload);
                finish_scope_panic(&panic_message);
                clear_scope();
                std::panic::resume_unwind(payload);
            }
        }
    }

    fn with_guest_scope_result_args_inner<T, E, F>(
        entry_fn: &str,
        input: ReplayJson,
        guest_args: Option<aver_rt::AverList<crate::AverStr>>,
        run: F,
    ) -> Result<T, E>
    where
        T: ReplayValue,
        E: aver_rt::AverDisplay,
        F: FnOnce() -> Result<T, E>,
    {
        if scope_is_active() {
            return run();
        }

        let mode = load_scope_mode(entry_fn, input.clone());
        activate_scope(mode.clone(), guest_args);

        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
        match result {
            Ok(Ok(value)) => {
                finish_scope_success(&value);
                clear_scope();
                Ok(value)
            }
            Ok(Err(err)) => {
                finish_scope_error(entry_fn, &err);
                clear_scope();
                Err(err)
            }
            Err(payload) => {
                let panic_message = panic_payload_to_string(&payload);
                finish_scope_panic(&panic_message);
                clear_scope();
                std::panic::resume_unwind(payload);
            }
        }
    }

    pub fn current_cli_args() -> aver_rt::AverList<crate::AverStr> {
        SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => aver_rt::cli_args().into_aver(),
            ScopeState::Active(scope) => match &scope.guest_args {
                Some(args) => args.clone(),
                None => aver_rt::cli_args().into_aver(),
            },
        })
    }

    pub fn is_record_mode() -> bool {
        matches!(current_scope_mode(), Some(ScopeMode::Record { .. }))
    }

    fn embedded_independence_mode_is_cancel() -> bool {
        false
    }

    pub fn independence_mode_is_cancel() -> bool {
        SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => embedded_independence_mode_is_cancel(),
            ScopeState::Active(scope) => scope.independence_mode_cancel,
        })
    }

    /// True when effects are being recorded or replayed — callers should
    /// execute ?!/! elements sequentially so effect tracking works correctly.
    pub fn is_effect_tracking_active() -> bool {
        matches!(
            current_scope_mode(),
            Some(ScopeMode::Record { .. } | ScopeMode::Replay { .. })
        )
    }

    pub fn capture_parallel_scope_context() -> ParallelScopeContext {
        SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => ParallelScopeContext(None),
            ScopeState::Active(scope) => ParallelScopeContext(Some(scope.clone())),
        })
    }

    pub fn with_parallel_scope_context<T, F>(context: ParallelScopeContext, run: F) -> T
    where
        F: FnOnce() -> T,
    {
        let previous = SCOPE_STATE.with(|cell| {
            std::mem::replace(
                &mut *cell.borrow_mut(),
                match context.0 {
                    Some(scope) => ScopeState::Active(scope),
                    None => ScopeState::Inactive,
                },
            )
        });
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(run));
        SCOPE_STATE.with(|cell| {
            *cell.borrow_mut() = previous;
        });
        match result {
            Ok(value) => value,
            Err(payload) => std::panic::resume_unwind(payload),
        }
    }

    pub fn enter_effect_group() {
        SCOPE_STATE.with(|cell| {
            if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                scope.next_group_id += 1;
                scope.group_stack.push(scope.next_group_id);
                scope.branch_stack.push(0);
                scope.effect_count_stack.push(0);
            }
        });
    }

    pub fn exit_effect_group() {
        SCOPE_STATE.with(|cell| {
            if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                scope.group_stack.pop();
                scope.branch_stack.pop();
                scope.effect_count_stack.pop();
            }
        });
    }

    pub fn set_effect_branch(index: u32) {
        SCOPE_STATE.with(|cell| {
            if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                if let Some(last) = scope.branch_stack.last_mut() {
                    *last = index;
                }
                if let Some(last) = scope.effect_count_stack.last_mut() {
                    *last = 0;
                }
            }
        });
    }

    fn current_branch_path(branch_stack: &[u32]) -> Option<String> {
        if branch_stack.is_empty() {
            None
        } else {
            Some(
                branch_stack
                    .iter()
                    .map(|i| i.to_string())
                    .collect::<Vec<_>>()
                    .join("."),
            )
        }
    }

    fn current_effect_occurrence(effect_count_stack: &[u32]) -> Option<u32> {
        effect_count_stack.last().copied()
    }

    fn bump_effect_occurrence(effect_count_stack: &mut Vec<u32>) {
        if let Some(last) = effect_count_stack.last_mut() {
            *last += 1;
        }
    }

    pub fn invoke_effect<T, F>(effect_type: &str, args: Vec<ReplayJson>, call: F) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        let mode = current_scope_mode();
        match mode {
            None => call(),
            Some(ScopeMode::Normal) => {
                check_policy(effect_type, &args);
                call()
            }
            Some(ScopeMode::Record { .. }) => {
                check_policy(effect_type, &args);
                let result = call();
                let outcome = RecordedOutcome::Value {
                    value: result.to_replay_json(),
                };
                SCOPE_STATE.with(|cell| {
                    if let ScopeState::Active(scope) = &mut *cell.borrow_mut() {
                        let group_id = scope.group_stack.last().copied();
                        if let ScopeMode::Record { session, .. } = &mut scope.mode {
                            let seq = session.effects.len() as u32 + 1;
                            session.effects.push(EffectRecord {
                                seq,
                                effect_type: effect_type.to_string(),
                                args,
                                outcome,
                                group_id,
                                branch_path: current_branch_path(&scope.branch_stack),
                                effect_occurrence: current_effect_occurrence(
                                    &scope.effect_count_stack,
                                ),
                            });
                            bump_effect_occurrence(&mut scope.effect_count_stack);
                        }
                    }
                });
                result
            }
            Some(ScopeMode::Replay { .. }) => replay_effect(effect_type, args),
        }
    }

    pub fn invoke_capability_effect<T, F>(
        effect_type: &str,
        replay: &str,
        args: Vec<ReplayJson>,
        call: F,
    ) -> T
    where
        T: ReplayValue,
        F: FnOnce() -> T,
    {
        match replay {
            "recorded" | "suppressed" => invoke_effect(effect_type, args, call),
            "reissued" if matches!(current_scope_mode(), Some(ScopeMode::Replay { .. })) => {
                let _: T = replay_effect(effect_type, args);
                call()
            }
            "reissued" => invoke_effect(effect_type, args, call),
            other => panic!("unknown capability replay semantics '{}'", other),
        }
    }

    fn capability_resource_json(type_name: &str, trace: u64) -> ReplayJson {
        let mut payload = serde_json::Map::new();
        payload.insert(
            "type".to_string(),
            ReplayJson::String(type_name.to_string()),
        );
        payload.insert("trace".to_string(), ReplayJson::String(trace.to_string()));
        wrap_marker("$capabilityResource", ReplayJson::Object(payload))
    }

    pub fn encode_live_capability_resource(
        type_name: &str,
        handle: &aver_rt::provider::ProviderResourceHandle,
    ) -> ReplayJson {
        let trace = SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Active(scope) => {
                let mut tokens = scope
                    .resource_tokens
                    .lock()
                    .unwrap_or_else(|_| panic!("capability replay resource-token store poisoned"));
                let key = (
                    handle.binding_id(),
                    handle.type_name().to_string(),
                    handle.slot(),
                    handle.generation(),
                );
                if let Some(trace) = tokens.live.get(&key) {
                    *trace
                } else {
                    let trace = tokens.next_trace;
                    tokens.next_trace = tokens
                        .next_trace
                        .checked_add(1)
                        .expect("capability replay resource-token space exhausted");
                    tokens.live.insert(key, trace);
                    trace
                }
            }
            ScopeState::Inactive => handle.slot().saturating_add(1),
        });
        capability_resource_json(type_name, trace)
    }

    pub fn encode_replay_capability_resource(type_name: &str, trace: u64) -> ReplayJson {
        capability_resource_json(type_name, trace)
    }

    pub fn decode_capability_resource(
        value: &ReplayJson,
        expected_type: &str,
    ) -> Result<u64, String> {
        let payload = expect_marker(value, "$capabilityResource")?;
        let obj = expect_object(payload, "$capabilityResource")?;
        let type_name = expect_string(
            obj.get("type")
                .ok_or_else(|| "$capabilityResource missing field 'type'".to_string())?,
            "$capabilityResource.type",
        )?;
        if type_name != expected_type {
            return Err(format!(
                "$capabilityResource type mismatch: expected {}, got {}",
                expected_type, type_name
            ));
        }
        let trace = expect_string(
            obj.get("trace")
                .ok_or_else(|| "$capabilityResource missing field 'trace'".to_string())?,
            "$capabilityResource.trace",
        )?
        .parse::<u64>()
        .map_err(|_| "$capabilityResource.trace must be a u64 string".to_string())?;
        if trace == 0 {
            return Err("$capabilityResource.trace must be non-zero".to_string());
        }
        Ok(trace)
    }

    #[derive(Clone, Debug, Default)]
    struct RuntimeEffectPolicy {
        hosts: Vec<String>,
        paths: Vec<String>,
        keys: Vec<String>,
    }

    #[derive(Clone, Debug)]
    struct RuntimePolicy {
        effect_policies: BTreeMap<String, RuntimeEffectPolicy>,
        tcp_connect_timeout_secs: u64,
        tcp_request_idle_timeout_secs: u64,
        tcp_max_connections: usize,
        independence_mode_cancel: bool,
    }

    impl Default for RuntimePolicy {
        fn default() -> Self {
            Self {
                effect_policies: BTreeMap::new(),
                tcp_connect_timeout_secs: aver_rt::tcp::DEFAULT_CONNECT_TIMEOUT_SECS,
                tcp_request_idle_timeout_secs: aver_rt::tcp::DEFAULT_REQUEST_IDLE_TIMEOUT_SECS,
                tcp_max_connections: aver_rt::tcp::DEFAULT_MAX_CONNECTIONS,
                independence_mode_cancel: false,
            }
        }
    }

    impl RuntimePolicy {
        fn load_from_dir(dir: &Path) -> Result<Option<Self>, String> {
            let path = dir.join("aver.toml");
            let content = match std::fs::read_to_string(&path) {
                Ok(content) => content,
                Err(err) if err.kind() == std::io::ErrorKind::NotFound => return Ok(None),
                Err(err) => return Err(format!("Failed to read {}: {}", path.display(), err)),
            };
            Self::parse(&content).map(Some)
        }

        fn parse(content: &str) -> Result<Self, String> {
            let table: toml::Table = content
                .parse()
                .map_err(|err: toml::de::Error| format!("aver.toml parse error: {}", err))?;

            let mut effect_policies = BTreeMap::new();
            let mut tcp_connect_timeout_secs = aver_rt::tcp::DEFAULT_CONNECT_TIMEOUT_SECS;
            let mut tcp_request_idle_timeout_secs = aver_rt::tcp::DEFAULT_REQUEST_IDLE_TIMEOUT_SECS;
            let mut tcp_max_connections = aver_rt::tcp::DEFAULT_MAX_CONNECTIONS;
            if let Some(toml::Value::Table(effects_table)) = table.get("effects") {
                for (name, value) in effects_table {
                    let section = value
                        .as_table()
                        .ok_or_else(|| format!("aver.toml: [effects.{}] must be a table", name))?;
                    validate_effect_section_keys(name, section)?;
                    if name == "Tcp" {
                        tcp_connect_timeout_secs = parse_positive_timeout_secs(
                            name,
                            section,
                            "connect_timeout_secs",
                            tcp_connect_timeout_secs,
                        )?;
                        tcp_request_idle_timeout_secs = parse_positive_timeout_secs(
                            name,
                            section,
                            "request_idle_timeout_secs",
                            tcp_request_idle_timeout_secs,
                        )?;
                        tcp_max_connections = parse_positive_usize(
                            name,
                            section,
                            "max_connections",
                            tcp_max_connections,
                        )?;
                    }
                    let hosts = parse_policy_list(section, "hosts", name)?;
                    for (index, host) in hosts.iter().enumerate() {
                        validate_host_pattern(name, index, host)?;
                    }
                    let paths = parse_policy_list(section, "paths", name)?;
                    for (index, path) in paths.iter().enumerate() {
                        validate_path_pattern(name, index, path)?;
                    }
                    let keys = parse_policy_list(section, "keys", name)?;
                    for (index, key) in keys.iter().enumerate() {
                        validate_env_key_pattern(name, index, key)?;
                    }
                    effect_policies
                        .insert(name.clone(), RuntimeEffectPolicy { hosts, paths, keys });
                }
            }

            let independence_mode_cancel = table
                .get("independence")
                .and_then(|value| value.as_table())
                .and_then(|section| section.get("mode"))
                .map(|value| {
                    value.as_str().ok_or_else(|| {
                        "aver.toml: [independence].mode must be a string".to_string()
                    })
                })
                .transpose()?
                .map(|mode| match mode {
                    "complete" => Ok(false),
                    "cancel" => Ok(true),
                    other => Err(format!(
                        "aver.toml: [independence].mode must be 'complete' or 'cancel', got '{}'",
                        other
                    )),
                })
                .transpose()?
                .unwrap_or(false);

            Ok(Self {
                effect_policies,
                tcp_connect_timeout_secs,
                tcp_request_idle_timeout_secs,
                tcp_max_connections,
                independence_mode_cancel,
            })
        }

        fn check_http(&self, method_name: &str, url_str: &str) -> Result<(), String> {
            let Some(policy) = self.find_policy(method_name) else {
                return Ok(());
            };
            if policy.hosts.is_empty() {
                return Ok(());
            }
            let parsed = url::Url::parse(url_str).map_err(|err| {
                format!(
                    "{} denied by aver.toml: invalid URL '{}': {}",
                    method_name, url_str, err
                )
            })?;
            let host = parsed.host_str().unwrap_or("");
            for allowed in &policy.hosts {
                if host_matches(host, allowed) {
                    return Ok(());
                }
            }
            Err(format!(
                "{} to '{}' denied by aver.toml policy (host '{}' not in allowed list)",
                method_name, url_str, host
            ))
        }

        fn check_disk(&self, method_name: &str, path_str: &str) -> Result<(), String> {
            let Some(policy) = self.find_policy(method_name) else {
                return Ok(());
            };
            if policy.paths.is_empty() {
                return Ok(());
            }
            let normalized = normalize_path(path_str);
            for allowed in &policy.paths {
                if path_matches(&normalized, allowed) {
                    return Ok(());
                }
            }
            Err(format!(
                "{} on '{}' denied by aver.toml policy (path not in allowed list)",
                method_name, path_str
            ))
        }

        fn check_env(&self, method_name: &str, key: &str) -> Result<(), String> {
            let Some(policy) = self.find_policy(method_name) else {
                return Ok(());
            };
            if policy.keys.is_empty() {
                return Ok(());
            }
            for allowed in &policy.keys {
                if env_key_matches(key, allowed) {
                    return Ok(());
                }
            }
            Err(format!(
                "{} on '{}' denied by aver.toml policy (key not in allowed list)",
                method_name, key
            ))
        }

        fn find_policy(&self, method_name: &str) -> Option<&RuntimeEffectPolicy> {
            let namespace = method_name.split('.').next().unwrap_or(method_name);
            self.effect_policies
                .get(method_name)
                .or_else(|| self.effect_policies.get(namespace))
        }
    }

    fn validate_effect_section_keys(effect: &str, section: &toml::Table) -> Result<(), String> {
        let namespace = effect.split('.').next().unwrap_or(effect);
        let allowed: &[&str] = match (namespace, effect) {
            ("Http", _) => &["hosts"],
            ("Disk", _) => &["paths"],
            ("Env", _) => &["keys"],
            ("Tcp", "Tcp") => &[
                "connect_timeout_secs",
                "request_idle_timeout_secs",
                "max_connections",
            ],
            ("Tcp", _) => &[],
            _ => &[],
        };
        for key in section.keys() {
            if !allowed.contains(&key.as_str()) {
                let expected = if namespace == "Tcp" && effect != "Tcp" {
                    "Tcp timeout settings belong under [effects.Tcp], not a method section"
                        .to_string()
                } else if allowed.is_empty() {
                    format!("[effects.{effect}] has no supported settings")
                } else {
                    format!("expected one of: {}", allowed.join(", "))
                };
                return Err(format!(
                    "aver.toml: unknown or misplaced key [effects.{effect}].{key}; {expected}"
                ));
            }
        }
        Ok(())
    }

    fn parse_positive_timeout_secs(
        effect: &str,
        section: &toml::Table,
        key: &str,
        default: u64,
    ) -> Result<u64, String> {
        let Some(value) = section.get(key) else {
            return Ok(default);
        };
        let seconds = value.as_integer().ok_or_else(|| {
            format!("aver.toml: [effects.{effect}].{key} must be a positive integer")
        })?;
        u64::try_from(seconds)
            .ok()
            .filter(|seconds| *seconds > 0)
            .ok_or_else(|| format!("aver.toml: [effects.{effect}].{key} must be greater than zero"))
    }

    fn parse_positive_usize(
        effect: &str,
        section: &toml::Table,
        key: &str,
        default: usize,
    ) -> Result<usize, String> {
        let Some(value) = section.get(key) else {
            return Ok(default);
        };
        let number = value.as_integer().ok_or_else(|| {
            format!("aver.toml: [effects.{effect}].{key} must be a positive integer")
        })?;
        usize::try_from(number)
            .ok()
            .filter(|number| *number > 0)
            .ok_or_else(|| format!("aver.toml: [effects.{effect}].{key} must be greater than zero"))
    }

    fn parse_policy_list(
        section: &toml::Table,
        key: &str,
        name: &str,
    ) -> Result<Vec<String>, String> {
        let Some(value) = section.get(key) else {
            return Ok(Vec::new());
        };
        let arr = value
            .as_array()
            .ok_or_else(|| format!("aver.toml: [effects.{}].{} must be an array", name, key))?;
        arr.iter()
            .enumerate()
            .map(|(idx, value)| {
                value.as_str().map(|item| item.to_string()).ok_or_else(|| {
                    format!(
                        "aver.toml: [effects.{}].{}[{}] must be a string",
                        name, key, idx
                    )
                })
            })
            .collect()
    }

    fn validate_path_pattern(effect: &str, index: usize, raw: &str) -> Result<(), String> {
        let prefix = format!("aver.toml: [effects.{effect}].paths[{index}]");
        if raw.is_empty() {
            return Err(format!(
                "{prefix} is empty; use \".\" for the project directory or \"/\" for the filesystem root"
            ));
        }
        if raw == "**" {
            return Err(format!(
                "{prefix} is ambiguous: '**'; use \"./**\" for the project subtree or \"/**\" for the filesystem root"
            ));
        }

        let body = match raw.strip_suffix("/**") {
            Some("") => "/",
            Some(base) => base,
            None => raw,
        };
        if body.contains('*') {
            return Err(format!(
                "{prefix} contains an unsupported glob '{raw}'; only a trailing \"/**\" is supported (for example, \"./data/**\")"
            ));
        }

        let base = normalize_path(body);
        if base == ".." || base.starts_with("../") {
            return Err(format!(
                "{prefix} escapes the project directory: '{raw}'; use an absolute pattern (for example, \"/srv/data/**\") to allow files outside the project"
            ));
        }

        Ok(())
    }

    fn validate_host_pattern(effect: &str, index: usize, raw: &str) -> Result<(), String> {
        if raw == "*" || raw == "**" {
            return Err(format!(
                "aver.toml: [effects.{effect}].hosts[{index}] contains an unsupported wildcard '{raw}'; use an exact host or a subdomain wildcard such as \"*.example.com\""
            ));
        }
        Ok(())
    }

    fn validate_env_key_pattern(effect: &str, index: usize, raw: &str) -> Result<(), String> {
        if raw == "**" {
            return Err(format!(
                "aver.toml: [effects.{effect}].keys[{index}] contains an unsupported wildcard '**'; use \"*\" for every key or a prefix wildcard such as \"APP_*\""
            ));
        }
        Ok(())
    }

    fn check_policy(effect_type: &str, args: &[ReplayJson]) {
        let policy = SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => None,
            ScopeState::Active(scope) => scope.runtime_policy.as_ref().cloned(),
        });
        let Some(policy) = policy else {
            return;
        };

        match (
            effect_type.split('.').next(),
            args.first().and_then(|value| value.as_str()),
        ) {
            (Some("Http"), Some(url)) => {
                policy
                    .check_http(effect_type, url)
                    .expect("aver.toml policy violation");
            }
            (Some("Disk"), Some(path)) => {
                policy
                    .check_disk(effect_type, path)
                    .expect("aver.toml policy violation");
            }
            (Some("Env"), Some(key)) => {
                policy
                    .check_env(effect_type, key)
                    .expect("aver.toml policy violation");
            }
            _ => {}
        }
    }

    fn load_runtime_policy_from_env() -> Result<Option<RuntimePolicy>, String> {
        let module_root = env_var("AVER_REPLAY_MODULE_ROOT").unwrap_or_else(|| ".".to_string());
        RuntimePolicy::load_from_dir(Path::new(&module_root))
            .map_err(|err| format!("aver.toml: {}", err))
    }

    pub(crate) fn tcp_provider_settings_from_env() -> Result<aver_rt::tcp::TcpSettings, String> {
        let policy = load_runtime_policy_from_env()?.unwrap_or_default();
        aver_rt::tcp::TcpSettings::from_policy(
            policy.tcp_connect_timeout_secs,
            policy.tcp_request_idle_timeout_secs,
            policy.tcp_max_connections,
        )
    }

    fn host_matches(host: &str, pattern: &str) -> bool {
        if pattern == host {
            return true;
        }
        if let Some(suffix) = pattern.strip_prefix("*.") {
            host.ends_with(suffix)
                && host.len() > suffix.len()
                && host.as_bytes()[host.len() - suffix.len() - 1] == b'.'
        } else {
            false
        }
    }

    fn normalize_path(path: &str) -> String {
        use std::path::Component;

        let mut components: Vec<String> = Vec::new();
        let mut is_absolute = false;

        for component in Path::new(path).components() {
            match component {
                Component::RootDir => {
                    is_absolute = true;
                    components.clear();
                }
                Component::CurDir => {}
                Component::ParentDir => {
                    if components.last().is_some_and(|item| item != "..") {
                        components.pop();
                    } else if !is_absolute {
                        components.push("..".to_string());
                    }
                }
                Component::Normal(segment) => {
                    components.push(segment.to_string_lossy().to_string());
                }
                Component::Prefix(prefix) => {
                    components.push(prefix.as_os_str().to_string_lossy().to_string());
                }
            }
        }

        let joined = components.join("/");
        if is_absolute {
            format!("/{}", joined)
        } else {
            joined
        }
    }

    fn path_matches(normalized: &str, pattern: &str) -> bool {
        if pattern.is_empty() || pattern == "**" {
            return false;
        }

        let body = match pattern.strip_suffix("/**") {
            Some("") => "/",
            Some(base) => base,
            None => pattern,
        };
        if body.contains('*') {
            return false;
        }

        let base = normalize_path(body);
        if base.is_empty() {
            return !normalized.starts_with('/')
                && normalized != ".."
                && !normalized.starts_with("../");
        }
        if base == "/" {
            return normalized.starts_with('/');
        }
        if base == ".." || base.starts_with("../") {
            return false;
        }

        normalized == base
            || (normalized.len() > base.len()
                && normalized.starts_with(&base)
                && normalized.as_bytes()[base.len()] == b'/')
    }

    fn env_key_matches(key: &str, pattern: &str) -> bool {
        if pattern == key {
            return true;
        }
        if let Some(prefix) = pattern.strip_suffix('*') {
            key.starts_with(prefix)
        } else {
            false
        }
    }

    pub fn wrap_marker(name: &str, value: ReplayJson) -> ReplayJson {
        let mut obj = serde_json::Map::new();
        obj.insert(name.to_string(), value);
        ReplayJson::Object(obj)
    }

    pub fn expect_marker<'a>(
        value: &'a ReplayJson,
        marker: &str,
    ) -> Result<&'a ReplayJson, String> {
        marker_payload(value, marker).ok_or_else(|| format!("expected replay marker '{}'", marker))
    }

    pub fn expect_object<'a>(
        value: &'a ReplayJson,
        path: &str,
    ) -> Result<&'a serde_json::Map<String, ReplayJson>, String> {
        match value {
            ReplayJson::Object(obj) => Ok(obj),
            _ => Err(format!("{} must be an object", path)),
        }
    }

    pub fn expect_array<'a>(
        value: &'a ReplayJson,
        path: &str,
    ) -> Result<&'a Vec<ReplayJson>, String> {
        match value {
            ReplayJson::Array(arr) => Ok(arr),
            _ => Err(format!("{} must be an array", path)),
        }
    }

    pub fn expect_string<'a>(value: &'a ReplayJson, path: &str) -> Result<&'a str, String> {
        value
            .as_str()
            .ok_or_else(|| format!("{} must be a string", path))
    }

    fn marker_payload<'a>(value: &'a ReplayJson, marker: &str) -> Option<&'a ReplayJson> {
        match value {
            ReplayJson::Object(obj) if obj.len() == 1 => obj.get(marker),
            _ => None,
        }
    }

    fn scope_is_active() -> bool {
        SCOPE_STATE.with(|cell| matches!(*cell.borrow(), ScopeState::Active(_)))
    }

    fn current_scope_mode() -> Option<ScopeMode> {
        SCOPE_STATE.with(|cell| match &*cell.borrow() {
            ScopeState::Inactive => None,
            ScopeState::Active(scope) => Some(scope.mode.clone()),
        })
    }

    fn activate_scope(mode: ScopeMode, guest_args: Option<aver_rt::AverList<crate::AverStr>>) {
        let runtime_policy = match &mode {
            ScopeMode::Replay { .. } => None,
            ScopeMode::Normal | ScopeMode::Record { .. } => {
                load_runtime_policy_from_env().unwrap_or_else(|err| panic!("{}", err))
            }
        };
        let independence_mode_cancel = runtime_policy
            .as_ref()
            .map_or(embedded_independence_mode_is_cancel(), |policy| {
                policy.independence_mode_cancel
            });
        SCOPE_STATE.with(|cell| {
            *cell.borrow_mut() = ScopeState::Active(ActiveScope {
                mode,
                guest_args,
                runtime_policy,
                independence_mode_cancel,
                group_stack: Vec::new(),
                branch_stack: Vec::new(),
                effect_count_stack: Vec::new(),
                next_group_id: 0,
                resource_tokens: std::sync::Arc::new(std::sync::Mutex::new(ResourceTokenState {
                    next_trace: 1,
                    live: BTreeMap::new(),
                })),
            });
        });
    }

    fn clear_scope() {
        SCOPE_STATE.with(|cell| {
            *cell.borrow_mut() = ScopeState::Inactive;
        });
    }

    fn replay_entry_name(entry_fn: &str) -> String {
        env_var("AVER_REPLAY_ENTRY_FN").unwrap_or_else(|| entry_fn.to_string())
    }

    fn load_scope_mode(entry_fn: &str, input: ReplayJson) -> ScopeMode {
        let logical_entry_fn = replay_entry_name(entry_fn);
        let record_path = env_var("AVER_REPLAY_RECORD");
        let replay_path = env_var("AVER_REPLAY_REPLAY");
        if record_path.is_some() && replay_path.is_some() {
            panic!("AVER_REPLAY_RECORD and AVER_REPLAY_REPLAY cannot both be set");
        }

        if let Some(path) = replay_path {
            let raw = std::fs::read_to_string(&path)
                .unwrap_or_else(|e| panic!("Cannot read replay recording '{}': {}", path, e));
            let session: SessionRecording = serde_json::from_str(&raw)
                .unwrap_or_else(|e| panic!("Invalid replay recording '{}': {}", path, e));
            if session.entry_fn != logical_entry_fn {
                panic!(
                    "Replay entry mismatch: recording expects '{}', generated guest scope is '{}'",
                    session.entry_fn, logical_entry_fn
                );
            }
            validate_capability_provenance(&session);
            return ScopeMode::Replay {
                session,
                position: 0,
                check_args: env_flag("AVER_REPLAY_CHECK_ARGS"),
            };
        }

        if let Some(path) = record_path {
            let request_id = env_var("AVER_REPLAY_REQUEST_ID").unwrap_or_else(default_request_id);
            let timestamp = env_var("AVER_REPLAY_TIMESTAMP").unwrap_or_else(default_timestamp);
            let program_file = env_var("AVER_REPLAY_PROGRAM_FILE").unwrap_or_default();
            let module_root = env_var("AVER_REPLAY_MODULE_ROOT").unwrap_or_else(|| ".".to_string());
            return ScopeMode::Record {
                path: PathBuf::from(path),
                session: SessionRecording {
                    schema_version: 1,
                    request_id,
                    timestamp,
                    program_file,
                    module_root,
                    entry_fn: logical_entry_fn,
                    input,
                    capabilities: standard_capability_provenance(),
                    effects: Vec::new(),
                    output: RecordedOutcome::Value {
                        value: ReplayJson::Null,
                    },
                },
            };
        }

        ScopeMode::Normal
    }

    fn standard_capability_provenance() -> Vec<CapabilityProvenance> {
        crate::provider_support::registry()
            .provenance()
            .into_iter()
            .map(|entry| CapabilityProvenance {
                capability: entry.capability,
                contract_hash: entry.contract_hash,
                model_hash: entry.model_hash,
                provider: entry.provider,
                fingerprint: entry.fingerprint,
            })
            .collect()
    }

    fn validate_capability_provenance(session: &SessionRecording) {
        let current = crate::provider_support::registry().contract_provenance();
        let mut seen = BTreeSet::new();
        for recorded in &session.capabilities {
            if !seen.insert(recorded.capability.as_str()) {
                panic!(
                    "Replay contains duplicate capability provenance for '{}'",
                    recorded.capability
                );
            }
            let Some(expected) = current
                .iter()
                .find(|entry| entry.capability == recorded.capability)
            else {
                panic!(
                    "Replay names capability '{}' which this Rust artifact does not declare",
                    recorded.capability
                );
            };
            if recorded.contract_hash != expected.contract_hash {
                panic!(
                    "Replay contract mismatch for '{}': recorded {}, current {}",
                    recorded.capability, recorded.contract_hash, expected.contract_hash
                );
            }
            if recorded.model_hash != expected.model_hash {
                panic!(
                    "Replay model mismatch for '{}': recorded {}, current {}",
                    recorded.capability, recorded.model_hash, expected.model_hash
                );
            }
        }
        for effect in &session.effects {
            let capability = match effect.effect_type.as_str() {
                "Args.get" => Some("Args"),
                "Console.error" => Some("Console"),
                "Console.print" => Some("Console"),
                "Console.readLine" => Some("Console"),
                "Console.warn" => Some("Console"),
                "Disk.appendBytes" => Some("Disk"),
                "Disk.appendText" => Some("Disk"),
                "Disk.delete" => Some("Disk"),
                "Disk.deleteDir" => Some("Disk"),
                "Disk.exists" => Some("Disk"),
                "Disk.listDir" => Some("Disk"),
                "Disk.makeDir" => Some("Disk"),
                "Disk.readBytes" => Some("Disk"),
                "Disk.readBytesAt" => Some("Disk"),
                "Disk.readText" => Some("Disk"),
                "Disk.size" => Some("Disk"),
                "Disk.sync" => Some("Disk"),
                "Disk.writeBytes" => Some("Disk"),
                "Disk.writeText" => Some("Disk"),
                "Env.get" => Some("Env"),
                "Env.set" => Some("Env"),
                "Http.delete" => Some("Http"),
                "Http.get" => Some("Http"),
                "Http.head" => Some("Http"),
                "Http.patch" => Some("Http"),
                "Http.post" => Some("Http"),
                "Http.put" => Some("Http"),
                "Random.float" => Some("Random"),
                "Random.int" => Some("Random"),
                "Tcp.accept" => Some("Tcp"),
                "Tcp.beginConnect" => Some("Tcp"),
                "Tcp.close" => Some("Tcp"),
                "Tcp.closeDial" => Some("Tcp"),
                "Tcp.closeListener" => Some("Tcp"),
                "Tcp.connect" => Some("Tcp"),
                "Tcp.dialled" => Some("Tcp"),
                "Tcp.listen" => Some("Tcp"),
                "Tcp.peerAddress" => Some("Tcp"),
                "Tcp.ping" => Some("Tcp"),
                "Tcp.poll" => Some("Tcp"),
                "Tcp.readBytes" => Some("Tcp"),
                "Tcp.readLine" => Some("Tcp"),
                "Tcp.readSome" => Some("Tcp"),
                "Tcp.send" => Some("Tcp"),
                "Tcp.sendBytes" => Some("Tcp"),
                "Tcp.writeBytes" => Some("Tcp"),
                "Tcp.writeLine" => Some("Tcp"),
                "Terminal.clear" => Some("Terminal"),
                "Terminal.disableRawMode" => Some("Terminal"),
                "Terminal.enableRawMode" => Some("Terminal"),
                "Terminal.flush" => Some("Terminal"),
                "Terminal.hideCursor" => Some("Terminal"),
                "Terminal.moveTo" => Some("Terminal"),
                "Terminal.print" => Some("Terminal"),
                "Terminal.readKey" => Some("Terminal"),
                "Terminal.resetColor" => Some("Terminal"),
                "Terminal.setColor" => Some("Terminal"),
                "Terminal.showCursor" => Some("Terminal"),
                "Terminal.size" => Some("Terminal"),
                "Time.now" => Some("Time"),
                "Time.sleep" => Some("Time"),
                "Time.unixMs" => Some("Time"),
                _ => None,
            };
            if let Some(capability) = capability {
                if !(capability == "Args"
                    || capability == "Console"
                    || capability == "Disk"
                    || capability == "Env"
                    || capability == "Http"
                    || capability == "Random"
                    || capability == "Tcp"
                    || capability == "Terminal"
                    || capability == "Time")
                    && !seen.contains(capability)
                {
                    panic!(
                        "Legacy replay event '{}' has no capability contract/model provenance; refusing to guess",
                        effect.effect_type
                    );
                }
            }
        }
        for capability in ["Console"] {
            let recorded = session
                .capabilities
                .iter()
                .find(|entry| entry.capability == capability)
                .unwrap_or_else(|| {
                    panic!(
                        "Live replay capability '{}' has no provider provenance in the replay",
                        capability
                    )
                });
            let expected = current
                .iter()
                .find(|entry| entry.capability == capability)
                .expect("live capability contract validated above");
            let (Some(provider), Some(fingerprint)) = (&expected.provider, &expected.fingerprint)
            else {
                panic!(
                    "Capability '{}' requires a live provider during replay",
                    capability
                );
            };
            let compatible_standard_adapter = recorded.fingerprint == *fingerprint
                && aver_rt::provider::standard_provider_adapters_replay_compatible(
                    capability,
                    &recorded.provider,
                    provider,
                );
            if (recorded.provider != *provider || recorded.fingerprint != *fingerprint)
                && !compatible_standard_adapter
            {
                panic!(
                    "Live provider mismatch for '{}': recorded {}@{}, current {}@{}",
                    capability, recorded.provider, recorded.fingerprint, provider, fingerprint
                );
            }
        }
    }

    fn canonical_unit(value: ReplayJson) -> ReplayJson {
        // A Unit return serializes as JSON null in the VM / wasm-gc
        // replay format; the self-host interpreter's value type wraps
        // it as a `ValUnit` variant object. Map that variant back to
        // null so a recording made by any backend replays cleanly on
        // the self-host (and vice versa). Every other value passes
        // through untouched.
        if let ReplayJson::Object(ref map) = value {
            if let Some(ReplayJson::Object(variant)) = map.get("$variant") {
                if variant.get("name").and_then(|n| n.as_str()) == Some("ValUnit") {
                    return ReplayJson::Null;
                }
            }
        }
        value
    }

    fn finish_scope_success<T: ReplayValue>(value: &T) {
        SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state else {
                return;
            };
            match &mut scope.mode {
                ScopeMode::Normal => {}
                ScopeMode::Record { path, session } => {
                    session.output = RecordedOutcome::Value {
                        value: canonical_unit(value.to_replay_json()),
                    };
                    write_recording(path, session);
                }
                ScopeMode::Replay {
                    session, position, ..
                } => {
                    if *position != session.effects.len() {
                        panic!(
                            "Replay finished with {} unconsumed recorded effect(s)",
                            session.effects.len().saturating_sub(*position)
                        );
                    }
                    let actual_json = canonical_unit(value.to_replay_json());
                    // Surface the live return value to the parent
                    // process via a stdout marker so the host
                    // (`run_self_host_replay` in
                    // `src/main/replay_cmd/backends.rs`) can fill
                    // `BackendReplayOutcome.actual` with the real
                    // JSON. Without this the host had no way to
                    // recover what the subprocess returned and
                    // hardcoded `actual = recording.output.clone()`,
                    // forcing every replay to claim MATCH even when
                    // the underlying value diverged. Marker is
                    // emitted on every replay (matched or not) so
                    // the host has a uniform path.
                    println!("__aver_return__: {}", actual_json);
                    let actual = RecordedOutcome::Value { value: actual_json };
                    if actual != session.output {
                        panic!(
                            "Replay output mismatch for '{}': expected {:?}, got {:?}",
                            session.entry_fn, session.output, actual
                        );
                    }
                }
            }
        });
    }

    fn returned_error_message<E: aver_rt::AverDisplay>(entry_fn: &str, err: &E) -> String {
        let logical_entry_fn = replay_entry_name(entry_fn);
        if logical_entry_fn == "main" {
            format!("Main returned error: {}", err.aver_display())
        } else {
            format!(
                "{} returned error: {}",
                logical_entry_fn,
                err.aver_display()
            )
        }
    }

    fn finish_scope_error<E: aver_rt::AverDisplay>(entry_fn: &str, err: &E) {
        let message = returned_error_message(entry_fn, err);
        SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state else {
                return;
            };
            match &mut scope.mode {
                ScopeMode::Normal => {}
                ScopeMode::Record { path, session } => {
                    session.output = RecordedOutcome::RuntimeError {
                        message: message.clone(),
                    };
                    write_recording(path, session);
                }
                ScopeMode::Replay {
                    session, position, ..
                } => {
                    if *position != session.effects.len() {
                        panic!(
                            "Replay finished with {} unconsumed recorded effect(s)",
                            session.effects.len().saturating_sub(*position)
                        );
                    }
                    let actual = RecordedOutcome::RuntimeError {
                        message: message.clone(),
                    };
                    if actual != session.output {
                        panic!(
                            "Replay output mismatch for '{}': expected {:?}, got {:?}",
                            session.entry_fn, session.output, actual
                        );
                    }
                }
            }
        });
    }

    fn finish_scope_panic(message: &str) {
        SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state else {
                return;
            };
            match &mut scope.mode {
                ScopeMode::Normal => {}
                ScopeMode::Record { path, session } => {
                    session.output = RecordedOutcome::RuntimeError {
                        message: message.to_string(),
                    };
                    write_recording(path, session);
                }
                ScopeMode::Replay {
                    session, position, ..
                } => {
                    if *position != session.effects.len() {
                        panic!(
                            "Replay finished with {} unconsumed recorded effect(s)",
                            session.effects.len().saturating_sub(*position)
                        );
                    }
                    let actual = RecordedOutcome::RuntimeError {
                        message: message.to_string(),
                    };
                    if actual != session.output {
                        panic!(
                            "Replay output mismatch for '{}': expected {:?}, got {:?}",
                            session.entry_fn, session.output, actual
                        );
                    }
                }
            }
        });
    }

    fn replay_effect<T: ReplayValue>(effect_type: &str, args: Vec<ReplayJson>) -> T {
        let record = SCOPE_STATE.with(|cell| {
            let mut state = cell.borrow_mut();
            let ScopeState::Active(scope) = &mut *state else {
                panic!("replay scope is not active");
            };
            let ScopeMode::Replay {
                session,
                position,
                check_args,
            } = &mut scope.mode
            else {
                panic!("replay scope is not active");
            };

            let Some(record) = session.effects.get(*position).cloned() else {
                panic!(
                    "Replay exhausted: no more recorded effects for '{}'",
                    effect_type
                );
            };

            // Group-aware matching: if current effect has a group_id,
            // search within the group by type+args instead of position
            if let Some(gid) = record.group_id {
                let group_start = *position;
                let group_end = session.effects[group_start..]
                    .iter()
                    .position(|e| e.group_id != Some(gid))
                    .map(|offset| group_start + offset)
                    .unwrap_or(session.effects.len());
                // Prefer exact branch+occurrence match; fall back for older recordings.
                let current_bp = current_branch_path(&scope.branch_stack);
                let current_occ = current_effect_occurrence(&scope.effect_count_stack);
                let mut fallback_idx: Option<usize> = None;
                for idx in group_start..group_end {
                    let candidate = &session.effects[idx];
                    if candidate.effect_type != effect_type {
                        continue;
                    }
                    if *check_args && candidate.args != args {
                        continue;
                    }
                    match (&current_bp, &candidate.branch_path) {
                        (Some(got), Some(rec)) if got == rec => {
                            match (current_occ, candidate.effect_occurrence) {
                                (Some(got_occ), Some(rec_occ)) if got_occ == rec_occ => {
                                    let matched = candidate.clone();
                                    bump_effect_occurrence(&mut scope.effect_count_stack);
                                    if idx != *position {
                                        session.effects.swap(*position, idx);
                                    }
                                    *position += 1;
                                    return matched;
                                }
                                (Some(_), Some(_)) => continue,
                                _ => {
                                    if fallback_idx.is_none() {
                                        fallback_idx = Some(idx);
                                    }
                                }
                            }
                        }
                        (Some(_), Some(_)) => continue,
                        _ => {
                            if fallback_idx.is_none() {
                                fallback_idx = Some(idx);
                            }
                        }
                    }
                }
                if let Some(idx) = fallback_idx {
                    let matched = session.effects[idx].clone();
                    bump_effect_occurrence(&mut scope.effect_count_stack);
                    if idx != *position {
                        session.effects.swap(*position, idx);
                    }
                    *position += 1;
                    return matched;
                }
                panic!(
                    "Replay group mismatch: no '{}' found in group {}",
                    effect_type, gid
                );
            }

            if record.effect_type != effect_type {
                panic!(
                    "Replay mismatch at #{}: expected '{}', got '{}'",
                    record.seq, record.effect_type, effect_type
                );
            }
            if *check_args && record.args != args {
                panic!(
                    "Replay args mismatch at #{} for '{}'",
                    record.seq, effect_type
                );
            }
            *position += 1;
            record
        });

        match record.outcome {
            RecordedOutcome::Value { value } => T::from_replay_json(&value)
                .unwrap_or_else(|e| panic!("Replay decode failed for '{}': {}", effect_type, e)),
            RecordedOutcome::RuntimeError { message } => {
                panic!("Replayed runtime error for '{}': {}", effect_type, message)
            }
        }
    }

    fn write_recording(path: &PathBuf, session: &SessionRecording) {
        let parent = path.parent().map(PathBuf::from);
        if let Some(parent) = parent {
            std::fs::create_dir_all(&parent).unwrap_or_else(|e| {
                panic!("Cannot create replay dir '{}': {}", parent.display(), e)
            });
        }
        let json = serde_json::to_string_pretty(session)
            .expect("generated replay recording should serialize");
        std::fs::write(path, json).unwrap_or_else(|e| {
            panic!("Cannot write replay recording '{}': {}", path.display(), e)
        });
    }

    fn env_var(name: &str) -> Option<String> {
        std::env::var(name)
            .ok()
            .map(|value| value.trim().to_string())
            .filter(|value| !value.is_empty())
    }

    fn env_flag(name: &str) -> bool {
        env_var(name)
            .map(|value| matches!(value.as_str(), "1" | "true" | "TRUE" | "yes" | "YES"))
            .unwrap_or(false)
    }

    fn default_request_id() -> String {
        format!("generated-{}", unix_millis())
    }

    fn default_timestamp() -> String {
        format!("unix-{}", unix_millis())
    }

    fn unix_millis() -> u128 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock before unix epoch")
            .as_millis()
    }

    fn panic_payload_to_string(payload: &Box<dyn std::any::Any + Send>) -> String {
        if let Some(msg) = payload.downcast_ref::<String>() {
            return msg.clone();
        }
        if let Some(msg) = payload.downcast_ref::<&str>() {
            return (*msg).to_string();
        }
        "panic".to_string()
    }
}

pub use aver_replay::*;
