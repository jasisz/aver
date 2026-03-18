use crate::nan_value::{Arena, NanValue};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::ast::*;
use crate::replay::{
    EffectRecord, JsonValue, RecordedOutcome, SessionRecording, json_to_string,
    session_recording_to_string_pretty, value_to_json, values_to_json_lossy,
};
#[cfg(feature = "terminal")]
use crate::services::terminal;
use crate::services::{args, console, disk, env, http, http_server, random, tcp, time};
use crate::source::{
    canonicalize_path, find_module_file, parse_source, require_module_declaration,
};
use crate::types::{bool, byte, char, float, int, list, map, option, result, string};
// Re-export value types so existing `use aver::interpreter::Value` imports keep working.
pub use crate::value::{Env, EnvFrame, RuntimeError, Value, aver_display, aver_repr};
use crate::value::{list_len, list_view};

#[derive(Debug, Clone)]
struct CallFrame {
    name: Rc<String>,
    effects: Rc<Vec<String>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExecutionMode {
    Normal,
    Record,
    Replay,
}

const MEMO_CACHE_CAP_PER_FN: usize = 4096;

#[derive(Debug, Clone)]
struct MemoEntry {
    id: u64,
    args: Vec<Value>,
    result: Value,
}

#[derive(Debug, Clone)]
struct RecordingSink {
    path: PathBuf,
    request_id: String,
    timestamp: String,
    program_file: String,
    module_root: String,
    entry_fn: String,
    input: JsonValue,
}

type MatchSiteKey = (usize, usize); // (line, arm_count)

#[derive(Debug, Clone)]
struct VerifyMatchCoverageTracker {
    target_fn: String,
    expected_arms: std::collections::BTreeMap<MatchSiteKey, usize>,
    visited_arms: HashMap<MatchSiteKey, HashSet<usize>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifyMatchCoverageMiss {
    pub line: usize,
    pub total_arms: usize,
    pub missing_arms: Vec<usize>, // 0-based arm indices
}

#[derive(Debug, Clone)]
pub struct RecordingConfig {
    pub path: PathBuf,
    pub request_id: String,
    pub timestamp: String,
    pub program_file: String,
    pub module_root: String,
    pub entry_fn: String,
    pub input: JsonValue,
}

/// Per-function memo cache with collision-safe buckets and true LRU eviction.
#[derive(Debug, Default, Clone)]
struct FnMemoCache {
    /// Primary index: hash(args) -> bucket of potentially colliding entries.
    buckets: HashMap<u64, Vec<MemoEntry>>,
    /// Entry id -> (bucket hash, index in bucket vec).
    positions: HashMap<u64, (u64, usize)>,
    /// LRU links: entry id -> (prev, next).
    links: HashMap<u64, (Option<u64>, Option<u64>)>,
    lru_head: Option<u64>,
    lru_tail: Option<u64>,
    next_id: u64,
    len: usize,
}

impl FnMemoCache {
    fn get(&mut self, hash: u64, args: &[Value]) -> Option<Value> {
        let found = self
            .buckets
            .get_mut(&hash)
            .and_then(|entries| entries.iter_mut().find(|entry| entry.args == args))
            .map(|entry| (entry.id, entry.result.clone()));

        if let Some((id, value)) = found {
            self.touch(id);
            Some(value)
        } else {
            None
        }
    }

    fn insert(&mut self, hash: u64, args: Vec<Value>, result: Value, cap: usize) {
        let update_hit = self
            .buckets
            .get_mut(&hash)
            .and_then(|entries| entries.iter_mut().find(|entry| entry.args == args))
            .map(|entry| {
                entry.result = result.clone();
                entry.id
            });

        if let Some(id) = update_hit {
            self.touch(id);
            return;
        }

        if self.len >= cap {
            self.evict_lru();
        }

        let id = self.alloc_id();
        let entry = MemoEntry { id, args, result };
        let idx = self.buckets.entry(hash).or_default().len();
        self.buckets.entry(hash).or_default().push(entry);
        self.positions.insert(id, (hash, idx));
        self.append_tail(id);
        self.len += 1;
    }

    fn alloc_id(&mut self) -> u64 {
        let id = self.next_id;
        self.next_id = self.next_id.wrapping_add(1);
        id
    }

    fn evict_lru(&mut self) {
        if let Some(id) = self.lru_head {
            self.remove_entry(id);
        }
    }

    fn touch(&mut self, id: u64) {
        if self.lru_tail == Some(id) {
            return;
        }
        self.detach(id);
        self.append_tail(id);
    }

    fn append_tail(&mut self, id: u64) {
        let prev = self.lru_tail;
        self.links.insert(id, (prev, None));
        if let Some(tail) = prev {
            if let Some((_, next)) = self.links.get_mut(&tail) {
                *next = Some(id);
            }
        } else {
            self.lru_head = Some(id);
        }
        self.lru_tail = Some(id);
    }

    fn detach(&mut self, id: u64) {
        let Some((prev, next)) = self.links.get(&id).copied() else {
            return;
        };

        if let Some(p) = prev {
            if let Some((_, p_next)) = self.links.get_mut(&p) {
                *p_next = next;
            }
        } else {
            self.lru_head = next;
        }

        if let Some(n) = next {
            if let Some((n_prev, _)) = self.links.get_mut(&n) {
                *n_prev = prev;
            }
        } else {
            self.lru_tail = prev;
        }

        if let Some(link) = self.links.get_mut(&id) {
            *link = (None, None);
        }
    }

    fn remove_entry(&mut self, id: u64) {
        let Some((hash, idx)) = self.positions.remove(&id) else {
            return;
        };
        self.detach(id);
        self.links.remove(&id);

        let mut remove_bucket = false;
        if let Some(entries) = self.buckets.get_mut(&hash) {
            entries.swap_remove(idx);
            if idx < entries.len() {
                let moved_id = entries[idx].id;
                self.positions.insert(moved_id, (hash, idx));
            }
            remove_bucket = entries.is_empty();
        }
        if remove_bucket {
            self.buckets.remove(&hash);
        }
        self.len = self.len.saturating_sub(1);
    }

    /// NanValue-native get — converts NanValue args to Value for comparison,
    /// returns cached Value. Arena conversion done by caller.
    fn get_nv_as_value(&mut self, hash: u64, nv_args: &[NanValue], arena: &Arena) -> Option<Value> {
        let args: Vec<Value> = nv_args.iter().map(|nv| nv.to_value(arena)).collect();
        self.get(hash, &args)
    }

    /// NanValue-native insert — stores args and result as Value (bridge).
    fn insert_nv(
        &mut self,
        hash: u64,
        nv_args: Vec<NanValue>,
        nv_result: NanValue,
        arena: &Arena,
        cap: usize,
    ) {
        let args: Vec<Value> = nv_args.iter().map(|nv| nv.to_value(arena)).collect();
        let result = nv_result.to_value(arena);
        self.insert(hash, args, result, cap);
    }
}

pub struct Interpreter {
    pub env: Env,
    /// Base index into `env` for the current function's frames.
    /// lookup_ref sees env[0] (global) + env[env_base..] (current fn).
    /// Caller frames in env[1..env_base] are invisible.
    env_base: usize,
    /// Arena for NaN-boxed value storage.
    pub arena: Arena,
    module_cache: HashMap<String, Value>,
    /// Record field order schemas by type name (used to validate and
    /// canonicalize `RecordCreate` runtime values).
    record_schemas: HashMap<String, Vec<String>>,
    call_stack: Vec<CallFrame>,
    /// Active slot mapping for resolved function bodies.
    /// Set when entering a resolved fn, cleared on exit.
    active_local_slots: Option<Rc<HashMap<String, u16>>>,
    /// Names of pure recursive functions eligible for auto-memoization.
    memo_fns: HashSet<String>,
    /// Per-function memo cache with collision-safe entries and LRU eviction.
    memo_cache: HashMap<String, FnMemoCache>,
    execution_mode: ExecutionMode,
    recorded_effects: Vec<EffectRecord>,
    replay_effects: Vec<EffectRecord>,
    replay_pos: usize,
    validate_replay_args: bool,
    recording_sink: Option<RecordingSink>,
    verify_match_coverage: Option<VerifyMatchCoverageTracker>,
    /// Runtime policy from `aver.toml` — constrains Http hosts, Disk paths, etc.
    runtime_policy: Option<crate::config::ProjectConfig>,
    /// Command-line arguments passed to the Aver program (available via `Args.get()`).
    cli_args: Vec<String>,
}

mod api;
mod builtins;
mod core;
mod effects;
mod eval;
mod exec;
pub(crate) mod lowered;
mod ops;
mod patterns;

#[cfg(test)]
mod memo_cache_tests {
    use super::*;

    #[test]
    fn collision_bucket_is_exact_match_on_args() {
        let mut cache = FnMemoCache::default();
        cache.insert(1, vec![Value::Int(1)], Value::Int(10), 8);
        cache.insert(1, vec![Value::Int(2)], Value::Int(20), 8);

        assert_eq!(cache.get(1, &[Value::Int(1)]), Some(Value::Int(10)));
        assert_eq!(cache.get(1, &[Value::Int(2)]), Some(Value::Int(20)));
        assert_eq!(cache.get(1, &[Value::Int(3)]), None);
    }

    #[test]
    fn lru_evicts_least_recently_used() {
        let mut cache = FnMemoCache::default();
        cache.insert(11, vec![Value::Int(1)], Value::Int(10), 2);
        cache.insert(22, vec![Value::Int(2)], Value::Int(20), 2);

        // Touch key=11 so key=22 becomes LRU.
        assert_eq!(cache.get(11, &[Value::Int(1)]), Some(Value::Int(10)));
        cache.insert(33, vec![Value::Int(3)], Value::Int(30), 2);

        assert_eq!(cache.get(11, &[Value::Int(1)]), Some(Value::Int(10)));
        assert_eq!(cache.get(22, &[Value::Int(2)]), None);
        assert_eq!(cache.get(33, &[Value::Int(3)]), Some(Value::Int(30)));
    }
}
