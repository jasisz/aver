use crate::value::hash_memo_args;
use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::rc::Rc;

use crate::ast::*;
use crate::replay::{
    json_to_string, value_to_json, values_to_json_lossy, EffectRecord, JsonValue, RecordedOutcome,
};
use crate::services::{console, disk, http, http_server, tcp};
use crate::source::{canonicalize_path, find_module_file, parse_source};
use crate::types::{byte, char, float, int, list, map, option, result, string};
// Re-export value types so existing `use aver::interpreter::Value` imports keep working.
pub use crate::value::{aver_display, aver_repr, Env, EnvFrame, RuntimeError, Value};
use crate::value::{list_from_vec, list_len, list_slice, list_tail_view};

#[derive(Debug, Clone)]
struct CallFrame {
    name: String,
    effects: Vec<String>,
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
}

pub struct Interpreter {
    pub env: Env,
    module_cache: HashMap<String, Value>,
    /// Record field order schemas by type name (used to validate and
    /// canonicalize `RecordCreate` runtime values).
    record_schemas: HashMap<String, Vec<String>>,
    call_stack: Vec<CallFrame>,
    /// Named effect aliases: `effects AppIO = [Console, Disk]`
    effect_aliases: HashMap<String, Vec<String>>,
    /// Active slot mapping for resolved function bodies.
    /// Set when entering a resolved fn, cleared on exit.
    active_local_slots: Option<HashMap<String, u16>>,
    /// Names of pure recursive functions eligible for auto-memoization.
    memo_fns: HashSet<String>,
    /// Per-function memo cache with collision-safe entries and LRU eviction.
    memo_cache: HashMap<String, FnMemoCache>,
    execution_mode: ExecutionMode,
    recorded_effects: Vec<EffectRecord>,
    replay_effects: Vec<EffectRecord>,
    replay_pos: usize,
    validate_replay_args: bool,
}

mod api;
mod builtins;
mod core;
mod effects;
mod eval;
mod exec;
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
