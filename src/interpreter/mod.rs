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
use crate::types::{float, int, list, map, string};
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
    /// Per-function memo cache: fn_name → (hash(args) → result).
    memo_cache: HashMap<String, HashMap<u64, Value>>,
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
