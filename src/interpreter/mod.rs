pub(crate) use crate::nan_value::NanValueConvert;
use crate::nan_value::{Arena, NanValue};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc as Rc;

use crate::ast::*;
use crate::replay::{
    EffectRecord, EffectReplayMode, EffectReplayState, JsonValue, RecordedOutcome,
    SessionRecording, session_recording_to_string_pretty, value_to_json, values_to_json_lossy,
};
#[cfg(feature = "terminal")]
use crate::services::terminal;
use crate::services::{args, console, disk, env, http, http_server, random, tcp, time};
use crate::source::{
    canonicalize_path, find_module_file, parse_source, require_module_declaration,
};
use crate::types::{bool, byte, char, float, int, list, map, option, result, string, vector};
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
    mounted_module_paths: HashSet<String>,
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
    replay_state: EffectReplayState,
    recording_sink: Option<RecordingSink>,
    verify_match_coverage: Option<VerifyMatchCoverageTracker>,
    /// Runtime policy from `aver.toml` — constrains Http hosts, Disk paths, etc.
    runtime_policy: Option<crate::config::ProjectConfig>,
    /// Command-line arguments passed to the Aver program (available via `Args.get()`).
    cli_args: Vec<String>,
    /// Source line of the current call expression (set before dispatch, used for
    /// effect recording and error decoration). 0 = unknown.
    pub(crate) last_call_line: usize,
}

mod api;
mod builtins;
mod core;
mod effects;
mod eval;
mod exec;
mod ir_bridge;
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

#[cfg(test)]
mod ir_bridge_tests {
    use aver_rt::AverVector;

    use crate::ir::CallLowerCtx;

    use super::ir_bridge::InterpreterLowerCtx;
    use super::lowered::{
        self, ExprId, LoweredDirectCallTarget, LoweredExpr, LoweredForwardArg, LoweredLeafOp,
        LoweredMatchArm, LoweredTailCallTarget,
    };
    use super::*;

    /// Shorthand: wrap an Expr in Spanned::bare (line=0).
    fn sb(expr: Expr) -> Spanned<Expr> {
        Spanned::bare(expr)
    }

    /// Shorthand: wrap an Expr in Box<Spanned::bare(...)>.
    fn sbb(expr: Expr) -> Box<Spanned<Expr>> {
        Box::new(Spanned::bare(expr))
    }

    fn register_task_event_type(interpreter: &mut Interpreter) {
        interpreter.register_type_def(&TypeDef::Sum {
            name: "TaskEvent".to_string(),
            variants: vec![
                TypeVariant {
                    name: "TaskCreated".to_string(),
                    fields: vec!["String".to_string()],
                },
                TypeVariant {
                    name: "TaskMoved".to_string(),
                    fields: vec!["String".to_string(), "Int".to_string()],
                },
            ],
            line: 1,
        });

        let mut members = HashMap::new();
        members.insert(
            "TaskEvent".to_string(),
            interpreter
                .lookup("TaskEvent")
                .expect("TaskEvent namespace should be defined"),
        );
        interpreter
            .define_module_path(
                "Domain.Types",
                Value::Namespace {
                    name: "Types".to_string(),
                    members,
                },
            )
            .expect("module path should be mountable");
    }

    fn register_expr_type(interpreter: &mut Interpreter) {
        interpreter.register_type_def(&TypeDef::Sum {
            name: "Expr".to_string(),
            variants: vec![TypeVariant {
                name: "ExprInt".to_string(),
                fields: vec!["Int".to_string()],
            }],
            line: 1,
        });
    }

    fn register_token_type(interpreter: &mut Interpreter) {
        interpreter.register_type_def(&TypeDef::Sum {
            name: "Token".to_string(),
            variants: vec![
                TypeVariant {
                    name: "TkInt".to_string(),
                    fields: vec!["Int".to_string()],
                },
                TypeVariant {
                    name: "TkQuestion".to_string(),
                    fields: vec![],
                },
            ],
            line: 1,
        });

        let mut members = HashMap::new();
        members.insert(
            "Token".to_string(),
            interpreter
                .lookup("Token")
                .expect("Token type namespace should be defined"),
        );
        interpreter
            .define_module_path(
                "Domain.Token",
                Value::Namespace {
                    name: "Domain.Token".to_string(),
                    members,
                },
            )
            .expect("module path should be mountable");
    }

    #[test]
    fn eval_constructor_uses_shared_semantics_for_wrappers_and_qualified_variants() {
        let mut interpreter = Interpreter::new();
        register_task_event_type(&mut interpreter);

        let ok_expr = sb(Expr::Constructor(
            "Ok".to_string(),
            Some(sbb(Expr::Literal(Literal::Int(7)))),
        ));
        let created_expr = sb(Expr::Constructor(
            "Domain.Types.TaskEvent.TaskCreated".to_string(),
            Some(sbb(Expr::Literal(Literal::Str("now".to_string())))),
        ));

        assert_eq!(
            interpreter
                .eval_expr(&ok_expr)
                .expect("Ok constructor should evaluate"),
            Value::Ok(Box::new(Value::Int(7)))
        );

        match interpreter
            .eval_expr(&created_expr)
            .expect("qualified constructor should build a variant")
        {
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                assert_eq!(type_name, "TaskEvent");
                assert_eq!(variant, "TaskCreated");
                assert_eq!(fields.as_ref(), &[Value::Str("now".to_string())]);
            }
            other => panic!("expected variant, got {other:?}"),
        }
    }

    #[test]
    fn qualified_module_function_call_prefers_deepest_module_prefix_over_type_namespace() {
        let mut interpreter = Interpreter::new();
        register_expr_type(&mut interpreter);

        let parse_expr = FnDef {
            name: "parseExpr".to_string(),
            line: 1,
            params: vec![
                ("tokens".to_string(), "List<Int>".to_string()),
                ("pos".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(sb(Expr::Literal(Literal::Int(7))))),
            resolution: None,
        };
        interpreter
            .exec_fn_def(&parse_expr)
            .expect("parseExpr function should register");

        let mut members = HashMap::new();
        members.insert(
            "parseExpr".to_string(),
            interpreter
                .lookup("parseExpr")
                .expect("parseExpr function should be available"),
        );
        interpreter
            .define_module_path(
                "Domain.Parser.Expr",
                Value::Namespace {
                    name: "Domain.Parser.Expr".to_string(),
                    members,
                },
            )
            .expect("module path should be mountable");

        let ctx = InterpreterLowerCtx::new(&interpreter);
        assert_eq!(
            ctx.resolve_module_call("Domain.Parser.Expr.parseExpr"),
            Some(("Domain.Parser.Expr", "parseExpr"))
        );

        let call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Attr(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Domain".to_string())),
                        "Parser".to_string(),
                    )),
                    "Expr".to_string(),
                )),
                "parseExpr".to_string(),
            )),
            vec![
                sb(Expr::List(vec![sb(Expr::Literal(Literal::Int(1)))])),
                sb(Expr::Literal(Literal::Int(0))),
            ],
        ));

        assert_eq!(
            interpreter
                .eval_expr(&call)
                .expect("qualified module function call should run"),
            Value::Int(7)
        );
    }

    #[test]
    fn qualified_constructor_call_keeps_outer_module_prefix_when_type_namespace_exists() {
        let mut interpreter = Interpreter::new();
        register_expr_type(&mut interpreter);

        let mut members = HashMap::new();
        members.insert(
            "Expr".to_string(),
            interpreter
                .lookup("Expr")
                .expect("Expr type namespace should be available"),
        );
        interpreter
            .define_module_path(
                "Domain.Ast",
                Value::Namespace {
                    name: "Domain.Ast".to_string(),
                    members,
                },
            )
            .expect("module path should be mountable");

        let ctx = InterpreterLowerCtx::new(&interpreter);
        assert_eq!(
            ctx.resolve_module_call("Domain.Ast.Expr.ExprInt"),
            Some(("Domain.Ast", "Expr.ExprInt"))
        );

        let ctor_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Attr(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Domain".to_string())),
                        "Ast".to_string(),
                    )),
                    "Expr".to_string(),
                )),
                "ExprInt".to_string(),
            )),
            vec![sb(Expr::Literal(Literal::Int(7)))],
        ));

        match interpreter
            .eval_expr(&ctor_call)
            .expect("qualified constructor call should run")
        {
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                assert_eq!(type_name, "Expr");
                assert_eq!(variant, "ExprInt");
                assert_eq!(fields.as_ref(), &[Value::Int(7)]);
            }
            other => panic!("expected variant, got {other:?}"),
        }
    }

    #[test]
    fn short_type_aliases_are_not_misclassified_as_module_prefixes() {
        let mut interpreter = Interpreter::new();
        register_token_type(&mut interpreter);

        let ctx = InterpreterLowerCtx::new(&interpreter);
        assert_eq!(ctx.resolve_module_call("Token.TkQuestion"), None);

        let pattern = Pattern::Constructor("Token.TkInt".to_string(), vec!["n".to_string()]);
        let value = Value::Variant {
            type_name: "Token".to_string(),
            variant: "TkInt".to_string(),
            fields: vec![Value::Int(1)].into(),
        };

        assert_eq!(
            interpreter.match_pattern(&pattern, &value),
            Some(vec![("n".to_string(), Value::Int(1))])
        );

        let nv = NanValue::from_value(&value, &mut interpreter.arena);
        let bindings = interpreter
            .match_pattern_nv(&pattern, nv)
            .expect("short type alias constructor pattern should match");
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, "n");
        assert_eq!(bindings[0].1.to_value(&interpreter.arena), Value::Int(1));
    }

    #[test]
    fn qualified_constructor_patterns_use_shared_semantics_in_both_match_paths() {
        let mut interpreter = Interpreter::new();
        register_task_event_type(&mut interpreter);

        let pattern = Pattern::Constructor(
            "Domain.Types.TaskEvent.TaskCreated".to_string(),
            vec!["at".to_string()],
        );
        let value = Value::Variant {
            type_name: "TaskEvent".to_string(),
            variant: "TaskCreated".to_string(),
            fields: vec![Value::Str("now".to_string())].into(),
        };

        assert_eq!(
            interpreter.match_pattern(&pattern, &value),
            Some(vec![("at".to_string(), Value::Str("now".to_string()))])
        );

        let nv = NanValue::from_value(&value, &mut interpreter.arena);
        let bindings = interpreter
            .match_pattern_nv(&pattern, nv)
            .expect("nan-value pattern path should match");
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, "at");
        assert_eq!(
            bindings[0].1.to_value(&interpreter.arena),
            Value::Str("now".to_string())
        );
    }

    #[test]
    fn constructor_patterns_match_inline_single_field_variants_in_nan_path() {
        let mut interpreter = Interpreter::new();
        interpreter.register_type_def(&TypeDef::Sum {
            name: "Expr".to_string(),
            variants: vec![
                TypeVariant {
                    name: "Int".to_string(),
                    fields: vec!["Int".to_string()],
                },
                TypeVariant {
                    name: "Text".to_string(),
                    fields: vec!["String".to_string()],
                },
            ],
            line: 1,
        });

        let pattern = Pattern::Constructor("Expr.Int".to_string(), vec!["n".to_string()]);
        let value = Value::Variant {
            type_name: "Expr".to_string(),
            variant: "Int".to_string(),
            fields: vec![Value::Int(7)].into(),
        };

        let nv = NanValue::from_value(&value, &mut interpreter.arena);
        let bindings = interpreter
            .match_pattern_nv(&pattern, nv)
            .expect("inline one-field variant should match in nan path");
        assert_eq!(bindings.len(), 1);
        assert_eq!(bindings[0].0, "n");
        assert_eq!(bindings[0].1.to_value(&interpreter.arena), Value::Int(7));
    }

    #[test]
    fn runtime_match_dispatch_plan_selects_bool_list_and_wrapper_arms() {
        let mut interpreter = Interpreter::new();

        let bool_arms = vec![
            LoweredMatchArm {
                pattern: Pattern::Literal(Literal::Bool(true)),
                body: ExprId(0),
            },
            LoweredMatchArm {
                pattern: Pattern::Ident("other".to_string()),
                body: ExprId(1),
            },
        ];
        let (bool_arm, bool_bindings) = interpreter
            .try_dispatch_match_plan_nv(NanValue::FALSE, &bool_arms)
            .expect("bool match plan should dispatch");
        assert_eq!(bool_arm, 1);
        assert_eq!(bool_bindings.len(), 1);
        assert_eq!(bool_bindings[0].0, "other");
        assert_eq!(bool_bindings[0].1.bits(), NanValue::FALSE.bits());

        let non_empty_list = NanValue::new_list(interpreter.arena.push_list(vec![NanValue::TRUE]));
        let list_arms = vec![
            LoweredMatchArm {
                pattern: Pattern::EmptyList,
                body: ExprId(0),
            },
            LoweredMatchArm {
                pattern: Pattern::Cons("head".to_string(), "tail".to_string()),
                body: ExprId(1),
            },
        ];
        let (list_arm, list_bindings) = interpreter
            .try_dispatch_match_plan_nv(non_empty_list, &list_arms)
            .expect("list match plan should dispatch");
        assert_eq!(list_arm, 1);
        assert_eq!(list_bindings.len(), 2);
        assert_eq!(list_bindings[0].0, "head");
        assert_eq!(list_bindings[0].1.bits(), NanValue::TRUE.bits());

        let wrapper_arms = vec![
            LoweredMatchArm {
                pattern: Pattern::Constructor("Option.None".to_string(), vec![]),
                body: ExprId(0),
            },
            LoweredMatchArm {
                pattern: Pattern::Constructor("Option.Some".to_string(), vec!["x".to_string()]),
                body: ExprId(1),
            },
            LoweredMatchArm {
                pattern: Pattern::Ident("fallback".to_string()),
                body: ExprId(2),
            },
        ];
        let some_subject = NanValue::new_some_value(
            NanValue::new_int(7, &mut interpreter.arena),
            &mut interpreter.arena,
        );
        let (wrapper_arm, wrapper_bindings) = interpreter
            .try_dispatch_match_plan_nv(some_subject, &wrapper_arms)
            .expect("wrapper match plan should dispatch");
        assert_eq!(wrapper_arm, 1);
        assert_eq!(wrapper_bindings.len(), 1);
        assert_eq!(wrapper_bindings[0].0, "x");
        assert_eq!(wrapper_bindings[0].1.as_int(&interpreter.arena), 7);

        let (default_arm, default_bindings) = interpreter
            .try_dispatch_match_plan_nv(NanValue::TRUE, &wrapper_arms)
            .expect("dispatch table default arm should match");
        assert_eq!(default_arm, 2);
        assert_eq!(default_bindings.len(), 1);
        assert_eq!(default_bindings[0].0, "fallback");
        assert_eq!(default_bindings[0].1.bits(), NanValue::TRUE.bits());
    }

    #[test]
    fn lowered_roots_classify_shared_builtin_leaf_ops() {
        let interpreter = Interpreter::new();
        let ctx = InterpreterLowerCtx::new(&interpreter);

        let map_get = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "get".to_string(),
            )),
            vec![
                sb(Expr::Ident("m".to_string())),
                sb(Expr::Literal(Literal::Str("k".to_string()))),
            ],
        ));
        let (lowered_map_get, map_get_root) = lowered::lower_expr_root(&map_get, &ctx);
        assert!(matches!(
            lowered_map_get.expr(map_get_root),
            LoweredExpr::Leaf(LoweredLeafOp::MapGet { .. })
        ));

        let vec_default = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "withDefault".to_string(),
            )),
            vec![
                sb(Expr::FnCall(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Vector".to_string())),
                        "get".to_string(),
                    )),
                    vec![
                        sb(Expr::Ident("v".to_string())),
                        sb(Expr::Ident("idx".to_string())),
                    ],
                )),
                sb(Expr::Literal(Literal::Int(0))),
            ],
        ));
        let (lowered_vec_default, vec_default_root) = lowered::lower_expr_root(&vec_default, &ctx);
        assert!(matches!(
            lowered_vec_default.expr(vec_default_root),
            LoweredExpr::Leaf(LoweredLeafOp::VectorGetOrDefaultLiteral {
                default_literal: Literal::Int(0),
                ..
            })
        ));
    }

    #[test]
    fn runtime_executes_shared_leaf_ops_in_host_interpreter() {
        let mut interpreter = Interpreter::new();

        let mut map_value = HashMap::new();
        map_value.insert(Value::Str("k".to_string()), Value::Int(7));
        interpreter.define("m".to_string(), Value::Map(map_value));
        interpreter.define(
            "v".to_string(),
            Value::Vector(AverVector::from_vec(vec![Value::Int(10), Value::Int(20)])),
        );
        interpreter.define("idx".to_string(), Value::Int(1));
        interpreter.define("miss".to_string(), Value::Int(5));

        let map_get = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Map".to_string())),
                "get".to_string(),
            )),
            vec![
                sb(Expr::Ident("m".to_string())),
                sb(Expr::Literal(Literal::Str("k".to_string()))),
            ],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&map_get)
                .expect("Map.get leaf should run"),
            Value::Some(Box::new(Value::Int(7)))
        );

        let vec_hit = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "withDefault".to_string(),
            )),
            vec![
                sb(Expr::FnCall(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Vector".to_string())),
                        "get".to_string(),
                    )),
                    vec![
                        sb(Expr::Ident("v".to_string())),
                        sb(Expr::Ident("idx".to_string())),
                    ],
                )),
                sb(Expr::Literal(Literal::Int(0))),
            ],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&vec_hit)
                .expect("Vector.get default leaf should return hit"),
            Value::Int(20)
        );

        let vec_miss = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "withDefault".to_string(),
            )),
            vec![
                sb(Expr::FnCall(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Vector".to_string())),
                        "get".to_string(),
                    )),
                    vec![
                        sb(Expr::Ident("v".to_string())),
                        sb(Expr::Ident("miss".to_string())),
                    ],
                )),
                sb(Expr::Literal(Literal::Int(0))),
            ],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&vec_miss)
                .expect("Vector.get default leaf should return fallback"),
            Value::Int(0)
        );
    }

    #[test]
    fn lowered_roots_classify_shared_call_plans_for_builtin_and_function_calls() {
        let mut interpreter = Interpreter::new();
        register_task_event_type(&mut interpreter);
        let ctx = InterpreterLowerCtx::new(&interpreter);

        let list_len = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("List".to_string())),
                "len".to_string(),
            )),
            vec![sb(Expr::Ident("xs".to_string()))],
        ));
        let (lowered_builtin, builtin_root) = lowered::lower_expr_root(&list_len, &ctx);
        assert!(matches!(
            lowered_builtin.expr(builtin_root),
            LoweredExpr::DirectCall {
                target: LoweredDirectCallTarget::Builtin(name),
                ..
            } if name == "List.len"
        ));

        let identity_call = sb(Expr::FnCall(
            sbb(Expr::Ident("identity".to_string())),
            vec![sb(Expr::Literal(Literal::Int(7)))],
        ));
        let (lowered_fn, fn_root) = lowered::lower_expr_root(&identity_call, &ctx);
        assert!(matches!(
            lowered_fn.expr(fn_root),
            LoweredExpr::DirectCall {
                target: LoweredDirectCallTarget::Function(name),
                ..
            } if name == "identity"
        ));

        let wrapper_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Result".to_string())),
                "Ok".to_string(),
            )),
            vec![sb(Expr::Literal(Literal::Int(1)))],
        ));
        let (lowered_wrapper, wrapper_root) = lowered::lower_expr_root(&wrapper_call, &ctx);
        assert!(matches!(
            lowered_wrapper.expr(wrapper_root),
            LoweredExpr::DirectCall {
                target: LoweredDirectCallTarget::Wrapper(crate::ir::WrapperKind::ResultOk),
                ..
            }
        ));

        let none_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "None".to_string(),
            )),
            vec![],
        ));
        let (lowered_none, none_root) = lowered::lower_expr_root(&none_call, &ctx);
        assert!(matches!(
            lowered_none.expr(none_root),
            LoweredExpr::DirectCall {
                target: LoweredDirectCallTarget::NoneValue,
                ..
            }
        ));

        let ctor_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Attr(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Domain".to_string())),
                        "Types".to_string(),
                    )),
                    "TaskEvent".to_string(),
                )),
                "TaskCreated".to_string(),
            )),
            vec![sb(Expr::Literal(Literal::Str("now".to_string())))],
        ));
        let (lowered_ctor, ctor_root) = lowered::lower_expr_root(&ctor_call, &ctx);
        assert!(matches!(
            lowered_ctor.expr(ctor_root),
            LoweredExpr::DirectCall {
                target: LoweredDirectCallTarget::TypeConstructor {
                    qualified_type_name,
                    variant_name,
                },
                ..
            } if qualified_type_name == "Domain.Types.TaskEvent" && variant_name == "TaskCreated"
        ));
    }

    #[test]
    fn runtime_executes_shared_direct_calls_in_host_interpreter() {
        let mut interpreter = Interpreter::new();
        register_task_event_type(&mut interpreter);

        let identity = FnDef {
            name: "identity".to_string(),
            line: 1,
            params: vec![("x".to_string(), "Int".to_string())],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(sb(Expr::Resolved(0)))),
            resolution: Some(FnResolution {
                local_slots: Rc::new(HashMap::from([(String::from("x"), 0u16)])),
                local_count: 1,
            }),
        };
        interpreter
            .exec_fn_def(&identity)
            .expect("identity function should register");

        let list_len = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("List".to_string())),
                "len".to_string(),
            )),
            vec![sb(Expr::List(vec![
                sb(Expr::Literal(Literal::Int(1))),
                sb(Expr::Literal(Literal::Int(2))),
            ]))],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&list_len)
                .expect("direct builtin call should run"),
            Value::Int(2)
        );

        let identity_call = sb(Expr::FnCall(
            sbb(Expr::Ident("identity".to_string())),
            vec![sb(Expr::Literal(Literal::Int(9)))],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&identity_call)
                .expect("direct function call should run"),
            Value::Int(9)
        );

        let wrapper_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Result".to_string())),
                "Ok".to_string(),
            )),
            vec![sb(Expr::Literal(Literal::Int(5)))],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&wrapper_call)
                .expect("direct wrapper call should run"),
            Value::Ok(Box::new(Value::Int(5)))
        );

        let none_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Ident("Option".to_string())),
                "None".to_string(),
            )),
            vec![],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&none_call)
                .expect("direct none call should run"),
            Value::None
        );

        let ctor_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Attr(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Domain".to_string())),
                        "Types".to_string(),
                    )),
                    "TaskEvent".to_string(),
                )),
                "TaskCreated".to_string(),
            )),
            vec![sb(Expr::Literal(Literal::Str("now".to_string())))],
        ));
        match interpreter
            .eval_expr(&ctor_call)
            .expect("direct qualified ctor call should run")
        {
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                assert_eq!(type_name, "TaskEvent");
                assert_eq!(variant, "TaskCreated");
                assert_eq!(fields.as_ref(), &[Value::Str("now".to_string())]);
            }
            other => panic!("expected variant, got {other:?}"),
        }

        let multi_ctor_call = sb(Expr::FnCall(
            sbb(Expr::Attr(
                sbb(Expr::Attr(
                    sbb(Expr::Attr(
                        sbb(Expr::Ident("Domain".to_string())),
                        "Types".to_string(),
                    )),
                    "TaskEvent".to_string(),
                )),
                "TaskMoved".to_string(),
            )),
            vec![
                sb(Expr::Literal(Literal::Str("later".to_string()))),
                sb(Expr::Literal(Literal::Int(3))),
            ],
        ));
        match interpreter
            .eval_expr(&multi_ctor_call)
            .expect("direct qualified multi-field ctor call should run")
        {
            Value::Variant {
                type_name,
                variant,
                fields,
            } => {
                assert_eq!(type_name, "TaskEvent");
                assert_eq!(variant, "TaskMoved");
                assert_eq!(
                    fields.as_ref(),
                    &[Value::Str("later".to_string()), Value::Int(3)]
                );
            }
            other => panic!("expected variant, got {other:?}"),
        }
    }

    #[test]
    fn lowered_fn_bodies_classify_forward_calls_through_shared_ir() {
        let interpreter = Interpreter::new();
        let ctx = InterpreterLowerCtx::new(&interpreter);
        let body = FnBody::from_expr(sb(Expr::FnCall(
            sbb(Expr::Ident("first".to_string())),
            vec![sb(Expr::Resolved(1)), sb(Expr::Resolved(0))],
        )));
        let lowered = lowered::lower_fn_body(&body, &ctx, "swap");

        assert!(matches!(
            lowered.expr(ExprId(0)),
            LoweredExpr::ForwardCall {
                target: LoweredDirectCallTarget::Function(name),
                args,
                ..
            } if name == "first"
                && matches!(
                    args.as_ref(),
                    [LoweredForwardArg::Slot(1), LoweredForwardArg::Slot(0)]
                )
        ));
    }

    #[test]
    fn runtime_executes_forward_calls_without_evaling_arg_exprs() {
        let mut interpreter = Interpreter::new();

        let first = FnDef {
            name: "first".to_string(),
            line: 1,
            params: vec![
                ("x".to_string(), "Int".to_string()),
                ("y".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(sb(Expr::Resolved(0)))),
            resolution: Some(FnResolution {
                local_slots: Rc::new(HashMap::from([
                    (String::from("x"), 0u16),
                    (String::from("y"), 1u16),
                ])),
                local_count: 2,
            }),
        };
        interpreter
            .exec_fn_def(&first)
            .expect("first function should register");

        let swap = FnDef {
            name: "swap".to_string(),
            line: 2,
            params: vec![
                ("a".to_string(), "Int".to_string()),
                ("b".to_string(), "Int".to_string()),
            ],
            return_type: "Int".to_string(),
            effects: vec![],
            desc: None,
            body: Rc::new(FnBody::from_expr(sb(Expr::FnCall(
                sbb(Expr::Ident("first".to_string())),
                vec![sb(Expr::Resolved(1)), sb(Expr::Resolved(0))],
            )))),
            resolution: Some(FnResolution {
                local_slots: Rc::new(HashMap::from([
                    (String::from("a"), 0u16),
                    (String::from("b"), 1u16),
                ])),
                local_count: 2,
            }),
        };
        interpreter
            .exec_fn_def(&swap)
            .expect("swap function should register");

        let swap_call = sb(Expr::FnCall(
            sbb(Expr::Ident("swap".to_string())),
            vec![
                sb(Expr::Literal(Literal::Int(3))),
                sb(Expr::Literal(Literal::Int(7))),
            ],
        ));
        assert_eq!(
            interpreter
                .eval_expr(&swap_call)
                .expect("forward call should run"),
            Value::Int(7)
        );
    }

    #[test]
    fn lowered_fn_bodies_classify_tail_calls_through_shared_ir() {
        let interpreter = Interpreter::new();
        let ctx = InterpreterLowerCtx::new(&interpreter);

        let self_body = FnBody::from_expr(sb(Expr::TailCall(Box::new((
            "loop".to_string(),
            vec![sb(Expr::Literal(Literal::Int(1)))],
        )))));
        let lowered_self = lowered::lower_fn_body(&self_body, &ctx, "loop");
        assert!(matches!(
            lowered_self.expr(ExprId(1)),
            LoweredExpr::TailCall {
                target: LoweredTailCallTarget::SelfCall,
                ..
            }
        ));

        let known_body = FnBody::from_expr(sb(Expr::TailCall(Box::new((
            "other".to_string(),
            vec![sb(Expr::Literal(Literal::Int(2)))],
        )))));
        let lowered_known = lowered::lower_fn_body(&known_body, &ctx, "loop");
        assert!(matches!(
            lowered_known.expr(ExprId(1)),
            LoweredExpr::TailCall {
                target: LoweredTailCallTarget::KnownFunction(name),
                ..
            } if name == "other"
        ));

        let unknown_body = FnBody::from_expr(sb(Expr::TailCall(Box::new((
            "Result.Ok".to_string(),
            vec![sb(Expr::Literal(Literal::Int(3)))],
        )))));
        let lowered_unknown = lowered::lower_fn_body(&unknown_body, &ctx, "loop");
        assert!(matches!(
            lowered_unknown.expr(ExprId(1)),
            LoweredExpr::TailCall {
                target: LoweredTailCallTarget::Unknown(name),
                ..
            } if name == "Result.Ok"
        ));
    }
}
