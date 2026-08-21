use crate::nan_value::{LaneMark, NanValue};

use super::symbol::VmSymbolTable;

/// A compiled function chunk — bytecode + metadata.
#[derive(Debug, Clone)]
pub struct FnChunk {
    pub name: String,
    pub arity: u8,
    pub local_count: u16,
    pub code: Vec<u8>,
    pub constants: Vec<NanValue>,
    /// Declared effects (e.g. `! [Console.print, Http]`). Empty for pure functions.
    pub effects: Vec<u32>,
    /// Conservatively classified "thin" function: likely to return without
    /// creating any frame-local heap survivors or dirtying globals.
    pub thin: bool,
    /// Narrow wrapper-like helper that borrows the caller young region and
    /// skips ordinary-return handoff as long as it stays out of yard/handoff.
    pub parent_thin: bool,
    /// Leaf function: no CALL_KNOWN or CALL_VALUE in bytecode (only builtins
    /// and opcodes). When also thin and args-only (local_count == arity),
    /// can be called without pushing a CallFrame.
    pub leaf: bool,
    /// Pure no-alloc function (per shared `ir::compute_alloc_info` under
    /// `NeutralAllocPolicy`): the body never produces a heap object. Disjoint from
    /// `thin` because mutual-TCO peers can be no-alloc but not bytecode-thin.
    /// `TAIL_CALL_KNOWN` skips `finalize_frame_locals_for_tail_call` when
    /// the target chunk has this flag set — the runtime guard is guaranteed
    /// to be a no-op for pure no-alloc bodies.
    pub no_alloc: bool,
    /// Source file path for this function (empty for synthetic/unknown).
    pub source_file: String,
    /// Run-length encoded line table: `(bytecode_offset, source_line)`.
    /// Sorted by offset. Lookup: find last entry where offset <= target ip.
    pub line_table: Vec<(u16, u16)>,
}

/// Call-frame metadata, with no closure/upvalue fields.
#[derive(Debug, Clone)]
pub struct CallFrame {
    /// Index into `CodeStore::functions`.
    pub fn_id: u32,
    /// Current instruction pointer (byte offset into `FnChunk::code`).
    pub ip: u32,
    /// Base pointer: index into VM stack where this frame's locals start.
    pub bp: u32,
    /// Number of local slots (params + local bindings).
    pub local_count: u16,
    /// Arena length at function entry; allocations above this mark are local
    /// to the frame unless promoted on return/tail-call.
    pub arena_mark: u32,
    /// Yard length at function entry; reused TCO frames compact this suffix
    /// so loop-carried survivors do not accumulate across iterations.
    pub yard_base: u32,
    /// Current yard suffix owned by this frame iteration. Reused TCO frames
    /// may advance this mark so older carried survivors become the shared
    /// prefix for the next iteration.
    pub yard_mark: u32,
    /// Handoff length at function entry; ordinary returns compact this suffix
    /// so helper results can survive into the caller without polluting stable.
    pub handoff_mark: u32,
    /// Snapshot paired with `arena_mark` / `yard_base` / `handoff_mark` for the
    /// frame's final return. Unlike `lane_mark`, this base is never rebased by
    /// a reused tail-call frame.
    pub lane_base: LaneMark,
    /// Snapshot paired with the current tail iteration's marks. Collection
    /// receipts at or below this watermark cannot contain allocations owned by
    /// that iteration; destructive tail boundaries rebase it with `yard_mark`.
    pub lane_mark: LaneMark,
    /// Whether this frame stored a young-region value into globals.
    pub globals_dirty: bool,
    /// Whether ordinary returns introduced caller-yard survivors that should
    /// be pruned on the next tail-call boundary.
    pub yard_dirty: bool,
    /// Whether helper returns introduced handoff survivors that should be
    /// pruned on the next boundary of this frame.
    pub handoff_dirty: bool,
    /// Whether an owned in-place vector write stored into an arena slot that
    /// lies outside this frame's regions — the one way a slot the boundary
    /// keeps can come to hold an index into a region the boundary drops.
    /// Set by `VECTOR_SET_OR_KEEP`'s owned branch and inherited from callees,
    /// it withholds the return path that truncates young without rewriting
    /// anything first.
    ///
    /// Once set it stays set for the life of the frame, unlike the three dirty
    /// bits above, which a tail call clears when it reuses the frame. It has to:
    /// a boundary can only discharge the obligation by dropping everything the
    /// frame owns, and the tail-call boundary compacts relative to `yard_mark`
    /// while the frame's return truncates to `yard_base`, which is taken once
    /// at entry and never re-marked. The survivors of a tail-call compaction
    /// therefore still sit inside the region the frame's own return drops.
    pub inplace_write_escaped: bool,
    /// Conservatively classified as cheap enough for a fast return path.
    pub thin: bool,
    /// Uses the caller young region as its allocation lane and skips
    /// ordinary-return handoff while it remains a pure wrapper frame.
    pub parent_thin: bool,
}

/// All compiled bytecode for a program.
#[derive(Debug, Clone)]
pub struct CodeStore {
    pub functions: Vec<FnChunk>,
    /// Map from function name to index in `functions`.
    pub fn_index: std::collections::HashMap<String, u32>,
    /// Compile-time-known symbol table for functions, builtins, effects, and other names.
    pub(crate) symbols: VmSymbolTable,
    /// Per-record-type field slot lookup: (type_id, field_symbol_id) -> field_idx.
    pub(crate) record_field_slots: std::collections::HashMap<(u32, u32), u8>,
    /// Capability operations that survived lowering into executable code.
    /// Declarations alone do not enter this set, so unused contracts never
    /// demand a provider.
    pub(crate) required_capability_operations: std::collections::BTreeSet<String>,
    /// Contract/model hashes captured from the checked source program for every
    /// required capability module. Provider installation is checked against
    /// these identities before bytecode executes.
    pub(crate) required_capability_contracts: std::collections::BTreeMap<String, (String, String)>,
}

impl Default for CodeStore {
    fn default() -> Self {
        Self::new()
    }
}

impl CodeStore {
    pub fn new() -> Self {
        CodeStore {
            functions: Vec::new(),
            fn_index: std::collections::HashMap::new(),
            symbols: VmSymbolTable::default(),
            record_field_slots: std::collections::HashMap::new(),
            required_capability_operations: std::collections::BTreeSet::new(),
            required_capability_contracts: std::collections::BTreeMap::new(),
        }
    }

    pub fn add_function(&mut self, chunk: FnChunk) -> u32 {
        let id = self.functions.len() as u32;
        self.fn_index.insert(chunk.name.clone(), id);
        self.functions.push(chunk);
        id
    }

    pub fn get(&self, id: u32) -> &FnChunk {
        &self.functions[id as usize]
    }

    pub fn find(&self, name: &str) -> Option<u32> {
        self.fn_index.get(name).copied()
    }

    pub fn register_record_fields(&mut self, type_id: u32, field_symbol_ids: &[u32]) {
        for (field_idx, symbol_id) in field_symbol_ids.iter().copied().enumerate() {
            self.record_field_slots
                .insert((type_id, symbol_id), field_idx as u8);
        }
    }

    /// Resolve a bytecode position to (source_file, source_line).
    /// Returns None if line table is empty or fn_id is invalid.
    pub fn resolve_source_location(&self, fn_id: u32, ip: u32) -> Option<(&str, u16)> {
        let chunk = self.functions.get(fn_id as usize)?;
        if chunk.line_table.is_empty() {
            return None;
        }
        // Binary search: find last entry where offset <= ip
        let ip16 = ip as u16;
        let idx = match chunk
            .line_table
            .binary_search_by_key(&ip16, |&(off, _)| off)
        {
            Ok(i) => i,
            Err(0) => return None,
            Err(i) => i - 1,
        };
        let (_, line) = chunk.line_table[idx];
        let file = if chunk.source_file.is_empty() {
            None
        } else {
            Some(chunk.source_file.as_str())
        };
        Some((file.unwrap_or(""), line))
    }
}

/// Source location resolved from line table (cold-path only).
#[derive(Debug, Default, Clone)]
pub struct VmSourceLoc {
    pub file: String,
    pub line: u16,
    pub fn_name: String,
}

/// VM runtime error.
#[derive(Debug)]
pub enum VmError {
    /// Runtime error with message and optional source line.
    Runtime { msg: String, line: u16 },
    /// Type error (e.g. adding int + string).
    Type { msg: String, line: u16 },
    /// Non-exhaustive match at source line.
    MatchFail(u16),
    /// Stack underflow (bug in compiler).
    StackUnderflow,
    /// Dispatched opcode count exceeded `VM::step_limit`. Carries the
    /// limit value so the caller (verify runner) can put it in the
    /// failure message — "did not converge in 10_000_000 steps".
    StepLimit { limit: u64, line: u16 },
}

impl VmError {
    pub fn runtime(msg: impl Into<String>) -> Self {
        VmError::Runtime {
            msg: msg.into(),
            line: 0,
        }
    }

    pub fn type_err(msg: impl Into<String>) -> Self {
        VmError::Type {
            msg: msg.into(),
            line: 0,
        }
    }

    /// Attach resolved source location (cold path).
    pub fn with_location(self, loc: Option<VmSourceLoc>) -> Self {
        let Some(loc) = loc else { return self };
        if loc.line == 0 {
            return self;
        }
        match self {
            VmError::Runtime { msg, line: 0 } => VmError::Runtime {
                msg,
                line: loc.line,
            },
            VmError::Type { msg, line: 0 } => VmError::Type {
                msg,
                line: loc.line,
            },
            other => other,
        }
    }
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::Runtime { msg, line } if *line > 0 => {
                write!(f, "Runtime error [line {}]: {}", line, msg)
            }
            VmError::Runtime { msg, .. } => write!(f, "Runtime error: {}", msg),
            VmError::Type { msg, line } if *line > 0 => {
                write!(f, "Type error [line {}]: {}", line, msg)
            }
            VmError::Type { msg, .. } => write!(f, "Type error: {}", msg),
            VmError::MatchFail(line) => write!(f, "Non-exhaustive match at line {}", line),
            VmError::StackUnderflow => write!(f, "Internal error: stack underflow"),
            VmError::StepLimit { limit, line } if *line > 0 => write!(
                f,
                "VM step limit exceeded ({} steps) [line {}]",
                limit, line
            ),
            VmError::StepLimit { limit, .. } => {
                write!(f, "VM step limit exceeded ({} steps)", limit)
            }
        }
    }
}

impl std::error::Error for VmError {}
