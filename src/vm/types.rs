use crate::nan_value::NanValue;

/// A compiled function chunk — bytecode + metadata.
#[derive(Debug, Clone)]
pub struct FnChunk {
    pub name: String,
    pub arity: u8,
    pub local_count: u16,
    pub code: Vec<u8>,
    pub constants: Vec<NanValue>,
    /// Declared effects (e.g. `! [Console.print, Http]`). Empty for pure functions.
    pub effects: Vec<String>,
}

/// Minimal call frame: 16 bytes of metadata, no closure/upvalue fields.
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
    /// Whether this frame stored a young-region value into globals.
    pub globals_dirty: bool,
}

/// All compiled bytecode for a program.
#[derive(Debug, Clone)]
pub struct CodeStore {
    pub functions: Vec<FnChunk>,
    /// Map from function name to index in `functions`.
    pub fn_index: std::collections::HashMap<String, u32>,
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
}

/// VM runtime error.
#[derive(Debug)]
pub enum VmError {
    /// Runtime error with message.
    Runtime(String),
    /// Type error (e.g. adding int + string).
    Type(String),
    /// Non-exhaustive match at source line.
    MatchFail(u16),
    /// Stack underflow (bug in compiler).
    StackUnderflow,
}

impl std::fmt::Display for VmError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VmError::Runtime(msg) => write!(f, "Runtime error: {}", msg),
            VmError::Type(msg) => write!(f, "Type error: {}", msg),
            VmError::MatchFail(line) => write!(f, "Non-exhaustive match at line {}", line),
            VmError::StackUnderflow => write!(f, "Internal error: stack underflow"),
        }
    }
}

impl std::error::Error for VmError {}
