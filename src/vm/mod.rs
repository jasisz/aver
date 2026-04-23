mod builtin;
mod compiler;
mod execute;
pub mod opcode;
mod profile;
mod runtime;
mod symbol;
mod types;

pub use compiler::{
    compile_program, compile_program_with_loaded_modules, compile_program_with_modules,
};
pub use execute::VM;
pub use opcode::opcode_name;
pub use profile::{
    VmBuiltinProfile, VmFunctionProfile, VmOpcodeProfile, VmProfileReport, VmReturnStats,
};
pub use types::{CallFrame, CodeStore, FnChunk, VmError};

/// Register builtin service record types (HttpResponse, HttpRequest, etc.)
/// in the arena before compilation. These types are used by services but
/// not declared in user code.
pub fn register_service_types(arena: &mut crate::nan_value::Arena) {
    arena.register_record_type(
        "HttpResponse",
        vec!["status".into(), "body".into(), "headers".into()],
    );
    arena.register_record_type(
        "HttpRequest",
        vec![
            "method".into(),
            "path".into(),
            "body".into(),
            "headers".into(),
        ],
    );
    arena.register_record_type("Header", vec!["name".into(), "value".into()]);
    arena.register_record_type(
        "Tcp.Connection",
        vec!["id".into(), "host".into(), "port".into()],
    );
    // Always register Terminal.Size so record-field access works in
    // every build (the playground wasm ships without `terminal` but
    // the compiler still emits field reads against the stubs).
    arena.register_record_type("Terminal.Size", vec!["width".into(), "height".into()]);
    // Oracle: BranchPath is an opaque builtin wrapping a dewey-decimal string.
    // Only reachable via BranchPath.root / .child / .parse constructors.
    arena.register_record_type("BranchPath", vec!["dewey".into()]);
    // Oracle: EffectEvent represents one recorded effect emission in a
    // verify-trace assertion's view of a function's trace. Produced by
    // context-sensitive elaboration of effect-method calls inside trace
    // blocks; never constructed by user code directly.
    crate::types::effect_event::register(arena);
}
