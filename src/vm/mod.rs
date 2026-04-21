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
    #[cfg(feature = "terminal")]
    arena.register_record_type("Terminal.Size", vec!["width".into(), "height".into()]);
}
