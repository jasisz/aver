// `VmBuiltin::ALL` is the list of builtins the VM can execute. The proof
// backends check their emission against the codegen table's `Builtin::ALL`
// (src/codegen/builtins.rs), which is the superset the emitters can render.
pub(crate) mod builtin;
mod compiler;
mod execute;
pub mod opcode;
mod profile;
pub mod runtime;
mod symbol;
mod types;

pub use compiler::{
    compile_program_with_loaded_modules, compile_program_with_mir_fallback,
    compile_program_with_modules,
};
/// Phase 4 of #252 — MIR vertical slice for the VM. Re-exported
/// so tests + future external consumers can reach
/// `classify_mir_program_coverage` without making the full
/// `vm::compiler` module public.
pub mod mir_vm {
    pub use super::compiler::mir::{
        MirVmCoverage, MirVmUnsupported, classify_mir_program_coverage,
    };
}
pub use execute::{
    VM, VmRuntimeOwnershipStats, VmSlotUniquenessStats, grants_the_mirror_could_not_afford,
};
pub use opcode::opcode_name;
pub use profile::{
    VmBuiltinProfile, VmFunctionProfile, VmOpcodeProfile, VmProfileReport, VmReturnStats,
};
pub use types::{CallFrame, CodeStore, FnChunk, VmError};

/// Register compiler-owned host record types in the arena before compilation.
/// Capability-owned records are compiled from their embedded Aver modules.
pub fn register_service_types(arena: &mut crate::nan_value::Arena) {
    arena.register_record_type(
        "HttpRequest",
        vec![
            "method".into(),
            "path".into(),
            "body".into(),
            "headers".into(),
        ],
    );
    arena.register_record_type(
        "Tcp.Connection",
        vec!["id".into(), "host".into(), "port".into()],
    );
    // Oracle: BranchPath is an opaque builtin wrapping a dewey-decimal string.
    // Only reachable via BranchPath.root / .child / .parse constructors.
    arena.register_record_type("BranchPath", vec!["dewey".into()]);
    // Oracle: EffectEvent represents one recorded effect emission in a
    // verify-trace assertion's view of a function's trace. Produced by
    // context-sensitive elaboration of effect-method calls inside trace
    // blocks; never constructed by user code directly.
    crate::types::effect_event::register(arena);
    // Oracle: Trace wraps the list of EffectEvent values emitted during a
    // verify-trace LHS evaluation. Only materialized via the `.trace`
    // projection on the verified function's return.
    crate::types::trace::register(arena);
}
