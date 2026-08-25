//! Rust-backend helpers shared by pure builtin and capability emission.
//!
//! Host operations are not composed here. Every standard effect is a
//! source-owned capability and reaches Rust through `provider_support`.

/// Pure builtins whose Rust return value needs conversion into the Aver
/// representation after the raw call is emitted.
pub(super) fn builtin_needs_str_conversion(name: &str) -> bool {
    matches!(
        name,
        "Int.fromString"
            | "Float.fromString"
            | "String.slice"
            | "String.charAt"
            | "String.toLower"
            | "String.toUpper"
            | "String.trim"
            | "String.trimStart"
            | "String.trimEnd"
            | "String.split"
            | "String.replace"
            | "String.replaceFirst"
            | "String.join"
            | "String.reverse"
            | "String.fromInt"
            | "String.fromFloat"
            | "String.fromBool"
            | "String.chars"
            | "String.fromCodePoint"
            | "Int.mod"
            | "Int.div"
            | "Bits.shiftLeft"
            | "Bits.shiftRight"
            | "Bits.low"
    )
}

/// Which manifest policy helper guards a capability namespace.
pub(super) fn policy_check_helper(name: &str) -> Option<&'static str> {
    if name.starts_with("Http.") {
        Some("aver_policy::check_http")
    } else if name.starts_with("Disk.") {
        Some("aver_policy::check_disk")
    } else if name.starts_with("Env.") {
        Some("aver_policy::check_env")
    } else {
        None
    }
}
