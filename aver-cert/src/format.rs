//! Stable, compiler-independent certificate format declarations.
//!
//! The producer imports this module from `aver-cert`; there is intentionally
//! no separate format crate. The verifier is the authority for this surface,
//! and the compiler depends on it in the non-trust-bearing direction.

/// Public certificate-package layout understood by this verifier.
pub const FORMAT_VERSION: u32 = 1;

/// Manifest schema accepted by the standalone verifier. Version 2 made the
/// subject's `hostRoleTable` optional: modules without the Int box helper
/// declare `null`, pinned against a byte-derived proof of the helper's
/// absence, while modules with the helper pin the exact decoded table. A
/// module whose role scan the decoder cannot complete matches no manifest
/// value.
pub const CERT_SCHEMA_VERSION: u32 = 2;

/// Named theorem audited by the checker-owned witness.
pub const ARTIFACT_CERTIFICATE_ROOT: &str = "AverCert.Artifact.certificate";

/// Identity of the exact checker-owned Lean wall shipped by this release.
pub const CURRENT_WALL_ID: &str =
    "sha256:7637c3f8bc2b5f89278fccefe42fc2a9ff9021f90d57339b2ea2fb4e869c6e97";

/// Complete host-import surface admitted by the wasm-gc certificate format.
///
/// This list is verifier-owned. `aver-lang` tests its `EffectName` lowering
/// against it, so adding a compiler import cannot silently broaden what the
/// independent verifier accepts.
pub const WASM_GC_CAPABILITIES: &[(&str, &str)] = &[
    ("aver", "console_print"),
    ("aver", "console_error"),
    ("aver", "console_warn"),
    ("aver", "time_unix_ms"),
    ("aver", "request_method"),
    ("aver", "request_url"),
    ("aver", "request_query"),
    ("aver", "request_body"),
    ("aver", "request_headers_load"),
    ("aver", "response_text"),
    ("aver", "response_set_header"),
    ("aver", "http_send"),
    ("aver", "http_add_request_header"),
    ("aver", "http_clear_request_headers"),
    ("aver", "env_get"),
    ("aver", "env_set"),
    ("aver", "console_read_line"),
    ("aver", "args_len"),
    ("aver", "args_get"),
    ("aver", "random_float"),
    ("aver", "random_int"),
    ("aver", "time_sleep"),
    ("aver", "time_now"),
    ("aver", "float_sin"),
    ("aver", "float_cos"),
    ("aver", "float_atan2"),
    ("aver", "float_pow"),
    ("aver", "terminal_enable_raw_mode"),
    ("aver", "terminal_disable_raw_mode"),
    ("aver", "terminal_clear"),
    ("aver", "terminal_move_to"),
    ("aver", "terminal_print"),
    ("aver", "terminal_set_color"),
    ("aver", "terminal_reset_color"),
    ("aver", "terminal_read_key"),
    ("aver", "terminal_size"),
    ("aver", "terminal_hide_cursor"),
    ("aver", "terminal_show_cursor"),
    ("aver", "terminal_flush"),
    ("aver", "disk_read_text"),
    ("aver", "disk_write_text"),
    ("aver", "disk_append_text"),
    ("aver", "disk_exists"),
    ("aver", "disk_delete"),
    ("aver", "disk_delete_dir"),
    ("aver", "disk_list_dir"),
    ("aver", "disk_make_dir"),
    ("aver", "tcp_connect"),
    ("aver", "tcp_write_line"),
    ("aver", "tcp_read_line"),
    ("aver", "tcp_close"),
    ("aver", "tcp_send"),
    ("aver", "tcp_ping"),
    ("aver", "http_get"),
    ("aver", "http_head"),
    ("aver", "http_delete"),
    ("aver", "http_post"),
    ("aver", "http_put"),
    ("aver", "http_patch"),
    ("aver", "record_enter_group"),
    ("aver", "record_set_branch"),
    ("aver", "record_exit_group"),
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn capability_pairs_are_unique() {
        let unique = WASM_GC_CAPABILITIES
            .iter()
            .copied()
            .collect::<std::collections::BTreeSet<_>>();
        assert_eq!(unique.len(), WASM_GC_CAPABILITIES.len());
    }
}
