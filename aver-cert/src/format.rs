//! Stable, compiler-independent certificate format declarations.
//!
//! The producer imports this module from `aver-cert`; there is intentionally
//! no separate format crate. The verifier is the authority for this surface,
//! and the compiler depends on it in the non-trust-bearing direction.

/// Public certificate-package layout understood by this verifier.
pub const FORMAT_VERSION: u32 = 1;

/// Only artifact target admitted by schema 5. Future component certificates
/// will add their own envelope instead of reinterpreting wasm-gc bytes.
pub const TARGET_WASM_GC: &str = "wasm-gc";

/// Only emitted-fragment profile admitted by schema 5.
pub const PROFILE_ID: &str = "AverUserProfile/v1";

/// Runtime ABI admitted for the wasm-gc artifact target in schema 5.
pub const RUNTIME_ABI_WASM_GC: &str = "aver-wasm-gc/0";

/// Manifest schema accepted by the standalone verifier. Version 2 made the
/// subject's `hostRoleTable` optional: modules without the Int box helper
/// declare `null`, pinned against a byte-derived proof of the helper's
/// absence, while modules with the helper pin the exact decoded table. A
/// module whose role scan the decoder cannot complete matches no manifest
/// value. The declared arith helper indices (carrier, limb, and the
/// decompose/normalize/strip/umagCmp sub-routines) are proof-term data in the
/// generated Lean certificate, not public JSON, so the acceptance pin can
/// confirm each declared helper body byte-for-byte — box and toIndex included,
/// alongside add/sub/mul — without moving the public schema. Version 3 added
/// the required `toIndex` key to the object form of `hostRoleTable`: the fused
/// vector-read face binds the `__aint_to_index` helper by its named function
/// export, exactly like `box`, and the exact-object manifest matching means a
/// version-2 table would reject every module carrying the helper. Version 4
/// added the required `cmp` and `eq` keys for the same reason: the two Int
/// value-comparison faces bind `__aint_cmp` and `__aint_eq` by their named
/// function exports, and because those two helpers declare the same function
/// type, the export name is the only thing that tells one role from the other.
/// Version 5 added the required top-level `target` field and moved the
/// target/profile/ABI identifiers into the checker-owned statement schema.
pub const CERT_SCHEMA_VERSION: u32 = 5;

/// Named theorem audited by the checker-owned witness.
pub const ARTIFACT_CERTIFICATE_ROOT: &str = "AverCert.Artifact.certificate";

/// Identity of the exact checker-owned Lean wall shipped by this release.
pub const CURRENT_WALL_ID: &str =
    "sha256:b06a0b43554911693b6f8b4b539dff7bb82b2dc8b1081b956c3a5db5238df8a0";

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
    ("aver", "process_stop_requested"),
    ("aver", "provider_contract_violation"),
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
    ("aver", "disk_read_bytes"),
    ("aver", "disk_read_bytes_at"),
    ("aver", "disk_write_bytes"),
    ("aver", "disk_append_bytes"),
    ("aver", "disk_size"),
    ("aver", "disk_exists"),
    ("aver", "disk_delete"),
    ("aver", "disk_delete_dir"),
    ("aver", "disk_list_dir"),
    ("aver", "disk_make_dir"),
    ("aver", "tcp_connect"),
    ("aver", "tcp_begin_connect"),
    ("aver", "tcp_dialled"),
    ("aver", "tcp_listen"),
    ("aver", "tcp_accept"),
    ("aver", "tcp_peer_address"),
    ("aver", "tcp_write_line"),
    ("aver", "tcp_write_bytes"),
    ("aver", "tcp_read_line"),
    ("aver", "tcp_read_bytes"),
    ("aver", "tcp_read_some"),
    ("aver", "tcp_poll"),
    ("aver", "tcp_close"),
    ("aver", "tcp_close_dial"),
    ("aver", "tcp_close_listener"),
    ("aver", "tcp_send"),
    ("aver", "tcp_send_bytes"),
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

/// Whether a raw wasm-gc import is part of the certificate's admitted host
/// surface. Compiler-shipped effects use the finite table above. Program-
/// defined capabilities use a deterministic contract-derived namespace:
/// `aver:user/cap-n<module-utf8-hex>-c<sha256>` and
/// `op-n<operation-utf8-hex>`.
pub fn is_wasm_gc_capability_import(module: &str, field: &str) -> bool {
    WASM_GC_CAPABILITIES.contains(&(module, field))
        || is_custom_wasm_gc_capability_import(module, field)
}

fn is_custom_wasm_gc_capability_import(module: &str, field: &str) -> bool {
    const MODULE_PREFIX: &str = "aver:user/cap-n";
    const OP_PREFIX: &str = "op-n";

    let Some(module_tail) = module.strip_prefix(MODULE_PREFIX) else {
        return false;
    };
    let Some((module_hex, contract_hash)) = module_tail.split_once("-c") else {
        return false;
    };
    let Some(operation_hex) = field.strip_prefix(OP_PREFIX) else {
        return false;
    };

    is_nonempty_even_lower_hex(module_hex)
        && contract_hash.len() == 64
        && is_lower_hex(contract_hash)
        && is_nonempty_even_lower_hex(operation_hex)
}

fn is_nonempty_even_lower_hex(value: &str) -> bool {
    !value.is_empty() && value.len().is_multiple_of(2) && is_lower_hex(value)
}

fn is_lower_hex(value: &str) -> bool {
    value
        .bytes()
        .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
}

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

    #[test]
    fn custom_capability_import_syntax_is_exact() {
        let hash = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef";
        assert!(is_wasm_gc_capability_import(
            &format!("aver:user/cap-n436c6f636b-c{hash}"),
            "op-n6e6f77"
        ));
        for (module, field) in [
            (format!("aver:user/cap-n-c{hash}"), "op-n6e6f77"),
            (format!("aver:user/cap-n436c6f636b-c{hash}0"), "op-n6e6f77"),
            (format!("aver:user/cap-n436C6f636b-c{hash}"), "op-n6e6f77"),
            (format!("aver:user/cap-n436c6f636b-c{hash}"), "op-n"),
            (format!("aver:user/cap-n436c6f636b-c{hash}"), "op-nxyz"),
        ] {
            assert!(!is_wasm_gc_capability_import(&module, field));
        }
    }

    /// The format specification (`docs/certificate-format.md`) prints the wall
    /// identity and the wall source count verbatim; both are hand-written and
    /// silently go stale when the wall changes (they did: the spec once printed
    /// a wall id and count from an earlier wall). An independent reimplementor
    /// takes these as normative, so a stale value makes the spec reject every
    /// real package. Keep the doc pinned to reality.
    #[test]
    fn format_spec_states_the_current_wall_identity_and_source_count() {
        let spec = include_str!("../../docs/certificate-format.md");
        assert!(
            spec.contains(CURRENT_WALL_ID),
            "certificate-format.md does not state the current wall id {CURRENT_WALL_ID}; \
             update the wall identity printed in the spec"
        );
        let count_phrase = format!(
            "{} embedded `.lean` wall sources",
            crate::wall::SOURCES.len()
        );
        assert!(
            spec.contains(&count_phrase),
            "certificate-format.md does not state \"{count_phrase}\"; \
             update the wall source count printed in the spec"
        );
    }

    /// The same staleness trap applies to the statement schema version: the
    /// spec prints it in the identity table and again in the envelope-check
    /// step of the acceptance pipeline, and a reimplementor takes both as
    /// normative. A spec left one version behind describes a verifier that
    /// rejects every package this release produces.
    #[test]
    fn format_spec_states_the_current_schema_version() {
        let spec = include_str!("../../docs/certificate-format.md");
        for phrase in [
            format!("`{CERT_SCHEMA_VERSION}` (`CERT_SCHEMA_VERSION`)"),
            format!("schema_version = {CERT_SCHEMA_VERSION}"),
        ] {
            assert!(
                spec.contains(&phrase),
                "certificate-format.md does not state \"{phrase}\"; \
                 update the schema version printed in the spec"
            );
        }
    }

    /// The schema version lives in TWO constants — this one, which the
    /// transport parser enforces, and `engine::CERT_SCHEMA_VERSION`, which the
    /// producer emits. They are read on opposite sides of the package
    /// boundary, so a bump applied to only one silently produces packages the
    /// same build refuses, and nothing else ties them together.
    ///
    /// Read as SOURCE TEXT rather than as a constant, deliberately: the engine
    /// module is feature-gated (`engine`/`plans`) while this module is not, so
    /// a typed comparison would compile away in exactly the lane that runs
    /// these tests. The doc-pin tests above use the same technique.
    #[test]
    fn the_two_schema_version_constants_agree() {
        let engine_src = include_str!("engine/mod.rs");
        let expected = format!("pub const CERT_SCHEMA_VERSION: u32 = {CERT_SCHEMA_VERSION};");
        assert!(
            engine_src.contains(&expected),
            "engine/mod.rs does not declare `{expected}`; the producer would emit a \
             schema version the transport parser rejects"
        );
    }

    /// Every runtime contract string is a VERBATIM twin: `ClaimAxes.lean`
    /// declares it in the wall, `engine/mod.rs` declares it for the producer,
    /// and the kernel's `contractsMatch` compares the manifest's list against
    /// the wall's own. A one-character drift on either side fails every package
    /// closed, and it is invisible in review — the two strings live in
    /// different languages, in different files, edited in different commits.
    #[test]
    fn every_wall_contract_string_has_a_verbatim_producer_twin() {
        let axes = include_str!("../assets/wall/current/ClaimAxes.lean");
        let engine_src = include_str!("engine/mod.rs");

        // `def <name>Contract : String :=` followed by the quoted literal.
        let mut seen = 0usize;
        let mut lines = axes.lines();
        while let Some(line) = lines.next() {
            let trimmed = line.trim();
            if !(trimmed.starts_with("def ") && trimmed.ends_with("Contract : String :=")) {
                continue;
            }
            let literal = lines
                .next()
                .expect("a contract definition is followed by its literal")
                .trim();
            assert!(
                literal.starts_with('"') && literal.ends_with('"'),
                "expected a one-line string literal under `{trimmed}`, got: {literal}"
            );
            assert!(
                engine_src.contains(literal),
                "wall contract `{trimmed}` has no verbatim producer twin in \
                 engine/mod.rs: {literal}"
            );
            seen += 1;
        }
        assert!(
            seen >= 12,
            "only {seen} wall contract definitions were found; the scan shape has \
             drifted and this test has stopped checking anything"
        );
    }
}
