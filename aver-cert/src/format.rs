//! Stable, compiler-independent certificate format declarations.
//!
//! The producer imports this module from `aver-cert`; there is intentionally
//! no separate format crate. The verifier is the authority for this surface,
//! and the compiler depends on it in the non-trust-bearing direction.

/// Public certificate-package layout understood by this verifier.
pub const FORMAT_VERSION: u32 = 1;

/// Raw wasm-gc module artifact target admitted by schema 6.
pub const TARGET_WASM_GC: &str = "wasm-gc";

/// WASI 0.2 Component Model artifact target admitted by schema 6 through a
/// declared component envelope.
pub const TARGET_WASIP2: &str = "wasip2";

/// Only emitted-fragment profile admitted by schema 6.
pub const PROFILE_ID: &str = "AverUserProfile/v1";

/// Runtime ABI admitted for the wasm-gc artifact target in schema 6.
pub const RUNTIME_ABI_WASM_GC: &str = "aver-wasm-gc/0";

/// Runtime ABI admitted for wasip2 component certificates in schema 6.
pub const RUNTIME_ABI_WASIP2: &str = "aver-wasip2/0";

/// Top-level manifest field for a wasip2 component envelope declaration.
/// It is intentionally target-specific and is rejected for wasm-gc artifacts.
pub const WASIP2_COMPONENT_ENVELOPE_FIELD: &str = "wasip2ComponentEnvelope";

/// Field names inside the wasip2 component-envelope object.
pub const WASIP2_COMPONENT_ENVELOPE_KIND_FIELD: &str = "kind";
pub const WASIP2_COMPONENT_ENVELOPE_PREFIX_LEN_FIELD: &str = "prefix_len";
pub const WASIP2_COMPONENT_ENVELOPE_CORE_LEN_FIELD: &str = "embedded_core_module_len";
pub const WASIP2_COMPONENT_ENVELOPE_SUFFIX_LEN_FIELD: &str = "suffix_len";

/// Version tag for the declared `prefix ++ embedded_core_module ++ suffix`
/// component split.
pub const WASIP2_COMPONENT_ENVELOPE_KIND: &str = "prefix-core-suffix/v1";

/// Manifest-facing declaration of the wasip2 component envelope.
///
/// The declaration carries byte counts only. A verifier that consumes it must
/// slice the delivered component by these declared lengths and confirm equality
/// against the separately supplied declared bytes; it must not rediscover the
/// embedded core by parsing or navigating the delivered component.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Wasip2ComponentEnvelopeDeclaration {
    /// Bytes before the embedded Aver user-core module payload.
    pub prefix_len: u64,
    /// Bytes in the embedded Aver user-core module payload.
    pub embedded_core_module_len: u64,
    /// Bytes after the embedded Aver user-core module payload.
    pub suffix_len: u64,
}

impl Wasip2ComponentEnvelopeDeclaration {
    pub const fn from_lengths(
        prefix_len: u64,
        embedded_core_module_len: u64,
        suffix_len: u64,
    ) -> Self {
        Self {
            prefix_len,
            embedded_core_module_len,
            suffix_len,
        }
    }

    pub const fn kind(&self) -> &'static str {
        WASIP2_COMPONENT_ENVELOPE_KIND
    }

    pub fn component_len(&self) -> Option<u64> {
        self.prefix_len
            .checked_add(self.embedded_core_module_len)?
            .checked_add(self.suffix_len)
    }

    pub fn embedded_core_module_range(&self) -> Option<std::ops::Range<u64>> {
        let start = self.prefix_len;
        let end = start.checked_add(self.embedded_core_module_len)?;
        Some(start..end)
    }

    /// Split component bytes by declared lengths only. This helper does not
    /// inspect the bytes and is therefore suitable for the future trusted path,
    /// provided the caller also checks the returned slices against the declared
    /// byte sequences.
    pub fn split_component<'a>(
        &self,
        component: &'a [u8],
    ) -> Option<(&'a [u8], &'a [u8], &'a [u8])> {
        if self.embedded_core_module_len == 0 {
            return None;
        }
        let prefix_len = usize::try_from(self.prefix_len).ok()?;
        let core_len = usize::try_from(self.embedded_core_module_len).ok()?;
        let suffix_len = usize::try_from(self.suffix_len).ok()?;
        let core_end = prefix_len.checked_add(core_len)?;
        let declared_len = core_end.checked_add(suffix_len)?;
        if declared_len != component.len() {
            return None;
        }
        Some((
            &component[..prefix_len],
            &component[prefix_len..core_end],
            &component[core_end..],
        ))
    }
}

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
/// Version 6 added the wasip2 component-envelope byte binding used by the
/// producer's `--target wasip2 --certify` path.
pub const CERT_SCHEMA_VERSION: u32 = 6;

/// Named theorem audited by the checker-owned witness.
pub const ARTIFACT_CERTIFICATE_ROOT: &str = "AverCert.Artifact.certificate";

/// Identity of the exact checker-owned Lean wall shipped by this release.
pub const CURRENT_WALL_ID: &str =
    "sha256:6b5c0326a4db9eeffc13ec0d08453d1a62413216fa96f57e34e82a7387016c9c";

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
    fn wasip2_envelope_surface_names_schema_six_component_binding() {
        assert_eq!(TARGET_WASM_GC, "wasm-gc");
        assert_eq!(TARGET_WASIP2, "wasip2");
        assert_eq!(RUNTIME_ABI_WASM_GC, "aver-wasm-gc/0");
        assert_eq!(RUNTIME_ABI_WASIP2, "aver-wasip2/0");
        assert_eq!(WASIP2_COMPONENT_ENVELOPE_FIELD, "wasip2ComponentEnvelope");
        assert_eq!(WASIP2_COMPONENT_ENVELOPE_KIND_FIELD, "kind");
        assert_eq!(WASIP2_COMPONENT_ENVELOPE_PREFIX_LEN_FIELD, "prefix_len");
        assert_eq!(
            WASIP2_COMPONENT_ENVELOPE_CORE_LEN_FIELD,
            "embedded_core_module_len"
        );
        assert_eq!(WASIP2_COMPONENT_ENVELOPE_SUFFIX_LEN_FIELD, "suffix_len");
        assert_eq!(WASIP2_COMPONENT_ENVELOPE_KIND, "prefix-core-suffix/v1");
        assert_eq!(CERT_SCHEMA_VERSION, 6);
    }

    #[test]
    fn wasip2_component_envelope_declaration_splits_by_declared_lengths_only() {
        let declaration = Wasip2ComponentEnvelopeDeclaration::from_lengths(2, 3, 1);
        assert_eq!(declaration.kind(), WASIP2_COMPONENT_ENVELOPE_KIND);
        assert_eq!(declaration.component_len(), Some(6));
        assert_eq!(declaration.embedded_core_module_range(), Some(2..5));

        let (prefix, core, suffix) = declaration
            .split_component(b"abcdef")
            .expect("declared lengths match the component");
        assert_eq!(prefix, b"ab");
        assert_eq!(core, b"cde");
        assert_eq!(suffix, b"f");

        assert!(
            Wasip2ComponentEnvelopeDeclaration::from_lengths(2, 0, 1)
                .split_component(b"abc")
                .is_none(),
            "an empty embedded core declaration is not meaningful"
        );
        assert!(
            Wasip2ComponentEnvelopeDeclaration::from_lengths(2, 2, 0)
                .split_component(b"abc")
                .is_none(),
            "the declaration must account for the whole delivered component"
        );
        assert_eq!(
            Wasip2ComponentEnvelopeDeclaration::from_lengths(u64::MAX, 1, 0).component_len(),
            None,
            "length overflow must fail closed"
        );
    }

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

    #[test]
    fn format_spec_documents_wasip2_envelope_surface() {
        let spec = include_str!("../../docs/certificate-format.md");
        for phrase in [
            TARGET_WASIP2,
            RUNTIME_ABI_WASIP2,
            WASIP2_COMPONENT_ENVELOPE_FIELD,
            WASIP2_COMPONENT_ENVELOPE_KIND_FIELD,
            WASIP2_COMPONENT_ENVELOPE_PREFIX_LEN_FIELD,
            WASIP2_COMPONENT_ENVELOPE_CORE_LEN_FIELD,
            WASIP2_COMPONENT_ENVELOPE_SUFFIX_LEN_FIELD,
            WASIP2_COMPONENT_ENVELOPE_KIND,
        ] {
            assert!(
                spec.contains(phrase),
                "certificate-format.md does not document wasip2 envelope surface `{phrase}`"
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
        let expected = "pub const CERT_SCHEMA_VERSION: u32 = crate::format::CERT_SCHEMA_VERSION;";
        assert!(
            engine_src.contains(expected),
            "engine/mod.rs does not delegate to `{expected}`; the producer would emit a \
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
