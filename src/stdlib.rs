//! Aver modules shipped with the compiler.
//!
//! Standard modules remain ordinary Aver source: they pass through the same
//! parser, type checker, VM compiler, and proof exporters as project modules.
//! Embedding only gives them an installation-independent home.

/// An Aver source module embedded in the compiler binary.
pub(crate) struct EmbeddedModule {
    pub(crate) virtual_path: &'static str,
    pub(crate) source: &'static str,
}

/// Resolve module names reserved by Aver's standard library.
///
/// Standard modules win over a same-named project file so a dependency cannot
/// silently change meaning with `--module-root`.
pub(crate) fn find(name: &str) -> Option<EmbeddedModule> {
    match name {
        "Bytes" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/bytes.av",
            source: include_str!("../stdlib/bytes.av"),
        }),
        "Crypto.Digest32" => Some(EmbeddedModule {
            virtual_path: "<aver-stdlib>/crypto/digest32.av",
            source: include_str!("../stdlib/crypto/digest32.av"),
        }),
        _ => None,
    }
}
