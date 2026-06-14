//! Bridge between the storage-layer `NanValue`/`Arena` (in `aver_memory`)
//! and the runtime's arbitrary-precision integer `AverInt` (in `aver_rt`).
//!
//! `aver_memory` deliberately does not depend on `aver_rt`: it stores the raw
//! `num_bigint::BigInt` payload in the ℤ-overflow arena slot and exposes it as
//! an `ArenaIntRef`. This trait, defined on the `aver_lang` side (which sees
//! both crates), reconstructs the canonical `AverInt` and writes it back,
//! preserving the small-int fast path: an `i64`-fitting value never allocates.

use aver_rt::AverInt;
use num_bigint::BigInt;

use super::{Arena, NanValue};
use aver_memory::ArenaIntRef;

/// Read/write `AverInt` values through a `NanValue` integer.
pub trait NanIntExt {
    /// Materialize the integer as a canonical `AverInt`. Inline and
    /// `i64`-overflow slots become `Small`; the ℤ-overflow slot becomes `Big`.
    // Takes `self` by value because `NanValue` is `Copy` (8 bytes), matching
    // the existing `NanValue::as_int(self, ...)` / `as_float(self)` convention.
    #[allow(clippy::wrong_self_convention)]
    fn as_aver_int(self, arena: &Arena) -> AverInt;

    /// Store an `AverInt`, keeping the representation canonical: a `Small`
    /// goes through the inline / `i64`-overflow path, a `Big` is boxed into
    /// the arena. Only values genuinely outside `i64` allocate.
    fn from_aver_int(value: AverInt, arena: &mut Arena) -> NanValue;
}

impl NanIntExt for NanValue {
    #[inline]
    fn as_aver_int(self, arena: &Arena) -> AverInt {
        match self.int_ref(arena) {
            ArenaIntRef::Small(n) => AverInt::from_i64(n),
            ArenaIntRef::Big(b) => big_to_aver_int(b),
        }
    }

    #[inline]
    fn from_aver_int(value: AverInt, arena: &mut Arena) -> NanValue {
        match value {
            AverInt::Small(n) => NanValue::new_int(n, arena),
            AverInt::Big(b) => NanValue::new_big_int(*b, arena),
        }
    }
}

/// Rebuild an `AverInt` from an arena `BigInt`. The arena only ever stores a
/// `BigInt` when it does not fit `i64`, so this is normally `Big` — but route
/// through `AverInt::from_bigint` so an in-range payload (e.g. a stale slot)
/// canonicalizes to `Small`, never a non-canonical `Big`.
#[inline]
fn big_to_aver_int(b: &BigInt) -> AverInt {
    AverInt::from_bigint(b.clone())
}
