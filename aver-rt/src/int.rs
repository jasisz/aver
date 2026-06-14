//! Arbitrary-precision integer for the Aver runtime.
//!
//! Aver's `Int` is mathematical ℤ: total, never wrapping, faithful to the
//! Lean/Dafny proof model. `AverInt` is the runtime carrier for that model.
//! It is small-int optimized: any value that fits an `i64` is stored inline
//! as `Small`, and only genuinely large magnitudes spill to a heap `BigInt`.
//!
//! Native machine-integer speed is a separate, opt-in concern (a bounded
//! refinement type whose carrier the compiler lowers to raw `i64`); it is not
//! this type's job. `AverInt` is correctness-first: every arithmetic operation
//! produces the exact mathematical result.
//!
//! ## Canonical form
//!
//! The single invariant every constructor and operation upholds: a value that
//! fits `i64` is **always** `Small`. A `Big` payload never holds a value in
//! `[i64::MIN, i64::MAX]`. This makes the representation canonical, so derived
//! `Eq`, `Ord`, and `Hash` are unique — two numerically-equal values always
//! compare and hash identically regardless of how they were built. Map/Set
//! keying depends on this.

use core::cmp::Ordering;
use core::fmt;
use core::str::FromStr;

use num_bigint::BigInt;
use num_integer::Integer;
use num_traits::{FromPrimitive, ToPrimitive, Zero};

/// Arbitrary-precision integer (mathematical ℤ) with a small-int fast path.
///
/// Invariant: `Big` never holds a value representable as `i64` (see module
/// docs). Always construct/renormalize through this type's API.
#[derive(Clone)]
pub enum AverInt {
    /// A value that fits `i64` — the common, allocation-free case.
    Small(i64),
    /// A value outside the `i64` range. Boxed to keep the enum small (a
    /// bare `BigInt` is three machine words; boxing keeps `AverInt` at two).
    Big(Box<BigInt>),
}

impl AverInt {
    /// The mathematical integer `n`, stored inline.
    #[inline]
    pub const fn from_i64(n: i64) -> Self {
        AverInt::Small(n)
    }

    /// Zero.
    #[inline]
    pub const fn zero() -> Self {
        AverInt::Small(0)
    }

    /// Renormalize a `BigInt` to canonical form: demote to `Small` when it
    /// fits `i64`, otherwise box it as `Big`. Every path that produces or
    /// reconstructs a `BigInt` must funnel through here so the canonical
    /// invariant (a value fitting `i64` is *always* `Small`) holds — this is
    /// the sole sanctioned way to build a `Big`, which is why `Big`'s payload
    /// is private.
    #[inline]
    pub fn from_bigint(b: BigInt) -> Self {
        match b.to_i64() {
            Some(n) => AverInt::Small(n),
            None => AverInt::Big(Box::new(b)),
        }
    }

    /// Borrow as a `BigInt` for an operation that cannot stay in `i64`.
    #[inline]
    fn to_bigint(&self) -> BigInt {
        match self {
            AverInt::Small(n) => BigInt::from(*n),
            AverInt::Big(b) => (**b).clone(),
        }
    }

    /// `true` for the additive identity.
    #[inline]
    pub fn is_zero(&self) -> bool {
        match self {
            AverInt::Small(n) => *n == 0,
            AverInt::Big(b) => b.is_zero(),
        }
    }

    // -- Arithmetic (non-wrapping) ----------------------------------------
    //
    // Each operation takes the i64 fast path via `checked_*`; only on an
    // i64-overflow does it promote to `BigInt`. The result is always
    // renormalized, so e.g. `Big - Big` that cancels back into range returns
    // `Small`.

    /// `self + rhs` over ℤ (never wraps).
    pub fn add(&self, rhs: &AverInt) -> AverInt {
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            if let Some(s) = a.checked_add(*b) {
                return AverInt::Small(s);
            }
        }
        AverInt::from_bigint(self.to_bigint() + rhs.to_bigint())
    }

    /// `self - rhs` over ℤ (never wraps).
    pub fn sub(&self, rhs: &AverInt) -> AverInt {
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            if let Some(s) = a.checked_sub(*b) {
                return AverInt::Small(s);
            }
        }
        AverInt::from_bigint(self.to_bigint() - rhs.to_bigint())
    }

    /// `self * rhs` over ℤ (never wraps).
    pub fn mul(&self, rhs: &AverInt) -> AverInt {
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            if let Some(s) = a.checked_mul(*b) {
                return AverInt::Small(s);
            }
        }
        AverInt::from_bigint(self.to_bigint() * rhs.to_bigint())
    }

    /// `-self` over ℤ (never wraps; `-i64::MIN` promotes to `Big`).
    pub fn neg(&self) -> AverInt {
        match self {
            AverInt::Small(n) => match n.checked_neg() {
                Some(v) => AverInt::Small(v),
                None => AverInt::from_bigint(-BigInt::from(*n)),
            },
            AverInt::Big(b) => AverInt::from_bigint(-(**b).clone()),
        }
    }

    /// Euclidean quotient, matching `i64::div_euclid` and the Lean/Dafny
    /// `Int.ediv` model the proofs cite: the unique `q` with a remainder in
    /// `[0, |rhs|)`. Returns `None` when `rhs == 0`. Over ℤ there is no
    /// `i64::MIN / -1` overflow edge — it is just `i64::MAX + 1`, returned as
    /// a `Big`.
    pub fn div_euclid(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            // `div_euclid` on i64 is total except for `MIN / -1`, which
            // overflows; fall through to BigInt for that single edge.
            if let Some(q) = a.checked_div_euclid(*b) {
                return Some(AverInt::Small(q));
            }
        }
        let (q, _r) = euclid_div_rem(&self.to_bigint(), &rhs.to_bigint());
        Some(AverInt::from_bigint(q))
    }

    /// Euclidean remainder `self - rhs * div_euclid(self, rhs)`, matching
    /// `i64::rem_euclid` and the Lean/Dafny `Int.emod` model. Returns `None`
    /// when `rhs == 0`. The result is always non-negative and in `[0, |rhs|)`,
    /// independent of the sign of either operand.
    pub fn rem_euclid(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            if let Some(r) = a.checked_rem_euclid(*b) {
                return Some(AverInt::Small(r));
            }
        }
        let (_q, r) = euclid_div_rem(&self.to_bigint(), &rhs.to_bigint());
        Some(AverInt::from_bigint(r))
    }

    /// Truncating quotient (rounds toward zero), the semantics of the raw
    /// `/` operator. Returns `None` when `rhs == 0`. Distinct from
    /// `div_euclid` for negative operands; provided for the low-level
    /// arithmetic opcodes (`Int.div` uses the Euclidean form).
    pub fn div_trunc(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            if let Some(q) = a.checked_div(*b) {
                return Some(AverInt::Small(q));
            }
        }
        Some(AverInt::from_bigint(self.to_bigint() / rhs.to_bigint()))
    }

    /// Truncating remainder (sign follows the dividend), the semantics of the
    /// raw `%` operator. Returns `None` when `rhs == 0`.
    pub fn rem_trunc(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            if let Some(r) = a.checked_rem(*b) {
                return Some(AverInt::Small(r));
            }
        }
        Some(AverInt::from_bigint(self.to_bigint() % rhs.to_bigint()))
    }

    /// `|self|` over ℤ (never wraps; `|i64::MIN|` promotes to `Big`).
    pub fn abs(&self) -> AverInt {
        match self {
            AverInt::Small(n) => match n.checked_abs() {
                Some(v) => AverInt::Small(v),
                None => AverInt::from_bigint(BigInt::from(*n).magnitude().clone().into()),
            },
            AverInt::Big(b) => AverInt::from_bigint(BigInt::from(b.magnitude().clone())),
        }
    }

    /// The smaller of `self` and `other` (borrowing form, to avoid the
    /// by-value `Ord::min`/`max` and keep the small-int clone cheap).
    pub fn min_ref(&self, other: &AverInt) -> AverInt {
        if self <= other {
            self.clone()
        } else {
            other.clone()
        }
    }

    /// The larger of `self` and `other`.
    pub fn max_ref(&self, other: &AverInt) -> AverInt {
        if self >= other {
            self.clone()
        } else {
            other.clone()
        }
    }

    // -- Checked conversions to machine integers --------------------------
    //
    // Every conversion to a fixed-width machine integer is checked: it
    // returns `None` on out-of-range rather than wrapping or truncating.
    // Callers at index/sentinel sites map `None` to the language's `None`;
    // callers at capacity/host sites map `None` to a clean error.

    /// `self` as `i64` if it fits, else `None`.
    #[inline]
    pub fn to_i64(&self) -> Option<i64> {
        match self {
            AverInt::Small(n) => Some(*n),
            AverInt::Big(b) => b.to_i64(),
        }
    }

    /// `self` as `usize` if it fits (non-negative and in range), else `None`.
    #[inline]
    pub fn to_usize(&self) -> Option<usize> {
        match self {
            AverInt::Small(n) => usize::try_from(*n).ok(),
            AverInt::Big(b) => b.to_usize(),
        }
    }

    /// `self` as `u16` if it fits, else `None`.
    #[inline]
    pub fn to_u16(&self) -> Option<u16> {
        match self {
            AverInt::Small(n) => u16::try_from(*n).ok(),
            AverInt::Big(b) => b.to_u16(),
        }
    }

    /// `self` as `u32` if it fits, else `None`.
    #[inline]
    pub fn to_u32(&self) -> Option<u32> {
        match self {
            AverInt::Small(n) => u32::try_from(*n).ok(),
            AverInt::Big(b) => b.to_u32(),
        }
    }

    /// `self` as `f64`, lossily. Huge magnitudes saturate to `±∞` (never
    /// `NaN`), matching the Lean prelude's `Float.ofInt`/IEEE coercion. This
    /// is the only intentionally-lossy conversion.
    #[inline]
    pub fn to_f64(&self) -> f64 {
        match self {
            AverInt::Small(n) => *n as f64,
            // `BigInt::to_f64` returns `Some(±inf)` for out-of-range
            // magnitudes and is never `None`, so the unwrap is total.
            AverInt::Big(b) => b.to_f64().unwrap_or(f64::INFINITY),
        }
    }

    /// Truncate a finite `f64` toward zero into ℤ. The exact mirror of the
    /// VM's `float_to_aver_int` (`src/types/int.rs`): non-finite (`NaN`/`±∞`)
    /// maps to `0`; an in-`i64`-range truncated value stays `Small`; an
    /// out-of-range *finite* magnitude is represented EXACTLY as a `Big` via
    /// `BigInt::from_f64`.
    ///
    /// This is the constructor `Int.fromFloat` and `Float.floor/ceil/round`
    /// must funnel through — a bare `f as i64` cast SATURATES huge finite
    /// floats to `i64::MAX`/`MIN` (a silent wrong value), which this avoids.
    pub fn from_f64_trunc(f: f64) -> AverInt {
        if !f.is_finite() {
            return AverInt::zero();
        }
        let truncated = f.trunc();
        match truncated.to_i64() {
            Some(n) => AverInt::Small(n),
            // Out of i64 range but finite: represent exactly via BigInt.
            None => match BigInt::from_f64(truncated) {
                Some(b) => AverInt::from_bigint(b),
                None => AverInt::zero(),
            },
        }
    }
}

/// Euclidean `(quotient, remainder)` for `BigInt` operands, value-identical to
/// `i64::div_euclid` / `i64::rem_euclid` across every sign combination: the
/// unique pair with `a == b*q + r` and `0 <= r < |b|`.
///
/// Note this is *not* num-integer's `div_floor`/`mod_floor`. Floored division
/// gives the remainder the sign of the divisor, so it coincides with Euclidean
/// only when `b > 0`; for `b < 0` a floored remainder is negative. We start
/// from the truncating quotient/remainder (`div_rem`, remainder takes the
/// dividend's sign) and, when that remainder is negative, step it into
/// `[0, |b|)`: toward `b > 0` add `b` and drop the quotient by one; toward
/// `b < 0` subtract `b` and raise the quotient by one.
fn euclid_div_rem(a: &BigInt, b: &BigInt) -> (BigInt, BigInt) {
    let (q, r) = a.div_rem(b);
    if r.sign() == num_bigint::Sign::Minus {
        if b.sign() == num_bigint::Sign::Plus {
            (q - 1, r + b)
        } else {
            (q + 1, r - b)
        }
    } else {
        (q, r)
    }
}

// -- Equality / ordering / hashing -----------------------------------------
//
// The canonical-form invariant makes these total over representations: equal
// numbers are always the same variant carrying the same payload.

impl PartialEq for AverInt {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (AverInt::Small(a), AverInt::Small(b)) => a == b,
            (AverInt::Big(a), AverInt::Big(b)) => a == b,
            // Canonical form guarantees a Small and a Big are never equal.
            _ => false,
        }
    }
}

impl Eq for AverInt {}

impl Ord for AverInt {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (AverInt::Small(a), AverInt::Small(b)) => a.cmp(b),
            (AverInt::Big(a), AverInt::Big(b)) => a.cmp(b),
            // A Big is out of i64 range, so its sign decides the comparison
            // against any Small.
            (AverInt::Small(_), AverInt::Big(b)) => {
                if b.sign() == num_bigint::Sign::Minus {
                    Ordering::Greater
                } else {
                    Ordering::Less
                }
            }
            (AverInt::Big(a), AverInt::Small(_)) => {
                if a.sign() == num_bigint::Sign::Minus {
                    Ordering::Less
                } else {
                    Ordering::Greater
                }
            }
        }
    }
}

impl PartialOrd for AverInt {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl core::hash::Hash for AverInt {
    #[inline]
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        // Hash on the canonical numeric form: a Small and a numerically-equal
        // Big can never coexist (canonical invariant), so hashing the variant
        // payload is sufficient and consistent with `Eq`.
        match self {
            AverInt::Small(n) => n.hash(state),
            AverInt::Big(b) => b.hash(state),
        }
    }
}

impl fmt::Display for AverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AverInt::Small(n) => write!(f, "{}", n),
            AverInt::Big(b) => write!(f, "{}", b),
        }
    }
}

impl fmt::Debug for AverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print the bare number so `Value::Int(..)`'s derived `Debug` reads
        // `Int(42)` (not `Int(AverInt(42))`), keeping golden strings stable.
        match self {
            AverInt::Small(n) => write!(f, "{}", n),
            AverInt::Big(b) => write!(f, "{}", b),
        }
    }
}

impl FromStr for AverInt {
    type Err = ();

    /// Parse a decimal integer of arbitrary length. Rejects empty/garbage
    /// input (and anything `BigInt` rejects) with `Err(())`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Fast path: most parsed ints fit i64.
        if let Ok(n) = s.parse::<i64>() {
            return Ok(AverInt::Small(n));
        }
        match BigInt::from_str(s) {
            Ok(b) => Ok(AverInt::from_bigint(b)),
            Err(_) => Err(()),
        }
    }
}

impl From<i64> for AverInt {
    #[inline]
    fn from(n: i64) -> Self {
        AverInt::Small(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn big(s: &str) -> AverInt {
        AverInt::from_str(s).unwrap()
    }

    #[test]
    fn canonical_form_demotes_to_small() {
        // i64::MAX * 2 / 2 returns to range and must be Small again.
        let a = AverInt::from_i64(i64::MAX);
        let doubled = a.add(&a);
        assert!(matches!(doubled, AverInt::Big(_)));
        let halved = doubled.div_euclid(&AverInt::from_i64(2)).unwrap();
        assert_eq!(halved, AverInt::from_i64(i64::MAX));
        assert!(matches!(halved, AverInt::Small(_)));
    }

    #[test]
    fn square_is_non_negative_past_i64() {
        // The exact runtime-vs-proof law: a*a >= 0 even where i64 would wrap.
        let a = AverInt::from_i64(i64::MAX);
        let sq = a.mul(&a);
        assert!(matches!(sq, AverInt::Big(_)));
        assert!(sq >= AverInt::zero());
    }

    #[test]
    fn equal_bigs_built_differently_are_equal_and_hash_equal() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        let big_a = AverInt::from_i64(i64::MAX).add(&AverInt::from_i64(1));
        let big_b = big("9223372036854775808");
        assert_eq!(big_a, big_b);
        let mut ha = DefaultHasher::new();
        let mut hb = DefaultHasher::new();
        big_a.hash(&mut ha);
        big_b.hash(&mut hb);
        assert_eq!(ha.finish(), hb.finish());
    }

    #[test]
    fn from_bigint_demotes_in_range_value_to_small() {
        use std::collections::hash_map::DefaultHasher;
        use std::hash::{Hash, Hasher};
        // A BigInt that fits i64 must canonicalize to Small, so it compares
        // AND hashes equal to the directly-built Small (the invariant Map/Set
        // keys depend on). The public `from_bigint` is the only sanctioned way
        // to build a Big, and it upholds this.
        let from_big = AverInt::from_bigint(BigInt::from(5));
        let small = AverInt::from_i64(5);
        assert!(matches!(from_big, AverInt::Small(5)));
        assert_eq!(from_big, small);
        let mut hb = DefaultHasher::new();
        let mut hs = DefaultHasher::new();
        from_big.hash(&mut hb);
        small.hash(&mut hs);
        assert_eq!(hb.finish(), hs.finish());

        // Boundary values that exactly fit i64 also demote.
        assert!(matches!(
            AverInt::from_bigint(BigInt::from(i64::MAX)),
            AverInt::Small(i64::MAX)
        ));
        assert!(matches!(
            AverInt::from_bigint(BigInt::from(i64::MIN)),
            AverInt::Small(i64::MIN)
        ));
        // One past the boundary stays Big.
        let past = AverInt::from_bigint(BigInt::from(i64::MAX) + 1);
        assert!(matches!(past, AverInt::Big(_)));
    }

    #[test]
    fn euclidean_div_mod_match_i64_in_range() {
        for a in [-7i64, -1, 0, 1, 7, 100] {
            for b in [-3i64, -1, 1, 3, 5] {
                let ai = AverInt::from_i64(a);
                let bi = AverInt::from_i64(b);
                assert_eq!(
                    ai.div_euclid(&bi).unwrap(),
                    AverInt::from_i64(a.div_euclid(b))
                );
                assert_eq!(
                    ai.rem_euclid(&bi).unwrap(),
                    AverInt::from_i64(a.rem_euclid(b))
                );
            }
        }
    }

    #[test]
    fn euclidean_div_mod_big_branch_negative_divisor() {
        // 2^63: the smallest magnitude that forces the Big fallback (no in-range
        // i64 dividend reaches it). The exact case the i64 fast path can never
        // exercise, so it pins the BigInt code directly.
        let two_63 = AverInt::from_i64(i64::MAX).add(&AverInt::from_i64(1));
        assert!(matches!(two_63, AverInt::Big(_)));

        let neg3 = AverInt::from_i64(-3);
        let q = two_63.div_euclid(&neg3).unwrap();
        let r = two_63.rem_euclid(&neg3).unwrap();
        // Euclidean: 2^63 = (-3)*(-3074457345618258602) + 2, with 0 <= 2 < 3.
        assert_eq!(q, big("-3074457345618258602"));
        assert_eq!(r, AverInt::from_i64(2));
        assert!(r >= AverInt::zero());
    }

    #[test]
    fn euclidean_div_mod_big_full_sign_matrix() {
        // Force the Big branch on both operands across the whole sign matrix and
        // assert the Euclidean contract: 0 <= r < |b| and a == b*q + r.
        let pos = big("9223372036854775808"); // 2^63 (just past i64::MAX)
        let neg = big("-9223372036854775809"); // -2^63 - 1 (just past i64::MIN)
        let dpos = big("100000000000000000000"); // 10^20, Big divisor
        let dneg = big("-100000000000000000000"); // -10^20

        for a in [&pos, &neg] {
            for b in [&dpos, &dneg] {
                assert!(matches!(*a, AverInt::Big(_)));
                assert!(matches!(*b, AverInt::Big(_)));
                let q = a.div_euclid(b).unwrap();
                let r = a.rem_euclid(b).unwrap();
                // 0 <= r
                assert!(r >= AverInt::zero(), "remainder negative for {a}/{b}");
                // r < |b|
                assert!(r < b.abs(), "remainder >= |divisor| for {a}/{b}");
                // a == b*q + r
                assert_eq!(&b.mul(&q).add(&r), a, "identity broken for {a}/{b}");
            }
        }
    }

    #[test]
    fn div_by_zero_is_none() {
        assert!(AverInt::from_i64(5).div_euclid(&AverInt::zero()).is_none());
        assert!(AverInt::from_i64(5).rem_euclid(&AverInt::zero()).is_none());
    }

    #[test]
    fn min_div_neg_one_promotes_not_panics() {
        let min = AverInt::from_i64(i64::MIN);
        let q = min.div_euclid(&AverInt::from_i64(-1)).unwrap();
        assert!(matches!(q, AverInt::Big(_)));
        assert_eq!(q, big("9223372036854775808"));
    }

    #[test]
    fn abs_of_min_promotes() {
        let q = AverInt::from_i64(i64::MIN).abs();
        assert_eq!(q, big("9223372036854775808"));
    }

    #[test]
    fn checked_conversions_reject_out_of_range() {
        let huge = big("99999999999999999999999999");
        assert_eq!(huge.to_i64(), None);
        assert_eq!(huge.to_usize(), None);
        assert_eq!(huge.to_u32(), None);
        assert_eq!(huge.to_u16(), None);
        assert_eq!(AverInt::from_i64(-1).to_usize(), None);
        assert_eq!(AverInt::from_i64(70000).to_u16(), None);
    }

    #[test]
    fn to_f64_saturates_to_infinity() {
        let huge = big("1").mul(&big("10").mul(&big("10"))); // small, sanity
        assert_eq!(huge.to_f64(), 100.0);
        let enormous = AverInt::from_i64(10).mul(&AverInt::from_i64(10));
        assert_eq!(enormous.to_f64(), 100.0);
        // 10^400 overflows f64 -> +inf, never NaN.
        let mut p = AverInt::from_i64(1);
        let ten = AverInt::from_i64(10);
        for _ in 0..400 {
            p = p.mul(&ten);
        }
        assert!(p.to_f64().is_infinite() && p.to_f64() > 0.0);
        assert!(p.neg().to_f64().is_infinite() && p.neg().to_f64() < 0.0);
    }

    #[test]
    fn parse_roundtrip_past_i64() {
        let s = "170141183460469231731687303715884105727"; // 2^127 - 1
        let v = big(s);
        assert!(matches!(v, AverInt::Big(_)));
        assert_eq!(v.to_string(), s);
    }

    #[test]
    fn from_str_rejects_garbage() {
        assert!(AverInt::from_str("").is_err());
        assert!(AverInt::from_str("12x").is_err());
        assert!(AverInt::from_str("1.5").is_err());
    }

    #[test]
    fn from_f64_trunc_preserves_huge_finite_magnitudes() {
        // The fix #1 case: a float far past i64::MAX must NOT saturate to
        // i64::MAX (`as i64`), but produce the EXACT BigInt — mirroring the
        // VM's `float_to_aver_int`.
        let v = AverInt::from_f64_trunc(1e20);
        assert!(matches!(v, AverInt::Big(_)));
        assert_eq!(v.to_string(), "100000000000000000000");
        // Negative huge magnitude is exact too.
        let n = AverInt::from_f64_trunc(-1e20);
        assert_eq!(n.to_string(), "-100000000000000000000");
    }

    #[test]
    fn from_f64_trunc_truncates_toward_zero_in_range() {
        assert_eq!(AverInt::from_f64_trunc(3.9), AverInt::from_i64(3));
        assert_eq!(AverInt::from_f64_trunc(-3.9), AverInt::from_i64(-3));
        assert_eq!(AverInt::from_f64_trunc(0.0), AverInt::zero());
    }

    #[test]
    fn from_f64_trunc_non_finite_is_zero() {
        // NaN / ±∞ have no integer; map to 0 (matching the VM's cast).
        assert_eq!(AverInt::from_f64_trunc(f64::NAN), AverInt::zero());
        assert_eq!(AverInt::from_f64_trunc(f64::INFINITY), AverInt::zero());
        assert_eq!(AverInt::from_f64_trunc(f64::NEG_INFINITY), AverInt::zero());
    }
}
