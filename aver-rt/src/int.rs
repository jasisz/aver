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

/// Why a shift count or bit width was refused by [`AverInt::shift_left`],
/// [`AverInt::shift_right`] or [`AverInt::low_bits`].
///
/// The two cases are deliberately distinct because they belong to different
/// layers. `Negative` is a value the language defines an answer for — a
/// catchable `Result.Err` — while `Unrepresentable` is the machine running
/// out of address space to name a bit position, which no amount of source
/// handling can recover and which the mathematical model does not even see.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftCountError {
    /// The count was below zero. A negative count has no meaning under
    /// infinite two's complement, and must not silently reverse direction,
    /// clamp, or wrap: the caller reports it as `Result.Err`.
    Negative,
    /// The count was non-negative but too large to name a bit position on
    /// this machine (roughly 2^64). Nothing could hold the answer.
    Unrepresentable,
}

/// Validate a shift count / bit width once, for all three width-taking
/// operations, so the negative rule is stated in exactly one place.
fn shift_count(n: &AverInt) -> Result<usize, ShiftCountError> {
    match n.to_usize() {
        Some(count) => Ok(count),
        None if n < &AverInt::zero() => Err(ShiftCountError::Negative),
        None => Err(ShiftCountError::Unrepresentable),
    }
}

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

    // -- Bit-level view (infinite two's complement) ------------------------
    //
    // The `Bits` namespace reads an `Int` as its infinite two's-complement
    // bit sequence: a non-negative value has infinitely many leading zeroes,
    // a negative one infinitely many leading ones. That is exactly the
    // sequence `i64` already carries (sign extension IS the infinite tail),
    // so the `Small`/`Small` fast path is the native machine operator with
    // no correction — and `num_bigint`'s `BitAnd`/`BitOr`/`BitXor` implement
    // the same convention for the promoted case. There is no width anywhere:
    // width is requested explicitly through `low_bits`.

    /// Pointwise `and` over the infinite two's-complement sequences.
    /// `bit_and(-1, x) == x` for every `x` (all-ones is the identity).
    pub fn bit_and(&self, rhs: &AverInt) -> AverInt {
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            return AverInt::Small(a & b);
        }
        AverInt::from_bigint(self.to_bigint() & rhs.to_bigint())
    }

    /// Pointwise `or`. `bit_or(-1, x) == -1`, `bit_or(0, x) == x`.
    pub fn bit_or(&self, rhs: &AverInt) -> AverInt {
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            return AverInt::Small(a | b);
        }
        AverInt::from_bigint(self.to_bigint() | rhs.to_bigint())
    }

    /// Pointwise `xor`. `bit_xor(-1, x) == bit_not(x)`, `bit_xor(x, x) == 0`.
    pub fn bit_xor(&self, rhs: &AverInt) -> AverInt {
        if let (AverInt::Small(a), AverInt::Small(b)) = (self, rhs) {
            return AverInt::Small(a ^ b);
        }
        AverInt::from_bigint(self.to_bigint() ^ rhs.to_bigint())
    }

    /// Pointwise complement — equivalently `-self - 1` over ℤ, which is the
    /// only total reading of "flip every bit" without a width to complement
    /// against. Computed as the subtraction so the identity is the
    /// definition rather than a coincidence of the carrier: the `Small` arm
    /// cannot overflow (`!n` is in range for every `i64`).
    pub fn bit_not(&self) -> AverInt {
        match self {
            AverInt::Small(n) => AverInt::Small(!n),
            AverInt::Big(b) => AverInt::from_bigint(-(&**b) - 1),
        }
    }

    /// `self * 2^n` — the bit sequence moved `n` places toward the more
    /// significant end. Over ℤ this never truncates.
    pub fn shift_left(&self, n: &AverInt) -> Result<AverInt, ShiftCountError> {
        let count = shift_count(n)?;
        if let AverInt::Small(a) = self
            && count < 63
        {
            let shifted = a << count;
            // Round-trip oracle: no bit left the i64 iff shifting back
            // restores the input (`>>` on a signed value is arithmetic, so
            // this holds for both signs).
            if (shifted >> count) == *a {
                return Ok(AverInt::Small(shifted));
            }
        }
        Ok(AverInt::from_bigint(self.to_bigint() << count))
    }

    /// `floor(self / 2^n)` — the bit sequence moved `n` places toward the
    /// less significant end. This is an ARITHMETIC shift: the sign tail is
    /// what gets shifted in, so `shift_right(-3, 1) == -2`, never `-1` and
    /// never a host language's logical shift.
    pub fn shift_right(&self, n: &AverInt) -> Result<AverInt, ShiftCountError> {
        let count = shift_count(n)?;
        if let AverInt::Small(a) = self {
            // Rust's `>>` is arithmetic but only DEFINED below the bit
            // width. At or above it every value bit has been shifted out
            // and all that remains is the infinite sign tail.
            return Ok(AverInt::Small(if count >= 63 {
                if *a < 0 { -1 } else { 0 }
            } else {
                a >> count
            }));
        }
        Ok(AverInt::from_bigint(self.to_bigint() >> count))
    }

    /// `self mod 2^width` — the non-negative integer carried by the lowest
    /// `width` bits, which is how a program asks for fixed-width behaviour
    /// explicitly. Always in `[0, 2^width)`, so `low_bits(-1, 8) == 255` and
    /// `low_bits(x, 0) == 0`.
    pub fn low_bits(&self, width: &AverInt) -> Result<AverInt, ShiftCountError> {
        let count = shift_count(width)?;
        if let AverInt::Small(a) = self {
            if count >= 64 {
                // A non-negative i64 is entirely below bit 64, so every bit
                // survives and the value is unchanged. A negative one has an
                // all-ones tail above bit 63 that the window still captures,
                // so it falls through to the exact bigint path.
                if *a >= 0 {
                    return Ok(AverInt::Small(*a));
                }
            } else {
                // `count == 63` gives the mask `i64::MAX`. Every result here
                // is non-negative and below `2^63`, so it always fits.
                let mask = if count == 63 {
                    i64::MAX
                } else {
                    (1i64 << count) - 1
                };
                return Ok(AverInt::Small(a & mask));
            }
        }
        let modulus = BigInt::from(1u8) << count;
        let (_, r) = euclid_div_rem(&self.to_bigint(), &modulus);
        Ok(AverInt::from_bigint(r))
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

    // -- Bit-level view ----------------------------------------------------
    //
    // The `Small`/`Big` split is an optimization, so every bit operation has
    // two implementations that must agree. These tests drive the seam: the
    // reference is the `BigInt` path (which handles both signs uniformly),
    // and the subject is the public API, which takes the `i64` fast path
    // wherever it can.

    /// Route a small value through the `BigInt` machinery so the promoted
    /// path is exercised on inputs the fast path would otherwise take. The
    /// canonical invariant demotes the RESULT back to `Small`, which is
    /// precisely what makes it comparable to the fast path's answer.
    fn via_bigint(n: i64) -> AverInt {
        AverInt::from_bigint((BigInt::from(n) << 200u32) >> 200u32)
    }

    /// A power of two as an `AverInt`, past the i64 cliff when `n >= 63`.
    fn two_pow(n: usize) -> AverInt {
        AverInt::from_bigint(BigInt::from(1u8) << n)
    }

    #[test]
    fn bitwise_matches_the_specified_identities() {
        let i = AverInt::from_i64;
        assert_eq!(i(6).bit_and(&i(3)), i(2));
        assert_eq!(i(6).bit_or(&i(3)), i(7));
        assert_eq!(i(6).bit_xor(&i(3)), i(5));

        // All-ones is the `and` identity and the `or` annihilator.
        assert_eq!(i(-1).bit_and(&i(42)), i(42));
        assert_eq!(i(-1).bit_or(&i(42)), i(-1));
        // `xor` against all-ones IS complement.
        assert_eq!(i(-1).bit_xor(&i(42)), i(42).bit_not());

        assert_eq!(i(0).bit_not(), i(-1));
        assert_eq!(i(-1).bit_not(), i(0));
        assert_eq!(i(123).bit_not().bit_not(), i(123));
    }

    #[test]
    fn bitwise_agrees_across_the_small_big_seam() {
        // Every sign combination, on values whose `Small` and `Big`
        // representations must produce identical answers.
        for a in [-1025i64, -257, -3, -1, 0, 1, 6, 255, 1024] {
            for b in [-1025i64, -257, -3, -1, 0, 1, 3, 255, 1024] {
                let (sa, sb) = (AverInt::from_i64(a), AverInt::from_i64(b));
                let (ba, bb) = (via_bigint(a), via_bigint(b));
                assert_eq!(sa.bit_and(&sb), ba.bit_and(&bb), "and {a} {b}");
                assert_eq!(sa.bit_or(&sb), ba.bit_or(&bb), "or {a} {b}");
                assert_eq!(sa.bit_xor(&sb), ba.bit_xor(&bb), "xor {a} {b}");
                // ... and against the arithmetic definition of complement.
                assert_eq!(sa.bit_not(), AverInt::from_i64(-a - 1), "not {a}");
                assert_eq!(ba.bit_not(), AverInt::from_i64(-a - 1), "big not {a}");
            }
        }
    }

    #[test]
    fn shifts_are_multiplication_and_floor_division_by_a_power_of_two() {
        let i = AverInt::from_i64;
        // Past the i64 cliff: the result is exact, not truncated.
        assert_eq!(
            i(1).shift_left(&i(100)).unwrap().to_string(),
            "1267650600228229401496703205376"
        );
        // Arithmetic, not logical: the sign tail shifts in.
        assert_eq!(i(-3).shift_right(&i(1)).unwrap(), i(-2));
        assert_eq!(i(-1).shift_right(&i(200)).unwrap(), i(-1));
        assert_eq!(i(1).shift_right(&i(200)).unwrap(), i(0));

        for x in [-1025i64, -257, -3, -1, 0, 1, 6, 255, 1024] {
            for n in [0usize, 1, 5, 62, 63, 64, 100] {
                let modulus = two_pow(n);
                let count = AverInt::from_i64(n as i64);
                assert_eq!(
                    AverInt::from_i64(x).shift_left(&count).unwrap(),
                    AverInt::from_i64(x).mul(&modulus),
                    "shl {x} {n}"
                );
                // Euclidean division by a POSITIVE divisor is floor division.
                assert_eq!(
                    AverInt::from_i64(x).shift_right(&count).unwrap(),
                    AverInt::from_i64(x).div_euclid(&modulus).unwrap(),
                    "shr {x} {n}"
                );
                assert_eq!(
                    via_bigint(x).shift_right(&count).unwrap(),
                    AverInt::from_i64(x).shift_right(&count).unwrap(),
                    "big shr {x} {n}"
                );
            }
        }
    }

    #[test]
    fn low_bits_is_the_non_negative_remainder_modulo_a_power_of_two() {
        let i = AverInt::from_i64;
        assert_eq!(i(257).low_bits(&i(8)).unwrap(), i(1));
        assert_eq!(i(-1).low_bits(&i(8)).unwrap(), i(255));
        assert_eq!(i(123).low_bits(&i(0)).unwrap(), i(0));

        for x in [-1025i64, -257, -3, -1, 0, 1, 6, 255, i64::MIN, i64::MAX] {
            for w in [0usize, 1, 8, 25, 32, 62, 63, 64, 65, 200] {
                let modulus = two_pow(w);
                let width = AverInt::from_i64(w as i64);
                let got = AverInt::from_i64(x).low_bits(&width).unwrap();
                assert_eq!(
                    got,
                    AverInt::from_i64(x).rem_euclid(&modulus).unwrap(),
                    "low {x} {w}"
                );
                // The two properties the docs promise, independently.
                assert!(got >= AverInt::zero(), "low {x} {w} is non-negative");
                assert!(got < modulus, "low {x} {w} is below 2^w");
            }
        }
    }

    #[test]
    fn negative_counts_are_refused_and_distinguished_from_unrepresentable() {
        let i = AverInt::from_i64;
        for op in [AverInt::shift_left, AverInt::shift_right, AverInt::low_bits] {
            assert_eq!(op(&i(42), &i(-1)), Err(ShiftCountError::Negative));
            // A huge NEGATIVE count is still `Negative`, not swallowed by
            // the representability check.
            let huge_negative = two_pow(200).neg();
            assert_eq!(op(&i(42), &huge_negative), Err(ShiftCountError::Negative));
            let huge = two_pow(200);
            assert_eq!(op(&i(42), &huge), Err(ShiftCountError::Unrepresentable));
        }
    }
}
