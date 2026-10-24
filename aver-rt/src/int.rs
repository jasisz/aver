//! Arbitrary-precision integer for the Aver runtime.
//!
//! Aver's `Int` is mathematical ℤ: total, never wrapping, faithful to the
//! Lean proof model. `AverInt` is the runtime carrier for that model.
//! It takes one machine word: a value in `[-2^62, 2^62)` is stored in the word
//! itself, and only a larger magnitude spills to a heap `BigInt` (see
//! [`repr`]).
//!
//! Native machine-integer speed is a separate, opt-in concern (a bounded
//! refinement type whose carrier the compiler lowers to raw `i64`); it is not
//! this type's job. `AverInt` is correctness-first: every arithmetic operation
//! produces the exact mathematical result.
//!
//! ## Canonical form
//!
//! The single invariant every constructor and operation upholds: a value in
//! the inline range is **always** stored inline. A heap `BigInt` never holds a
//! value in `[-2^62, 2^62)`. This makes the representation canonical, so `Eq`,
//! `Ord`, and `Hash` can read the word first — two numerically-equal values
//! always compare and hash identically regardless of how they were built.
//! Map/Set keying depends on this.

use core::cmp::Ordering;
use core::fmt;
use core::str::FromStr;
use std::borrow::Cow;

use num_bigint::{BigInt, BigUint, Sign};
use num_integer::Integer;
use num_traits::{FromPrimitive, ToPrimitive};

/// Why a shift count or bit width was refused by [`AverInt::shift_left`],
/// [`AverInt::shift_right`] or [`AverInt::low_bits`].
///
/// Both cases are language-level, catchable refusals. The upper bound is
/// fixed across targets so a program cannot turn an ordinary `Int` into a
/// host-dependent allocation bomb.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ShiftCountError {
    /// The count was below zero. A negative count has no meaning under
    /// infinite two's complement, and must not silently reverse direction,
    /// clamp, or wrap: the caller reports it as `Result.Err`.
    Negative,
    /// The operation would have to materialize more than
    /// [`MAX_MATERIALIZED_BITS`] result bits. `shift_right` never returns this
    /// error because its result only gets smaller.
    TooLarge,
}

/// Maximum number of result bits one `shift_left` / `low_bits` call may have
/// to materialize. The corresponding raw magnitude is about 2 MiB. This is a
/// language constant, not a host `usize` limit, so VM, generated Rust, wasm,
/// and proof models share one refusal boundary.
pub const MAX_MATERIALIZED_BITS: usize = 16 * 1024 * 1024;

/// Public fixed-width Int encoder refusal text, derived from the shared
/// portable sequence budget rather than repeated in compiler backends.
pub fn int_endian_width_error_message(operation: &str) -> String {
    format!(
        "{operation}: width must be between 0 and {}",
        crate::MAX_MATERIALIZED_SEQUENCE_ELEMENTS
    )
}

/// Public unsigned fixed-width Int encoder overflow text.
pub fn int_endian_value_error_message(operation: &str) -> String {
    format!("{operation}: value does not fit in the requested width")
}

#[derive(Clone, Copy)]
enum EndianOrder {
    Big,
    Little,
}

fn fixed_width_octets(
    value: &AverInt,
    width: &AverInt,
    operation: &str,
    order: EndianOrder,
) -> Result<Vec<u8>, String> {
    let Some(width) = width.to_usize() else {
        return Err(int_endian_width_error_message(operation));
    };
    if width > crate::MAX_MATERIALIZED_SEQUENCE_ELEMENTS {
        return Err(int_endian_width_error_message(operation));
    }
    if value < &AverInt::zero() {
        return Err(int_endian_value_error_message(operation));
    }

    let (_, mut magnitude) = match order {
        EndianOrder::Big => value.to_bigint().to_bytes_be(),
        EndianOrder::Little => value.to_bigint().to_bytes_le(),
    };
    // num-bigint serializes zero as one zero octet; the fixed-width language
    // contract gives zero the empty minimal magnitude so `(0, 0) -> []`.
    if value.is_zero() {
        magnitude.clear();
    }
    if magnitude.len() > width {
        return Err(int_endian_value_error_message(operation));
    }

    let mut out = vec![0; width];
    match order {
        EndianOrder::Big => out[width - magnitude.len()..].copy_from_slice(&magnitude),
        EndianOrder::Little => out[..magnitude.len()].copy_from_slice(&magnitude),
    }
    Ok(out)
}

/// Encode a non-negative mathematical Int into exactly `width` big-endian
/// octets. Zero has the unique zero-width representation `[]`.
pub fn int_to_big_endian(value: &AverInt, width: &AverInt) -> Result<Vec<u8>, String> {
    fixed_width_octets(value, width, "Int.toBigEndian", EndianOrder::Big)
}

/// Encode a non-negative mathematical Int into exactly `width` little-endian
/// octets. Zero has the unique zero-width representation `[]`.
pub fn int_to_little_endian(value: &AverInt, width: &AverInt) -> Result<Vec<u8>, String> {
    fixed_width_octets(value, width, "Int.toLittleEndian", EndianOrder::Little)
}

/// Decode unsigned big-endian magnitude bytes into mathematical Int.
pub fn int_from_big_endian(bytes: &[u8]) -> AverInt {
    AverInt::from_bigint(BigInt::from_bytes_be(Sign::Plus, bytes))
}

/// Decode unsigned little-endian magnitude bytes into mathematical Int.
pub fn int_from_little_endian(bytes: &[u8]) -> AverInt {
    AverInt::from_bigint(BigInt::from_bytes_le(Sign::Plus, bytes))
}

/// Public `Bits.shiftLeft` refusal text, derived from the same language
/// constant that guards the allocation. Compiler backends call this while
/// emitting their own models so changing the boundary cannot leave stale
/// decimal text behind.
pub fn shift_count_too_large_message() -> String {
    format!("shift count exceeds the {MAX_MATERIALIZED_BITS} bit limit")
}

/// Public `Bits.low` refusal text for a negative input whose finite result
/// would exceed the shared materialization boundary.
pub fn bit_width_too_large_message() -> String {
    format!("bit width exceeds the {MAX_MATERIALIZED_BITS} bit limit")
}

/// Validate a count for an operation that may allocate proportionally to it.
fn materializing_count(n: &AverInt) -> Result<usize, ShiftCountError> {
    if n < &AverInt::zero() {
        return Err(ShiftCountError::Negative);
    }
    match n.to_usize() {
        Some(count) if count <= MAX_MATERIALIZED_BITS => Ok(count),
        Some(_) | None => Err(ShiftCountError::TooLarge),
    }
}

/// Whether a non-negative count reaches or passes the magnitude of `value`.
/// A count past the inline range (2^62 bits) is necessarily beyond every
/// magnitude that can already be resident in this process, so it reaches the
/// sign tail without conversion to a host-sized integer.
fn count_reaches_sign_tail(n: &AverInt, value: &AverInt) -> bool {
    let value_bits = match value.view() {
        View::Small(value) => 64 - value.unsigned_abs().leading_zeros() as u64,
        View::Big(value) => value.bits(),
    };
    match n.view() {
        View::Small(count) => (count as u64) >= value_bits,
        View::Big(_) => true,
    }
}

/// The one-word carrier behind [`AverInt`], and the only `unsafe` code of the
/// type.
///
/// The word is odd for an inline integer `n`, stored as `2n + 1`, which covers
/// `[-2^62, 2^62)`. It is even for any other value: then it is the address of
/// a heap `BigInt` that this value owns, allocated by a `Box` (so aligned to at
/// least 8, its lowest bit is 0). Neither form is zero, so `Option<AverInt>`
/// takes the zero word for `None` and is one word too.
///
/// Odd for inline values keeps every fast path a word operation: both
/// operands are inline when `a & b & 1` is set, `2x+1 + 2y` is `2(x+y)+1`,
/// and an `i64` overflow of the word is exactly a result past the inline
/// range. Comparison, `and`, `or` and arithmetic shift work on the words
/// unchanged.
mod repr {
    use core::marker::PhantomData;
    use core::mem::ManuallyDrop;
    use core::num::NonZeroU64;

    use num_bigint::BigInt;

    /// The smallest value stored inline.
    pub(super) const INLINE_MIN: i64 = -(1 << 62);
    /// The largest value stored inline.
    pub(super) const INLINE_MAX: i64 = (1 << 62) - 1;

    pub(super) struct Repr {
        word: NonZeroU64,
        // An even word owns a `Box<BigInt>`: `Send`, `Sync` and drop check
        // follow from it.
        _owns: PhantomData<Box<BigInt>>,
    }

    impl Repr {
        /// The word as a signed integer.
        #[inline(always)]
        pub(super) const fn word(&self) -> i64 {
            self.word.get() as i64
        }

        #[inline(always)]
        pub(super) const fn is_inline(&self) -> bool {
            self.word.get() & 1 == 1
        }

        /// An inline value from its word. The lowest bit is set here, so no
        /// word can be taken for an address; the callers pass words that are
        /// already odd, where this folds away.
        #[inline(always)]
        pub(super) const fn from_word(word: i64) -> Self {
            match NonZeroU64::new(word as u64 | 1) {
                Some(word) => Repr {
                    word,
                    _owns: PhantomData,
                },
                None => unreachable!(),
            }
        }

        /// Box `b`. Canonical form wants `b` outside the inline range; that is
        /// the caller's rule, not a condition of memory safety.
        pub(super) fn from_big(b: BigInt) -> Self {
            let address = Box::into_raw(Box::new(b)).expose_provenance() as u64;
            debug_assert!(address & 1 == 0, "a BigInt box is aligned");
            Repr {
                word: NonZeroU64::new(address).expect("a Box address is not null"),
                _owns: PhantomData,
            }
        }

        #[inline(always)]
        fn pointer(&self) -> *mut BigInt {
            core::ptr::with_exposed_provenance_mut(self.word.get() as usize)
        }

        /// The heap integer, when the word is an address.
        #[inline]
        pub(super) fn big(&self) -> Option<&BigInt> {
            if self.is_inline() {
                return None;
            }
            // SAFETY: only `from_big` makes an even word, from a box whose
            // provenance it exposed, and the box lives until this value is
            // dropped or taken apart by `into_big`.
            Some(unsafe { &*self.pointer() })
        }

        /// The heap integer by value, or the inline value's word.
        pub(super) fn into_big(self) -> Result<BigInt, i64> {
            let this = ManuallyDrop::new(self);
            if this.is_inline() {
                return Err(this.word());
            }
            // SAFETY: as in `big`; `this` is never dropped, so the box is
            // freed once, by the returned value.
            Ok(*unsafe { Box::from_raw(this.pointer()) })
        }
    }

    // Clone and drop keep the inline case at the call site and the boxed one
    // out of line, so a function that copies or drops an `Int` field stays
    // small enough to inline.

    impl Drop for Repr {
        #[inline(always)]
        fn drop(&mut self) {
            if !self.is_inline() {
                self.drop_big();
            }
        }
    }

    impl Clone for Repr {
        #[inline(always)]
        fn clone(&self) -> Self {
            if self.is_inline() {
                Repr {
                    word: self.word,
                    _owns: PhantomData,
                }
            } else {
                self.clone_big()
            }
        }
    }

    impl Repr {
        #[cold]
        #[inline(never)]
        fn drop_big(&mut self) {
            // SAFETY: as in `big`; drop runs once, and only for a boxed word.
            drop(unsafe { Box::from_raw(self.pointer()) });
        }

        #[cold]
        #[inline(never)]
        fn clone_big(&self) -> Self {
            // SAFETY: as in `big`; called only for a boxed word.
            Repr::from_big(unsafe { &*self.pointer() }.clone())
        }
    }
}

use repr::{INLINE_MAX, INLINE_MIN, Repr};

const _: () = assert!(core::mem::size_of::<AverInt>() == 8);
const _: () = assert!(core::mem::size_of::<Option<AverInt>>() == 8);

/// Arbitrary-precision integer (mathematical ℤ) in one machine word.
///
/// Invariant: a value in `[-2^62, 2^62)` is always inline, never boxed (see
/// module docs). Always construct/renormalize through this type's API.
#[derive(Clone)]
pub struct AverInt(Repr);

/// One value read either way: inline, or the heap integer.
enum View<'a> {
    Small(i64),
    Big(&'a BigInt),
}

impl AverInt {
    /// An inline value. `n` must be in the inline range.
    #[inline(always)]
    const fn inline(n: i64) -> Self {
        debug_assert!(INLINE_MIN <= n && n <= INLINE_MAX);
        AverInt(Repr::from_word((n << 1) | 1))
    }

    #[inline(always)]
    fn view(&self) -> View<'_> {
        match self.0.big() {
            None => View::Small(self.0.word() >> 1),
            Some(b) => View::Big(b),
        }
    }

    /// The words of two inline operands, if both are.
    #[inline(always)]
    fn words(a: &AverInt, b: &AverInt) -> Option<(i64, i64)> {
        let (a, b) = (a.0.word(), b.0.word());
        (a & b & 1 != 0).then_some((a, b))
    }

    /// The mathematical integer `n`; inline when it is in `[-2^62, 2^62)`.
    #[inline]
    pub fn from_i64(n: i64) -> Self {
        if (INLINE_MIN..=INLINE_MAX).contains(&n) {
            AverInt::inline(n)
        } else {
            from_i64_slow(n)
        }
    }

    /// Zero.
    #[inline]
    pub const fn zero() -> Self {
        AverInt::inline(0)
    }

    /// Renormalize a `BigInt` to canonical form: inline when it fits the
    /// inline range, otherwise boxed. Every path that produces or
    /// reconstructs a `BigInt` must funnel through here so the canonical
    /// invariant holds — this is the sole sanctioned way to box a value.
    #[inline]
    pub fn from_bigint(b: BigInt) -> Self {
        match b.to_i64() {
            Some(n) if (INLINE_MIN..=INLINE_MAX).contains(&n) => AverInt::inline(n),
            _ => AverInt(Repr::from_big(b)),
        }
    }

    /// The value as a `BigInt`, moving the heap integer out when there is one.
    pub fn into_bigint(self) -> BigInt {
        match self.0.into_big() {
            Ok(b) => b,
            Err(word) => BigInt::from(word >> 1),
        }
    }

    /// The value as a `BigInt` for an operation that cannot stay in `i64`:
    /// a heap integer is borrowed where it lies, never copied.
    #[inline]
    fn to_bigint(&self) -> Cow<'_, BigInt> {
        match self.view() {
            View::Small(n) => Cow::Owned(BigInt::from(n)),
            View::Big(b) => Cow::Borrowed(b),
        }
    }

    /// `true` for the additive identity.
    #[inline]
    pub fn is_zero(&self) -> bool {
        // Zero is inline, and no other value has its word.
        self.0.word() == 1
    }

    // -- Arithmetic (non-wrapping) ----------------------------------------
    //
    // Each operation takes the inline fast path on the words via `checked_*`;
    // only when the word overflows does it promote to `BigInt`. The result is
    // always renormalized, so e.g. `Big - Big` that cancels back into range
    // returns an inline value.

    /// `self + rhs` over ℤ (never wraps).
    #[inline]
    pub fn add(&self, rhs: &AverInt) -> AverInt {
        // `2a+1 + 2b` is `2(a+b)+1`.
        if let Some((a, b)) = AverInt::words(self, rhs)
            && let Some(s) = a.checked_add(b ^ 1)
        {
            return AverInt(Repr::from_word(s));
        }
        add_slow(self, rhs)
    }

    /// `self - rhs` over ℤ (never wraps).
    #[inline]
    pub fn sub(&self, rhs: &AverInt) -> AverInt {
        // `2a+1 - 2b` is `2(a-b)+1`.
        if let Some((a, b)) = AverInt::words(self, rhs)
            && let Some(s) = a.checked_sub(b ^ 1)
        {
            return AverInt(Repr::from_word(s));
        }
        sub_slow(self, rhs)
    }

    /// `self * rhs` over ℤ (never wraps).
    #[inline]
    pub fn mul(&self, rhs: &AverInt) -> AverInt {
        // `a * 2b` is `2ab`, and the set lowest bit makes it `2ab+1`.
        if let Some((a, b)) = AverInt::words(self, rhs)
            && let Some(p) = (a >> 1).checked_mul(b ^ 1)
        {
            return AverInt(Repr::from_word(p | 1));
        }
        mul_slow(self, rhs)
    }

    /// `-self` over ℤ (never wraps; `-(-2^62)` leaves the inline range).
    pub fn neg(&self) -> AverInt {
        match self.view() {
            View::Small(n) => AverInt::from_i64(-n),
            View::Big(b) => AverInt::from_bigint(-b.clone()),
        }
    }

    /// Euclidean quotient, matching `i64::div_euclid` and the Lean
    /// `Int.ediv` model the proofs cite: the unique `q` with a remainder in
    /// `[0, |rhs|)`. Returns `None` when `rhs == 0`. Over ℤ there is no
    /// `MIN / -1` overflow edge — the quotient is just one past the inline
    /// maximum, returned boxed.
    #[inline]
    pub fn div_euclid(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (View::Small(a), View::Small(b)) = (self.view(), rhs.view()) {
            // An inline dividend is above `i64::MIN`, so this never overflows.
            return Some(AverInt::from_i64(a.div_euclid(b)));
        }
        Some(div_euclid_slow(self, rhs))
    }

    /// Euclidean remainder `self - rhs * div_euclid(self, rhs)`, matching
    /// `i64::rem_euclid` and the Lean `Int.emod` model. Returns `None`
    /// when `rhs == 0`. The result is always non-negative and in `[0, |rhs|)`,
    /// independent of the sign of either operand.
    #[inline]
    pub fn rem_euclid(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (View::Small(a), View::Small(b)) = (self.view(), rhs.view()) {
            // Below `|b|`, so inline.
            return Some(AverInt::inline(a.rem_euclid(b)));
        }
        Some(rem_euclid_slow(self, rhs))
    }

    /// Truncating quotient (rounds toward zero), the semantics of the raw
    /// `/` operator. Returns `None` when `rhs == 0`. Distinct from
    /// `div_euclid` for negative operands; provided for the low-level
    /// arithmetic opcodes (`Int.div` uses the Euclidean form).
    pub fn div_trunc(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (View::Small(a), View::Small(b)) = (self.view(), rhs.view()) {
            return Some(AverInt::from_i64(a / b));
        }
        Some(AverInt::from_bigint(&*self.to_bigint() / &*rhs.to_bigint()))
    }

    /// Truncating remainder (sign follows the dividend), the semantics of the
    /// raw `%` operator. Returns `None` when `rhs == 0`.
    pub fn rem_trunc(&self, rhs: &AverInt) -> Option<AverInt> {
        if rhs.is_zero() {
            return None;
        }
        if let (View::Small(a), View::Small(b)) = (self.view(), rhs.view()) {
            return Some(AverInt::inline(a % b));
        }
        Some(AverInt::from_bigint(&*self.to_bigint() % &*rhs.to_bigint()))
    }

    /// `|self|` over ℤ (never wraps; `|-2^62|` leaves the inline range).
    pub fn abs(&self) -> AverInt {
        match self.view() {
            View::Small(n) => AverInt::from_i64(n.abs()),
            View::Big(b) => AverInt::from_bigint(BigInt::from(b.magnitude().clone())),
        }
    }

    /// The smaller of `self` and `other` (borrowing form, to avoid the
    /// by-value `Ord::min`/`max`). Two inline values compare as their words,
    /// and the result is the chosen word, with no clone.
    #[inline]
    pub fn min_ref(&self, other: &AverInt) -> AverInt {
        if let Some((a, b)) = Self::words(self, other) {
            return AverInt(Repr::from_word(a.min(b)));
        }
        if self <= other {
            self.clone()
        } else {
            other.clone()
        }
    }

    /// The larger of `self` and `other`, as `min_ref`.
    #[inline]
    pub fn max_ref(&self, other: &AverInt) -> AverInt {
        if let Some((a, b)) = Self::words(self, other) {
            return AverInt(Repr::from_word(a.max(b)));
        }
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
    // sequence the word already carries above its lowest bit (sign extension
    // IS the infinite tail), so the inline fast path is the native machine
    // operator on the words, with the lowest bit kept set — and
    // `num_bigint`'s `BitAnd`/`BitOr`/`BitXor` implement the same convention
    // for the promoted case. There is no width anywhere: width is requested
    // explicitly through `low_bits`.

    /// Pointwise `and` over the infinite two's-complement sequences.
    /// `bit_and(-1, x) == x` for every `x` (all-ones is the identity).
    #[inline]
    pub fn bit_and(&self, rhs: &AverInt) -> AverInt {
        if let Some((a, b)) = AverInt::words(self, rhs) {
            return AverInt(Repr::from_word(a & b));
        }
        bitwise_slow(self, rhs, |a, b| a & b)
    }

    /// Pointwise `or`. `bit_or(-1, x) == -1`, `bit_or(0, x) == x`.
    #[inline]
    pub fn bit_or(&self, rhs: &AverInt) -> AverInt {
        if let Some((a, b)) = AverInt::words(self, rhs) {
            return AverInt(Repr::from_word(a | b));
        }
        bitwise_slow(self, rhs, |a, b| a | b)
    }

    /// Pointwise `xor`. `bit_xor(-1, x) == bit_not(x)`, `bit_xor(x, x) == 0`.
    #[inline]
    pub fn bit_xor(&self, rhs: &AverInt) -> AverInt {
        if let Some((a, b)) = AverInt::words(self, rhs) {
            // The lowest bits cancel; `from_word` sets it again.
            return AverInt(Repr::from_word(a ^ b));
        }
        bitwise_slow(self, rhs, |a, b| a ^ b)
    }

    /// Pointwise complement — equivalently `-self - 1` over ℤ, which is the
    /// only total reading of "flip every bit" without a width to complement
    /// against. Computed as the subtraction so the identity is the
    /// definition rather than a coincidence of the carrier: the inline arm
    /// cannot leave the range (`!n` is inline for every inline `n`).
    pub fn bit_not(&self) -> AverInt {
        match self.view() {
            View::Small(n) => AverInt::inline(!n),
            View::Big(b) => AverInt::from_bigint(-b - 1),
        }
    }

    /// `self * 2^n` — the bit sequence moved `n` places toward the more
    /// significant end. Over ℤ this never truncates.
    pub fn shift_left(&self, n: &AverInt) -> Result<AverInt, ShiftCountError> {
        let count = materializing_count(n)?;
        if let View::Small(a) = self.view()
            && count < 63
        {
            let shifted = a << count;
            // Round-trip oracle: no bit left the i64 iff shifting back
            // restores the input (`>>` on a signed value is arithmetic, so
            // this holds for both signs).
            if (shifted >> count) == a {
                return Ok(AverInt::from_i64(shifted));
            }
        }
        Ok(AverInt::from_bigint(&*self.to_bigint() << count))
    }

    /// `floor(self / 2^n)` — the bit sequence moved `n` places toward the
    /// less significant end. This is an ARITHMETIC shift: the sign tail is
    /// what gets shifted in, so `shift_right(-3, 1) == -2`, never `-1` and
    /// never a host language's logical shift.
    #[inline]
    pub fn shift_right(&self, n: &AverInt) -> Result<AverInt, ShiftCountError> {
        // On the word `2a+1`, `>> c` drops the tag bit and `c - 1` bits of
        // `a`, and setting the lowest bit again gives `2(a >> c)+1`. A count
        // of 63 or more leaves only the sign tail, which is what the
        // arithmetic `>> 63` gives, so an inline value and a non-negative
        // inline count (a positive word) need no other check. A literal
        // count folds the whole test away once this is inlined.
        if let Some((a, count)) = AverInt::words(self, n)
            && count > 0
        {
            return Ok(AverInt(Repr::from_word(a >> (count >> 1).min(63))));
        }
        shift_right_slow(self, n)
    }

    /// `self mod 2^width` — the non-negative integer carried by the lowest
    /// `width` bits, which is how a program asks for fixed-width behaviour
    /// explicitly. Always in `[0, 2^width)`, so `low_bits(-1, 8) == 255` and
    /// `low_bits(x, 0) == 0`.
    pub fn low_bits(&self, width: &AverInt) -> Result<AverInt, ShiftCountError> {
        if width < &AverInt::zero() {
            return Err(ShiftCountError::Negative);
        }
        // A non-negative value already has an infinite zero tail. Once the
        // requested window reaches its highest set bit, `low` is exactly the
        // original value and allocates nothing — even for a Big width.
        let count = if self >= &AverInt::zero() {
            if count_reaches_sign_tail(width, self) {
                return Ok(self.clone());
            }
            width
                .to_usize()
                .expect("a width below a resident integer's bit length fits usize")
        } else {
            // A negative integer has an infinite one tail, so the requested
            // width is the size of a genuinely new non-negative magnitude.
            materializing_count(width)?
        };
        // Only a negative value gets here with an inline `self` (a
        // non-negative one returned above), so the window always captures
        // part of its one tail.
        if let View::Small(a) = self.view()
            && count < 64
        {
            // `count == 63` gives the mask `i64::MAX`. Every result here is
            // non-negative and below `2^63`, so it fits `i64`.
            let mask = if count == 63 {
                i64::MAX
            } else {
                (1i64 << count) - 1
            };
            return Ok(AverInt::from_i64(a & mask));
        }
        Ok(AverInt::from_bigint(low_bits_of(&self.to_bigint(), count)))
    }

    /// The number of one bits among the lowest `width` bits — exactly the
    /// bits `low_bits(self, width)` keeps, so a negative value counts its
    /// infinite one tail up to the window: `pop_count(-1, 8) == 8`. Refuses
    /// only a negative width. The count never exceeds `width`, so there is
    /// no materialization bound and a Big width needs no allocation of its
    /// size.
    #[inline]
    pub fn pop_count(&self, width: &AverInt) -> Result<AverInt, ShiftCountError> {
        // An inline value read as an `i64` carries its sign tail, so up to 64
        // low bits are the machine's. The count is at most 64, so inline.
        if let Some((a, w)) = AverInt::words(self, width)
            && let w @ 0..=64 = w >> 1
        {
            let mask = u64::MAX.checked_shr(64 - w as u32).unwrap_or(0);
            return Ok(AverInt::inline(
                (((a >> 1) as u64) & mask).count_ones() as i64
            ));
        }
        pop_count_slow(self, width)
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
        match self.view() {
            View::Small(n) => Some(n),
            View::Big(b) => b.to_i64(),
        }
    }

    /// `self` as `usize` if it fits (non-negative and in range), else `None`.
    #[inline]
    pub fn to_usize(&self) -> Option<usize> {
        match self.view() {
            View::Small(n) => usize::try_from(n).ok(),
            View::Big(b) => b.to_usize(),
        }
    }

    /// `self` as `u16` if it fits, else `None`.
    #[inline]
    pub fn to_u16(&self) -> Option<u16> {
        match self.view() {
            View::Small(n) => u16::try_from(n).ok(),
            View::Big(b) => b.to_u16(),
        }
    }

    /// `self` as `u32` if it fits, else `None`.
    #[inline]
    pub fn to_u32(&self) -> Option<u32> {
        match self.view() {
            View::Small(n) => u32::try_from(n).ok(),
            View::Big(b) => b.to_u32(),
        }
    }

    /// `self` as `f64`, lossily. Huge magnitudes saturate to `±∞` (never
    /// `NaN`), matching the Lean prelude's `Float.ofInt`/IEEE coercion. This
    /// is the only intentionally-lossy conversion.
    #[inline]
    pub fn to_f64(&self) -> f64 {
        match self.view() {
            View::Small(n) => n as f64,
            // `BigInt::to_f64` returns `Some(±inf)` for out-of-range
            // magnitudes and is never `None`, so the unwrap is total.
            View::Big(b) => b.to_f64().unwrap_or(f64::INFINITY),
        }
    }

    /// Truncate a finite `f64` toward zero into ℤ. The exact mirror of the
    /// VM's `float_to_aver_int` (`src/types/int.rs`): non-finite (`NaN`/`±∞`)
    /// maps to `0`; an in-`i64`-range truncated value goes through
    /// `from_i64`; an out-of-range *finite* magnitude is represented EXACTLY
    /// via `BigInt::from_f64`.
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
            Some(n) => AverInt::from_i64(n),
            // Out of i64 range but finite: represent exactly via BigInt.
            None => match BigInt::from_f64(truncated) {
                Some(b) => AverInt::from_bigint(b),
                None => AverInt::zero(),
            },
        }
    }
}

#[cold]
#[inline(never)]
fn from_i64_slow(n: i64) -> AverInt {
    AverInt(Repr::from_big(BigInt::from(n)))
}

/// The lowest `count` bits of `value` in two's complement, read as a
/// non-negative integer: `value mod 2^count`. The window is cut out of the
/// magnitude's digits, so it costs the window, with no division.
fn low_bits_of(value: &BigInt, count: usize) -> BigInt {
    let words = count.div_ceil(32);
    let mut digits: Vec<u32> = value.magnitude().iter_u32_digits().take(words).collect();
    digits.resize(words, 0);
    if value.sign() == Sign::Minus {
        // `-m` is `!m + 1`; whatever carries out of the window is dropped.
        let mut carry = true;
        for digit in &mut digits {
            (*digit, carry) = (!*digit).overflowing_add(u32::from(carry));
        }
    }
    if let (Some(top), bits @ 1..) = (digits.last_mut(), count % 32) {
        *top &= (1u32 << bits) - 1;
    }
    BigInt::from_biguint(Sign::Plus, BigUint::new(digits))
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
// numbers are either the same inline word or two boxes of equal integers.

impl PartialEq for AverInt {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        let (a, b) = (self.0.word(), other.0.word());
        // Canonical form: an inline value never equals a boxed one, and the
        // words of an inline and a boxed value differ in the lowest bit.
        if (a | b) & 1 != 0 {
            return a == b;
        }
        eq_slow(self, other)
    }
}

impl Eq for AverInt {}

#[cold]
#[inline(never)]
fn eq_slow(a: &AverInt, b: &AverInt) -> bool {
    a.0.big() == b.0.big()
}

// The `BigInt` paths of the arithmetic above, kept out of line so the
// inlined inline-value fast path stays a few instructions at every call site.

#[cold]
#[inline(never)]
fn add_slow(a: &AverInt, b: &AverInt) -> AverInt {
    AverInt::from_bigint(&*a.to_bigint() + &*b.to_bigint())
}

#[cold]
#[inline(never)]
fn sub_slow(a: &AverInt, b: &AverInt) -> AverInt {
    AverInt::from_bigint(&*a.to_bigint() - &*b.to_bigint())
}

#[cold]
#[inline(never)]
fn mul_slow(a: &AverInt, b: &AverInt) -> AverInt {
    AverInt::from_bigint(&*a.to_bigint() * &*b.to_bigint())
}

#[cold]
#[inline(never)]
fn bitwise_slow(a: &AverInt, b: &AverInt, op: fn(&BigInt, &BigInt) -> BigInt) -> AverInt {
    AverInt::from_bigint(op(&a.to_bigint(), &b.to_bigint()))
}

#[cold]
#[inline(never)]
fn shift_right_slow(a: &AverInt, n: &AverInt) -> Result<AverInt, ShiftCountError> {
    if n < &AverInt::zero() {
        return Err(ShiftCountError::Negative);
    }
    if count_reaches_sign_tail(n, a) {
        return Ok(AverInt::from_i64(if a < &AverInt::zero() { -1 } else { 0 }));
    }
    let count = n
        .to_usize()
        .expect("a count below a resident integer's bit length fits usize");
    Ok(AverInt::from_bigint(&*a.to_bigint() >> count))
}

#[cold]
#[inline(never)]
fn pop_count_slow(x: &AverInt, width: &AverInt) -> Result<AverInt, ShiftCountError> {
    if width < &AverInt::zero() {
        return Err(ShiftCountError::Negative);
    }
    let value = x.to_bigint();
    if value.sign() == Sign::Minus {
        // The low bits of a negative `x` are the complement of the low bits
        // of `-x - 1`, which is non-negative.
        let zeros = low_ones(&(-&*value - 1u8), width);
        return Ok(width.sub(&AverInt::from_i64(zeros as i64)));
    }
    Ok(AverInt::from_i64(low_ones(&value, width) as i64))
}

/// The one bits of a non-negative `value` among its lowest `width` bits.
fn low_ones(value: &BigInt, width: &AverInt) -> u64 {
    let magnitude = value.magnitude();
    let Some(width) = width
        .to_usize()
        .filter(|width| (*width as u64) < magnitude.bits())
    else {
        // The window covers every set bit.
        return magnitude.count_ones();
    };
    let mut digits = magnitude.iter_u64_digits();
    let whole: u64 = digits
        .by_ref()
        .take(width / 64)
        .map(|digit| u64::from(digit.count_ones()))
        .sum();
    let rest = width % 64;
    let partial = digits
        .next()
        .map_or(0, |digit| (digit & ((1u64 << rest) - 1)).count_ones());
    whole + u64::from(partial)
}

#[cold]
#[inline(never)]
fn div_euclid_slow(a: &AverInt, b: &AverInt) -> AverInt {
    let (q, _r) = euclid_div_rem(&a.to_bigint(), &b.to_bigint());
    AverInt::from_bigint(q)
}

#[cold]
#[inline(never)]
fn rem_euclid_slow(a: &AverInt, b: &AverInt) -> AverInt {
    let (_q, r) = euclid_div_rem(&a.to_bigint(), &b.to_bigint());
    AverInt::from_bigint(r)
}

#[cold]
#[inline(never)]
fn cmp_slow(a: &AverInt, b: &AverInt) -> Ordering {
    match (a.view(), b.view()) {
        (View::Big(a), View::Big(b)) => a.cmp(b),
        // A boxed value is out of the inline range, so its sign decides the
        // comparison against any inline one.
        (View::Small(_), View::Big(b)) => {
            if b.sign() == num_bigint::Sign::Minus {
                Ordering::Greater
            } else {
                Ordering::Less
            }
        }
        (View::Big(a), View::Small(_)) => {
            if a.sign() == num_bigint::Sign::Minus {
                Ordering::Less
            } else {
                Ordering::Greater
            }
        }
        (View::Small(a), View::Small(b)) => a.cmp(&b),
    }
}

impl Ord for AverInt {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        // `2n+1` is monotone, so inline words compare as their values.
        if let Some((a, b)) = AverInt::words(self, other) {
            return a.cmp(&b);
        }
        cmp_slow(self, other)
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
        // Hash the number, consistently with `Eq`: every value that fits
        // `i64` hashes as that `i64`, whichever way it is stored.
        match self.view() {
            View::Small(n) => n.hash(state),
            View::Big(b) => hash_slow(b, state),
        }
    }
}

#[cold]
#[inline(never)]
fn hash_slow<H: core::hash::Hasher>(b: &BigInt, state: &mut H) {
    use core::hash::Hash;
    match b.to_i64() {
        Some(n) => n.hash(state),
        None => b.hash(state),
    }
}

impl fmt::Display for AverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.view() {
            View::Small(n) => write!(f, "{}", n),
            View::Big(b) => write!(f, "{}", b),
        }
    }
}

impl fmt::Debug for AverInt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Print the bare number so `Value::Int(..)`'s derived `Debug` reads
        // `Int(42)` (not `Int(AverInt(42))`), keeping golden strings stable.
        write!(f, "{self}")
    }
}

impl FromStr for AverInt {
    type Err = ();

    /// Parse a decimal integer of arbitrary length. Rejects empty/garbage
    /// input (and anything `BigInt` rejects) with `Err(())`.
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // Fast path: most parsed ints fit i64.
        if let Ok(n) = s.parse::<i64>() {
            return Ok(AverInt::from_i64(n));
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
        AverInt::from_i64(n)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use num_traits::Zero;

    fn big(s: &str) -> AverInt {
        AverInt::from_str(s).unwrap()
    }

    #[test]
    fn fixed_width_endian_codecs_cover_zero_padding_and_bigints() {
        let zero = AverInt::zero();
        let four = AverInt::from_i64(4);
        assert_eq!(int_to_big_endian(&zero, &four).unwrap(), [0, 0, 0, 0]);
        assert_eq!(
            int_to_big_endian(&AverInt::from_i64(258), &AverInt::from_i64(2)).unwrap(),
            [1, 2]
        );
        assert_eq!(
            int_to_little_endian(&AverInt::from_i64(258), &AverInt::from_i64(2)).unwrap(),
            [2, 1]
        );

        let past_u64 = big("18446744073709551616");
        let encoded = int_to_big_endian(&past_u64, &AverInt::from_i64(9)).unwrap();
        assert_eq!(encoded, [1, 0, 0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(int_from_big_endian(&encoded), past_u64);
    }

    #[test]
    fn fixed_width_endian_codecs_define_empty_and_refuse_bad_inputs() {
        assert_eq!(
            int_to_big_endian(&AverInt::zero(), &AverInt::zero()).unwrap(),
            Vec::<u8>::new()
        );
        assert_eq!(int_from_big_endian(&[]), AverInt::zero());
        assert_eq!(int_from_little_endian(&[]), AverInt::zero());

        assert_eq!(
            int_to_big_endian(&AverInt::from_i64(1), &AverInt::zero()).unwrap_err(),
            "Int.toBigEndian: value does not fit in the requested width"
        );
        assert_eq!(
            int_to_big_endian(&AverInt::from_i64(-1), &AverInt::from_i64(4)).unwrap_err(),
            "Int.toBigEndian: value does not fit in the requested width"
        );
        assert_eq!(
            int_to_little_endian(&AverInt::zero(), &AverInt::from_i64(-1)).unwrap_err(),
            format!(
                "Int.toLittleEndian: width must be between 0 and {}",
                crate::MAX_MATERIALIZED_SEQUENCE_ELEMENTS
            )
        );
    }

    #[test]
    fn canonical_form_demotes_to_small() {
        // The inline maximum doubled and halved returns to range and must be
        // inline again. i64::MAX doubled and halved is past it, so boxed.
        let a = AverInt::from_i64(INLINE_MAX);
        let doubled = a.add(&a);
        assert!(!doubled.0.is_inline());
        let halved = doubled.div_euclid(&AverInt::from_i64(2)).unwrap();
        assert_eq!(halved, AverInt::from_i64(INLINE_MAX));
        assert!(halved.0.is_inline());
        let a = AverInt::from_i64(i64::MAX);
        let halved = a.add(&a).div_euclid(&AverInt::from_i64(2)).unwrap();
        assert_eq!(halved.to_i64(), Some(i64::MAX));
        assert!(!halved.0.is_inline());
    }

    #[test]
    fn square_is_non_negative_past_i64() {
        // The exact runtime-vs-proof law: a*a >= 0 even where i64 would wrap.
        let a = AverInt::from_i64(i64::MAX);
        let sq = a.mul(&a);
        assert!(!sq.0.is_inline());
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

    /// Clone, drop, `==` and hash of boxed values, which run out of line.
    #[test]
    fn boxed_values_clone_drop_compare_and_hash() {
        let hash = |x: &AverInt| ahash::RandomState::with_seeds(1, 2, 3, 4).hash_one(x);
        let a = big("123456789012345678901234567890");
        let b = a.clone();
        drop(a);
        assert_eq!(b.to_string(), "123456789012345678901234567890");
        let a = b.clone();
        assert!(a == b);
        assert!(a != big("123456789012345678901234567891"));
        assert!(a != AverInt::from_i64(1));
        assert!(AverInt::from_i64(1) != a);
        assert_eq!(hash(&a), hash(&b));
        // Boxed but within `i64`: hashes as that `i64`, like an inline value.
        let edge = AverInt::from_i64(i64::MAX);
        assert_eq!(hash(&edge.clone()), hash(&edge));
        assert_eq!(
            hash(&edge),
            ahash::RandomState::with_seeds(1, 2, 3, 4).hash_one(i64::MAX)
        );
        let negative = AverInt::from_i64(i64::MIN);
        assert!(negative.clone() == negative);
        assert!(negative != edge);
        let many: Vec<AverInt> = (0..1000).map(|_| a.clone()).collect();
        assert!(many.iter().all(|x| *x == b));
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
        assert_eq!((from_big.0.is_inline(), from_big.to_i64()), (true, Some(5)));
        assert_eq!(from_big, small);
        let mut hb = DefaultHasher::new();
        let mut hs = DefaultHasher::new();
        from_big.hash(&mut hb);
        small.hash(&mut hs);
        assert_eq!(hb.finish(), hs.finish());

        // The ends of the inline range demote; one past either end, and
        // the ends of i64, stay boxed.
        for n in [INLINE_MAX, INLINE_MIN] {
            let v = AverInt::from_bigint(BigInt::from(n));
            assert!(v.0.is_inline());
            assert_eq!(v.to_i64(), Some(n));
        }
        for n in [INLINE_MAX + 1, INLINE_MIN - 1, i64::MAX, i64::MIN] {
            let v = AverInt::from_bigint(BigInt::from(n));
            assert!(!v.0.is_inline());
            assert_eq!(v, AverInt::from_i64(n));
        }
        let past = AverInt::from_bigint(BigInt::from(i64::MAX) + 1);
        assert!(!past.0.is_inline());
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
        assert!(!two_63.0.is_inline());

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
                assert!(!a.0.is_inline());
                assert!(!b.0.is_inline());
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
        assert!(!q.0.is_inline());
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
        assert!(!v.0.is_inline());
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
        assert!(!v.0.is_inline());
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

        for x in [i64::MIN, -1025, -257, -3, -1, 0, 1, 6, 255, 1024, i64::MAX] {
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
    fn pop_count_counts_the_bits_low_bits_keeps() {
        let i = AverInt::from_i64;
        assert_eq!(i(-1).pop_count(&i(8)).unwrap(), i(8));
        assert_eq!(i(0b1011).pop_count(&i(3)).unwrap(), i(2));
        assert_eq!(i(123).pop_count(&i(0)).unwrap(), i(0));
        // A negative value's one tail is counted up to any width, Big too.
        let huge = two_pow(200);
        assert_eq!(i(-1).pop_count(&huge).unwrap(), huge);
        assert_eq!(i(5).pop_count(&huge).unwrap(), i(2));

        let big = two_pow(100).add(&i(5));
        for x in [
            -1025i64,
            -257,
            -3,
            -1,
            0,
            1,
            6,
            255,
            i64::MIN,
            i64::MAX,
            1 << 62,
        ]
        .map(AverInt::from_i64)
        .into_iter()
        .chain([big.clone(), big.neg(), two_pow(63), two_pow(64).neg()])
        {
            for w in [0usize, 1, 8, 16, 32, 63, 64, 65, 100, 101, 128, 200] {
                let width = AverInt::from_i64(w as i64);
                let low = x.low_bits(&width).unwrap().to_bigint().into_owned();
                let expected = AverInt::from_i64(low.magnitude().count_ones() as i64);
                assert_eq!(x.pop_count(&width).unwrap(), expected, "popCount {x} {w}");
            }
        }
    }

    /// `low_bits` is the Euclidean remainder mod `2^width` (Int = ℤ), and so
    /// are the window cut out of the digits and the arithmetic on borrowed
    /// heap integers, at every word boundary and on a spread of values.
    #[test]
    fn low_bits_and_big_arithmetic_agree_with_the_definition() {
        let mut values: Vec<BigInt> = Vec::new();
        for k in [
            0usize, 1, 31, 32, 33, 62, 63, 64, 65, 96, 127, 128, 129, 200,
        ] {
            let p = BigInt::from(1u8) << k;
            for d in [-1i64, 0, 1] {
                values.push(&p + d);
                values.push(-&p + d);
            }
        }
        let mut seed = 0x9e37_79b9_7f4a_7c15u64;
        for _ in 0..200 {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            let high = BigInt::from(seed) << (seed % 150) as usize;
            values.push(&high + seed % 1000);
            values.push(-high - seed % 7);
        }
        for x in &values {
            let a = AverInt::from_bigint(x.clone());
            for w in [
                0usize, 1, 7, 8, 31, 32, 33, 62, 63, 64, 65, 100, 128, 129, 300,
            ] {
                let modulus = BigInt::from(1u8) << w;
                let (_, expected) = euclid_div_rem(x, &modulus);
                let got = a.low_bits(&AverInt::from_i64(w as i64)).unwrap();
                assert_eq!(*got.to_bigint(), expected, "low_bits({x}, {w})");
            }
            for y in values.iter().step_by(7) {
                let b = AverInt::from_bigint(y.clone());
                canonical(a.add(&b), &(x + y), "add");
                canonical(a.sub(&b), &(x - y), "sub");
                canonical(a.mul(&b), &(x * y), "mul");
                canonical(a.bit_and(&b), &(x & y), "and");
                canonical(a.bit_or(&b), &(x | y), "or");
                canonical(a.bit_xor(&b), &(x ^ y), "xor");
            }
        }
    }

    #[test]
    fn negative_counts_are_refused_by_every_counted_bits_operation() {
        let i = AverInt::from_i64;
        for op in [
            AverInt::shift_left,
            AverInt::shift_right,
            AverInt::low_bits,
            AverInt::pop_count,
        ] {
            assert_eq!(op(&i(42), &i(-1)), Err(ShiftCountError::Negative));
            // A huge NEGATIVE count is still `Negative`, not swallowed by
            // the representability check.
            let huge_negative = two_pow(200).neg();
            assert_eq!(op(&i(42), &huge_negative), Err(ShiftCountError::Negative));
        }
    }

    #[test]
    fn materialization_limit_only_refuses_operations_that_can_grow_the_result() {
        let i = AverInt::from_i64;
        let above_limit = i(MAX_MATERIALIZED_BITS as i64 + 1);
        let huge = two_pow(200);

        assert_eq!(
            i(42).shift_left(&above_limit),
            Err(ShiftCountError::TooLarge)
        );
        assert_eq!(i(42).shift_left(&huge), Err(ShiftCountError::TooLarge));

        assert_eq!(i(42).shift_right(&above_limit).unwrap(), i(0));
        assert_eq!(i(-42).shift_right(&huge).unwrap(), i(-1));

        assert_eq!(i(42).low_bits(&above_limit).unwrap(), i(42));
        assert_eq!(i(42).low_bits(&huge).unwrap(), i(42));
        assert_eq!(
            i(-42).low_bits(&above_limit),
            Err(ShiftCountError::TooLarge)
        );
        assert_eq!(i(-42).low_bits(&huge), Err(ShiftCountError::TooLarge));
    }

    #[test]
    fn materialization_messages_are_derived_from_the_runtime_boundary() {
        assert_eq!(
            shift_count_too_large_message(),
            format!("shift count exceeds the {MAX_MATERIALIZED_BITS} bit limit")
        );
        assert_eq!(
            bit_width_too_large_message(),
            format!("bit width exceeds the {MAX_MATERIALIZED_BITS} bit limit")
        );
    }

    // -- The inline boundary -----------------------------------------------
    //
    // Every operation against `BigInt` arithmetic on values around the ends
    // of the inline range (±2^62), of i64 (±2^63) and of u64 (2^64), and
    // every result checked to be in canonical form.

    fn boundary_values() -> Vec<BigInt> {
        let mut out = Vec::new();
        for base in [0u32, 1, 31, 62, 63, 64, 100] {
            let magnitude = if base == 0 {
                BigInt::zero()
            } else {
                BigInt::from(1u8) << base
            };
            for sign in [1, -1] {
                for delta in -2..=2 {
                    let v = &magnitude * sign + delta;
                    if !out.contains(&v) {
                        out.push(v);
                    }
                }
            }
        }
        out
    }

    /// The value, checked to be in canonical form.
    fn canonical(v: AverInt, expected: &BigInt, what: &str) -> AverInt {
        let inline = expected >= &BigInt::from(INLINE_MIN) && expected <= &BigInt::from(INLINE_MAX);
        assert_eq!(v.0.is_inline(), inline, "{what}: form of {expected}");
        assert_eq!(*v.to_bigint(), *expected, "{what}");
        v
    }

    fn hash_of(v: &AverInt) -> u64 {
        use std::hash::{Hash, Hasher};
        let mut h = std::collections::hash_map::DefaultHasher::new();
        v.hash(&mut h);
        h.finish()
    }

    #[test]
    fn size_is_one_word_and_option_takes_the_niche() {
        assert_eq!(core::mem::size_of::<AverInt>(), 8);
        assert_eq!(core::mem::size_of::<Option<AverInt>>(), 8);
    }

    #[test]
    fn construction_and_conversion_at_the_inline_boundary() {
        for x in boundary_values() {
            let what = format!("{x}");
            let built = canonical(AverInt::from_bigint(x.clone()), &x, &what);
            let parsed = canonical(AverInt::from_str(&what).unwrap(), &x, &what);
            assert_eq!(built.to_string(), what);
            assert_eq!(format!("{built:?}"), what);
            assert_eq!(built, parsed);
            assert_eq!(hash_of(&built), hash_of(&parsed));
            assert_eq!(built.to_i64(), x.to_i64(), "{what}");
            assert_eq!(built.to_usize(), x.to_usize(), "{what}");
            assert_eq!(built.to_f64(), x.to_f64().unwrap(), "{what}");
            if let Some(n) = x.to_i64() {
                canonical(AverInt::from_i64(n), &x, &what);
                canonical(AverInt::from(n), &x, &what);
                // Every value that fits i64 hashes as that i64, as it did
                // before the inline range was narrowed.
                let mut h = std::collections::hash_map::DefaultHasher::new();
                std::hash::Hash::hash(&n, &mut h);
                assert_eq!(hash_of(&built), std::hash::Hasher::finish(&h), "{what}");
                if n.unsigned_abs() < 1 << 53 {
                    canonical(AverInt::from_f64_trunc(n as f64), &x, &what);
                }
            }
            let cloned = built.clone();
            assert_eq!(cloned, built);
            assert_eq!(cloned.into_bigint(), x);
            assert_eq!(built.is_zero(), x.is_zero());
            canonical(built.neg(), &-&x, &what);
            canonical(built.abs(), &BigInt::from(x.magnitude().clone()), &what);
            canonical(built.bit_not(), &(-&x - 1), &what);
            let some = Some(built.clone());
            assert_eq!(some.as_ref(), Some(&built));
        }
    }

    #[test]
    fn binary_operations_at_the_inline_boundary() {
        let values = boundary_values();
        for x in &values {
            for y in &values {
                let (a, b) = (
                    AverInt::from_bigint(x.clone()),
                    AverInt::from_bigint(y.clone()),
                );
                let what = |op: &str| format!("{x} {op} {y}");
                canonical(a.add(&b), &(x + y), &what("+"));
                canonical(a.sub(&b), &(x - y), &what("-"));
                canonical(a.mul(&b), &(x * y), &what("*"));
                canonical(a.bit_and(&b), &(x & y), &what("&"));
                canonical(a.bit_or(&b), &(x | y), &what("|"));
                canonical(a.bit_xor(&b), &(x ^ y), &what("^"));
                assert_eq!(a.cmp(&b), x.cmp(y), "{}", what("cmp"));
                assert_eq!(a == b, x == y, "{}", what("=="));
                canonical(a.min_ref(&b), x.min(y), &what("min"));
                canonical(a.max_ref(&b), x.max(y), &what("max"));
                if y.is_zero() {
                    assert!(a.div_euclid(&b).is_none() && a.rem_euclid(&b).is_none());
                    assert!(a.div_trunc(&b).is_none() && a.rem_trunc(&b).is_none());
                } else {
                    let (q, r) = euclid_div_rem(x, y);
                    canonical(a.div_euclid(&b).unwrap(), &q, &what("div"));
                    canonical(a.rem_euclid(&b).unwrap(), &r, &what("mod"));
                    canonical(a.div_trunc(&b).unwrap(), &(x / y), &what("/"));
                    canonical(a.rem_trunc(&b).unwrap(), &(x % y), &what("%"));
                }
            }
        }
    }

    #[test]
    fn shifts_and_low_bits_at_the_inline_boundary() {
        for x in boundary_values() {
            let a = AverInt::from_bigint(x.clone());
            for n in [0usize, 1, 2, 30, 61, 62, 63, 64, 65, 100, 130] {
                let count = AverInt::from_i64(n as i64);
                let modulus = BigInt::from(1u8) << n;
                let what = |op: &str| format!("{x} {op} {n}");
                canonical(a.shift_left(&count).unwrap(), &(&x * &modulus), &what("<<"));
                let (q, r) = euclid_div_rem(&x, &modulus);
                canonical(a.shift_right(&count).unwrap(), &q, &what(">>"));
                canonical(a.low_bits(&count).unwrap(), &r, &what("low"));
                // The ones among the lowest `n` bits are those of `x mod 2^n`.
                let ones = BigInt::from(r.magnitude().count_ones());
                canonical(a.pop_count(&count).unwrap(), &ones, &what("popCount"));
            }
            // A count past the inline range reaches the sign tail.
            let huge = AverInt::from_i64(INLINE_MAX + 1);
            let tail = if x.sign() == Sign::Minus { -1 } else { 0 };
            canonical(
                a.shift_right(&huge).unwrap(),
                &BigInt::from(tail),
                "huge >>",
            );
            assert_eq!(a.shift_left(&huge), Err(ShiftCountError::TooLarge));
        }
    }
}
