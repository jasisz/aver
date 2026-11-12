//! Compact native carrier for proof-bounded `List<Int>` refinements.
//!
//! The compiler may use this representation only after its refinement
//! recognizer proves every element lies in the interval of the element type
//! (`0..=255` for a `u8`, `-32768..=32767` for an `i16`, …) and its
//! construction audit proves that the smart constructor cannot be bypassed.
//! It is not an alternative representation for an arbitrary Aver list.
//!
//! The elements sit in one allocation with their count, length and room
//! ([`crate::block::Block`]), and a carrier is a view of a range of them, so
//! dropping or taking a prefix shares the body.

use crate::block::Block;
use crate::int_list::{NarrowInts, Width};
use crate::{AverDisplay, AverInt, AverIntList, AverList};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::num::NonZeroUsize;

/// A machine integer a packed carrier holds its elements as: `u8`, `i8`,
/// `u16`, `i16`, `u32`, `i32` or `i64`.
pub trait PackedInt:
    Copy + Ord + Hash + fmt::Debug + fmt::Display + Into<i64> + TryFrom<i64> + Send + Sync + 'static
{
    /// What an element is called in the message for a value outside the
    /// width.
    const NOUN: &'static str;

    /// `values` as the `List<Int>` it is, sharing its body.
    #[doc(hidden)]
    fn into_list(values: AverPacked<Self>) -> AverIntList;

    /// The carrier `values` already is, when it is one of this width:
    /// its body taken over, nothing walked; the list back otherwise.
    #[doc(hidden)]
    fn from_list(values: AverIntList) -> Result<AverPacked<Self>, AverIntList>;
}

impl PackedInt for u8 {
    const NOUN: &'static str = "byte";

    fn into_list(values: AverPacked<Self>) -> AverIntList {
        AverIntList::Packed(values)
    }

    fn from_list(values: AverIntList) -> Result<AverPacked<Self>, AverIntList> {
        match values {
            AverIntList::Packed(packed) => Ok(packed),
            other => Err(other),
        }
    }
}

macro_rules! narrow_packed_int {
    ($($int:ty => $width:ident),* $(,)?) => {$(
        impl PackedInt for $int {
            const NOUN: &'static str = "value";

            fn into_list(values: AverPacked<Self>) -> AverIntList {
                let (Some(storage), Ok(start), Ok(end)) = (
                    values.storage.clone(),
                    u32::try_from(values.start),
                    u32::try_from(values.end()),
                ) else {
                    return match values.is_empty() {
                        true => AverIntList::empty(),
                        false => AverIntList::from_vec(values.to_aver_ints()),
                    };
                };
                AverIntList::Narrow(NarrowInts::new(storage, Width::$width, start, end))
            }

            fn from_list(values: AverIntList) -> Result<AverPacked<Self>, AverIntList> {
                let AverIntList::Narrow(narrow) = &values else {
                    return Err(values);
                };
                // SAFETY: `$int` is the element type of `Width::$width`.
                let Some((storage, start, end)) = (unsafe { narrow.typed::<$int>(Width::$width) })
                else {
                    return Err(values);
                };
                Ok(AverPacked::view(Some(storage.clone()), start, end))
            }
        }
    )*};
}

narrow_packed_int!(
    i8 => I8,
    u16 => U16,
    i16 => I16,
    u32 => U32,
    i32 => I32,
    i64 => I64,
);

/// The end of a carrier's range, kept as its complement so that it is never
/// zero: `Option<AverPacked<E>>` keeps its `None` there and is as large as
/// the carrier. No range ends at `usize::MAX`, which no allocation reaches.
#[derive(Clone, Copy)]
struct End(NonZeroUsize);

impl End {
    #[inline]
    fn new(end: usize) -> Self {
        Self(NonZeroUsize::new(!end).expect("a range ends below usize::MAX"))
    }

    #[inline]
    fn get(self) -> usize {
        !self.0.get()
    }
}

/// An immutable, cheaply cloned sequence of `E` with slice views.
pub struct AverPacked<E> {
    /// None for the empty list, which allocates nothing.
    storage: Option<Block<E>>,
    start: usize,
    end: End,
}

/// The carrier of a refinement whose elements are octets, the body of
/// `Bytes`.
pub type AverPackedU8 = AverPacked<u8>;

impl<E> Clone for AverPacked<E> {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            storage: self.storage.clone(),
            start: self.start,
            end: self.end,
        }
    }
}

impl<E: PackedInt> AverPacked<E> {
    #[inline]
    fn view(storage: Option<Block<E>>, start: usize, end: usize) -> Self {
        Self {
            storage,
            start,
            end: End::new(end),
        }
    }

    #[inline]
    fn end(&self) -> usize {
        self.end.get()
    }

    /// `values`, moved into one allocation.
    #[inline]
    pub fn from_vec(values: Vec<E>) -> Self {
        let end = values.len();
        Self::view((end != 0).then(|| Block::from_vec(values)), 0, end)
    }

    /// The `len` items `items` yields, in one allocation of exactly that
    /// room.
    pub fn from_iter_len(len: usize, items: impl IntoIterator<Item = E>) -> Self {
        if len == 0 {
            return Self::view(None, 0, 0);
        }
        let block = Block::from_iter_len(len, items);
        let end = block.len();
        Self::view(Some(block), 0, end)
    }

    /// A copy of `values`.
    #[inline]
    pub fn from_slice(values: &[E]) -> Self {
        let end = values.len();
        let storage = (end != 0).then(|| Block::from_iter_len(end, values.iter().copied()));
        Self::view(storage, 0, end)
    }

    /// Pack a general `List<Int>`, rejecting a value outside the width.
    /// Generated smart constructors have already established this
    /// condition; retaining the check makes a codegen mistake explicit rather
    /// than truncating it.
    pub fn try_from_aver_list(values: &AverList<AverInt>) -> Result<Self, String> {
        Self::try_from_ints(values.len(), values.iter())
    }

    fn try_from_ints<I: std::borrow::Borrow<AverInt>>(
        len: usize,
        values: impl Iterator<Item = I>,
    ) -> Result<Self, String> {
        if len == 0 {
            return Ok(Self::view(None, 0, 0));
        }
        // Written straight into the one allocation the carrier keeps.
        let mut packed = Block::with_capacity(len);
        for (index, value) in values.enumerate() {
            let value = value.borrow();
            let word = value.to_i64().ok_or_else(|| {
                format!("packed {} at index {index} is outside host Int", E::NOUN)
            })?;
            let element = E::try_from(word).map_err(|_| {
                format!(
                    "packed {} {word} at index {index} is outside its width",
                    E::NOUN
                )
            })?;
            packed.push_built(element);
        }
        let end = packed.len();
        Ok(Self::view((end != 0).then_some(packed), 0, end))
    }

    /// Pack a `List<Int>`: its own body when it is already a carrier of
    /// this width, each element checked against the width otherwise.
    pub fn try_from_int_list(values: AverIntList) -> Result<Self, String> {
        if let AverIntList::Wide(values) = &values {
            return Self::try_from_aver_list(values);
        }
        match E::from_list(values) {
            Ok(packed) => Ok(packed),
            Err(values) => Self::try_from_ints(values.len(), values.iter_cloned()),
        }
    }

    #[inline(always)]
    pub fn as_slice(&self) -> &[E] {
        match self.storage.as_ref() {
            // Every view is made with `start <= end <= len` of its body, and
            // a body never shrinks, so the range is not checked again at each
            // read.
            Some(items) => {
                debug_assert!(self.start <= self.end() && self.end() <= items.len());
                unsafe { items.get_unchecked(self.start..self.end()) }
            }
            None => &[],
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.end() - self.start
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end()
    }

    pub fn drop_first(&self, count: usize) -> Self {
        let start = self.start.saturating_add(count).min(self.end());
        Self::view(self.storage.clone(), start, self.end())
    }

    /// `drop_first(1)` of this very list: the offset moves, the shared
    /// elements and their count are left alone.
    #[inline]
    pub fn step_first(&mut self) {
        self.start = (self.start + 1).min(self.end());
    }

    pub fn take_first(&self, count: usize) -> Self {
        let end = self.start.saturating_add(count).min(self.end());
        Self::view(self.storage.clone(), self.start, end)
    }

    fn to_aver_ints(&self) -> Vec<AverInt> {
        self.as_slice()
            .iter()
            .map(|item| AverInt::from_i64((*item).into()))
            .collect()
    }

    pub fn to_aver_list(&self) -> AverList<AverInt> {
        AverList::from_vec(self.to_aver_ints())
    }

    /// The element at `index` as an `Int`, read in place: `get` of the
    /// `List<Int>` this is, without making that list (and so without
    /// touching the body's count).
    #[inline]
    pub fn get_int(&self, index: usize) -> Option<crate::AverInt> {
        self.as_slice()
            .get(index)
            .map(|e| crate::AverInt::from_i64((*e).into()))
    }

    /// Expose the semantic `List<Int>` as a zero-copy hybrid list view.
    pub fn to_int_list(&self) -> AverIntList {
        E::into_list(self.clone())
    }

    /// These elements followed by `more`, from a carrier the caller hands
    /// over. A body this carrier alone holds, read to its end, grows in
    /// place, so a loop that keeps appending to one accumulator copies each
    /// piece once rather than the whole accumulator every step.
    pub fn extend_owned(mut self, more: &[E]) -> Self {
        if more.is_empty() {
            return self;
        }
        let end = self.end();
        if let Some(storage) = self.storage.as_mut()
            && end == storage.len()
            && storage.extend_unique(more)
        {
            self.end = End::new(storage.len());
            return self;
        }
        Self::from_iter_len(
            self.len() + more.len(),
            self.as_slice().iter().chain(more).copied(),
        )
    }

    /// The elements in reverse order: a whole body this carrier alone
    /// holds is reversed where it lies, any other is copied.
    pub fn into_reversed(mut self) -> Self {
        let (start, end) = (self.start, self.end());
        if let Some(storage) = self.storage.as_mut()
            && start == 0
            && end == storage.len()
            && Block::strong_count(storage) == 1
        {
            storage.make_mut().reverse();
            return self;
        }
        Self::from_iter_len(self.len(), self.as_slice().iter().rev().copied())
    }

    pub fn into_vec(self) -> Vec<E> {
        self.as_slice().to_vec()
    }
}

impl AverPackedU8 {
    /// The octet at `position`, or `-1` at a negative position and at one at
    /// or past the length: the answer of the standard library's `Bytes.get`
    /// with `Option.None` as `-1`, which no octet is.
    ///
    /// One comparison against the length decides: a negative position is a
    /// large one as an unsigned word. The range of the view is not checked
    /// again (see [`AverPacked::as_slice`]), so a run of reads from one buffer
    /// stays a few instructions each and inlines where it is written.
    #[inline(always)]
    pub fn byte_at_i64(&self, position: i64) -> i64 {
        let index = position as u64;
        match self.storage.as_ref() {
            Some(items) if index < self.len() as u64 => {
                debug_assert!(self.end() <= items.len());
                i64::from(unsafe { *items.get_unchecked(self.start + index as usize) })
            }
            _ => -1,
        }
    }
}

impl<E: PackedInt> fmt::Debug for AverPacked<E> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.as_slice()).finish()
    }
}

impl<E: PackedInt> PartialEq for AverPacked<E> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl<E: PackedInt> Eq for AverPacked<E> {}

impl<E: PackedInt> PartialOrd for AverPacked<E> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<E: PackedInt> Ord for AverPacked<E> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl<E: PackedInt> Hash for AverPacked<E> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Match `AverList<AverInt>` exactly: the list tag, visible length,
        // then each element hashed as the `Int` it is.
        8u8.hash(state);
        self.len().hash(state);
        for item in self.as_slice() {
            AverInt::from_i64((*item).into()).hash(state);
        }
    }
}

impl<E: PackedInt> AverDisplay for AverPacked<E> {
    fn aver_display(&self) -> String {
        let mut out = String::from("[");
        for (index, item) in self.as_slice().iter().enumerate() {
            if index != 0 {
                out.push_str(", ");
            }
            out.push_str(&item.to_string());
        }
        out.push(']');
        out
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::hash_map::DefaultHasher;

    fn hash(value: &impl Hash) -> u64 {
        let mut state = DefaultHasher::new();
        value.hash(&mut state);
        state.finish()
    }

    #[test]
    fn views_preserve_value_semantics() {
        let packed = AverPackedU8::from_vec(vec![1, 2, 3, 4]);
        assert_eq!(packed.drop_first(1).take_first(2).as_slice(), &[2, 3]);
        assert_eq!(packed.drop_first(1).take_first(2).aver_display(), "[2, 3]");
    }

    #[test]
    fn an_empty_carrier_allocates_nothing() {
        let empty = AverPackedU8::from_vec(Vec::new());
        assert!(empty.storage.is_none());
        assert!(empty.as_slice().is_empty());
        assert!(empty.drop_first(2).take_first(1).is_empty());
        assert_eq!(empty.aver_display(), "[]");
        assert!(empty.into_vec().is_empty());
        let bytes = AverPackedU8::from_vec(vec![1, 2]);
        assert_eq!(bytes.drop_first(1).into_vec(), vec![2]);
        assert_eq!(bytes.into_vec(), vec![1, 2]);
    }

    #[test]
    #[cfg(target_pointer_width = "64")]
    fn a_carrier_is_three_words_and_so_is_its_option() {
        use std::mem::size_of;
        assert_eq!(size_of::<AverPacked<u8>>(), 24);
        assert_eq!(size_of::<Option<AverPacked<u8>>>(), 24);
        assert_eq!(size_of::<Option<AverPacked<i64>>>(), 24);
        assert_eq!(size_of::<AverIntList>(), 32, "every List<Int>");
    }

    #[test]
    fn hash_matches_general_int_list() {
        let packed = AverPackedU8::from_vec(vec![0, 127, 255]);
        assert_eq!(hash(&packed), hash(&packed.to_aver_list()));
        let wide = AverPacked::<i32>::from_vec(vec![-70_000, 0, 70_000]);
        assert_eq!(hash(&wide), hash(&wide.to_aver_list()));
        assert_eq!(hash(&wide.to_int_list()), hash(&wide.to_aver_list()));
    }

    #[test]
    fn extend_owned_grows_a_sole_body_and_copies_a_shared_one() {
        let first = AverPackedU8::from_vec(vec![1, 2]);
        let grown = first.extend_owned(&[3]).extend_owned(&[]);
        assert_eq!(grown.as_slice(), &[1, 2, 3]);
        let kept = grown.clone();
        let copied = grown.extend_owned(&[4]);
        assert_eq!(kept.as_slice(), &[1, 2, 3]);
        assert_eq!(copied.as_slice(), &[1, 2, 3, 4]);
        let view = copied.drop_first(1).take_first(2);
        drop(copied);
        assert_eq!(view.extend_owned(&[9]).as_slice(), &[2, 3, 9]);
        let empty = AverPackedU8::from_vec(Vec::new()).extend_owned(&[5]);
        assert_eq!(empty.as_slice(), &[5]);
    }

    #[test]
    fn reversing_a_sole_body_keeps_it_and_a_shared_one_is_copied() {
        let sole = AverPacked::<u16>::from_vec(vec![1, 2, 3]);
        assert_eq!(sole.into_reversed().as_slice(), &[3, 2, 1]);
        let shared = AverPacked::<u16>::from_vec(vec![1, 2, 3]);
        let kept = shared.clone();
        assert_eq!(shared.into_reversed().as_slice(), &[3, 2, 1]);
        assert_eq!(kept.as_slice(), &[1, 2, 3]);
        let view = AverPacked::<u16>::from_vec(vec![1, 2, 3]).drop_first(1);
        assert_eq!(view.into_reversed().as_slice(), &[3, 2]);
    }

    #[test]
    fn rejects_values_outside_the_width() {
        let values = AverList::from_vec(vec![AverInt::from_i64(0), AverInt::from_i64(256)]);
        assert!(AverPackedU8::try_from_aver_list(&values).is_err());
        assert!(AverPacked::<u16>::try_from_aver_list(&values).is_ok());
        let negative = AverList::from_vec(vec![AverInt::from_i64(-1)]);
        assert!(AverPacked::<u32>::try_from_aver_list(&negative).is_err());
        assert!(AverPacked::<i8>::try_from_aver_list(&negative).is_ok());
    }

    #[test]
    fn a_narrow_carrier_reads_as_its_list_and_back_without_a_copy() {
        let packed = AverPacked::<i16>::from_vec(vec![-300, 5, 300]);
        let list = packed.drop_first(1).to_int_list();
        assert!(matches!(list, AverIntList::Narrow(_)));
        assert_eq!(list.len(), 2);
        assert_eq!(list.get(1), Some(AverInt::from_i64(300)));
        assert_eq!(
            list,
            AverIntList::from_vec(vec![AverInt::from_i64(5), AverInt::from_i64(300)])
        );
        let back = AverPacked::<i16>::try_from_int_list(list.clone()).unwrap();
        assert_eq!(back.as_slice(), &[5, 300]);
        assert!(std::ptr::eq(
            back.as_slice().as_ptr(),
            packed.as_slice()[1..].as_ptr()
        ));
        assert!(AverPacked::<u8>::try_from_int_list(list.clone()).is_err());
        assert_eq!(
            AverPacked::<i32>::try_from_int_list(list)
                .unwrap()
                .as_slice(),
            &[5, 300]
        );
    }
}
