//! Hybrid storage for Aver `List<Int>` values.
//!
//! A refined carrier can expose its contents without eagerly inflating
//! every element into an `AverInt`: octets as `Packed`, the other widths of
//! a packed carrier ([`crate::AverPacked`]) as `Narrow`. A list of octets
//! built by prepending, as an encoder builds its output backwards, is a
//! `Stack` that pushes each byte into a buffer instead of a cell. Other
//! values use the ordinary persistent list, so this changes
//! representation, never semantics.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::block::{Block, Untyped};
use crate::packed::{AverPacked, PackedInt};
use crate::{AverDisplay, AverInt, AverList, AverPackedU8};

#[derive(Clone)]
pub enum AverIntList {
    Packed(AverPackedU8),
    /// The body of a packed carrier of a width other than a byte, read as
    /// the list it is.
    Narrow(NarrowInts),
    Wide(AverList<AverInt>),
    /// Octets built by prepending, kept in an append-only buffer in reverse
    /// (see [`ByteStack`]).
    Stack(ByteStack),
}

/// A list of octets built by prepending: a chain of append-only buffers,
/// each holding its part of the list backwards, so a prepend is a push.
/// A value sees the first `len` bytes of its buffer, then the list below
/// it. Bytes once pushed never change, so a shorter value that shares a
/// buffer still reads the list it was. A prepend onto the value whose
/// `len` is its buffer's own pushes in place; onto any other it pushes in
/// place only when no other value holds the buffer (cutting it back to its
/// `len` first), and otherwise starts a new buffer over this value, so no
/// prepend copies and a list prepended from many places is at worst a
/// chain of one-byte buffers.
#[derive(Clone)]
pub struct ByteStack {
    seg: std::rc::Rc<Segment>,
    len: usize,
}

struct Segment {
    buf: std::cell::UnsafeCell<Vec<u8>>,
    /// The list after this buffer's bytes, and its length.
    below: Option<ByteStack>,
    base: usize,
}

impl Drop for Segment {
    /// A long chain is taken apart one segment at a time, not by recursion.
    fn drop(&mut self) {
        let mut next = self.below.take();
        while let Some(stack) = next {
            match std::rc::Rc::try_unwrap(stack.seg) {
                Ok(mut seg) => next = seg.below.take(),
                Err(_) => break,
            }
        }
    }
}

impl ByteStack {
    /// The bytes this value sees in its own buffer, oldest first.
    #[inline]
    fn own(&self) -> &[u8] {
        // SAFETY: a buffer only changes through `push_onto`, which takes no
        // reference out of it and runs on this thread (`Rc` is neither
        // `Send` nor `Sync`); a slice handed out here lives as long as the
        // borrow of `self`, and nothing called while it lives pushes.
        unsafe { &(&*self.seg.buf.get())[..self.len] }
    }

    #[inline]
    fn total(&self) -> usize {
        self.len + self.seg.base
    }

    /// The `index`th element of the list.
    #[inline]
    fn get(&self, mut index: usize) -> Option<u8> {
        let mut at = self;
        loop {
            if index < at.len {
                return Some(at.own()[at.len - 1 - index]);
            }
            index -= at.len;
            at = at.seg.below.as_ref()?;
        }
    }

    /// The list after its first `count` elements; `None` when that is empty.
    fn drop_first(&self, mut count: usize) -> Option<ByteStack> {
        let mut at = self;
        loop {
            if count < at.len {
                return Some(ByteStack {
                    seg: at.seg.clone(),
                    len: at.len - count,
                });
            }
            count -= at.len;
            at = at.seg.below.as_ref()?;
        }
    }

    fn iter(&self) -> ByteStackIter<'_> {
        ByteStackIter {
            at: Some(self),
            index: 0,
            left: self.total(),
        }
    }

    /// `byte` prepended to the list this value is.
    fn push_onto(&self, byte: u8) -> ByteStack {
        // SAFETY: as in `own`; no slice of the buffer is alive here, since
        // every reader takes one only for the call it makes.
        let buf = unsafe { &mut *self.seg.buf.get() };
        if buf.len() == self.len || std::rc::Rc::strong_count(&self.seg) == 1 {
            buf.truncate(self.len);
            buf.push(byte);
            return ByteStack {
                seg: self.seg.clone(),
                len: self.len + 1,
            };
        }
        let mut bytes = Vec::with_capacity(16);
        bytes.push(byte);
        ByteStack::new(bytes, Some(self.clone()))
    }

    fn new(bytes: Vec<u8>, below: Option<ByteStack>) -> ByteStack {
        let len = bytes.len();
        let base = below.as_ref().map_or(0, ByteStack::total);
        ByteStack {
            seg: std::rc::Rc::new(Segment {
                buf: std::cell::UnsafeCell::new(bytes),
                below,
                base,
            }),
            len,
        }
    }

    /// The list forwards, as octets: a single buffer no other value holds
    /// is reused in place.
    fn into_reversed(self) -> AverPackedU8 {
        if self.seg.below.is_none() {
            let len = self.len;
            match std::rc::Rc::try_unwrap(self.seg) {
                Ok(mut seg) => {
                    let mut bytes = std::mem::take(seg.buf.get_mut());
                    bytes.truncate(len);
                    return AverPackedU8::from_vec(bytes);
                }
                Err(seg) => return ByteStack { seg, len }.reversed(),
            }
        }
        self.reversed()
    }

    /// The list forwards: the deepest buffer's bytes first.
    fn reversed(&self) -> AverPackedU8 {
        let mut parts = Vec::new();
        let mut at = Some(self);
        while let Some(stack) = at {
            parts.push(stack.own());
            at = stack.seg.below.as_ref();
        }
        let mut bytes = Vec::with_capacity(self.total());
        for part in parts.into_iter().rev() {
            bytes.extend_from_slice(part);
        }
        AverPackedU8::from_vec(bytes)
    }
}

pub struct ByteStackIter<'a> {
    at: Option<&'a ByteStack>,
    index: usize,
    left: usize,
}

impl Iterator for ByteStackIter<'_> {
    type Item = u8;

    #[inline]
    fn next(&mut self) -> Option<u8> {
        loop {
            let at = self.at?;
            if self.index < at.len {
                self.index += 1;
                self.left -= 1;
                return Some(at.own()[at.len - self.index]);
            }
            self.at = at.seg.below.as_ref();
            self.index = 0;
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.left, Some(self.left))
    }
}

/// The byte `value` is, if it is one.
#[inline]
fn as_byte(value: &AverInt) -> Option<u8> {
    value.to_i64().and_then(|v| u8::try_from(v).ok())
}

/// The width of a [`NarrowInts`]: every width a packed carrier has past a
/// byte.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum Width {
    I8,
    U16,
    I16,
    U32,
    I32,
    I64,
}

/// A range of the body of a packed carrier of a width past a byte: the
/// carrier read as a `List<Int>`. The body is held without its element type
/// and `width` says which it is, so copying or dropping one, which a walk
/// does at every step, only changes the count. The range is kept in 32
/// bits so that it, and so every `List<Int>`, is no larger than a packed
/// list of octets; a longer carrier is read as a general list.
pub struct NarrowInts {
    body: Untyped,
    width: Width,
    start: u32,
    end: u32,
}

impl Clone for NarrowInts {
    #[inline]
    fn clone(&self) -> Self {
        Self {
            body: self.body.share(),
            width: self.width,
            start: self.start,
            end: self.end,
        }
    }
}

impl Drop for NarrowInts {
    #[inline]
    fn drop(&mut self) {
        if self.body.release() {
            self.free();
        }
    }
}

/// `$body` with `$items` bound to the elements of `$narrow`, a slice of
/// their own width.
macro_rules! with_items {
    ($narrow:expr, |$items:ident| $body:expr) => {{
        let narrow = $narrow;
        let range = narrow.start as usize..narrow.end as usize;
        // SAFETY: the body was made as a block of the width it is marked
        // with (`NarrowInts::new`).
        unsafe {
            match narrow.width {
                Width::I8 => {
                    let $items = &narrow.body.typed::<i8>()[range];
                    $body
                }
                Width::U16 => {
                    let $items = &narrow.body.typed::<u16>()[range];
                    $body
                }
                Width::I16 => {
                    let $items = &narrow.body.typed::<i16>()[range];
                    $body
                }
                Width::U32 => {
                    let $items = &narrow.body.typed::<u32>()[range];
                    $body
                }
                Width::I32 => {
                    let $items = &narrow.body.typed::<i32>()[range];
                    $body
                }
                Width::I64 => {
                    let $items = &narrow.body.typed::<i64>()[range];
                    $body
                }
            }
        }
    }};
}

impl NarrowInts {
    /// `start..end` of `body`, a block of `E`s of width `width`.
    pub(crate) fn new<E: PackedInt>(body: Block<E>, width: Width, start: u32, end: u32) -> Self {
        Self {
            body: body.into_untyped(),
            width,
            start,
            end,
        }
    }

    /// The block and range, when the body is of width `width`, read as the
    /// block of `E`s it was made as.
    ///
    /// # Safety
    /// `E` is the element type of `width`.
    pub(crate) unsafe fn typed<E: PackedInt>(
        &self,
        width: Width,
    ) -> Option<(&Block<E>, usize, usize)> {
        // SAFETY: the caller pairs `E` with `width`.
        (self.width == width).then(|| {
            (
                unsafe { self.body.typed::<E>() },
                self.start as usize,
                self.end as usize,
            )
        })
    }

    #[cold]
    #[inline(never)]
    fn free(&mut self) {
        // SAFETY: the last count is gone, and the body was made as a block of
        // the width it is marked with.
        unsafe {
            match self.width {
                Width::I8 => self.body.free_as::<i8>(),
                Width::U16 => self.body.free_as::<u16>(),
                Width::I16 => self.body.free_as::<i16>(),
                Width::U32 => self.body.free_as::<u32>(),
                Width::I32 => self.body.free_as::<i32>(),
                Width::I64 => self.body.free_as::<i64>(),
            }
        }
    }

    #[inline]
    fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    /// The first element. Out of line, so that the walks over every
    /// `List<Int>` keep a small match on the two common shapes.
    #[inline(never)]
    pub(crate) fn first(&self) -> Option<AverInt> {
        self.get(0)
    }

    /// `drop_first(1)` of this very list: the offset moves, the shared body
    /// and its count are left alone.
    #[inline]
    pub(crate) fn step_first(&mut self) {
        self.start = (self.start + 1).min(self.end);
    }

    #[inline]
    fn get(&self, index: usize) -> Option<AverInt> {
        fn int<E: PackedInt>(item: &E) -> AverInt {
            AverInt::from_i64((*item).into())
        }
        with_items!(self, |items| items.get(index).map(int))
    }

    fn iter(&self) -> NarrowIter<'_> {
        with_items!(self, |items| NarrowIter::from(items))
    }

    #[inline]
    fn drop_first(&self, count: usize) -> Self {
        let count = u32::try_from(count).unwrap_or(u32::MAX);
        let mut rest = self.clone();
        rest.start = self.start.saturating_add(count).min(self.end);
        rest
    }

    #[inline]
    fn take_first(&self, count: usize) -> Self {
        let count = u32::try_from(count).unwrap_or(u32::MAX);
        let mut first = self.clone();
        first.end = self.start.saturating_add(count).min(self.end);
        first
    }

    fn contains(&self, needle: &AverInt) -> bool {
        let Some(word) = needle.to_i64() else {
            return false;
        };
        fn find<E: PackedInt>(items: &[E], word: i64) -> bool {
            E::try_from(word).is_ok_and(|item| items.contains(&item))
        }
        with_items!(self, |items| find(items, word))
    }
}

/// The elements of a [`NarrowInts`], read as `Int`s.
pub enum NarrowIter<'a> {
    I8(std::slice::Iter<'a, i8>),
    U16(std::slice::Iter<'a, u16>),
    I16(std::slice::Iter<'a, i16>),
    U32(std::slice::Iter<'a, u32>),
    I32(std::slice::Iter<'a, i32>),
    I64(std::slice::Iter<'a, i64>),
}

macro_rules! narrow_iter_from {
    ($($int:ty => $variant:ident),*) => {$(
        impl<'a> From<&'a [$int]> for NarrowIter<'a> {
            fn from(items: &'a [$int]) -> Self {
                Self::$variant(items.iter())
            }
        }
    )*};
}

narrow_iter_from!(i8 => I8, u16 => U16, i16 => I16, u32 => U32, i32 => I32, i64 => I64);

impl Iterator for NarrowIter<'_> {
    type Item = AverInt;

    // Kept out of line: the iterator of every `List<Int>` matches on it,
    // and the widths' arms would keep that match from being inlined.
    #[inline(never)]
    fn next(&mut self) -> Option<Self::Item> {
        let word = match self {
            Self::I8(iter) => i64::from(*iter.next()?),
            Self::U16(iter) => i64::from(*iter.next()?),
            Self::I16(iter) => i64::from(*iter.next()?),
            Self::U32(iter) => i64::from(*iter.next()?),
            Self::I32(iter) => i64::from(*iter.next()?),
            Self::I64(iter) => *iter.next()?,
        };
        Some(AverInt::from_i64(word))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::I8(iter) => iter.size_hint(),
            Self::U16(iter) => iter.size_hint(),
            Self::I16(iter) => iter.size_hint(),
            Self::U32(iter) => iter.size_hint(),
            Self::I32(iter) => iter.size_hint(),
            Self::I64(iter) => iter.size_hint(),
        }
    }
}

pub enum AverIntListIter<'a> {
    Packed(std::slice::Iter<'a, u8>),
    Narrow(NarrowIter<'a>),
    Wide(crate::AverListIter<'a, AverInt>),
    Stack(ByteStackIter<'a>),
}

impl Iterator for AverIntListIter<'_> {
    type Item = AverInt;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Packed(iter) => iter.next().map(|byte| AverInt::from_i64(i64::from(*byte))),
            Self::Narrow(iter) => iter.next(),
            Self::Wide(iter) => iter.next().cloned(),
            Self::Stack(iter) => iter.next().map(|byte| AverInt::from_i64(i64::from(byte))),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Packed(iter) => iter.size_hint(),
            Self::Narrow(iter) => iter.size_hint(),
            Self::Wide(iter) => iter.size_hint(),
            Self::Stack(iter) => iter.size_hint(),
        }
    }
}

impl ExactSizeIterator for AverIntListIter<'_> {}
impl std::iter::FusedIterator for AverIntListIter<'_> {}

impl AverIntList {
    pub fn empty() -> Self {
        Self::Packed(AverPackedU8::from_vec(Vec::new()))
    }

    pub fn from_vec(values: Vec<AverInt>) -> Self {
        let mut bytes = Vec::with_capacity(values.len());
        for value in &values {
            let Some(byte) = value.to_u32().and_then(|value| u8::try_from(value).ok()) else {
                return Self::Wide(AverList::from_vec(values));
            };
            bytes.push(byte);
        }
        Self::Packed(AverPackedU8::from_vec(bytes))
    }

    /// The list a packed carrier of any width is, sharing its body.
    pub fn from_packed<E: PackedInt>(values: AverPacked<E>) -> Self {
        E::into_list(values)
    }

    /// Borrow the compact carrier when this list already has one.
    ///
    /// A `Packed` value proves every element is in `0..=255` by
    /// construction. Rust smart constructors can therefore clone the
    /// immutable carrier in O(1) instead of walking it to establish the same
    /// fact again. `Wide` deliberately returns `None`: its elements still
    /// require ordinary validation.
    #[inline]
    pub fn as_packed(&self) -> Option<&AverPackedU8> {
        match self {
            Self::Packed(values) => Some(values),
            Self::Narrow(_) | Self::Wide(_) | Self::Stack(_) => None,
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        match self {
            Self::Packed(values) => values.len(),
            Self::Narrow(values) => values.len(),
            Self::Wide(values) => values.len(),
            Self::Stack(stack) => stack.total(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter_cloned(&self) -> AverIntListIter<'_> {
        match self {
            Self::Packed(values) => AverIntListIter::Packed(values.as_slice().iter()),
            Self::Narrow(values) => AverIntListIter::Narrow(values.iter()),
            Self::Wide(values) => AverIntListIter::Wide(values.iter()),
            Self::Stack(stack) => AverIntListIter::Stack(stack.iter()),
        }
    }

    pub fn to_vec(&self) -> Vec<AverInt> {
        self.iter_cloned().collect()
    }

    pub fn to_aver_list(&self) -> AverList<AverInt> {
        match self {
            Self::Packed(_) | Self::Narrow(_) | Self::Stack(_) => AverList::from_vec(self.to_vec()),
            Self::Wide(values) => values.clone(),
        }
    }

    #[inline]
    pub fn into_aver_list(self) -> AverList<AverInt> {
        match self {
            Self::Wide(values) => values,
            packed => packed.widened(),
        }
    }

    /// A packed list, of any width, as a general one. Out of line: a list
    /// built by prepending widens once, at its first step.
    #[cold]
    #[inline(never)]
    fn widened(self) -> AverList<AverInt> {
        AverList::from_vec(self.to_vec())
    }

    /// This list as a packed carrier of the width the field it goes into
    /// holds: its own body when it already is one, each element checked
    /// against the width otherwise.
    pub fn into_packed<E: PackedInt>(self) -> Result<AverPacked<E>, String> {
        AverPacked::try_from_int_list(self)
    }

    #[inline(always)]
    pub fn uncons_cloned(&self) -> Option<(AverInt, Self)> {
        match self {
            Self::Packed(values) => values.as_slice().first().map(|byte| {
                (
                    AverInt::from_i64(i64::from(*byte)),
                    Self::Packed(values.drop_first(1)),
                )
            }),
            Self::Narrow(values) => Self::narrow_uncons(values),
            Self::Wide(values) => {
                crate::list_uncons_cloned(values).map(|(head, tail)| (head, Self::Wide(tail)))
            }
            Self::Stack(stack) => stack.get(0).map(|byte| {
                (
                    AverInt::from_i64(i64::from(byte)),
                    Self::stack_or_empty(stack.drop_first(1)),
                )
            }),
        }
    }

    /// `uncons_cloned` of a `Narrow` list, out of line so that the hot
    /// match keeps to the two common shapes, and in one piece so that a
    /// step is one call.
    #[inline(never)]
    fn narrow_uncons(values: &NarrowInts) -> Option<(AverInt, Self)> {
        let head = values.get(0)?;
        Some((head, Self::Narrow(values.drop_first(1))))
    }

    fn stack_or_empty(stack: Option<ByteStack>) -> Self {
        stack.map_or_else(Self::empty, Self::Stack)
    }

    #[inline]
    pub fn drop_first(&self, count: usize) -> Self {
        match self {
            Self::Packed(values) => Self::Packed(values.drop_first(count)),
            Self::Narrow(values) => Self::Narrow(values.drop_first(count)),
            Self::Wide(values) => Self::Wide(values.drop_first(count)),
            Self::Stack(stack) => Self::stack_or_empty(stack.drop_first(count)),
        }
    }

    pub fn take_first(&self, count: usize) -> Self {
        match self {
            Self::Packed(values) => Self::Packed(values.take_first(count)),
            Self::Narrow(values) => Self::Narrow(values.take_first(count)),
            Self::Wide(values) => Self::from_vec(values.iter().take(count).cloned().collect()),
            Self::Stack(stack) => {
                Self::Packed(AverPackedU8::from_vec(stack.iter().take(count).collect()))
            }
        }
    }

    pub fn prepend(head: AverInt, tail: &Self) -> Self {
        if let Some(byte) = as_byte(&head) {
            match tail {
                Self::Stack(stack) => return Self::Stack(stack.push_onto(byte)),
                // Octets already in hand are copied in once, as a wide
                // list would copy them into its cells.
                Self::Packed(values) => {
                    let values = values.as_slice();
                    let mut bytes = Vec::with_capacity((values.len() + 1).max(16));
                    bytes.extend(values.iter().rev());
                    bytes.push(byte);
                    return Self::Stack(ByteStack::new(bytes, None));
                }
                _ => {}
            }
        }
        // Widen once at a persistent-list mutation boundary. Subsequent
        // accumulator prepends stay O(1), including when `head` is not a byte.
        Self::Wide(AverList::prepend(head, &tail.to_aver_list()))
    }

    /// `prepend` onto a list the caller hands over (see
    /// `AverList::prepend_owned`).
    #[inline]
    pub fn prepend_owned(head: AverInt, tail: Self) -> Self {
        if matches!(&tail, Self::Stack(_) | Self::Packed(_)) {
            return Self::prepend(head, &tail);
        }
        Self::Wide(AverList::prepend_owned(head, tail.into_aver_list()))
    }

    /// The element at `index`, read in place.
    #[inline]
    pub fn get(&self, index: usize) -> Option<AverInt> {
        match self {
            Self::Packed(values) => values
                .as_slice()
                .get(index)
                .map(|byte| AverInt::from_i64(i64::from(*byte))),
            Self::Narrow(values) => values.get(index),
            Self::Wide(values) => values.get(index).cloned(),
            Self::Stack(stack) => stack
                .get(index)
                .map(|byte| AverInt::from_i64(i64::from(byte))),
        }
    }

    pub fn concat(left: &Self, right: &Self) -> Self {
        match (left, right) {
            (Self::Packed(left), Self::Packed(right)) => Self::Packed(AverPackedU8::from_iter_len(
                left.len() + right.len(),
                left.as_slice().iter().chain(right.as_slice()).copied(),
            )),
            _ => Self::Wide(AverList::concat(
                &left.to_aver_list(),
                &right.to_aver_list(),
            )),
        }
    }

    /// `concat` of a left list the caller hands over: two packed carriers
    /// join by growing the left body in place when it is not shared (see
    /// [`AverPackedU8::extend_owned`]); any other pair joins as `concat`.
    pub fn concat_owned(left: Self, right: &Self) -> Self {
        match (left, right) {
            (Self::Packed(left), Self::Packed(right)) if !left.is_empty() => {
                Self::Packed(left.extend_owned(right.as_slice()))
            }
            (left, right) => Self::concat(&left, right),
        }
    }

    pub fn reverse(&self) -> Self {
        match self {
            Self::Packed(values) => Self::Packed(values.clone().into_reversed()),
            Self::Stack(stack) => Self::Packed(stack.reversed()),
            Self::Narrow(_) | Self::Wide(_) => {
                let mut out = self.to_vec();
                out.reverse();
                Self::from_vec(out)
            }
        }
    }

    /// `reverse` of a list the caller hands over: a body it alone holds is
    /// reversed where it lies rather than copied.
    pub fn reverse_owned(self) -> Self {
        match self {
            Self::Packed(values) => Self::Packed(values.into_reversed()),
            Self::Narrow(_) => self.reverse(),
            Self::Wide(values) => Self::from_vec(values.into_reversed_vec()),
            Self::Stack(stack) => Self::Packed(stack.into_reversed()),
        }
    }

    pub fn contains(&self, needle: &AverInt) -> bool {
        match self {
            Self::Packed(values) => needle
                .to_u32()
                .and_then(|value| u8::try_from(value).ok())
                .is_some_and(|byte| values.as_slice().contains(&byte)),
            Self::Narrow(values) => values.contains(needle),
            Self::Wide(values) => values.contains(needle),
            Self::Stack(stack) => {
                as_byte(needle).is_some_and(|byte| stack.iter().any(|b| b == byte))
            }
        }
    }
}

impl IntoIterator for AverIntList {
    type Item = AverInt;
    type IntoIter = std::vec::IntoIter<AverInt>;

    fn into_iter(self) -> Self::IntoIter {
        self.to_vec().into_iter()
    }
}

impl fmt::Debug for AverIntList {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter_cloned()).finish()
    }
}

impl PartialEq for AverIntList {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Packed(left), Self::Packed(right)) => left.as_slice() == right.as_slice(),
            _ => self.len() == other.len() && self.iter_cloned().eq(other.iter_cloned()),
        }
    }
}

impl Eq for AverIntList {}

impl PartialOrd for AverIntList {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AverIntList {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Self::Packed(left), Self::Packed(right)) => left.as_slice().cmp(right.as_slice()),
            _ => self.iter_cloned().cmp(other.iter_cloned()),
        }
    }
}

impl Hash for AverIntList {
    fn hash<H: Hasher>(&self, state: &mut H) {
        8u8.hash(state);
        self.len().hash(state);
        for item in self.iter_cloned() {
            item.hash(state);
        }
    }
}

impl AverDisplay for AverIntList {
    fn aver_display(&self) -> String {
        let parts: Vec<String> = self
            .iter_cloned()
            .map(|value| value.aver_display_inner())
            .collect();
        format!("[{}]", parts.join(", "))
    }

    fn aver_display_inner(&self) -> String {
        self.aver_display()
    }
}

/// A `List<Int>`, either Rust form of it, that packs into a carrier.
pub trait IntoPacked {
    fn into_packed<E: PackedInt>(self) -> Result<AverPacked<E>, String>;
}

impl IntoPacked for AverIntList {
    fn into_packed<E: PackedInt>(self) -> Result<AverPacked<E>, String> {
        AverPacked::try_from_int_list(self)
    }
}

impl IntoPacked for AverList<AverInt> {
    fn into_packed<E: PackedInt>(self) -> Result<AverPacked<E>, String> {
        AverPacked::try_from_aver_list(&self)
    }
}

/// `values` as a packed carrier of the width the place it goes to holds.
pub fn into_packed<E: PackedInt, T: IntoPacked>(values: T) -> Result<AverPacked<E>, String> {
    values.into_packed()
}

/// `values` as a packed carrier of octets.
pub fn into_packed_u8<T: IntoPacked>(values: T) -> Result<AverPackedU8, String> {
    values.into_packed()
}

/// Builder entry points used by the list-deforestation intrinsics when their
/// accumulator's semantic type is `List<Int>`. Builders deliberately start in
/// the wide append-friendly form; the byte-sink pass has its own validating
/// `ByteBuilder` and bypasses these functions entirely.
pub fn int_list_builder_new(capacity: usize) -> AverIntList {
    AverIntList::Wide(crate::list_builder_new(capacity))
}

pub fn int_list_builder_push(builder: AverIntList, item: AverInt) -> AverIntList {
    AverIntList::Wide(crate::list_builder_push(builder.into_aver_list(), item))
}

pub fn int_list_builder_finalize(builder: AverIntList) -> AverIntList {
    builder
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
    fn byte_values_stay_packed_through_views() {
        let values = AverIntList::from_vec(
            [0, 1, 127, 255]
                .into_iter()
                .map(AverInt::from_i64)
                .collect(),
        );
        assert!(matches!(values, AverIntList::Packed(_)));
        assert!(matches!(values.drop_first(1), AverIntList::Packed(_)));
        assert_eq!(
            values.drop_first(1).take_first(2).aver_display(),
            "[1, 127]"
        );
    }

    #[test]
    fn packed_access_reuses_the_backing_storage() {
        let values = AverIntList::from_vec(
            [0, 1, 127, 255]
                .into_iter()
                .map(AverInt::from_i64)
                .collect(),
        );
        let packed = values.as_packed().expect("byte values should be packed");
        let reused = packed.clone();

        assert_eq!(packed.as_slice().as_ptr(), reused.as_slice().as_ptr());
        assert_eq!(packed.as_slice(), reused.as_slice());
    }

    #[test]
    fn values_outside_u8_remain_wide() {
        let values = AverIntList::from_vec([-1, 256].into_iter().map(AverInt::from_i64).collect());
        assert!(matches!(values, AverIntList::Wide(_)));
        assert_eq!(values.aver_display(), "[-1, 256]");
    }

    #[test]
    fn packed_and_wide_forms_are_observationally_equal() {
        let packed =
            AverIntList::from_vec([0, 127, 255].into_iter().map(AverInt::from_i64).collect());
        let wide = AverIntList::Wide(AverList::from_vec(
            [0, 127, 255].into_iter().map(AverInt::from_i64).collect(),
        ));
        assert_eq!(packed, wide);
        assert_eq!(packed.cmp(&wide), Ordering::Equal);
        assert_eq!(hash(&packed), hash(&wide));
    }

    #[test]
    fn mutation_boundaries_widen_without_truncating() {
        let bytes = AverIntList::from_vec([1, 2].into_iter().map(AverInt::from_i64).collect());
        let with_negative = AverIntList::prepend(AverInt::from_i64(-1), &bytes);
        assert!(matches!(with_negative, AverIntList::Wide(_)));
        assert_eq!(with_negative.aver_display(), "[-1, 1, 2]");

        let beyond_byte = AverIntList::from_vec(vec![AverInt::from_i64(256)]);
        let joined = AverIntList::concat(&bytes, &beyond_byte);
        assert!(matches!(joined, AverIntList::Wide(_)));
        assert_eq!(joined.aver_display(), "[1, 2, 256]");
    }

    #[test]
    fn owned_concat_and_reverse_leave_a_shared_list_as_it_was() {
        let ints = |values: &[i64]| {
            AverIntList::from_vec(values.iter().copied().map(AverInt::from_i64).collect())
        };
        let shared = ints(&[1, 2]);
        let joined = AverIntList::concat_owned(shared.clone(), &ints(&[3]));
        assert_eq!(shared.aver_display(), "[1, 2]");
        let grown = AverIntList::concat_owned(joined, &ints(&[4]));
        assert!(matches!(grown, AverIntList::Packed(_)));
        assert_eq!(grown.aver_display(), "[1, 2, 3, 4]");
        let wide = AverIntList::concat_owned(grown.clone(), &ints(&[300]));
        assert_eq!(wide.aver_display(), "[1, 2, 3, 4, 300]");
        assert_eq!(grown.aver_display(), "[1, 2, 3, 4]");
        let started = AverIntList::concat_owned(AverIntList::empty(), &ints(&[7]));
        assert_eq!(started.aver_display(), "[7]");

        assert_eq!(
            wide.clone().reverse_owned().aver_display(),
            "[300, 4, 3, 2, 1]"
        );
        assert_eq!(wide.reverse().aver_display(), "[300, 4, 3, 2, 1]");
        assert_eq!(grown.clone().reverse_owned().aver_display(), "[4, 3, 2, 1]");
        assert_eq!(grown.aver_display(), "[1, 2, 3, 4]");
    }

    fn prepended(onto: &AverIntList, values: &[i64]) -> AverIntList {
        values.iter().fold(onto.clone(), |list, v| {
            AverIntList::prepend(AverInt::from_i64(*v), &list)
        })
    }

    #[test]
    fn a_list_prepended_from_many_places_reads_as_it_was_built() {
        let base = prepended(&AverIntList::empty(), &[1, 2, 3]);
        assert!(matches!(base, AverIntList::Stack(_)));
        assert_eq!(base.aver_display(), "[3, 2, 1]");
        // Two values prepended onto the same one, and onto a shorter view.
        let left = prepended(&base, &[4, 5]);
        let right = prepended(&base, &[6]);
        let tail = base.drop_first(1);
        let from_tail = prepended(&tail, &[7]);
        assert_eq!(base.aver_display(), "[3, 2, 1]");
        assert_eq!(left.aver_display(), "[5, 4, 3, 2, 1]");
        assert_eq!(right.aver_display(), "[6, 3, 2, 1]");
        assert_eq!(tail.aver_display(), "[2, 1]");
        assert_eq!(from_tail.aver_display(), "[7, 2, 1]");
        assert_eq!(prepended(&right, &[8]).aver_display(), "[8, 6, 3, 2, 1]");
        assert_eq!(right.get(3), Some(AverInt::from_i64(1)));
        assert_eq!(right.get(4), None);
        assert_eq!(right.len(), 4);
        assert_eq!(right.drop_first(2).aver_display(), "[2, 1]");
        assert_eq!(right.drop_first(4).aver_display(), "[]");
        assert_eq!(right.take_first(2).aver_display(), "[6, 3]");
        assert!(right.contains(&AverInt::from_i64(1)));
        assert!(!right.contains(&AverInt::from_i64(4)));
        assert_eq!(right.reverse().aver_display(), "[1, 2, 3, 6]");
        assert_eq!(right.clone().reverse_owned().aver_display(), "[1, 2, 3, 6]");
        assert_eq!(
            left.clone().reverse_owned().aver_display(),
            "[1, 2, 3, 4, 5]"
        );
        // A value past a byte widens, and every form compares the same.
        let wide = prepended(&right, &[300]);
        assert!(matches!(wide, AverIntList::Wide(_)));
        assert_eq!(wide.aver_display(), "[300, 6, 3, 2, 1]");
        let packed = AverIntList::from_vec([6, 3, 2, 1].map(AverInt::from_i64).to_vec());
        assert_eq!(right, packed);
        assert_eq!(hash(&right), hash(&packed));
        // Octets already packed are copied in once.
        let onto_packed = prepended(&packed, &[9]);
        assert!(matches!(onto_packed, AverIntList::Stack(_)));
        assert_eq!(onto_packed.aver_display(), "[9, 6, 3, 2, 1]");
        assert_eq!(packed.aver_display(), "[6, 3, 2, 1]");
    }

    #[test]
    fn a_reverse_of_a_sole_buffer_reuses_it_and_a_long_chain_drops() {
        let built = prepended(&AverIntList::empty(), &[1, 2, 3]);
        let AverIntList::Packed(forwards) = built.reverse_owned() else {
            panic!("a stack reverses into octets");
        };
        assert_eq!(forwards.as_slice(), &[1, 2, 3]);
        // Every prepend onto an older value starts a buffer of its own.
        let mut chain = prepended(&AverIntList::empty(), &[0]);
        for _ in 0..200_000 {
            let _sibling = prepended(&chain, &[1]);
            chain = prepended(&chain, &[2]);
        }
        assert_eq!(chain.len(), 200_001);
        assert_eq!(chain.get(200_000), Some(AverInt::from_i64(0)));
        assert_eq!(chain.reverse().get(1), Some(AverInt::from_i64(2)));
        drop(chain);
    }

    #[test]
    fn get_and_owned_prepend_read_both_carriers() {
        let packed = AverIntList::from_vec(vec![AverInt::from_i64(3), AverInt::from_i64(4)]);
        assert!(matches!(packed, AverIntList::Packed(_)));
        assert_eq!(packed.get(1), Some(AverInt::from_i64(4)));
        assert_eq!(packed.drop_first(1).get(0), Some(AverInt::from_i64(4)));
        assert_eq!(packed.get(2), None);
        let wide = AverIntList::prepend_owned(AverInt::from_i64(300), packed);
        assert!(matches!(wide, AverIntList::Wide(_)));
        assert_eq!(wide.aver_display(), "[300, 3, 4]");
        assert_eq!(wide.get(0), Some(AverInt::from_i64(300)));
        assert_eq!(wide.get(2), Some(AverInt::from_i64(4)));
        assert_eq!(wide.get(3), None);
    }
}
