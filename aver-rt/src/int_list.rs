//! Hybrid storage for Aver `List<Int>` values.
//!
//! A byte-refined carrier can expose its contents without eagerly inflating
//! every octet into an `AverInt`. Values outside 0..=255 use the ordinary
//! persistent list, so this changes representation, never semantics.

use std::cmp::Ordering;
use std::fmt;
use std::hash::{Hash, Hasher};

use crate::{AverDisplay, AverInt, AverList, AverPackedU8};

#[derive(Clone)]
pub enum AverIntList {
    Packed(AverPackedU8),
    Wide(AverList<AverInt>),
}

pub enum AverIntListIter<'a> {
    Packed(std::slice::Iter<'a, u8>),
    Wide(crate::AverListIter<'a, AverInt>),
}

impl Iterator for AverIntListIter<'_> {
    type Item = AverInt;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::Packed(iter) => iter.next().map(|byte| AverInt::from_i64(i64::from(*byte))),
            Self::Wide(iter) => iter.next().cloned(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            Self::Packed(iter) => iter.size_hint(),
            Self::Wide(iter) => iter.size_hint(),
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

    pub fn from_packed(values: AverPackedU8) -> Self {
        Self::Packed(values)
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
            Self::Wide(_) => None,
        }
    }

    pub fn len(&self) -> usize {
        match self {
            Self::Packed(values) => values.len(),
            Self::Wide(values) => values.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn iter_cloned(&self) -> AverIntListIter<'_> {
        match self {
            Self::Packed(values) => AverIntListIter::Packed(values.as_slice().iter()),
            Self::Wide(values) => AverIntListIter::Wide(values.iter()),
        }
    }

    pub fn to_vec(&self) -> Vec<AverInt> {
        self.iter_cloned().collect()
    }

    pub fn to_aver_list(&self) -> AverList<AverInt> {
        match self {
            Self::Packed(_) => AverList::from_vec(self.to_vec()),
            Self::Wide(values) => values.clone(),
        }
    }

    pub fn into_aver_list(self) -> AverList<AverInt> {
        match self {
            Self::Packed(values) => AverList::from_vec(
                values
                    .into_vec()
                    .into_iter()
                    .map(|byte| AverInt::from_i64(i64::from(byte)))
                    .collect(),
            ),
            Self::Wide(values) => values,
        }
    }

    pub fn into_packed(self) -> Result<AverPackedU8, String> {
        match self {
            Self::Packed(values) => Ok(values),
            Self::Wide(values) => AverPackedU8::try_from_aver_list(&values),
        }
    }

    pub fn uncons_cloned(&self) -> Option<(AverInt, Self)> {
        match self {
            Self::Packed(values) => values.as_slice().first().map(|byte| {
                (
                    AverInt::from_i64(i64::from(*byte)),
                    Self::Packed(values.drop_first(1)),
                )
            }),
            Self::Wide(values) => {
                crate::list_uncons_cloned(values).map(|(head, tail)| (head, Self::Wide(tail)))
            }
        }
    }

    pub fn drop_first(&self, count: usize) -> Self {
        match self {
            Self::Packed(values) => Self::Packed(values.drop_first(count)),
            Self::Wide(values) => Self::Wide(values.drop_first(count)),
        }
    }

    pub fn take_first(&self, count: usize) -> Self {
        match self {
            Self::Packed(values) => Self::Packed(values.take_first(count)),
            Self::Wide(values) => Self::from_vec(values.iter().take(count).cloned().collect()),
        }
    }

    pub fn prepend(head: AverInt, tail: &Self) -> Self {
        // Widen once at a persistent-list mutation boundary. Subsequent
        // accumulator prepends stay O(1), including when `head` is not a byte.
        Self::Wide(AverList::prepend(head, &tail.to_aver_list()))
    }

    pub fn concat(left: &Self, right: &Self) -> Self {
        match (left, right) {
            (Self::Packed(left), Self::Packed(right)) => {
                let mut bytes = Vec::with_capacity(left.len() + right.len());
                bytes.extend_from_slice(left.as_slice());
                bytes.extend_from_slice(right.as_slice());
                Self::Packed(AverPackedU8::from_vec(bytes))
            }
            _ => Self::Wide(AverList::concat(
                &left.to_aver_list(),
                &right.to_aver_list(),
            )),
        }
    }

    pub fn reverse(&self) -> Self {
        match self {
            Self::Packed(values) => {
                let mut bytes = values.as_slice().to_vec();
                bytes.reverse();
                Self::Packed(AverPackedU8::from_vec(bytes))
            }
            Self::Wide(values) => Self::from_vec(values.reverse().to_vec()),
        }
    }

    pub fn contains(&self, needle: &AverInt) -> bool {
        match self {
            Self::Packed(values) => needle
                .to_u32()
                .and_then(|value| u8::try_from(value).ok())
                .is_some_and(|byte| values.as_slice().contains(&byte)),
            Self::Wide(values) => values.contains(needle),
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
        self.len() == other.len() && self.iter_cloned().eq(other.iter_cloned())
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
        self.iter_cloned().cmp(other.iter_cloned())
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

pub trait IntoPackedU8 {
    fn into_packed_u8(self) -> Result<AverPackedU8, String>;
}

impl IntoPackedU8 for AverIntList {
    fn into_packed_u8(self) -> Result<AverPackedU8, String> {
        self.into_packed()
    }
}

impl IntoPackedU8 for AverList<AverInt> {
    fn into_packed_u8(self) -> Result<AverPackedU8, String> {
        AverPackedU8::try_from_aver_list(&self)
    }
}

pub fn into_packed_u8<T: IntoPackedU8>(values: T) -> Result<AverPackedU8, String> {
    values.into_packed_u8()
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
}
