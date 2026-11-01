//! Compact native carrier for proof-bounded `List<Int>` refinements.
//!
//! The compiler may use this representation only after its refinement
//! recognizer proves every element lies in `0..=255` and its construction
//! audit proves that the smart constructor cannot be bypassed. It is not an
//! alternative representation for an arbitrary Aver list.

use crate::shared::Shared as Rc;
use crate::{AverDisplay, AverInt, AverList};
use std::fmt;
use std::hash::{Hash, Hasher};

/// An immutable, cheaply cloned byte sequence with slice views.
#[derive(Clone)]
pub struct AverPackedU8 {
    /// None for the empty list, which allocates nothing.
    storage: Option<Rc<Vec<u8>>>,
    start: usize,
    end: usize,
}

impl AverPackedU8 {
    #[inline]
    pub fn from_vec(bytes: Vec<u8>) -> Self {
        let end = bytes.len();
        Self {
            storage: (end != 0).then(|| Rc::new(bytes)),
            start: 0,
            end,
        }
    }

    /// Pack a general `List<Int>`, rejecting a value outside the proven
    /// interval. Generated smart constructors have already established this
    /// condition; retaining the check makes a codegen mistake explicit rather
    /// than truncating it.
    pub fn try_from_aver_list(values: &AverList<AverInt>) -> Result<Self, String> {
        let mut bytes = Vec::with_capacity(values.len());
        for (index, value) in values.iter().enumerate() {
            let value = value
                .to_i64()
                .ok_or_else(|| format!("packed byte at index {index} is outside host Int"))?;
            let byte = u8::try_from(value)
                .map_err(|_| format!("packed byte {value} at index {index} is outside 0..=255"))?;
            bytes.push(byte);
        }
        Ok(Self::from_vec(bytes))
    }

    #[inline]
    pub fn as_slice(&self) -> &[u8] {
        self.storage
            .as_ref()
            .map_or(&[], |bytes| &bytes[self.start..self.end])
    }

    /// The octet at `position`, or `-1` at a negative position and at one at
    /// or past the length: the answer of the standard library's `Bytes.get`
    /// with `Option.None` as `-1`, which no octet is.
    #[inline]
    pub fn byte_at_i64(&self, position: i64) -> i64 {
        usize::try_from(position)
            .ok()
            .and_then(|position| self.as_slice().get(position))
            .map_or(-1, |&byte| i64::from(byte))
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.end - self.start
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub fn drop_first(&self, count: usize) -> Self {
        let start = self.start.saturating_add(count).min(self.end);
        Self {
            storage: self.storage.clone(),
            start,
            end: self.end,
        }
    }

    pub fn take_first(&self, count: usize) -> Self {
        let end = self.start.saturating_add(count).min(self.end);
        Self {
            storage: self.storage.clone(),
            start: self.start,
            end,
        }
    }

    pub fn to_aver_list(&self) -> AverList<AverInt> {
        AverList::from_vec(
            self.as_slice()
                .iter()
                .copied()
                .map(|byte| AverInt::from_i64(i64::from(byte)))
                .collect(),
        )
    }

    /// Expose the semantic `List<Int>` as a zero-copy hybrid list view.
    pub fn to_int_list(&self) -> crate::AverIntList {
        crate::AverIntList::from_packed(self.clone())
    }

    /// These bytes followed by `more`, from a carrier the caller hands over.
    /// A body this carrier alone holds, read to its end, grows in place, so
    /// a loop that keeps appending to one accumulator copies each piece once
    /// rather than the whole accumulator every step.
    pub fn extend_owned(mut self, more: &[u8]) -> Self {
        if more.is_empty() {
            return self;
        }
        if let Some(storage) = self.storage.as_mut()
            && self.end == storage.len()
            && let Some(bytes) = Rc::get_mut(storage)
        {
            bytes.extend_from_slice(more);
            self.end = bytes.len();
            return self;
        }
        let mut bytes = Vec::with_capacity(self.len() + more.len());
        bytes.extend_from_slice(self.as_slice());
        bytes.extend_from_slice(more);
        Self::from_vec(bytes)
    }

    pub fn into_vec(self) -> Vec<u8> {
        match self.storage {
            Some(storage) if self.start == 0 && self.end == storage.len() => {
                Rc::try_unwrap(storage).unwrap_or_else(|shared| shared.as_ref().clone())
            }
            _ => self.as_slice().to_vec(),
        }
    }
}

impl fmt::Debug for AverPackedU8 {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.as_slice()).finish()
    }
}

impl PartialEq for AverPackedU8 {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}

impl Eq for AverPackedU8 {}

impl PartialOrd for AverPackedU8 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for AverPackedU8 {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_slice().cmp(other.as_slice())
    }
}

impl Hash for AverPackedU8 {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // Match `AverList<AverInt>` exactly: the list tag, visible length,
        // then each canonical Small integer hashed as i64.
        8u8.hash(state);
        self.len().hash(state);
        for byte in self.as_slice() {
            i64::from(*byte).hash(state);
        }
    }
}

impl AverDisplay for AverPackedU8 {
    fn aver_display(&self) -> String {
        let mut out = String::from("[");
        for (index, byte) in self.as_slice().iter().enumerate() {
            if index != 0 {
                out.push_str(", ");
            }
            out.push_str(&byte.to_string());
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
    fn hash_matches_general_int_list() {
        let packed = AverPackedU8::from_vec(vec![0, 127, 255]);
        let general = packed.to_aver_list();
        let mut packed_hash = DefaultHasher::new();
        let mut general_hash = DefaultHasher::new();
        packed.hash(&mut packed_hash);
        general.hash(&mut general_hash);
        assert_eq!(packed_hash.finish(), general_hash.finish());
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
    fn rejects_out_of_range_general_values() {
        let values = AverList::from_vec(vec![AverInt::from_i64(0), AverInt::from_i64(256)]);
        assert!(AverPackedU8::try_from_aver_list(&values).is_err());
    }
}
