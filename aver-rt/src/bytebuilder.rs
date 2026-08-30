//! The bytes a fused collect-then-validate loop builds into.
//!
//! A loop that collects octets into a list only so `Bytes.fromList` can
//! walk the same elements a second time pays for the answer twice: once
//! to build the list, once to check every element against `0..=255` and
//! find the first one outside it. These three routines are that pair
//! fused: the range check rides every push, so the finalizer already
//! knows whether the collection is a byte sequence and which element
//! said otherwise first.
//!
//! The contract is exactly `Bytes.fromList` over the pushed elements,
//! in push order — including the error path. The first out-of-range
//! value wins, its index is how many elements were pushed before it,
//! and the message is the one the standard library's `fromList` builds:
//! `byte <value> at index <index> is outside 0..=255`, with the value
//! rendered the way Aver renders any `Int`. `Bytes.fromList` walks its
//! list head-first, and the push order IS that walk order, so "first
//! bad element" means the same thing on both paths — the differential
//! suite pins this rather than leaving it as an argument.
//!
//! Like its list sibling in [`crate::listbuilder`], the push writes in
//! place exactly when the builder is the only handle to its body — the
//! shape [`byte_builder_new`] produces and every push preserves — and
//! copies otherwise, which costs the walk it was avoiding but answers
//! the same.

use std::sync::Arc as Rc;

use crate::{AverInt, AverIntList, AverPackedU8, AverStr};

/// The collected bytes, plus the first element that was not a byte.
#[derive(Debug, Clone)]
struct ByteBuilderInner {
    /// Every in-range element pushed before the first out-of-range one,
    /// in push order, already narrowed to the octet it is.
    bytes: Vec<u8>,
    /// The first out-of-range element and its index, if one arrived.
    /// Sticky: pushes after it change nothing, because `fromList`
    /// reports the FIRST offender and nothing later can un-offend it.
    bad: Option<(AverInt, usize)>,
}

/// A byte sequence under construction. Cheap to clone (the body is
/// shared until a push needs to write), so the generated code can
/// thread it through a loop like any other value.
#[derive(Debug, Clone)]
pub struct ByteBuilder {
    inner: Rc<ByteBuilderInner>,
}

/// A fresh builder, with room for `capacity` bytes if the caller has a
/// useful guess and no harm done when it does not.
pub fn byte_builder_new(capacity: usize) -> ByteBuilder {
    ByteBuilder {
        inner: Rc::new(ByteBuilderInner {
            bytes: Vec::with_capacity(capacity),
            bad: None,
        }),
    }
}

/// Push `value`, consuming the builder and handing back the one that
/// holds it. In range and error-free: the byte is appended. Out of
/// range with no earlier offender: the value and its index are
/// recorded. After an offender: nothing, the first one already decided
/// the answer.
pub fn byte_builder_push(mut builder: ByteBuilder, value: AverInt) -> ByteBuilder {
    let inner = match Rc::get_mut(&mut builder.inner) {
        Some(unique) => unique,
        None => {
            builder.inner = Rc::new(builder.inner.as_ref().clone());
            Rc::get_mut(&mut builder.inner).expect("freshly cloned, so unique")
        }
    };
    if inner.bad.is_none() {
        match value.to_i64() {
            Some(byte) if (0..=255).contains(&byte) => inner.bytes.push(byte as u8),
            // Too big for i64 is certainly too big for a byte, so the
            // unrepresentable case is the same case.
            _ => inner.bad = Some((value, inner.bytes.len())),
        }
    }
    builder
}

/// What `Bytes.fromList` would answer for the pushed elements: the
/// collected list when every element was an octet, or the standard
/// library's own words about the first one that was not.
pub fn byte_builder_finalize(builder: ByteBuilder) -> Result<AverIntList, AverStr> {
    let inner = match Rc::try_unwrap(builder.inner) {
        Ok(owned) => owned,
        Err(shared) => shared.as_ref().clone(),
    };
    match inner.bad {
        Some((value, index)) => Err(AverStr::from(format!(
            "byte {value} at index {index} is outside 0..=255"
        ))),
        None => Ok(AverIntList::from_packed(AverPackedU8::from_vec(
            inner.bytes,
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn collect(values: &[i64]) -> Result<Vec<i64>, String> {
        let mut b = byte_builder_new(0);
        for v in values {
            b = byte_builder_push(b, AverInt::from_i64(*v));
        }
        byte_builder_finalize(b)
            .map(|list| list.to_vec().iter().map(|v| v.to_i64().unwrap()).collect())
            .map_err(|e| e.to_string())
    }

    /// The reference the builder replaces: `Bytes.fromList` over the
    /// prepend-then-reverse list, written out in Rust. Kept beside the
    /// builder so the two shapes are compared element for element.
    fn from_list_reference(values: &[i64]) -> Result<Vec<i64>, String> {
        match values.iter().position(|v| !(0..=255).contains(v)) {
            Some(index) => Err(format!(
                "byte {} at index {} is outside 0..=255",
                values[index], index
            )),
            None => Ok(values.to_vec()),
        }
    }

    /// The order the loop pushes in is the order the bytes read in.
    #[test]
    fn the_builder_reads_back_in_push_order() {
        let values: Vec<i64> = (0..300).map(|i| i % 256).collect();
        assert_eq!(collect(&values), Ok(values));
    }

    /// Bit for bit what `fromList` answers, error path included, for
    /// every placement of the offender.
    #[test]
    fn it_agrees_with_the_from_list_walk() {
        let cases: Vec<Vec<i64>> = vec![
            vec![],
            vec![0],
            vec![255],
            vec![256],
            vec![-1],
            vec![0, 127, 255],
            vec![0, 256, 300],
            vec![255, -7],
            vec![1, 2, 3, 999, 4, -5],
        ];
        for values in cases {
            assert_eq!(
                collect(&values),
                from_list_reference(&values),
                "values {values:?}"
            );
        }
    }

    /// The FIRST offender wins, and its index counts the elements
    /// pushed before it — later offenders change nothing.
    #[test]
    fn the_first_out_of_range_element_is_the_one_reported() {
        assert_eq!(
            collect(&[10, 20, 300, -1, 400]),
            Err("byte 300 at index 2 is outside 0..=255".to_string())
        );
    }

    /// A value outside `i64` is outside `0..=255`, and the message
    /// carries the whole value — the same digits Aver's `Int` renders.
    #[test]
    fn a_big_integer_is_reported_in_full() {
        use std::str::FromStr;
        let mut b = byte_builder_new(0);
        b = byte_builder_push(b, AverInt::from_i64(7));
        b = byte_builder_push(
            b,
            AverInt::from_str("340282366920938463463374607431768211456").unwrap(),
        );
        assert_eq!(
            byte_builder_finalize(b).map_err(|e| e.to_string()),
            Err(
                "byte 340282366920938463463374607431768211456 at index 1 is outside 0..=255"
                    .to_string()
            )
        );
    }

    /// A push whose builder is shared cannot write into the body the
    /// other holder is reading. It copies, and both answers stay right.
    #[test]
    fn a_shared_builder_is_copied_not_written_through() {
        let mut b = byte_builder_new(0);
        b = byte_builder_push(b, AverInt::from_i64(1));
        b = byte_builder_push(b, AverInt::from_i64(2));
        let kept = b.clone();
        let grown = byte_builder_push(b, AverInt::from_i64(3));
        assert_eq!(
            byte_builder_finalize(kept).map(|l| l.to_vec()),
            Ok(vec![AverInt::from_i64(1), AverInt::from_i64(2)])
        );
        assert_eq!(
            byte_builder_finalize(grown).map(|l| l.to_vec()),
            Ok(vec![
                AverInt::from_i64(1),
                AverInt::from_i64(2),
                AverInt::from_i64(3)
            ])
        );
    }
}
