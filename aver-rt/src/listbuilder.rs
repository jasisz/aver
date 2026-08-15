//! The list a fused prepend-accumulator loop builds into.
//!
//! A loop that collects with `List.prepend(x, acc)` and reverses once on
//! the way out spends a cons cell per element and then walks every one of
//! them again. The elements arrive in the order the result wants them, so
//! the only reason for the reversal is the shape of the accumulator, not
//! the answer. These three routines are that accumulator with the shape
//! taken out: the loop appends, and the finalizer hands back what it
//! appended.
//!
//! The builder IS an [`AverList`] — a flat one, whose backing vector the
//! loop owns. That is what keeps the rewrite typeless: a `List<T>`
//! parameter stays a `List<T>` parameter, the synthesized variant has the
//! same signature as the function it was built from, and nothing
//! downstream has to learn a builder type.
//!
//! Ownership is what makes the push cheap, and the pass is what
//! guarantees it: the accumulator is read exactly once on every path, as
//! the thing being appended to, so the list handed to [`list_builder_push`]
//! is the only handle to its backing vector and the push lands in place.
//! When it is not — a caller that kept a copy, a shape this module is
//! reused for later — the push copies instead, which costs the walk it
//! was avoiding but answers the same.

use std::sync::Arc as Rc;

use crate::{AverList, AverListInner};

/// A fresh builder, with room for `capacity` elements if the caller has
/// a useful guess and no harm done when it does not.
pub fn list_builder_new<T>(capacity: usize) -> AverList<T> {
    AverList::from_vec(Vec::with_capacity(capacity))
}

/// Append `item`, consuming the builder and handing back the one that
/// holds it.
///
/// In place when this is the only handle to a flat body that starts at
/// its own beginning — the shape [`list_builder_new`] produces and every
/// push preserves. Otherwise a copy, which is what `AverList::append`
/// would have done anyway.
pub fn list_builder_push<T: Clone>(mut builder: AverList<T>, item: T) -> AverList<T> {
    if builder.start == 0
        && let Some(AverListInner::Flat { items }) = Rc::get_mut(&mut builder.inner)
        && let Some(vec) = Rc::get_mut(items)
    {
        vec.push(item);
        return builder;
    }
    let mut copied = builder.to_vec();
    copied.push(item);
    AverList::from_vec(copied)
}

/// The list the builder collected, in the order it was appended.
///
/// Identity: a flat builder already IS that list. It exists so the
/// rewrite has one name for "the accumulator stops being an accumulator
/// here" — the virtual machine, which cannot hold a growable list in its
/// arena without paying for it at every frame boundary, does real work
/// under this name.
pub fn list_builder_finalize<T>(builder: AverList<T>) -> AverList<T> {
    builder
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The order the loop appends in is the order the list reads in —
    /// the whole claim of the rewrite, since the shape it replaces
    /// prepends and reverses to get here.
    #[test]
    fn the_builder_reads_back_in_push_order() {
        let mut b = list_builder_new(0);
        for i in 0..64 {
            b = list_builder_push(b, i);
        }
        let list = list_builder_finalize(b);
        assert_eq!(list.to_vec(), (0..64).collect::<Vec<i32>>());
    }

    /// Prepending and reversing is the shape this replaces. It has to
    /// answer the same for every length, including none.
    #[test]
    fn it_agrees_with_prepend_then_reverse() {
        for n in [0usize, 1, 2, 7, 128, 300] {
            let mut prepended = AverList::empty();
            let mut built = list_builder_new(0);
            for i in 0..n {
                prepended = AverList::prepend(i, &prepended);
                built = list_builder_push(built, i);
            }
            assert_eq!(
                prepended.reverse().to_vec(),
                list_builder_finalize(built).to_vec(),
                "length {n}"
            );
        }
    }

    /// A push whose builder is shared cannot write into the body the
    /// other holder is reading. It copies, and both answers stay right.
    #[test]
    fn a_shared_builder_is_copied_not_written_through() {
        let mut b = list_builder_new(0);
        b = list_builder_push(b, 1);
        b = list_builder_push(b, 2);
        let kept = b.clone();
        let grown = list_builder_push(b, 3);
        assert_eq!(kept.to_vec(), vec![1, 2]);
        assert_eq!(grown.to_vec(), vec![1, 2, 3]);
    }

    /// A builder whose body starts past its own beginning is not the
    /// shape the in-place push is allowed on; it still has to append.
    #[test]
    fn a_list_that_starts_past_its_body_still_appends() {
        let list = AverList::from_vec(vec![1, 2, 3]);
        let stepped = list.tail().expect("non-empty");
        let grown = list_builder_push(stepped, 9);
        assert_eq!(grown.to_vec(), vec![2, 3, 9]);
    }
}
