//! The pointer a large record that its program copies is kept behind (see
//! `representation::is_counted_record` in the compiler): one heap node
//! holding the record's fields, with the count of its owners among them.
//! Copying the record copies the pointer and adds one to the count.
//!
//! The fields are a struct that holds its own [`Count`] (see [`Counts`]), so
//! the node is exactly a `Box` of that struct, and the pointer turns into
//! that `Box` and back without moving the fields (see [`Counted::into_box`]
//! and [`Counted::from_box`]). An update turns its record into the `Box`
//! once, which copies the fields only when another owner shares them, then
//! writes and moves fields through the `Box` as it does for a record that is
//! always boxed, and turns it back. A `Counted` is never written through.
//!
//! The count follows `Shared` (and `std::sync::Arc`'s strong count): atomic,
//! as a record crosses threads in `(a, b)!`, with the same orderings.

use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::atomic::{self, AtomicUsize, Ordering};

/// A soft limit on the count, as in `Shared`.
const MAX_REFCOUNT: usize = isize::MAX as usize;

/// The count of owners kept in a record's fields (see the module docs). It
/// is not a field of the record: two records are equal, ordered and hashed
/// without it, and a copy of the fields starts with a count of one.
pub struct Count(AtomicUsize);

impl Count {
    #[inline]
    pub const fn new() -> Self {
        Self(AtomicUsize::new(1))
    }
}

impl Default for Count {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for Count {
    /// A copy of the fields is a node of its own, with one owner.
    #[inline]
    fn clone(&self) -> Self {
        Self::new()
    }
}

impl PartialEq for Count {
    #[inline]
    fn eq(&self, _: &Self) -> bool {
        true
    }
}

impl Eq for Count {}

impl PartialOrd for Count {
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Count {
    #[inline]
    fn cmp(&self, _: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl Hash for Count {
    #[inline]
    fn hash<H: Hasher>(&self, _: &mut H) {}
}

impl fmt::Debug for Count {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("_")
    }
}

/// Fields that hold their own [`Count`].
///
/// # Safety
/// `count` returns the same `Count`, a field of `self`, every time.
pub unsafe trait Counts {
    fn count(&self) -> &Count;
}

/// A record's fields behind one counted pointer (see the module docs).
pub struct Counted<T: Counts> {
    ptr: NonNull<T>,
    /// Owns a `T` for the drop check, as `Shared` owns its node.
    phantom: PhantomData<T>,
}

// As `Shared`: sending a `Counted` shares the fields with the threads that
// keep a copy, and the last owner drops them on whichever thread it is.
unsafe impl<T: Counts + Sync + Send> Send for Counted<T> {}
unsafe impl<T: Counts + Sync + Send> Sync for Counted<T> {}

impl<T: Counts> Counted<T> {
    /// A new node holding `fields`, with a count of one.
    #[inline]
    pub fn new(fields: T) -> Self {
        Self::from_box(Box::new(fields))
    }

    /// The fields' only owner, made a `Counted` without moving them.
    #[inline]
    pub fn from_box(fields: Box<T>) -> Self {
        // Fields held by value always count one owner: they are built or
        // copied with a count of one (`Count::new`, `Count::clone`), and come
        // out of a node only from its only owner (`into_box`).
        debug_assert_eq!(fields.count().0.load(Ordering::Relaxed), 1);
        Self {
            ptr: NonNull::from(Box::leak(fields)),
            phantom: PhantomData,
        }
    }

    #[inline]
    fn count(&self) -> &AtomicUsize {
        &self.count_of().0
    }

    #[inline]
    fn count_of(&self) -> &Count {
        // The node lives as long as any `Counted` to it, and this is one.
        unsafe { self.ptr.as_ref() }.count()
    }

    /// The fields as a `Box` of their own: the node itself when this is
    /// their only owner, a copy otherwise. The `Acquire` load pairs with the
    /// `Release` decrement of every other owner's drop, as in
    /// `Shared::get_mut`.
    #[inline]
    pub fn into_box(this: Self) -> Box<T>
    where
        T: Clone,
    {
        if this.count().load(Ordering::Acquire) == 1 {
            let ptr = this.ptr;
            std::mem::forget(this);
            // This was the last owner: the node was a `Box` of `T`.
            return unsafe { Box::from_raw(ptr.as_ptr()) };
        }
        Self::copy_into_box(this)
    }

    /// [`Self::into_box`] for fields another owner shares: a copy, kept out
    /// of line so the check before it stays small where it is inlined.
    #[cold]
    #[inline(never)]
    fn copy_into_box(this: Self) -> Box<T>
    where
        T: Clone,
    {
        Box::new((*this).clone())
    }

    /// The fields, moved out when this is their only owner and copied
    /// otherwise.
    #[inline]
    pub fn into_inner(this: Self) -> T
    where
        T: Clone,
    {
        *Self::into_box(this)
    }

    /// Whether both point to the same node.
    #[inline]
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        this.ptr == other.ptr
    }
}

impl<T: Counts> Clone for Counted<T> {
    /// As `Shared::clone`.
    #[inline]
    fn clone(&self) -> Self {
        let old = self.count().fetch_add(1, Ordering::Relaxed);
        if old > MAX_REFCOUNT {
            std::process::abort();
        }
        Self {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T: Counts> Drop for Counted<T> {
    /// As `Shared::drop`.
    #[inline]
    fn drop(&mut self) {
        if self.count().fetch_sub(1, Ordering::Release) != 1 {
            return;
        }
        atomic::fence(Ordering::Acquire);
        drop(unsafe { Box::from_raw(self.ptr.as_ptr()) });
    }
}

impl<T: Counts> Deref for Counted<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        // The node lives as long as any `Counted` to it, and this is one.
        unsafe { self.ptr.as_ref() }
    }
}

impl<T: Counts + fmt::Debug> fmt::Debug for Counted<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: Counts + PartialEq> PartialEq for Counted<T> {
    /// Two copies of one record are equal without comparing their fields.
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        Self::ptr_eq(self, other) || **self == **other
    }
}

impl<T: Counts + Eq> Eq for Counted<T> {}

impl<T: Counts + PartialOrd> PartialOrd for Counted<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: Counts + Ord> Ord for Counted<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: Counts + Hash> Hash for Counted<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

#[cfg(test)]
mod tests {
    use super::{Count, Counted, Counts};

    #[derive(Clone, Debug, PartialEq)]
    struct F {
        count: Count,
        a: u64,
        b: Vec<u64>,
    }

    unsafe impl Counts for F {
        fn count(&self) -> &Count {
            &self.count
        }
    }

    fn f(a: u64) -> F {
        F {
            count: Count::new(),
            a,
            b: vec![a],
        }
    }

    #[test]
    fn an_only_owner_turns_into_its_box_in_place() {
        let a = Counted::new(f(1));
        let at = &*a as *const F;
        let mut boxed = Counted::into_box(a);
        assert_eq!(&*boxed as *const F, at);
        boxed.a = 7;
        let mut b = boxed.b;
        b.push(2);
        boxed.b = b;
        let a = Counted::from_box(boxed);
        assert_eq!(&*a as *const F, at);
        assert_eq!(a.a, 7);
        assert_eq!(a.b, vec![1, 2]);
    }

    #[test]
    fn a_shared_record_is_copied_into_its_box() {
        let a = Counted::new(f(1));
        let kept = a.clone();
        let mut boxed = Counted::into_box(a);
        boxed.a = 7;
        let a = Counted::from_box(boxed);
        assert_eq!(kept.a, 1);
        assert_eq!(a.a, 7);
        assert!(!Counted::ptr_eq(&a, &kept));
        // Both are only owners again.
        let at = &*kept as *const F;
        assert_eq!(&*Counted::into_box(kept) as *const F, at);
    }

    #[test]
    fn copies_are_equal_without_their_counts() {
        let a = Counted::new(f(3));
        let b = a.clone();
        assert_eq!(a, Counted::new(f(3)));
        assert_eq!(*a, *b);
        drop(a);
        assert_eq!(Counted::into_inner(b), f(3));
    }

    #[test]
    fn an_option_of_a_counted_record_is_one_word() {
        assert_eq!(
            std::mem::size_of::<Option<Counted<F>>>(),
            std::mem::size_of::<usize>()
        );
    }
}
