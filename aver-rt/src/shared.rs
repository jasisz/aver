//! The reference-counted box that holds a value of a user type, and the
//! nodes of the runtime's lists, vectors, maps and strings.
//!
//! [`Shared<T>`] is `std::sync::Arc<T>` without weak references: the node is
//! one atomic count followed by the value, so it costs 8 bytes of header, not
//! the 16 of an `Arc` (a strong and a weak count). Values never take a weak
//! reference; only the runtime's work engine does, and it keeps `Arc`. A
//! `Shared<str>` is one node holding the count and the bytes, as `Arc<str>`.
//!
//! The count is atomic because a value crosses threads in `(a, b)!`. Every
//! atomic operation and its ordering is the one `std::sync::Arc` uses for
//! its strong count (library/alloc/src/sync.rs), with the weak count's part
//! left out; each method says which `Arc` method it follows. `PartialEq`
//! compares the values and never the addresses: `Arc` skips the comparison
//! for the same allocation only when `T: Eq`, which needs specialization, so
//! a type that wants that shortcut takes it in its own `PartialEq`.
//!
//! The node is `#[repr(C)]` with the count first, so its header is the count
//! alone and a later layout can put a tag after it or keep a node in a
//! static.

use std::alloc::{self, Layout};
use std::borrow::Borrow;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::NonNull;
use std::sync::Arc;
use std::sync::atomic::{self, AtomicUsize, Ordering};

/// A soft limit on the count, as in `Arc`: past it, a clone aborts rather
/// than let the count wrap around (it would take `isize::MAX` leaked clones).
const MAX_REFCOUNT: usize = isize::MAX as usize;

/// The count a node kept in a static starts with. No owner frees it: the
/// last drop would have to bring it down from here to 0. It sits below
/// [`MAX_REFCOUNT`], so a clone or drop that does count it stays correct,
/// and far above any count of a heap node, which is how
/// [`Shared::is_static`] tells the two apart.
const STATIC_COUNT: usize = MAX_REFCOUNT / 4 * 3;

#[repr(C)]
struct Node<T: ?Sized> {
    count: AtomicUsize,
    value: T,
}

/// A string of `N` bytes kept in a static, laid out as the `Node<str>` of a
/// heap string, so a [`Shared<str>`] can point to it (see
/// [`Shared::from_static`]).
#[repr(C)]
pub struct StaticStr<const N: usize> {
    count: AtomicUsize,
    bytes: [u8; N],
}

impl<const N: usize> StaticStr<N> {
    pub const fn new(text: &str) -> Self {
        let text = text.as_bytes();
        assert!(text.len() == N);
        let mut bytes = [0u8; N];
        let mut i = 0;
        while i < N {
            bytes[i] = text[i];
            i += 1;
        }
        Self {
            count: AtomicUsize::new(STATIC_COUNT),
            bytes,
        }
    }
}

/// A thread-safe reference-counted pointer to a `T` (see the module docs).
pub struct Shared<T: ?Sized> {
    ptr: NonNull<Node<T>>,
    /// Owns a `Node<T>` for the drop check, as `Arc` does.
    phantom: PhantomData<Node<T>>,
}

// As `Arc`: sending a `Shared` shares the `T` with the threads that keep a
// clone, and the last owner drops it on whichever thread it is.
unsafe impl<T: ?Sized + Sync + Send> Send for Shared<T> {}
unsafe impl<T: ?Sized + Sync + Send> Sync for Shared<T> {}

impl<T> Shared<T> {
    /// A new node holding `value`, with a count of one.
    #[inline]
    pub fn new(value: T) -> Self {
        let node = Box::new(Node {
            count: AtomicUsize::new(1),
            value,
        });
        Self::from_node(NonNull::from(Box::leak(node)))
    }
}

impl<T: ?Sized> Shared<T> {
    #[inline]
    fn from_node(ptr: NonNull<Node<T>>) -> Self {
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    #[inline]
    fn node(&self) -> &Node<T> {
        // The node lives as long as any `Shared` to it, and this is one.
        unsafe { self.ptr.as_ref() }
    }

    /// The address of the value, as `Arc::as_ptr`.
    #[inline]
    pub fn as_ptr(this: &Self) -> *const T {
        let node = this.ptr.as_ptr();
        unsafe { &raw const (*node).value }
    }

    /// Whether both point to the same node, as `Arc::ptr_eq`.
    #[inline]
    pub fn ptr_eq(this: &Self, other: &Self) -> bool {
        std::ptr::addr_eq(this.ptr.as_ptr(), other.ptr.as_ptr())
    }

    /// How many `Shared` point to this node, as `Arc::strong_count` (a
    /// `Relaxed` load: another thread can change it at once).
    #[inline]
    pub fn strong_count(this: &Self) -> usize {
        this.node().count.load(Ordering::Relaxed)
    }

    /// Whether the node is kept in a static, so cloning or dropping this
    /// `Shared` needs no count. Nothing changes a static node's count by
    /// half of [`STATIC_COUNT`], and no heap node gets that many owners.
    #[inline]
    pub fn is_static(this: &Self) -> bool {
        this.node().count.load(Ordering::Relaxed) > STATIC_COUNT / 2
    }

    /// Whether this is the only `Shared` to its node. The `Acquire` load
    /// pairs with the `Release` decrement of every other owner's drop, so
    /// what they did with the value happens before what the caller does with
    /// it next. `Arc::is_unique` does the same load after locking out weak
    /// references, of which there are none here. No other thread can clone
    /// meanwhile: it would need a `Shared` to this node, and the caller holds
    /// the only one.
    #[inline]
    fn is_unique(this: &mut Self) -> bool {
        this.node().count.load(Ordering::Acquire) == 1
    }

    /// The value, mutably, when this is its only owner, as `Arc::get_mut`.
    #[inline]
    pub fn get_mut(this: &mut Self) -> Option<&mut T> {
        if Self::is_unique(this) {
            Some(unsafe { &mut (*this.ptr.as_ptr()).value })
        } else {
            None
        }
    }
}

impl<T> Shared<T> {
    /// The value, mutably, copied first into a node of its own when another
    /// owner shares it, as `Arc::make_mut`. `Arc` swaps the count to 0 here
    /// to keep a weak reference from upgrading meanwhile; with none, the
    /// unique check of `get_mut` is enough.
    #[inline]
    pub fn make_mut(this: &mut Self) -> &mut T
    where
        T: Clone,
    {
        if !Self::is_unique(this) {
            *this = Self::new((**this).clone());
        }
        unsafe { &mut (*this.ptr.as_ptr()).value }
    }

    /// The value, when this is its only owner, as `Arc::try_unwrap`; else
    /// the `Shared` back. `Arc` brings the count from 1 to 0 to keep a weak
    /// reference from upgrading meanwhile; with none, the unique check of
    /// `get_mut` is enough: no one else can read the count or make another
    /// owner from it, so the node is taken back without writing it.
    #[inline]
    pub fn try_unwrap(mut this: Self) -> Result<T, Self> {
        if !Self::is_unique(&mut this) {
            return Err(this);
        }
        let ptr = this.ptr;
        std::mem::forget(this);
        // This was the last owner: take the node back.
        let node = unsafe { Box::from_raw(ptr.as_ptr()) };
        Ok(node.value)
    }

    /// The value, moved out when this is its only owner and cloned
    /// otherwise, as `Arc::unwrap_or_clone`.
    #[inline]
    pub fn unwrap_or_clone(this: Self) -> T
    where
        T: Clone,
    {
        Self::try_unwrap(this).unwrap_or_else(|shared| (*shared).clone())
    }
}

impl<T: ?Sized> Clone for Shared<T> {
    /// As `Arc::clone`: a `Relaxed` increment, since a new owner is made
    /// only from an existing one, which keeps the node alive meanwhile.
    #[inline]
    fn clone(&self) -> Self {
        let old = self.node().count.fetch_add(1, Ordering::Relaxed);
        if old > MAX_REFCOUNT {
            std::process::abort();
        }
        Self::from_node(self.ptr)
    }
}

impl<T: ?Sized> Drop for Shared<T> {
    /// As `Arc::drop`: a `Release` decrement, so this owner's use of the
    /// value happens before the drop; the last owner's `Acquire` fence makes
    /// every other owner's use happen before it frees the node.
    #[inline]
    fn drop(&mut self) {
        if self.node().count.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }
        atomic::fence(Ordering::Acquire);
        drop(unsafe { Box::from_raw(self.ptr.as_ptr()) });
    }
}

impl<T: ?Sized> Deref for Shared<T> {
    type Target = T;

    #[inline]
    fn deref(&self) -> &T {
        &self.node().value
    }
}

impl<T: ?Sized> AsRef<T> for Shared<T> {
    fn as_ref(&self) -> &T {
        self
    }
}

impl<T: ?Sized> Borrow<T> for Shared<T> {
    fn borrow(&self) -> &T {
        self
    }
}

impl<T> From<T> for Shared<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

/// From hand-written code that holds an `Arc`: the value is moved when the
/// `Arc` is its only owner and cloned otherwise.
impl<T: Clone> From<Arc<T>> for Shared<T> {
    fn from(arc: Arc<T>) -> Self {
        Self::new(Arc::unwrap_or_clone(arc))
    }
}

/// To hand-written code that wants an `Arc`: the value is moved when this is
/// its only owner and cloned otherwise.
impl<T: Clone> From<Shared<T>> for Arc<T> {
    fn from(shared: Shared<T>) -> Self {
        Arc::new(Shared::unwrap_or_clone(shared))
    }
}

impl From<&str> for Shared<str> {
    /// As `Arc::from(&str)`: one node holding the count and a copy of the
    /// bytes.
    fn from(text: &str) -> Self {
        Self::from_parts(|| std::iter::once(text))
    }
}

/// The layout of the node of a `len`-byte string, laid out as `Node<str>`
/// so dropping it through a `Box` frees it with the layout it was allocated
/// with, and the offset of its bytes.
fn str_node_layout(len: usize) -> (Layout, usize) {
    let (layout, offset) = Layout::new::<AtomicUsize>()
        .extend(Layout::array::<u8>(len).expect("a str fits in isize"))
        .expect("a str fits in isize");
    (layout.pad_to_align(), offset)
}

impl Shared<str> {
    /// One node holding the count and the parts `parts()` walks, end to
    /// end: one allocation and one copy of each part, however many there
    /// are. `parts` is walked twice, once to size the node.
    pub fn from_parts<'a, I: Iterator<Item = &'a str>>(parts: impl Fn() -> I) -> Self {
        let len = parts().map(str::len).sum::<usize>();
        let (layout, offset) = str_node_layout(len);
        unsafe {
            let memory = alloc::alloc(layout);
            if memory.is_null() {
                alloc::handle_alloc_error(layout);
            }
            let mut written = 0;
            for part in parts() {
                // A second walk that differs from the first leaks the
                // memory rather than writing past it.
                assert!(part.len() <= len - written, "parts changed length");
                let at = memory.add(offset + written);
                std::ptr::copy_nonoverlapping(part.as_ptr(), at, part.len());
                written += part.len();
            }
            assert!(written == len, "parts changed length");
            (memory as *mut AtomicUsize).write(AtomicUsize::new(1));
            let node = std::ptr::slice_from_raw_parts_mut(memory, len) as *mut Node<str>;
            Self::from_node(NonNull::new_unchecked(node))
        }
    }

    /// Appends `tail` to the string in place when this is the only owner
    /// of a heap node: the node grows (`realloc`, which copies only when
    /// the allocator cannot extend it), so a string built by repeated
    /// appends costs what it holds, not the sum of every step. False, and
    /// nothing changes, when the node is shared or static. No other owner
    /// can see the change: there is none.
    pub fn append(this: &mut Self, tail: &str) -> bool {
        if !Self::is_unique(this) {
            return false;
        }
        let len = this.len();
        let (old, offset) = str_node_layout(len);
        let (new, _) = str_node_layout(len.checked_add(tail.len()).expect("a str fits in isize"));
        unsafe {
            let memory = alloc::realloc(this.ptr.as_ptr().cast::<u8>(), old, new.size());
            if memory.is_null() {
                alloc::handle_alloc_error(new);
            }
            std::ptr::copy_nonoverlapping(tail.as_ptr(), memory.add(offset + len), tail.len());
            let node =
                std::ptr::slice_from_raw_parts_mut(memory, len + tail.len()) as *mut Node<str>;
            this.ptr = NonNull::new_unchecked(node);
        }
        true
    }

    /// The string a static holds, with no allocation or copy. The node is
    /// never freed (see [`STATIC_COUNT`]).
    #[inline]
    pub fn from_static<const N: usize>(node: &'static StaticStr<N>) -> Self {
        let bytes = std::ptr::slice_from_raw_parts(node as *const StaticStr<N> as *const u8, N);
        // `StaticStr<N>` is `Node<str>` with `N` bytes of a `str`.
        Self::from_node(unsafe { NonNull::new_unchecked(bytes as *mut Node<str>) })
    }
}

impl<T: Default> Default for Shared<T> {
    fn default() -> Self {
        Self::new(T::default())
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for Shared<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for Shared<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Shared<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: ?Sized + Eq> Eq for Shared<T> {}

impl<T: ?Sized + PartialOrd> PartialOrd for Shared<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: ?Sized + Ord> Ord for Shared<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: ?Sized + Hash> Hash for Shared<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

#[cfg(test)]
mod tests {
    use super::Shared;
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// Counts its drops into a shared counter.
    #[derive(Clone)]
    struct Counted(Arc<AtomicUsize>, u64);

    impl Drop for Counted {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }
    }

    #[test]
    fn header_is_one_word() {
        assert_eq!(size_of::<Shared<u64>>(), size_of::<usize>());
        assert_eq!(size_of::<Option<Shared<u64>>>(), size_of::<usize>());
        assert_eq!(size_of::<super::Node<[u64; 4]>>(), size_of::<usize>() + 32);
    }

    #[test]
    fn clone_and_drop_count_owners_and_free_once() {
        let drops = Arc::new(AtomicUsize::new(0));
        let a = Shared::new(Counted(drops.clone(), 7));
        let b = a.clone();
        assert_eq!(Shared::strong_count(&a), 2);
        assert!(Shared::ptr_eq(&a, &b));
        assert_eq!(Shared::as_ptr(&a), &*b as *const Counted);
        drop(a);
        assert_eq!(Shared::strong_count(&b), 1);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(b);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn clones_dropped_on_many_threads_free_once() {
        let drops = Arc::new(AtomicUsize::new(0));
        let root = Shared::new(Counted(drops.clone(), 1));
        let threads = if cfg!(miri) { 4 } else { 8 };
        let rounds = if cfg!(miri) { 20 } else { 2000 };
        std::thread::scope(|scope| {
            for _ in 0..threads {
                let mine = root.clone();
                scope.spawn(move || {
                    let mut held = Vec::new();
                    for i in 0..rounds {
                        held.push(mine.clone());
                        if i % 3 == 0 {
                            held.clear();
                        }
                        assert_eq!(mine.1, 1);
                    }
                });
            }
        });
        assert_eq!(Shared::strong_count(&root), 1);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(root);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn last_owner_on_another_thread_frees() {
        let drops = Arc::new(AtomicUsize::new(0));
        let a = Shared::new(Counted(drops.clone(), 3));
        let b = a.clone();
        let other = std::thread::spawn(move || {
            assert_eq!(b.1, 3);
            drop(b);
        });
        drop(a);
        other.join().unwrap();
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn get_mut_only_when_unique() {
        let mut a = Shared::new(5u64);
        *Shared::get_mut(&mut a).unwrap() += 1;
        let b = a.clone();
        assert!(Shared::get_mut(&mut a).is_none());
        drop(b);
        assert_eq!(*Shared::get_mut(&mut a).unwrap(), 6);
    }

    #[test]
    fn make_mut_copies_a_shared_value_and_keeps_a_unique_one() {
        let mut a = Shared::new(vec![1, 2]);
        let before = Shared::as_ptr(&a);
        Shared::make_mut(&mut a).push(3);
        assert_eq!(Shared::as_ptr(&a), before);
        let b = a.clone();
        Shared::make_mut(&mut a).push(4);
        assert!(!Shared::ptr_eq(&a, &b));
        assert_eq!(*a, vec![1, 2, 3, 4]);
        assert_eq!(*b, vec![1, 2, 3]);
        assert_eq!(Shared::strong_count(&a), 1);
        assert_eq!(Shared::strong_count(&b), 1);
    }

    #[test]
    fn make_mut_after_other_threads_drop_their_clones() {
        let mut a = Shared::new(vec![0u64; 4]);
        std::thread::scope(|scope| {
            for _ in 0..4 {
                let mine = a.clone();
                scope.spawn(move || {
                    assert_eq!(mine.iter().sum::<u64>(), 0);
                });
            }
        });
        let before = Shared::as_ptr(&a);
        Shared::make_mut(&mut a)[0] = 9;
        assert_eq!(Shared::as_ptr(&a), before);
        assert_eq!(a[0], 9);
    }

    #[test]
    fn try_unwrap_moves_only_the_last_owner() {
        let drops = Arc::new(AtomicUsize::new(0));
        let a = Shared::new(Counted(drops.clone(), 4));
        let b = a.clone();
        let a = Shared::try_unwrap(a)
            .err()
            .expect("shared, so not unwrapped");
        drop(b);
        let value = Shared::try_unwrap(a).ok().expect("now the only owner");
        assert_eq!(value.1, 4);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(value);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn unwrap_or_clone_and_arc_conversions() {
        let a = Shared::new(String::from("x"));
        let b = a.clone();
        assert_eq!(Shared::unwrap_or_clone(a), "x");
        let arc: Arc<String> = b.into();
        let back: Shared<String> = arc.clone().into();
        assert_eq!(*back, "x");
        assert_eq!(Arc::strong_count(&arc), 1);
    }

    #[test]
    fn a_str_is_one_node() {
        assert_eq!(size_of::<Shared<str>>(), 2 * size_of::<usize>());
        for text in ["", "a", "seven b", "a longer text than one word"] {
            let a: Shared<str> = Shared::from(text);
            let b = a.clone();
            assert_eq!(&*a, text);
            assert!(Shared::ptr_eq(&a, &b));
            assert_eq!(Shared::strong_count(&a), 2);
            assert_eq!(a, Shared::from(text));
        }
        let texts: Vec<Shared<str>> = (0..64).map(|i| Shared::from(&*"x".repeat(i))).collect();
        std::thread::scope(|scope| {
            for _ in 0..4 {
                let mine = texts.clone();
                scope.spawn(move || assert_eq!(mine[63].len(), 63));
            }
        });
        assert!(texts.iter().all(|t| Shared::strong_count(t) == 1));
    }

    #[test]
    fn a_str_grows_in_place_only_with_one_owner() {
        let mut a: Shared<str> = Shared::from_parts(|| ["ab", "", "c"].into_iter());
        assert_eq!(&*a, "abc");
        for _ in 0..if cfg!(miri) { 40 } else { 4000 } {
            assert!(Shared::append(&mut a, "xyz"));
        }
        assert_eq!(a.len(), 3 + 3 * if cfg!(miri) { 40 } else { 4000 });
        assert!(a.starts_with("abcxyzxyz") && a.ends_with("xyzxyz"));
        let b = a.clone();
        assert!(!Shared::append(&mut a, "!"), "a shared node is not changed");
        assert_eq!(&*a, &*b);
        drop(b);
        assert!(Shared::append(&mut a, "!"));
        assert!(a.ends_with("xyz!"));
        static NODE: super::StaticStr<2> = super::StaticStr::new("hi");
        let mut c = Shared::<str>::from_static(&NODE);
        assert!(!Shared::append(&mut c, "!"), "a static node is not changed");
        assert_eq!(&*c, "hi");
        let mut empty = Shared::<str>::from("");
        assert!(Shared::append(&mut empty, "é"));
        assert_eq!(&*empty, "é");
    }

    #[test]
    fn a_static_str_is_never_freed() {
        static NODE: super::StaticStr<5> = super::StaticStr::new("hello");
        let a = Shared::<str>::from_static(&NODE);
        assert!(Shared::is_static(&a));
        assert!(!Shared::is_static(&Shared::<str>::from("hello")));
        assert_eq!(&*a, "hello");
        assert_eq!(a, Shared::from("hello"));
        // Counting it as a heap node is still correct, and frees nothing.
        let b = a.clone();
        drop(a);
        drop(b);
        assert_eq!(&*Shared::<str>::from_static(&NODE), "hello");
    }

    #[test]
    fn traits_delegate_to_the_value() {
        let nan = Shared::new(f64::NAN);
        assert!(nan != nan.clone(), "== compares values, not addresses");
        assert!(Shared::new(1) < Shared::new(2));
        assert_eq!(format!("{:?}", Shared::new("a")), "\"a\"");
    }
}
