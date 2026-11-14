//! The body of a `Vector` and of a flat list: one reference-counted
//! allocation that holds its count, its length, its room and its elements.
//!
//! A [`Block<T>`] is what `Shared<Vec<T>>` was, a shared growable sequence,
//! in one allocation instead of two: the node of the `Shared` (count and the
//! `Vec`'s pointer, capacity and length) and the `Vec`'s buffer. A vector of
//! one element is one allocation, and reading an element follows one pointer,
//! not two. The pointer is one word, so `Option<Block<T>>` is one word too.
//!
//! The count changes as [`crate::shared::Shared`]'s does: the operations and
//! orderings of `Arc`'s strong count, through [`crate::counts`], so the
//! `plain-counts` build changes it with plain instructions here as well.
//! A block whose count is 1 is changed in place (`push_unique`,
//! `make_mut`, `drain_tail_into`); a shared one is copied first or not at all.

use std::alloc::{self, Layout};
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::ops::Deref;
use std::ptr::{self, NonNull};
use std::sync::atomic::{AtomicUsize, Ordering};

/// The front of the allocation; the elements follow it, aligned for `T`.
#[repr(C)]
struct Header {
    count: AtomicUsize,
    len: usize,
    cap: usize,
}

#[repr(transparent)]
pub(crate) struct Block<T> {
    ptr: NonNull<Header>,
    /// Owns `T`s for the drop check.
    phantom: PhantomData<T>,
}

/// One count of a block whose element type its holder keeps beside it
/// rather than in the type (the packed `List<Int>` widths,
/// [`crate::int_list::NarrowInts`]). The count is the same for every element
/// type, so copying and dropping one needs no type; reading the elements and
/// freeing the last one take the type the block was made with.
#[repr(transparent)]
pub(crate) struct Untyped(NonNull<Header>);

// As `Block`: the elements are integers, which any thread may read.
unsafe impl Send for Untyped {}
unsafe impl Sync for Untyped {}

impl Untyped {
    /// One more count of the same block.
    #[inline]
    pub(crate) fn share(&self) -> Self {
        // SAFETY: the allocation lives while this count does.
        if crate::counts::inc(unsafe { &self.0.as_ref().count }) {
            std::process::abort();
        }
        Self(self.0)
    }

    /// Gives up this count: true when it was the last one, and the caller
    /// must then free the block with [`Untyped::free_as`].
    #[inline]
    pub(crate) fn release(&self) -> bool {
        // SAFETY: the allocation lives while this count does.
        crate::counts::dec(unsafe { &self.0.as_ref().count })
    }

    /// The block, read as the `Block<T>` it was made as.
    ///
    /// # Safety
    /// It was made from a `Block<T>` ([`Block::into_untyped`]).
    #[inline]
    pub(crate) unsafe fn typed<T>(&self) -> &Block<T> {
        // SAFETY: both are one `NonNull<Header>` (`repr(transparent)`).
        unsafe { &*(self as *const Self as *const Block<T>) }
    }

    /// Frees a block whose last count [`Untyped::release`] gave up.
    ///
    /// # Safety
    /// It was made from a `Block<T>`, and its count is zero.
    pub(crate) unsafe fn free_as<T>(&self) {
        let mut block = std::mem::ManuallyDrop::new(Block::<T> {
            ptr: self.0,
            phantom: PhantomData,
        });
        block.drop_slow();
    }
}

// As `Shared`/`Arc`: sending a block shares its elements with the threads
// that keep a clone, and the last owner drops them on whichever thread it is.
unsafe impl<T: Sync + Send> Send for Block<T> {}
unsafe impl<T: Sync + Send> Sync for Block<T> {}

/// The layout of a block with room for `cap` elements, and the offset of
/// its first element.
#[inline]
fn layout<T>(cap: usize) -> (Layout, usize) {
    let (layout, offset) = Layout::new::<Header>()
        .extend(Layout::array::<T>(cap).expect("capacity overflow"))
        .expect("capacity overflow");
    (layout.pad_to_align(), offset)
}

impl<T> Block<T> {
    /// The offset of the elements, the same for every capacity.
    const OFFSET: usize = {
        let align = std::mem::align_of::<T>();
        let header = std::mem::size_of::<Header>();
        header.div_ceil(align) * align
    };

    /// This count of the block, its element type left to the holder.
    pub(crate) fn into_untyped(self) -> Untyped {
        let block = std::mem::ManuallyDrop::new(self);
        Untyped(block.ptr)
    }

    /// An empty block with room for `cap` elements.
    pub(crate) fn with_capacity(cap: usize) -> Self {
        let (layout, offset) = layout::<T>(cap);
        debug_assert_eq!(offset, Self::OFFSET);
        // SAFETY: the layout holds the header, so it is never zero-sized.
        let memory = unsafe { alloc::alloc(layout) };
        let Some(ptr) = NonNull::new(memory as *mut Header) else {
            alloc::handle_alloc_error(layout)
        };
        // SAFETY: fresh memory laid out for a header at its front.
        unsafe {
            ptr.as_ptr().write(Header {
                count: AtomicUsize::new(1),
                len: 0,
                cap,
            })
        };
        Self {
            ptr,
            phantom: PhantomData,
        }
    }

    #[inline]
    fn header(&self) -> &Header {
        // SAFETY: the allocation lives as long as any block to it.
        unsafe { self.ptr.as_ref() }
    }

    #[inline]
    fn data(&self) -> *mut T {
        // SAFETY: the elements start `OFFSET` bytes in, within the
        // allocation (at its end when there is no room).
        unsafe { (self.ptr.as_ptr() as *mut u8).add(Self::OFFSET) as *mut T }
    }

    #[inline]
    fn set_len(&mut self, len: usize) {
        // SAFETY: only an owner that holds the block alone calls this (or
        // the one building it), so nothing else reads the header meanwhile.
        unsafe { (*self.ptr.as_ptr()).len = len }
    }

    /// Writes `item` past the last element. The caller has made sure there
    /// is room and that no one else holds the block.
    #[inline]
    unsafe fn push_within(&mut self, item: T) {
        let len = self.header().len;
        debug_assert!(len < self.header().cap);
        unsafe { self.data().add(len).write(item) };
        self.set_len(len + 1);
    }

    /// The elements of `items`, moved into a block with the vector's room.
    /// One allocation and one copy of the bytes; the vector's buffer is
    /// freed.
    pub(crate) fn from_vec(mut items: Vec<T>) -> Self {
        let len = items.len();
        let block = Self::with_capacity(items.capacity());
        // SAFETY: room for `len` elements, which move over bitwise; the
        // vector forgets them before it frees its buffer.
        unsafe {
            // A short vector, the common one, moves item by item: a call
            // to `memcpy` costs more than the few words it copies.
            if len <= 4 {
                for i in 0..len {
                    block.data().add(i).write(items.as_ptr().add(i).read());
                }
            } else {
                ptr::copy_nonoverlapping(items.as_ptr(), block.data(), len);
            }
            items.set_len(0);
            (*block.ptr.as_ptr()).len = len;
        }
        block
    }

    /// The `len` items `items` yields, in a block of exactly that room.
    /// Fewer items leave a shorter block; more are not taken.
    pub(crate) fn from_iter_len(len: usize, items: impl IntoIterator<Item = T>) -> Self {
        let mut block = Self::with_capacity(len);
        for item in items.into_iter().take(len) {
            // SAFETY: at most `len` writes into room for `len`; the block is
            // new, so it is held alone. The length grows with each write, so
            // a panic in `items` drops what was written so far.
            unsafe { block.push_within(item) };
        }
        block
    }

    /// The items `items` yields, however many: room for the iterator's
    /// lower bound first, doubled when it runs out, so a list gathered from
    /// an iterator needs no vector in between.
    pub(crate) fn from_iter_grow(items: impl IntoIterator<Item = T>) -> Self {
        let items = items.into_iter();
        let mut block = Self::with_capacity(items.size_hint().0);
        for item in items {
            block.push_built(item);
        }
        block
    }

    /// Appends `item` to a block its builder holds alone (one it made
    /// itself, not yet shared), growing it when it has no room left.
    #[inline]
    pub(crate) fn push_built(&mut self, item: T) {
        debug_assert_eq!(Self::strong_count(self), 1);
        let (len, cap) = (self.header().len, self.header().cap);
        if len == cap {
            self.grow(cap.saturating_mul(2).max(4));
        }
        // SAFETY: room for one more; the builder holds the block alone.
        unsafe { self.push_within(item) };
    }

    /// `len` copies of `item`.
    pub(crate) fn filled(len: usize, item: T) -> Self
    where
        T: Clone,
    {
        if len == 0 {
            return Self::with_capacity(0);
        }
        let mut block = Self::with_capacity(len);
        for _ in 1..len {
            // SAFETY: `len - 1` writes, then the last below, into room for
            // `len`, in a new block.
            unsafe { block.push_within(item.clone()) };
        }
        unsafe { block.push_within(item) };
        block
    }

    /// How many blocks point to this allocation (a `Relaxed` load, as
    /// `Arc::strong_count`).
    #[inline]
    pub(crate) fn strong_count(this: &Self) -> usize {
        this.header().count.load(Ordering::Relaxed)
    }

    /// Whether this is the only block to its allocation. As
    /// `Shared::is_unique`: the `Acquire` load pairs with the `Release`
    /// decrement of every other owner's drop.
    #[inline]
    fn is_unique(&mut self) -> bool {
        self.header().count.load(Ordering::Acquire) == 1
    }

    /// The elements, mutably, copied first into a block of their own when
    /// another owner shares them, as `Arc::make_mut`.
    #[inline]
    pub(crate) fn make_mut(&mut self) -> &mut [T]
    where
        T: Clone,
    {
        if !self.is_unique() {
            *self = Self::from_iter_len(self.len(), self.iter().cloned());
        }
        let len = self.header().len;
        // SAFETY: held alone now.
        unsafe { std::slice::from_raw_parts_mut(self.data(), len) }
    }

    /// Appends `item` in place when this is the only owner, growing the
    /// allocation when it has no room left (`realloc`, which copies only
    /// when the allocator cannot extend it). The item back, and nothing
    /// changed, when the block is shared.
    #[inline]
    pub(crate) fn push_unique(&mut self, item: T) -> Result<(), T> {
        if !self.is_unique() {
            return Err(item);
        }
        let (len, cap) = (self.header().len, self.header().cap);
        if len == cap {
            self.grow(cap.saturating_mul(2).max(4));
        }
        // SAFETY: room for one more, held alone.
        unsafe { self.push_within(item) };
        Ok(())
    }

    /// Appends `more` in place when this is the only owner, growing the
    /// allocation once to at least twice its room when it has too little;
    /// false, and nothing changed, when the block is shared.
    pub(crate) fn extend_unique(&mut self, more: &[T]) -> bool
    where
        T: Copy,
    {
        if !self.is_unique() {
            return false;
        }
        let (len, cap) = (self.header().len, self.header().cap);
        if cap - len < more.len() {
            self.grow((len + more.len()).max(cap.saturating_mul(2)));
        }
        // SAFETY: room for `more` past the last element, held alone; `T` is
        // `Copy`, so the copies need no drop of their own.
        unsafe {
            ptr::copy_nonoverlapping(more.as_ptr(), self.data().add(len), more.len());
        }
        self.set_len(len + more.len());
        true
    }

    #[cold]
    #[inline(never)]
    fn grow(&mut self, cap: usize) {
        let old_cap = self.header().cap;
        let (old, _) = layout::<T>(old_cap);
        let (new, _) = layout::<T>(cap);
        // SAFETY: the allocation was made with `old`; the new size is that
        // of a layout with the same alignment. Held alone, so moving it
        // leaves no other pointer behind.
        let memory = unsafe { alloc::realloc(self.ptr.as_ptr() as *mut u8, old, new.size()) };
        let Some(ptr) = NonNull::new(memory as *mut Header) else {
            alloc::handle_alloc_error(new)
        };
        self.ptr = ptr;
        unsafe { (*ptr.as_ptr()).cap = cap };
    }

    /// Moves the elements from `start` on to the end of `out`, another block
    /// its builder holds alone, when this is the only owner; false, and
    /// nothing changed, when this block is shared.
    pub(crate) fn drain_tail_into_block(&mut self, start: usize, out: &mut Block<T>) -> bool {
        if !self.is_unique() {
            return false;
        }
        let len = self.header().len;
        let start = start.min(len);
        let moved = len - start;
        let (out_len, out_cap) = (out.header().len, out.header().cap);
        if out_cap - out_len < moved {
            out.grow(out_len + moved);
        }
        // SAFETY: both held alone; the moved elements leave this block (its
        // length drops to `start` first) and land in room just made.
        unsafe {
            self.set_len(start);
            ptr::copy_nonoverlapping(self.data().add(start), out.data().add(out_len), moved);
            out.set_len(out_len + moved);
        }
        true
    }

    /// The elements, mutably, of a block its builder holds alone (one it
    /// made itself, not yet shared). Shared, it is copied first.
    #[inline]
    pub(crate) fn as_mut_slice(&mut self) -> &mut [T]
    where
        T: Clone,
    {
        self.make_mut()
    }

    /// Moves the elements from `start` on to the end of `out` when this is
    /// the only owner, and keeps the ones before it; false, and nothing
    /// changed, when the block is shared.
    pub(crate) fn drain_tail_into(&mut self, start: usize, out: &mut Vec<T>) -> bool {
        if !self.is_unique() {
            return false;
        }
        let len = self.header().len;
        let start = start.min(len);
        let moved = len - start;
        out.reserve(moved);
        // SAFETY: held alone; the moved elements leave the block (its length
        // drops to `start` first) and land in room `reserve` made.
        unsafe {
            self.set_len(start);
            ptr::copy_nonoverlapping(
                self.data().add(start),
                out.as_mut_ptr().add(out.len()),
                moved,
            );
            out.set_len(out.len() + moved);
        }
        true
    }
}

#[cfg(not(feature = "plain-counts"))]
impl<T> Block<T> {
    /// Freezes the block once `item` froze each element (see
    /// `crate::freeze`).
    pub(crate) fn freeze_with(
        &self,
        f: &mut crate::freeze::Freezer,
        item: impl Fn(&T, &mut crate::freeze::Freezer) -> bool,
    ) -> bool {
        let bytes = Self::OFFSET + self.header().cap * size_of::<T>();
        f.node64(&self.header().count, bytes, |f| {
            self.iter().all(|x| item(x, f))
        })
    }
}

#[cfg(not(feature = "plain-counts"))]
impl Untyped {
    /// Freezes a block of integers of `bytes` bytes.
    pub(crate) fn freeze(&self, f: &mut crate::freeze::Freezer, bytes: usize) -> bool {
        // SAFETY: the allocation lives while this count does.
        f.node64(unsafe { &self.0.as_ref().count }, bytes, |_| true)
    }
}

impl<T> Deref for Block<T> {
    type Target = [T];

    #[inline]
    fn deref(&self) -> &[T] {
        // SAFETY: the first `len` elements are written and stay so while
        // anyone holds the block; only a sole owner changes them.
        unsafe { std::slice::from_raw_parts(self.data(), self.header().len) }
    }
}

impl<T> Clone for Block<T> {
    /// As `Arc::clone`: a `Relaxed` increment.
    #[inline]
    fn clone(&self) -> Self {
        if crate::counts::inc(&self.header().count) {
            std::process::abort();
        }
        Self {
            ptr: self.ptr,
            phantom: PhantomData,
        }
    }
}

impl<T> Drop for Block<T> {
    /// As `Arc::drop`; the last owner drops the elements and frees the
    /// allocation with the layout it was made with.
    #[inline]
    fn drop(&mut self) {
        if !crate::counts::dec(&self.header().count) {
            return;
        }
        self.drop_slow();
    }
}

impl<T> Block<T> {
    #[inline(never)]
    fn drop_slow(&mut self) {
        let (len, cap) = (self.header().len, self.header().cap);
        // SAFETY: the last owner: the `len` elements are dropped once, then
        // the allocation is freed with the layout of its room.
        unsafe {
            ptr::drop_in_place(ptr::slice_from_raw_parts_mut(self.data(), len));
            alloc::dealloc(self.ptr.as_ptr() as *mut u8, layout::<T>(cap).0);
        }
    }
}

impl<T: PartialEq> PartialEq for Block<T> {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Eq> Eq for Block<T> {}

/// As a `Vec<T>` hashes, so a value hashes as it did when it held one.
impl<T: Hash> Hash for Block<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

impl<T: fmt::Debug> fmt::Debug for Block<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

#[cfg(test)]
mod tests {
    use super::Block;
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

    fn counted(drops: &Arc<AtomicUsize>, n: u64) -> Counted {
        Counted(drops.clone(), n)
    }

    #[test]
    fn block_is_one_word_and_one_allocation() {
        assert_eq!(size_of::<Block<u64>>(), size_of::<usize>());
        assert_eq!(size_of::<Option<Block<u64>>>(), size_of::<usize>());
        assert_eq!(Block::<u8>::OFFSET, 3 * size_of::<usize>());
        #[repr(align(64))]
        struct Wide(#[allow(dead_code)] u8);
        assert_eq!(Block::<Wide>::OFFSET, 64);
        let wide = Block::from_iter_len(3, (0..3).map(Wide));
        assert_eq!(wide.as_ptr() as usize % 64, 0);
        assert_eq!(wide.len(), 3);
    }

    #[test]
    fn block_elements_drop_once_with_the_last_owner() {
        let drops = Arc::new(AtomicUsize::new(0));
        let a = Block::from_vec((0..5).map(|n| counted(&drops, n)).collect());
        let b = a.clone();
        assert_eq!(Block::strong_count(&a), 2);
        drop(a);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        assert_eq!(
            b.iter().map(|c| c.1).collect::<Vec<_>>(),
            vec![0, 1, 2, 3, 4]
        );
        drop(b);
        assert_eq!(drops.load(Ordering::SeqCst), 5);
    }

    #[test]
    fn block_from_vec_keeps_its_room_for_pushes() {
        let mut a = Block::from_vec(Vec::<u32>::with_capacity(3));
        let before = a.as_ptr();
        for i in 0..3 {
            assert!(a.push_unique(i).is_ok());
        }
        assert_eq!(a.as_ptr(), before, "within its room nothing moves");
        for i in 3..100 {
            assert!(a.push_unique(i).is_ok());
        }
        assert_eq!(&*a, &(0..100).collect::<Vec<_>>()[..]);
        let b = a.clone();
        assert_eq!(a.push_unique(7), Err(7), "a shared block is not changed");
        assert_eq!(a.len(), 100);
        drop(b);
        assert!(a.push_unique(100).is_ok());
        assert_eq!(a.len(), 101);
    }

    #[test]
    fn block_pushes_counted_items_across_growth() {
        let drops = Arc::new(AtomicUsize::new(0));
        let mut a = Block::with_capacity(0);
        for n in 0..37 {
            assert!(a.push_unique(counted(&drops, n)).is_ok());
        }
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        assert_eq!(a[36].1, 36);
        drop(a);
        assert_eq!(drops.load(Ordering::SeqCst), 37);
    }

    #[test]
    fn block_make_mut_copies_a_shared_block_and_keeps_a_unique_one() {
        let drops = Arc::new(AtomicUsize::new(0));
        let mut a = Block::filled(4, counted(&drops, 1));
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        let before = a.as_ptr();
        a.make_mut()[0] = counted(&drops, 9);
        assert_eq!(drops.load(Ordering::SeqCst), 1, "the replaced item");
        assert_eq!(a.as_ptr(), before);
        let b = a.clone();
        a.make_mut()[1] = counted(&drops, 8);
        assert_ne!(a.as_ptr(), b.as_ptr());
        assert_eq!(a.iter().map(|c| c.1).collect::<Vec<_>>(), vec![9, 8, 1, 1]);
        assert_eq!(b.iter().map(|c| c.1).collect::<Vec<_>>(), vec![9, 1, 1, 1]);
        assert_eq!(Block::strong_count(&a), 1);
        assert_eq!(Block::strong_count(&b), 1);
        drop(a);
        drop(b);
        assert_eq!(drops.load(Ordering::SeqCst), 2 + 8);
    }

    #[test]
    fn block_drain_moves_the_tail_and_keeps_the_head() {
        let drops = Arc::new(AtomicUsize::new(0));
        let mut a = Block::from_vec((0..6).map(|n| counted(&drops, n)).collect());
        let mut out = vec![counted(&drops, 100)];
        let b = a.clone();
        assert!(
            !a.drain_tail_into(2, &mut out),
            "a shared block keeps its items"
        );
        drop(b);
        assert!(a.drain_tail_into(2, &mut out));
        assert_eq!(
            out.iter().map(|c| c.1).collect::<Vec<_>>(),
            vec![100, 2, 3, 4, 5]
        );
        assert_eq!(a.len(), 2);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(a);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
        drop(out);
        assert_eq!(drops.load(Ordering::SeqCst), 7);
    }

    #[test]
    fn block_from_iter_len_takes_at_most_len() {
        let short = Block::from_iter_len(5, [1, 2]);
        assert_eq!(&*short, &[1, 2]);
        let long = Block::from_iter_len(2, [1, 2, 3]);
        assert_eq!(&*long, &[1, 2]);
        let empty = Block::<String>::filled(0, String::from("x"));
        assert!(empty.is_empty());
    }

    #[test]
    fn block_of_zero_sized_items() {
        let mut a = Block::filled(3, ());
        assert!(a.push_unique(()).is_ok());
        assert_eq!(a.len(), 4);
        let b = a.clone();
        a.make_mut()[0] = ();
        assert_eq!(a.len(), b.len());
    }

    #[test]
    fn block_compares_and_hashes_as_its_elements() {
        use std::hash::{BuildHasher, BuildHasherDefault, DefaultHasher};
        let a = Block::from_vec(vec![1u8, 2, 3]);
        let b = Block::from_iter_len(3, [1u8, 2, 3]);
        assert_eq!(a, b);
        let state = BuildHasherDefault::<DefaultHasher>::default();
        assert_eq!(state.hash_one(&a), state.hash_one(vec![1u8, 2, 3]));
        assert_eq!(format!("{a:?}"), "[1, 2, 3]");
    }

    #[cfg(not(feature = "plain-counts"))] // shares values between threads
    #[test]
    fn block_clones_dropped_on_many_threads_free_once() {
        let drops = Arc::new(AtomicUsize::new(0));
        let root = Block::from_vec(vec![counted(&drops, 1), counted(&drops, 2)]);
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
                        assert_eq!(mine[1].1, 2);
                    }
                });
            }
        });
        assert_eq!(Block::strong_count(&root), 1);
        drop(root);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
    }
}
