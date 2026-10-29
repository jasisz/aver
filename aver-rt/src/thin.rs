//! A value of a recursive or a wide sum as one pointer.
//!
//! The Rust backend keeps a value of a recursive sum (a sum a value of which
//! can hold a value of the same type with no collection in between) as a
//! [`Thin`]: one pointer to a node that holds a header and then the fields of
//! THAT constructor only. The header is a 32-bit atomic count and the 32-bit
//! tag of the constructor, so a value is 8 bytes, an `Option` of one is 8
//! bytes too, and a node is 8 bytes plus its own fields rather than the room
//! of the largest constructor. A sum one constructor of which has wide
//! fields is kept the same way, so that copying a value copies a pointer.
//!
//! A constructor without fields has no node. Its value is a pointer whose
//! address is the tag plus one, never read through: [`Thin::tag`],
//! `clone` and `drop` tell it apart from a node by that address alone, so
//! such a value costs no allocation and no atomic operation.
//!
//! All the unsafe code is here and in the expansion of [`thin_sum!`], which
//! pairs tag `i` with the `i`-th constructor's field struct by construction
//! and checks the tags at compile time. Generated code calls only the safe
//! functions and constants the macro defines: one per constructor to build a
//! value, `view` to match on one, `view_pair` to compare two.
//!
//! A sum some `lazy f(…)` of the program returns has one more, hidden
//! constructor: the call, not yet made (see [`Thin::deferred`]). Every way of
//! looking at a value (`view`, `view_pair`, `tag`, and through them `==`,
//! hashing and printing) makes the call first, once, and from then on looks
//! at what it returned, so no code can tell such a value from the one the
//! call returns. Making the call writes what it returned into the call's own
//! node, which from then on is a node of that constructor (see [`Lazy`]).
//!
//! The atomic operations and their orderings are those `Shared` uses, which
//! are `std::sync::Arc`'s strong count (library/alloc/src/sync.rs). The count
//! is 32 bits: a clone past `i32::MAX` aborts, as `Arc` does past
//! `isize::MAX`; reaching it takes two billion owners of one node, 16 GB of
//! pointers to it alone.

use std::alloc::Layout;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ptr::NonNull;
use std::sync::atomic::{self, AtomicU32, Ordering};
use std::sync::{Condvar, Mutex, PoisonError};

/// Past it, a clone aborts rather than let the count wrap around.
const MAX_REFCOUNT: u32 = i32::MAX as u32;

/// The most constructors a sum laid out this way can have. A value of a
/// constructor without fields is a pointer to address `tag + 1`, so every
/// such address is at most `MAX_TAGS`, below any address an allocation gets.
pub const MAX_TAGS: usize = 256;

/// The header at the start of a node: the count, then the constructor's tag,
/// which never changes after the node is made, except in a node a deferred
/// call was made in (see [`Lazy`]).
#[repr(C)]
pub struct Header {
    count: AtomicU32,
    tag: AtomicU32,
}

#[repr(C)]
struct Node<P> {
    header: Header,
    fields: P,
}

/// What [`thin_sum!`] implements for a sum.
///
/// # Safety
/// `free` drops the node as the field struct of the constructor `tag` names,
/// and every node of the sum is made by [`Thin::new`] with that pairing.
pub unsafe trait ThinSum: Sized + 'static {
    /// The field structs, so that `Thin<Self>` is `Send` and `Sync` only
    /// when they all are.
    type Fields;

    /// Frees a node of constructor `tag` whose count reached 0: hands its
    /// fields over to `pending` (see [`HandOver`]) and gives back its memory.
    ///
    /// # Safety
    /// The node was made by `Thin::new` for constructor `tag`, and no owner
    /// is left.
    unsafe fn free(node: NonNull<Header>, tag: u32, pending: Pending<'_>);

    /// Whether some constructor has no fields, so that a value may be one
    /// without a node. When none has, every value is a node, and the check
    /// for a constructor without fields before each clone, drop and view
    /// folds away.
    const HAS_EMPTY: bool = true;

    /// The value's pointer, for the code of a deferred call.
    fn into_thin(self) -> Thin<Self>;

    /// For a sum some `lazy f(…)` returns: the most bytes a node of any of
    /// its constructors takes, which a node a call was deferred in has room
    /// for (see [`Lazy`]).
    const ROOM: usize = 0;

    /// For a sum some `lazy f(…)` returns: moves the fields of `value`, a
    /// node of a declared constructor, into the node `room` a call was
    /// deferred in (copies them if `value` has other owners), and gives
    /// back the constructor's tag.
    ///
    /// # Safety
    /// `room` is a node made by [`Thin::deferred`] whose call this thread
    /// is making, and `value` is a node of a declared constructor.
    unsafe fn fill(_room: NonNull<Header>, _value: Thin<Self>) -> u32 {
        unreachable!("only a sum some `lazy f(…)` returns has deferred calls")
    }

    /// For a sum some `lazy f(…)` returns: hands over the fields of the
    /// node a call was made in, now of the declared constructor `tag`.
    ///
    /// # Safety
    /// As [`ThinSum::free`]; the caller gives back the memory.
    unsafe fn hand_over_fields(_node: NonNull<Header>, _tag: u32, _pending: Pending<'_>) {
        unreachable!("only a sum some `lazy f(…)` returns has deferred calls")
    }
}

/// One value of the recursive sum `S` (see the module docs).
pub struct Thin<S: ThinSum> {
    ptr: NonNull<Header>,
    /// Owns the fields for the drop check.
    phantom: PhantomData<S::Fields>,
}

// As `Shared`: sending a value shares its fields with the threads that keep a
// clone, and the last owner drops them on whichever thread it is.
unsafe impl<S: ThinSum> Send for Thin<S> where S::Fields: Send + Sync {}
unsafe impl<S: ThinSum> Sync for Thin<S> where S::Fields: Send + Sync {}

impl<S: ThinSum> Thin<S> {
    #[inline]
    fn from_ptr(ptr: NonNull<Header>) -> Self {
        Thin {
            ptr,
            phantom: PhantomData,
        }
    }

    /// Whether this is a constructor without fields, which has no node.
    #[inline]
    fn is_empty_ctor(&self) -> bool {
        S::HAS_EMPTY && self.ptr.as_ptr().addr() <= MAX_TAGS
    }

    /// A new node of constructor `tag` holding `fields`, with a count of 1.
    ///
    /// # Safety
    /// `P` is the field struct of constructor `tag` of `S`.
    #[inline]
    pub unsafe fn new<P>(tag: u32, fields: P) -> Self {
        let node = Box::new(Node {
            header: Header {
                count: AtomicU32::new(1),
                tag: AtomicU32::new(tag),
            },
            fields,
        });
        let ptr = NonNull::from(Box::leak(node)).cast::<Header>();
        // An allocation never sits in the first page; were one to, a node
        // would read as a constructor without fields. A sum without such a
        // constructor never reads a node as one, so it has nothing to check.
        if S::HAS_EMPTY {
            assert!(ptr.as_ptr().addr() > MAX_TAGS);
        }
        Self::from_ptr(ptr)
    }

    /// The value of constructor `tag`, which has no fields.
    ///
    /// # Safety
    /// Constructor `tag` of `S` has no fields, and `tag < MAX_TAGS`.
    #[inline]
    pub const unsafe fn empty(tag: u32) -> Self {
        let address = tag as usize + 1;
        Thin {
            ptr: unsafe { NonNull::new_unchecked(std::ptr::without_provenance_mut(address)) },
            phantom: PhantomData,
        }
    }

    #[inline]
    fn header(&self) -> &Header {
        // Only called on a node, which lives as long as any owner of it.
        unsafe { self.ptr.as_ref() }
    }

    /// The tag of the value's constructor.
    #[inline]
    pub fn tag(&self) -> u32 {
        if self.is_empty_ctor() {
            (self.ptr.as_ptr().addr() - 1) as u32
        } else {
            self.header().tag.load(Ordering::Relaxed)
        }
    }

    /// The fields of the value's constructor.
    ///
    /// # Safety
    /// `self.tag()` names a constructor with fields, and `P` is its struct.
    #[inline]
    pub unsafe fn fields<P>(&self) -> &P {
        unsafe { &(*self.ptr.cast::<Node<P>>().as_ptr()).fields }
    }

    /// Whether both are the same node, or the same constructor without
    /// fields.
    #[inline]
    pub fn ptr_eq(a: &Self, b: &Self) -> bool {
        a.ptr == b.ptr
    }

    /// The address of the node, which identifies it while it lives.
    #[inline]
    pub fn addr(this: &Self) -> usize {
        this.ptr.as_ptr().addr()
    }

    /// How many values point to this node, as `Arc::strong_count` (a
    /// `Relaxed` load). A constructor without fields has no node and no
    /// count; it reads as `usize::MAX`, owned everywhere.
    #[inline]
    pub fn strong_count(this: &Self) -> usize {
        if this.is_empty_ctor() {
            usize::MAX
        } else {
            this.header().count.load(Ordering::Relaxed) as usize
        }
    }

    /// For [`ThinSum::free`]: frees a node made by `new::<P>` and hands its
    /// fields over to `pending`.
    ///
    /// # Safety
    /// The node was made by `new::<P>` and its count is 0.
    #[inline]
    pub unsafe fn free_node<P: HandOver>(node: NonNull<Header>, pending: Pending<'_>) {
        let node = node.cast::<Node<P>>().as_ptr();
        // The fields are handed over before the memory is given back, so they
        // are read from the node where they are used rather than first copied
        // out of it. SAFETY: they are moved out once, and the box is then
        // freed without dropping them.
        unsafe { std::ptr::read(&raw const (*node).fields) }.hand_over(pending);
        drop(unsafe { Box::from_raw(node.cast::<ManuallyDrop<Node<P>>>()) });
    }

    /// The last owner's part of `drop`, kept out of line as
    /// `Arc::drop_slow` is: the `Acquire` fence makes every other owner's
    /// use of the fields, and a call made in the node, happen before they
    /// are dropped.
    #[cold]
    #[inline(never)]
    fn drop_slow(&mut self) {
        atomic::fence(Ordering::Acquire);
        let tag = self.header().tag.load(Ordering::Relaxed);
        let mut put_aside = Vec::new();
        unsafe { S::free(self.ptr, tag, Pending::new(&mut put_aside)) };
        while let Some((node, release)) = put_aside.pop() {
            unsafe { release(node, Pending::new(&mut put_aside)) };
        }
    }
}

/// How many frees of nodes run inside one another before [`Pending`] puts
/// the next owner aside.
const NESTED_FREES: u32 = 64;

/// One owner of a node, and how to drop it: [`release`] for the node's sum.
type Owner = (NonNull<Header>, unsafe fn(NonNull<Header>, Pending<'_>));

/// Drops one owner of a node of `S`, and frees the node if it was the last.
///
/// # Safety
/// `node` is a node of `S`, and the owner is the caller's to drop.
#[inline]
unsafe fn release<S: ThinSum>(node: NonNull<Header>, pending: Pending<'_>) {
    let header = unsafe { node.as_ref() };
    if header.count.fetch_sub(1, Ordering::Release) != 1 {
        return;
    }
    atomic::fence(Ordering::Acquire);
    unsafe { S::free(node, header.tag.load(Ordering::Relaxed), pending) }
}

/// How the drop of a value frees its nodes.
///
/// Dropping a node's fields as they are would free a child no one else owns
/// inside its parent's free, and its child inside that, so a chain a million
/// nodes deep would take a million frames of stack. Instead a free hands
/// its children over to the `Pending` it was given, which frees them inside
/// it only up to [`NESTED_FREES`] deep; past that it puts them aside on a
/// list of the drop that began it, which frees them one at a time once the
/// first free returns. Dropping a shallow value puts nothing aside and
/// allocates nothing. It is passed by value, the depth in a register.
#[derive(Clone, Copy)]
pub struct Pending<'a> {
    depth: u32,
    put_aside: NonNull<Vec<Owner>>,
    /// Borrows the list for as long as the free runs.
    phantom: PhantomData<&'a mut Vec<Owner>>,
}

impl<'a> Pending<'a> {
    #[inline]
    fn new(put_aside: &'a mut Vec<Owner>) -> Self {
        Pending {
            depth: 0,
            put_aside: NonNull::from(put_aside),
            phantom: PhantomData,
        }
    }

    /// Takes `value`, to drop now or once the first free returns.
    #[inline]
    pub fn take<S: ThinSum>(self, value: Thin<S>) {
        let value = ManuallyDrop::new(value);
        if value.is_empty_ctor() {
            return;
        }
        if self.depth == NESTED_FREES {
            // The list is borrowed for `'a` and only through a `Pending`,
            // all of them on this thread, none of them using it meanwhile.
            unsafe { (*self.put_aside.as_ptr()).push((value.ptr, release::<S>)) };
            return;
        }
        let deeper = Pending {
            depth: self.depth + 1,
            ..self
        };
        unsafe { release::<S>(value.ptr, deeper) };
    }
}

/// What a node's fields do when it is freed (see [`Pending`]): a value of a
/// recursive sum hands its node over, an `Option` and a record on the cycle
/// of a recursive sum hand over what they hold, and every other field is
/// dropped as it is, which the default does. [`thin_sum!`] implements it for
/// the sum; the Rust backend for each constructor's field struct and each
/// such record.
pub trait HandOver: Sized {
    #[inline]
    fn hand_over(self, _pending: Pending<'_>) {}
}

impl<T: HandOver> HandOver for Option<T> {
    #[inline]
    fn hand_over(self, pending: Pending<'_>) {
        if let Some(value) = self {
            value.hand_over(pending);
        }
    }
}

/// The start of the node of the hidden constructor of a sum some `lazy f(…)`
/// returns (see the module docs): the call, not yet made. The arguments and
/// the function follow it in the node ([`Deferred`]), of types only `calls`
/// knows.
///
/// The node has room for a node of any constructor of the sum
/// ([`ThinSum::ROOM`]) and keeps its size in words in its tag, above
/// [`WORDS_SHIFT`]. The first look at the value makes the call, on whichever
/// thread looks first, and writes what the call returned over the call, in
/// the same node: from then on the node is one of that constructor, read as
/// any other, and its tag, written last with `Release`, says which. Until
/// then the tag is the hidden constructor's, and says besides:
///
/// - [`BUSY`]: a thread is making the call. Another one that looks waits for
///   it ([`WAITED`]).
/// - [`FAILED`]: the call panicked, having used up its arguments. A later
///   look panics too, as one at a poisoned `std::sync::Once` does.
/// - [`FORWARD`]: the call returned a deferred call someone else also owns.
///   The node holds that value and stands for it.
///
/// A call that returns a node no one else owns moves that node's fields in
/// and frees it; one that returns a node with other owners copies its
/// fields, each one more owner of what it holds. A call that returns a
/// deferred call no one else owns makes that call in its place, and so on,
/// one after another rather than one inside another.
#[repr(C)]
struct Lazy<S: 'static> {
    calls: &'static Calls<S>,
}

/// What a node of one [`Deferred`] type does with its call.
struct Calls<S> {
    /// Makes the call, using up its arguments; when told to, first gives
    /// back the node's memory.
    make: unsafe fn(NonNull<Header>, bool) -> S,
    /// Hands over the arguments of a call never made.
    drop_args: unsafe fn(NonNull<Header>, Pending<'_>),
}

/// The call: `K` is `()`, or [`Known`] for a call that knows a value, kept
/// in the node past this and past the room for what the call returns.
#[repr(C)]
struct Deferred<S: 'static, A, F, K> {
    lazy: Lazy<S>,
    /// Moved out by the call.
    args: ManuallyDrop<A>,
    run: F,
    known: PhantomData<K>,
}

/// What a deferred call made by [`Thin::deferred_knowing`] knows of what
/// it will return: the value of the projection `projection` of it, which a
/// law gives (see `docs/language.md`, "Deferred calls that know a value").
/// It takes the last [`KNOWN_WORDS`] words of the node, which nothing writes
/// over, so a thread reads it while another one makes the call.
#[derive(Clone, Copy)]
#[repr(C)]
struct Known {
    projection: u32,
    value: i64,
}

const KNOWN_WORDS: u32 = (size_of::<Known>() / 8) as u32;

/// Where a node of `words` words made by [`Thin::deferred_knowing`] keeps
/// its [`Known`].
///
/// # Safety
/// `node` is such a node.
unsafe fn known_at(node: NonNull<Header>, words: u32) -> NonNull<Known> {
    unsafe {
        node.cast::<u64>()
            .add((words - KNOWN_WORDS) as usize)
            .cast()
    }
}

/// The constructor's part of the tag of a node a call was deferred in.
pub const CTOR: u32 = 0xff;
const BUSY: u32 = 1 << 8;
const WAITED: u32 = 1 << 9;
const FAILED: u32 = 1 << 10;
const FORWARD: u32 = 1 << 11;
/// The node was made by [`Thin::deferred_knowing`]: it ends in a [`Known`].
const KNOWS: u32 = 1 << 12;
/// Where the size in 8-byte words of a node a call was deferred in starts in
/// its tag. Every other node has 0 there.
pub const WORDS_SHIFT: u32 = 16;

/// Where a thread waits for a call another one is making.
static WAITING: Mutex<()> = Mutex::new(());
static MADE: Condvar = Condvar::new();

/// The size of a node of a constructor with fields `P`, for
/// [`ThinSum::ROOM`].
pub const fn node_size<P>() -> usize {
    assert!(
        align_of::<Node<P>>() <= 8,
        "a sum some `lazy f(…)` returns holds no field aligned past 8 bytes"
    );
    size_of::<Node<P>>()
}

/// Gives back the memory of a node made by [`Thin::deferred`].
///
/// # Safety
/// The node was made by `Thin::deferred` with `words` words, and nothing
/// reads it any more.
unsafe fn free_room(node: NonNull<Header>, words: u32) {
    let layout = unsafe { Layout::from_size_align_unchecked(words as usize * 8, 8) };
    unsafe { std::alloc::dealloc(node.as_ptr().cast(), layout) }
}

impl<S: ThinSum + HandOver, A: HandOver, F: FnOnce(A) -> S + Copy, K> Deferred<S, A, F, K> {
    const CALLS: &'static Calls<S> = &Calls {
        make: Self::make,
        drop_args: Self::drop_args,
    };

    /// The size of the node in words: room for the call and for a node of
    /// any constructor, then for a [`Known`] when `K` is one.
    const WORDS: u32 = {
        let size = node_size::<Self>();
        let size = if size > S::ROOM { size } else { S::ROOM };
        let words = size.div_ceil(8) + size_of::<K>().div_ceil(8);
        assert!(
            words < 1 << (32 - WORDS_SHIFT),
            "the arguments of a deferred call take at most 512 KB"
        );
        words as u32
    };

    /// # Safety
    /// `node` is a node of this type whose call no one has made or is
    /// making but the caller, and, when `free`, no one else owns.
    unsafe fn make(node: NonNull<Header>, free: bool) -> S {
        let deferred = unsafe { &raw const (*node.cast::<Node<Self>>().as_ptr()).fields };
        let args = ManuallyDrop::into_inner(unsafe { std::ptr::read(&raw const (*deferred).args) });
        let run = unsafe { std::ptr::read(&raw const (*deferred).run) };
        if free {
            unsafe { free_room(node, Self::WORDS) };
        }
        run(args)
    }

    /// # Safety
    /// `node` is a node of this type whose count is 0 and whose call was
    /// never made.
    unsafe fn drop_args(node: NonNull<Header>, pending: Pending<'_>) {
        let deferred = unsafe { &raw const (*node.cast::<Node<Self>>().as_ptr()).fields };
        let args = ManuallyDrop::into_inner(unsafe { std::ptr::read(&raw const (*deferred).args) });
        args.hand_over(pending);
    }
}

/// Writes the tag that ends a call, and wakes whoever waits for it.
fn publish(header: &Header, tag: u32) {
    if header.tag.swap(tag, Ordering::Release) & WAITED != 0 {
        let _waiting = WAITING.lock().unwrap_or_else(PoisonError::into_inner);
        MADE.notify_all();
    }
}

/// Waits until the call another thread is making, whose node's tag was
/// `tag`, is made, and gives back the tag then.
#[cold]
fn wait(header: &Header, tag: u32) -> u32 {
    if tag & WAITED == 0 {
        if let Err(now) =
            header
                .tag
                .compare_exchange(tag, tag | WAITED, Ordering::Acquire, Ordering::Acquire)
        {
            return now;
        }
    }
    // `publish` wakes the waiters holding the lock, so the call cannot end
    // between the look below and the wait.
    let mut waiting = WAITING.lock().unwrap_or_else(PoisonError::into_inner);
    loop {
        let now = header.tag.load(Ordering::Acquire);
        if now & BUSY == 0 {
            return now;
        }
        waiting = MADE.wait(waiting).unwrap_or_else(PoisonError::into_inner);
    }
}

impl<S: ThinSum + HandOver> Thin<S> {
    /// A node of the hidden constructor `tag`, holding the call `run(args)`.
    ///
    /// # Safety
    /// `tag` is the tag of the hidden constructor of `S`, and `S` is laid
    /// out by [`thin_sum!`] with `lazy tag`.
    #[inline]
    pub unsafe fn deferred<A, F>(tag: u32, args: A, run: F) -> Self
    where
        A: HandOver + Send + Sync + 'static,
        F: FnOnce(A) -> S + Copy + Send + Sync + 'static,
    {
        unsafe { Self::deferred_with::<A, F, ()>(tag, args, run) }
    }

    /// [`Thin::deferred`] for a call a law says the projection `projection`
    /// of whose result is `value`: [`Thin::deferred_known`] gives it without
    /// making the call.
    ///
    /// # Safety
    /// As `Thin::deferred`.
    #[inline]
    pub unsafe fn deferred_knowing<A, F>(
        tag: u32,
        args: A,
        run: F,
        projection: u32,
        value: i64,
    ) -> Self
    where
        A: HandOver + Send + Sync + 'static,
        F: FnOnce(A) -> S + Copy + Send + Sync + 'static,
    {
        let this = unsafe { Self::deferred_with::<A, F, Known>(tag | KNOWS, args, run) };
        let at = unsafe { known_at(this.ptr, Deferred::<S, A, F, Known>::WORDS) };
        unsafe { at.write(Known { projection, value }) };
        this
    }

    #[inline]
    unsafe fn deferred_with<A, F, K>(tag: u32, args: A, run: F) -> Self
    where
        A: HandOver + Send + Sync + 'static,
        F: FnOnce(A) -> S + Copy + Send + Sync + 'static,
    {
        // `force` reads a `Lazy` at the start of the fields, wherever the
        // whole `Deferred` puts its fields.
        const {
            assert!(
                std::mem::offset_of!(Node<Deferred<S, A, F, K>>, fields)
                    == std::mem::offset_of!(Node<Lazy<S>>, fields)
            )
        };
        let words = Deferred::<S, A, F, K>::WORDS;
        let layout = unsafe { Layout::from_size_align_unchecked(words as usize * 8, 8) };
        let Some(ptr) = NonNull::new(unsafe { std::alloc::alloc(layout) }) else {
            std::alloc::handle_alloc_error(layout)
        };
        let node = Node {
            header: Header {
                count: AtomicU32::new(1),
                tag: AtomicU32::new(tag | (words << WORDS_SHIFT)),
            },
            fields: Deferred {
                lazy: Lazy {
                    calls: Deferred::<S, A, F, K>::CALLS,
                },
                args: ManuallyDrop::new(args),
                run,
                known: PhantomData,
            },
        };
        unsafe { ptr.cast::<Node<Deferred<S, A, F, K>>>().write(node) };
        let ptr = ptr.cast::<Header>();
        assert!(ptr.as_ptr().addr() > MAX_TAGS);
        Self::from_ptr(ptr)
    }

    /// The value a law gives for the projection `projection` of a call not
    /// yet made, which was made by [`Thin::deferred_knowing`] with it, in a
    /// sum whose hidden constructor is `lazy`; `None` for any other value,
    /// and once the call is made or has panicked. Never makes the call.
    #[inline]
    pub fn deferred_known(&self, lazy: u32, projection: u32) -> Option<i64> {
        self.known(lazy)
            .and_then(|known| (known.projection == projection).then_some(known.value))
    }

    /// For `==`: whether `a` and `b` are both calls not yet made that know
    /// the same projection of what they return, and know different values of
    /// it, so are not equal.
    #[inline]
    pub fn known_apart(a: &Self, b: &Self, lazy: u32) -> bool {
        match (a.known(lazy), b.known(lazy)) {
            (Some(x), Some(y)) => x.projection == y.projection && x.value != y.value,
            _ => false,
        }
    }

    fn known(&self, lazy: u32) -> Option<Known> {
        if self.is_empty_ctor() {
            return None;
        }
        // The `Known` was written before the node had a second owner and
        // is never written again, so a `Relaxed` look at the tag will do.
        let tag = self.header().tag.load(Ordering::Relaxed);
        if tag & (CTOR | KNOWS | FAILED) != lazy | KNOWS {
            return None;
        }
        // SAFETY: the node was made by `deferred_knowing`, which wrote the
        // `Known` there, past all a call or its result writes.
        Some(unsafe { known_at(self.ptr, tag >> WORDS_SHIFT).read() })
    }

    /// The tag of the value's constructor, in a sum some `lazy f(…)`
    /// returns: an `Acquire` load, which sees the fields of a node a call
    /// was made in.
    #[inline]
    pub fn lazy_tag(&self) -> u32 {
        if self.is_empty_ctor() {
            (self.ptr.as_ptr().addr() - 1) as u32
        } else {
            self.header().tag.load(Ordering::Acquire) & CTOR
        }
    }

    /// Makes the call the node holds if no one has, waiting for it if
    /// another thread is making it, and gives back what stands for it: the
    /// node itself, of a declared constructor now, or the value it holds.
    ///
    /// # Safety
    /// The value is of the hidden constructor `lazy`, made by
    /// [`Thin::deferred`].
    #[cold]
    #[inline(never)]
    pub unsafe fn force(&self, lazy: u32) -> &Thin<S> {
        let header = self.header();
        let mut tag = header.tag.load(Ordering::Acquire);
        loop {
            if tag & CTOR != lazy {
                return self;
            }
            if tag & FORWARD != 0 {
                return unsafe { self.fields::<Thin<S>>() };
            }
            if tag & FAILED != 0 {
                panic!("the deferred call this value stands for panicked");
            }
            if tag & BUSY != 0 {
                tag = wait(header, tag);
                continue;
            }
            match header.tag.compare_exchange_weak(
                tag,
                tag | BUSY,
                Ordering::Acquire,
                Ordering::Acquire,
            ) {
                Ok(_) => {
                    unsafe { self.make(lazy, tag) };
                    tag = header.tag.load(Ordering::Acquire);
                }
                Err(now) => tag = now,
            }
        }
    }

    /// Makes the call, writes what it returned into the node and publishes
    /// the tag that says what the node now is.
    ///
    /// # Safety
    /// This thread set [`BUSY`] in the node's tag, which was `tag`.
    unsafe fn make(&self, lazy: u32, tag: u32) {
        /// Marks the call failed should it panic.
        struct Failing<'a>(&'a Header, u32);
        impl Drop for Failing<'_> {
            fn drop(&mut self) {
                publish(self.0, self.1 | FAILED);
            }
        }
        let failing = Failing(self.header(), tag);
        let calls = |node: NonNull<Header>| unsafe {
            (*node.cast::<Node<Lazy<S>>>().as_ptr()).fields.calls
        };
        let mut value = unsafe { (calls(self.ptr).make)(self.ptr, false) }.into_thin();
        // A call no one else owns and no one has looked at is made here, in
        // the place of this one.
        while !value.is_empty_ctor() {
            // The hidden constructor and no mark: a call no one has begun.
            let unmade =
                value.header().tag.load(Ordering::Acquire) & !(KNOWS | !0 << WORDS_SHIFT) == lazy;
            if !unmade || value.header().count.load(Ordering::Acquire) != 1 {
                break;
            }
            let node = ManuallyDrop::new(value).ptr;
            value = unsafe { (calls(node).make)(node, true) }.into_thin();
        }
        let made = if value.is_empty_ctor() {
            value.lazy_tag()
        } else if value.lazy_tag() == lazy {
            unsafe { (&raw mut (*self.ptr.cast::<Node<Thin<S>>>().as_ptr()).fields).write(value) };
            lazy | FORWARD
        } else {
            unsafe { S::fill(self.ptr, value) }
        };
        std::mem::forget(failing);
        publish(self.header(), made | (tag & !CTOR));
    }

    /// For [`ThinSum::fill`]: moves the fields of `value`, a node of the
    /// constructor with fields `P`, into `room`, or copies them if `value`
    /// has other owners.
    ///
    /// # Safety
    /// As `ThinSum::fill`, and `P` is the constructor's field struct.
    #[inline]
    pub unsafe fn fill_with<P: Clone>(room: NonNull<Header>, value: Self) {
        let into = unsafe { &raw mut (*room.cast::<Node<P>>().as_ptr()).fields };
        // `Acquire`, as `Arc::get_mut`: every other owner is done with it.
        if value.header().count.load(Ordering::Acquire) != 1 {
            unsafe { into.write(value.fields::<P>().clone()) };
            return;
        }
        let value = ManuallyDrop::new(value);
        let from = value.ptr.cast::<Node<P>>().as_ptr();
        unsafe { into.write(std::ptr::read(&raw const (*from).fields)) };
        match value.header().tag.load(Ordering::Relaxed) >> WORDS_SHIFT {
            0 => drop(unsafe { Box::from_raw(from.cast::<ManuallyDrop<Node<P>>>()) }),
            words => unsafe { free_room(value.ptr, words) },
        }
    }

    /// For [`ThinSum::hand_over_fields`]: hands over the fields `P` of a
    /// node a call was made in.
    ///
    /// # Safety
    /// As `ThinSum::hand_over_fields`, and `P` is the constructor's field
    /// struct.
    #[inline]
    pub unsafe fn hand_over_fields_of<P: HandOver>(node: NonNull<Header>, pending: Pending<'_>) {
        let node = node.cast::<Node<P>>().as_ptr();
        unsafe { std::ptr::read(&raw const (*node).fields) }.hand_over(pending);
    }

    /// For [`ThinSum::free`]: frees a node made by [`Thin::deferred`], of a
    /// sum whose hidden constructor is `lazy`, whose tag is `tag`.
    ///
    /// # Safety
    /// The node was made by `Thin::deferred` and its count is 0.
    #[inline]
    pub unsafe fn free_deferred(node: NonNull<Header>, tag: u32, lazy: u32, pending: Pending<'_>) {
        // With no owner left no call is under way: the call was made (what
        // it returned is there), began and panicked (nothing is), or never
        // began (the arguments are).
        if tag & CTOR != lazy {
            unsafe { S::hand_over_fields(node, tag & CTOR, pending) };
        } else if tag & FORWARD != 0 {
            let held =
                unsafe { (&raw const (*node.cast::<Node<Thin<S>>>().as_ptr()).fields).read() };
            pending.take(held);
        } else if tag & FAILED == 0 {
            let calls = unsafe { (*node.cast::<Node<Lazy<S>>>().as_ptr()).fields.calls };
            unsafe { (calls.drop_args)(node, pending) };
        }
        unsafe { free_room(node, tag >> WORDS_SHIFT) };
    }
}

/// A value [`HandOver`] drops as it is: an argument of a deferred call that
/// holds no node of a recursive sum.
pub struct Kept<T>(pub T);

impl<T> HandOver for Kept<T> {}

/// The arguments of a deferred call, one pair per argument, ending in `()`.
impl HandOver for () {}

impl<A: HandOver, B: HandOver> HandOver for (A, B) {
    #[inline]
    fn hand_over(self, pending: Pending<'_>) {
        self.0.hand_over(pending);
        self.1.hand_over(pending);
    }
}

impl<S: ThinSum> Clone for Thin<S> {
    /// As `Arc::clone`: a `Relaxed` increment, since a new owner is made only
    /// from an existing one, which keeps the node alive meanwhile.
    #[inline]
    fn clone(&self) -> Self {
        if !self.is_empty_ctor()
            && self.header().count.fetch_add(1, Ordering::Relaxed) > MAX_REFCOUNT
        {
            std::process::abort();
        }
        Self::from_ptr(self.ptr)
    }
}

impl<S: ThinSum> Drop for Thin<S> {
    /// As `Arc::drop`: a `Release` decrement.
    #[inline]
    fn drop(&mut self) {
        if self.is_empty_ctor() {
            return;
        }
        if self.header().count.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }
        self.drop_slow();
    }
}

impl<S: ThinSum> crate::NodeId for Thin<S> {
    #[inline]
    fn addr(&self) -> usize {
        Thin::addr(self)
    }

    #[inline]
    fn count(&self) -> usize {
        Thin::strong_count(self)
    }
}

/// A recursive sum laid out as a [`Thin`] pointer.
///
/// ```
/// # #![allow(non_camel_case_types)]
/// pub struct List__Cons(pub i64, pub List);
///
/// impl aver_rt::thin::HandOver for List__Cons {
///     fn hand_over(self, pending: aver_rt::thin::Pending<'_>) {
///         self.1.hand_over(pending);
///     }
/// }
///
/// aver_rt::thin_sum! {
///     pub List / List__View / List__Pair {
///         0 Nil,
///         1 Cons(List__Cons),
///     }
/// }
///
/// fn sum(list: &List) -> i64 {
///     match list.view() {
///         List__View::Nil => 0,
///         List__View::Cons(cons) => cons.0 + sum(&cons.1),
///     }
/// }
///
/// let list = List::Cons(List__Cons(2, List::Cons(List__Cons(3, List::Nil))));
/// assert_eq!(sum(&list), 5);
/// assert_eq!(size_of::<List>(), 8);
///
/// let mut long = List::Nil;
/// for i in 0..1_000_000 {
///     long = List::Cons(List__Cons(i, long));
/// }
/// drop(long);
/// ```
///
/// It defines the type `List` (cloned by copying the pointer), the view
/// `List__View<'_>` (an enum of references to each constructor's fields, in
/// declaration order, with the attributes written before the type's name),
/// the pair `List__Pair<'_>` (both values' fields when two values are of the
/// same constructor), one function per constructor with fields that builds a
/// value and one constant per constructor without (as an enum spells them),
/// and `view`, `view_pair`, `tag`, `ptr_eq`. The tags are the declaration order
/// 0, 1, 2, …, at most [`MAX_TAGS`] of them, checked at compile time. Each
/// field struct implements [`HandOver`], handing over the fields that hold
/// values of the sum, so that dropping a value takes no stack however deep it
/// is (see [`Pending`]).
#[macro_export]
macro_rules! thin_sum {
    (@no_fields $fields:ty) => { false };
    (@no_fields) => { true };
    (
        $(#[$view_meta:meta])*
        $vis:vis $name:ident / $view:ident / $pair:ident {
            $( $tag:literal $ctor:ident $( ( $fields:ty ) )? ),+ $(,)?
        } $( lazy $lazy:literal )?
    ) => {
        #[derive(Clone)]
        #[allow(non_camel_case_types)]
        $vis struct $name($crate::thin::Thin<$name>);

        /// The value's constructor and a reference to its fields.
        #[allow(dead_code, non_camel_case_types)]
        $(#[$view_meta])*
        $vis enum $view<'a> {
            $( $ctor $( (&'a $fields) )?, )+
        }

        /// Two values of the same constructor and both their fields.
        #[allow(dead_code, non_camel_case_types)]
        $vis enum $pair<'a> {
            $( $ctor $( (&'a $fields, &'a $fields) )?, )+
        }

        const _: () = {
            let tags: &[u32] = &[$( $tag, )+ $( $lazy )?];
            assert!(tags.len() <= $crate::thin::MAX_TAGS, "too many constructors");
            let mut i = 0;
            while i < tags.len() {
                assert!(tags[i] as usize == i, "tags must be 0, 1, 2, … in order");
                i += 1;
            }
        };

        unsafe impl $crate::thin::ThinSum for $name {
            type Fields = ( $( $( $fields, )? )+ );

            const HAS_EMPTY: bool = false $( || $crate::thin_sum!(@no_fields $( $fields )?) )+;

            unsafe fn free(
                node: ::std::ptr::NonNull<$crate::thin::Header>,
                tag: u32,
                pending: $crate::thin::Pending<'_>,
            ) {
                match tag {
                    $( $tag => $crate::thin_sum!(@free $name node pending $( $fields )?), )+
                    // A node a call was deferred in (`aver_rt::thin::Lazy`).
                    $( _ if tag >> $crate::thin::WORDS_SHIFT != 0 => unsafe {
                        $crate::thin::Thin::<$name>::free_deferred(node, tag, $lazy, pending)
                    }, )?
                    // The caller's contract: `Thin::new` made the node for
                    // constructor `tag`, and safe code reaches it only
                    // through the constructors above, so no other tag is
                    // ever read here (as in `view`).
                    _ => {
                        debug_assert!(false, "no constructor has tag {tag}");
                        unsafe { ::std::hint::unreachable_unchecked() }
                    }
                }
            }

            #[inline]
            fn into_thin(self) -> $crate::thin::Thin<Self> {
                self.0
            }

            $crate::thin_sum!(@lazy $name $( $lazy )? ; $( $tag $( ( $fields ) )? ),+);
        }

        impl $crate::thin::HandOver for $name {
            #[inline]
            fn hand_over(self, pending: $crate::thin::Pending<'_>) {
                pending.take(self.0)
            }
        }

        #[allow(non_snake_case, dead_code)]
        impl $name {
            $( $crate::thin_sum!(@ctor $vis $name $tag $ctor $( $fields )?); )+

            /// The value and its constructor's tag, making the call first
            /// if the value is a call no one has made yet
            /// (`aver_rt::thin::Thin::deferred`), as no value of a sum no
            /// `lazy f(…)` returns is.
            #[inline]
            fn made(&self) -> (&$crate::thin::Thin<$name>, u32) {
                $crate::thin_sum!(@made &self.0 $( , $lazy )?)
            }

            $(
                /// The call `run(args)`, made when the value is first looked at.
                #[inline]
                $vis fn deferred<A, F>(args: A, run: F) -> Self
                where
                    A: $crate::thin::HandOver + Send + Sync + 'static,
                    F: FnOnce(A) -> Self + Copy + Send + Sync + 'static,
                {
                    $name(unsafe { $crate::thin::Thin::deferred($lazy, args, run) })
                }

                /// The call `run(args)`, which a law says the projection
                /// `projection` of whose result is `value`.
                #[inline]
                $vis fn deferred_knowing<A, F>(args: A, run: F, projection: u32, value: i64) -> Self
                where
                    A: $crate::thin::HandOver + Send + Sync + 'static,
                    F: FnOnce(A) -> Self + Copy + Send + Sync + 'static,
                {
                    $name(unsafe {
                        $crate::thin::Thin::deferred_knowing($lazy, args, run, projection, value)
                    })
                }

                /// What a call not yet made knows of the projection
                /// `projection` of its result (`aver_rt::thin::Thin::deferred_known`).
                #[inline]
                $vis fn deferred_known(&self, projection: u32) -> ::std::option::Option<i64> {
                    self.0.deferred_known($lazy, projection)
                }
            )?

            /// The constructor and its fields, to match on.
            #[inline]
            $vis fn view(&self) -> $view<'_> {
                let (this, tag) = self.made();
                match tag {
                    $( $tag => $view::$ctor $( (unsafe { this.fields::<$fields>() }) )?, )+
                    // Safe code makes a value only through the constructors
                    // above, whose tags are these, checked at compile time.
                    _ => unsafe { ::std::hint::unreachable_unchecked() },
                }
            }

            /// Both values' fields when they are of the same constructor.
            #[inline]
            $vis fn view_pair<'a>(a: &'a Self, b: &'a Self) -> ::std::option::Option<$pair<'a>> {
                // Two calls not yet made that know different values of one
                // projection of what they return are not equal.
                $( if $crate::thin::Thin::known_apart(&a.0, &b.0, $lazy) {
                    return None;
                } )?
                let ((a, tag), (b, other)) = (a.made(), b.made());
                if tag != other {
                    return None;
                }
                Some(match tag {
                    $( $tag => $pair::$ctor $( (unsafe { a.fields::<$fields>() }, unsafe { b.fields::<$fields>() }) )?, )+
                    // As in `view`.
                    _ => unsafe { ::std::hint::unreachable_unchecked() },
                })
            }

            /// The tag of the value's constructor (its declaration index).
            #[inline]
            $vis fn tag(&self) -> u32 {
                self.made().1
            }

            /// Whether both are the same node, or the same constructor
            /// without fields.
            #[inline]
            $vis fn ptr_eq(a: &Self, b: &Self) -> bool {
                $crate::thin::Thin::ptr_eq(&a.0, &b.0)
            }
        }

        // A comparison or a hash reaches a node through `view`, which has
        // made the call already; it goes on with the node the call returned.
        impl $crate::NodeId for $name {
            #[inline]
            fn addr(&self) -> usize {
                $crate::thin::Thin::addr(self.made().0)
            }

            #[inline]
            fn count(&self) -> usize {
                $crate::thin::Thin::strong_count(self.made().0)
            }
        }
    };

    (@made $value:expr) => {{
        let value = $value;
        (value, value.tag())
    }};
    (@made $value:expr, $lazy:literal) => {{
        let mut value = $value;
        loop {
            let tag = value.lazy_tag();
            if tag != $lazy {
                break (value, tag);
            }
            // SAFETY: a node of tag `$lazy` is made only by `deferred`.
            value = unsafe { value.force($lazy) };
        }
    }};
    (@lazy $name:ident ; $( $tag:literal $( ( $fields:ty ) )? ),+) => {};
    (@lazy $name:ident $lazy:literal ; $( $tag:literal $( ( $fields:ty ) )? ),+) => {
        const ROOM: usize = {
            let mut room = 0;
            $( $(
                let size = $crate::thin::node_size::<$fields>();
                if size > room {
                    room = size;
                }
            )? )+
            room
        };

        unsafe fn fill(
            room: ::std::ptr::NonNull<$crate::thin::Header>,
            value: $crate::thin::Thin<Self>,
        ) -> u32 {
            let tag = value.lazy_tag();
            match tag {
                $( $tag => $crate::thin_sum!(@fill $name room value $( $fields )?), )+
                _ => unreachable!("no declared constructor has tag {tag}"),
            }
            tag
        }

        unsafe fn hand_over_fields(
            node: ::std::ptr::NonNull<$crate::thin::Header>,
            tag: u32,
            pending: $crate::thin::Pending<'_>,
        ) {
            match tag {
                $( $tag => $crate::thin_sum!(@hand $name node pending $( $fields )?), )+
                _ => unreachable!("no declared constructor has tag {tag}"),
            }
        }
    };
    (@fill $name:ident $room:ident $value:ident $fields:ty) => {
        unsafe { $crate::thin::Thin::<$name>::fill_with::<$fields>($room, $value) }
    };
    (@fill $name:ident $room:ident $value:ident) => {
        drop($value)
    };
    (@hand $name:ident $node:ident $pending:ident $fields:ty) => {
        unsafe { $crate::thin::Thin::<$name>::hand_over_fields_of::<$fields>($node, $pending) }
    };
    (@hand $name:ident $node:ident $pending:ident) => {
        ()
    };
    (@free $name:ident $node:ident $pending:ident $fields:ty) => {
        unsafe { $crate::thin::Thin::<$name>::free_node::<$fields>($node, $pending) }
    };
    // A constructor without fields has no node: its value is the address
    // `tag + 1`, which `drop` and `Pending::take` tell apart and never free.
    (@free $name:ident $node:ident $pending:ident) => {{
        debug_assert!(false, "a constructor without fields has no node");
        unsafe { ::std::hint::unreachable_unchecked() }
    }};
    (@ctor $vis:vis $name:ident $tag:literal $ctor:ident $fields:ty) => {
        #[inline]
        $vis fn $ctor(fields: $fields) -> Self {
            $name(unsafe { $crate::thin::Thin::new($tag, fields) })
        }
    };
    (@ctor $vis:vis $name:ident $tag:literal $ctor:ident) => {
        #[allow(non_upper_case_globals)]
        $vis const $ctor: Self = $name(unsafe { $crate::thin::Thin::empty($tag) });
    };
}

#[cfg(test)]
#[allow(non_camel_case_types)]
mod tests {
    use crate::shared_eq::{Walk, WalkEq, walk_eq};
    use std::sync::Arc;
    use std::sync::atomic::{AtomicUsize, Ordering};

    /// Counts its drops into a shared counter.
    #[derive(Debug)]
    struct Counted(Arc<AtomicUsize>, u64);

    impl Drop for Counted {
        fn drop(&mut self) {
            self.0.fetch_add(1, Ordering::SeqCst);
        }
    }

    impl PartialEq for Counted {
        fn eq(&self, other: &Self) -> bool {
            self.1 == other.1
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct T__Leaf(Counted);
    #[derive(Debug)]
    pub struct T__Pair(T, T, u8);
    #[derive(Debug)]
    pub struct T__Opt(Option<T>);

    impl super::HandOver for T__Leaf {}

    impl super::HandOver for T__Pair {
        fn hand_over(self, pending: super::Pending<'_>) {
            self.0.hand_over(pending);
            self.1.hand_over(pending);
        }
    }

    impl super::HandOver for T__Opt {
        fn hand_over(self, pending: super::Pending<'_>) {
            self.0.hand_over(pending);
        }
    }

    crate::thin_sum! {
        #[derive(Debug)]
        pub T / T__View / T__Pairs {
            0 Leaf(T__Leaf),
            1 Nil,
            2 Pair(T__Pair),
            3 Unit,
            4 Opt(T__Opt),
        }
    }

    impl std::fmt::Debug for T {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Debug::fmt(&self.view(), f)
        }
    }

    impl WalkEq for T {
        fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool {
            match T::view_pair(self, other) {
                None => false,
                Some(T__Pairs::Leaf(a, b)) => a == b,
                Some(T__Pairs::Nil | T__Pairs::Unit) => true,
                Some(T__Pairs::Opt(a, b)) => a.0.walk_eq(&b.0, walk),
                Some(T__Pairs::Pair(a, b)) => walk.node(self, other, true, |walk| {
                    a.2 == b.2 && a.0.walk_eq(&b.0, walk) && a.1.walk_eq(&b.1, walk)
                }),
            }
        }
    }

    fn leaf(drops: &Arc<AtomicUsize>, n: u64) -> T {
        T::Leaf(T__Leaf(Counted(drops.clone(), n)))
    }

    fn pair(a: T, b: T, n: u8) -> T {
        T::Pair(T__Pair(a, b, n))
    }

    fn sum(t: &T) -> u64 {
        match t.view() {
            T__View::Leaf(l) => l.0.1,
            T__View::Nil | T__View::Unit => 0,
            T__View::Pair(p) => sum(&p.0) + sum(&p.1) + p.2 as u64,
            T__View::Opt(o) => o.0.as_ref().map_or(0, sum),
        }
    }

    fn assert_send_sync<X: Send + Sync>() {}

    /// A sum whose every constructor has fields.
    #[derive(Debug, PartialEq)]
    pub struct F__A(Counted);
    #[derive(Debug)]
    pub struct F__B(F, u8);

    impl super::HandOver for F__A {}

    impl super::HandOver for F__B {
        fn hand_over(self, pending: super::Pending<'_>) {
            self.0.hand_over(pending);
        }
    }

    crate::thin_sum! {
        #[derive(Debug)]
        pub F / F__View / F__Pairs {
            0 A(F__A),
            1 B(F__B),
        }
    }

    impl std::fmt::Debug for F {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Debug::fmt(&self.view(), f)
        }
    }

    /// `None` of a thin sum is the null pointer: a value without a node is a
    /// small non-zero address, so zero is still free for `Option`.
    #[test]
    fn an_option_of_a_thin_sum_is_one_word() {
        let word = std::mem::size_of::<usize>();
        assert_eq!(std::mem::size_of::<T>(), word);
        assert_eq!(std::mem::size_of::<Option<T>>(), word);
        assert_eq!(std::mem::size_of::<Option<F>>(), word);
        assert_eq!(std::mem::size_of::<Option<Option<T>>>(), 2 * word);
    }

    #[test]
    fn a_sum_knows_whether_a_value_can_be_without_a_node() {
        use super::ThinSum;
        const { assert!(<T as ThinSum>::HAS_EMPTY) };
        const { assert!(<L as ThinSum>::HAS_EMPTY) };
        const { assert!(!<F as ThinSum>::HAS_EMPTY) };

        let drops = Arc::new(AtomicUsize::new(0));
        let a = F::A(F__A(Counted(drops.clone(), 7)));
        let b = F::B(F__B(a.clone(), 2));
        assert_eq!(a.tag(), 0);
        assert_eq!(b.tag(), 1);
        assert!(F::ptr_eq(&a, &a.clone()));
        assert!(!F::ptr_eq(&a, &b));
        assert!(matches!(b.view(), F__View::B(p) if p.1 == 2 && p.0.tag() == 0));
        drop(a);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(b);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn a_value_is_one_word_and_a_node_holds_only_its_fields() {
        assert_eq!(size_of::<T>(), size_of::<usize>());
        assert_eq!(size_of::<Option<T>>(), size_of::<usize>());
        assert_eq!(size_of::<super::Node<T__Pair>>(), 8 + size_of::<T__Pair>());
        assert_eq!(size_of::<super::Node<T__Leaf>>(), 8 + size_of::<T__Leaf>());
        assert_send_sync::<T>();
    }

    #[test]
    fn tags_follow_declaration_order() {
        let drops = Arc::new(AtomicUsize::new(0));
        assert_eq!(leaf(&drops, 1).tag(), 0);
        assert_eq!(T::Nil.tag(), 1);
        assert_eq!(pair(T::Nil, T::Unit, 0).tag(), 2);
        assert_eq!(T::Unit.tag(), 3);
        assert_eq!(format!("{:?}", T::Unit.view()), "Unit");
        assert_eq!(
            format!("{:?}", pair(T::Nil, T::Unit, 4).view()),
            "Pair(T__Pair(Nil, Unit, 4))"
        );
    }

    #[test]
    fn constructors_without_fields_have_no_node() {
        let a = T::Nil;
        let b = a.clone();
        assert!(T::ptr_eq(&a, &T::Nil));
        assert!(!T::ptr_eq(&a, &T::Unit));
        assert_eq!(super::Thin::strong_count(&b.0), usize::MAX);
        assert!(matches!(b.view(), T__View::Nil));
        const NIL: T = T::Nil;
        assert!(matches!(NIL.view(), T__View::Nil));
    }

    /// `free` takes the tag `Thin::new` wrote and checks it only in a debug
    /// build: these break its contract, which a release build trusts. The
    /// check panics before `free` reads the node, which the value still
    /// owns and frees as the panic unwinds.
    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "no constructor has tag 9")]
    fn a_debug_build_catches_a_free_of_a_tag_no_constructor_has() {
        use super::ThinSum;
        let drops = Arc::new(AtomicUsize::new(0));
        let value = leaf(&drops, 1);
        let mut put_aside = Vec::new();
        unsafe { T::free(value.0.ptr, 9, super::Pending::new(&mut put_aside)) };
    }

    #[test]
    #[cfg(debug_assertions)]
    #[should_panic(expected = "a constructor without fields has no node")]
    fn a_debug_build_catches_a_free_of_a_constructor_without_fields() {
        use super::ThinSum;
        let drops = Arc::new(AtomicUsize::new(0));
        let value = leaf(&drops, 1);
        let mut put_aside = Vec::new();
        unsafe { T::free(value.0.ptr, 1, super::Pending::new(&mut put_aside)) };
    }

    #[test]
    fn clone_and_drop_count_owners_and_free_once() {
        let drops = Arc::new(AtomicUsize::new(0));
        let a = leaf(&drops, 3);
        let t = pair(a.clone(), pair(a, T::Nil, 1), 2);
        assert_eq!(sum(&t), 3 + 3 + 1 + 2);
        let u = t.clone();
        assert_eq!(super::Thin::strong_count(&t.0), 2);
        drop(t);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(u);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn view_pair_needs_the_same_constructor() {
        let drops = Arc::new(AtomicUsize::new(0));
        assert!(T::view_pair(&T::Nil, &T::Unit).is_none());
        assert!(matches!(
            T::view_pair(&T::Nil, &T::Nil),
            Some(T__Pairs::Nil)
        ));
        let (a, b) = (leaf(&drops, 5), leaf(&drops, 6));
        assert!(
            matches!(T::view_pair(&a, &b), Some(T__Pairs::Leaf(x, y)) if x.0.1 == 5 && y.0.1 == 6)
        );
        assert!(T::view_pair(&a, &T::Nil).is_none());
    }

    #[test]
    fn walk_eq_compares_values_and_shares_equal_nodes() {
        let drops = Arc::new(AtomicUsize::new(0));
        let build = |n: u64| {
            let mut t = leaf(&drops, n);
            for i in 0..200u8 {
                t = pair(t.clone(), t, i);
            }
            t
        };
        assert!(walk_eq(&build(1), &build(1)));
        assert!(!walk_eq(&build(1), &build(2)));
        assert!(walk_eq(&T::Unit, &T::Unit));
        assert!(!walk_eq(&T::Unit, &T::Nil));
    }

    #[test]
    fn a_match_that_moves_the_subject_after_viewing_it_compiles() {
        // `view` borrows; an arm that is done with the fields may move the
        // value itself, since a view has no `Drop`.
        let drops = Arc::new(AtomicUsize::new(0));
        let t = pair(leaf(&drops, 1), T::Nil, 0);
        let kept = match t.view() {
            T__View::Pair(p) if p.2 == 0 => t,
            _ => T::Nil,
        };
        assert_eq!(sum(&kept), 1);
    }

    #[test]
    fn clones_dropped_on_many_threads_free_once() {
        let drops = Arc::new(AtomicUsize::new(0));
        let root = pair(leaf(&drops, 1), T::Nil, 0);
        let threads = if cfg!(miri) { 4 } else { 8 };
        let rounds = if cfg!(miri) { 20 } else { 2000 };
        std::thread::scope(|scope| {
            for _ in 0..threads {
                let mine = root.clone();
                scope.spawn(move || {
                    let mut held = Vec::new();
                    for i in 0..rounds {
                        held.push(mine.clone());
                        held.push(T::Unit);
                        if i % 3 == 0 {
                            held.clear();
                        }
                        assert_eq!(sum(&mine), 1);
                    }
                });
            }
        });
        assert_eq!(super::Thin::strong_count(&root.0), 1);
        drop(root);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    /// Deep enough to overflow a test thread's stack were each node freed
    /// inside its parent's free; under Miri, past `NESTED_FREES` a few times.
    const DEEP: u64 = if cfg!(miri) { 300 } else { 1_000_000 };

    #[test]
    fn a_deep_chain_frees_without_deep_recursion() {
        let drops = Arc::new(AtomicUsize::new(0));
        let mut left = leaf(&drops, 1);
        let mut right = leaf(&drops, 2);
        let mut some = leaf(&drops, 3);
        for i in 0..DEEP {
            left = pair(left, T::Nil, i as u8);
            right = pair(T::Unit, right, i as u8);
            some = T::Opt(T__Opt(Some(some)));
        }
        drop(left);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        drop(right);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
        drop(some);
        assert_eq!(drops.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn a_shared_part_of_a_deep_chain_outlives_the_rest() {
        let drops = Arc::new(AtomicUsize::new(0));
        let mut t = leaf(&drops, 1);
        let mut kept = T::Nil;
        for i in 0..DEEP {
            if i == DEEP / 2 {
                kept = t.clone();
            }
            t = pair(t, leaf(&drops, 0), 0);
        }
        drop(t);
        assert_eq!(drops.load(Ordering::SeqCst), (DEEP - DEEP / 2) as usize);
        assert_eq!(super::Thin::strong_count(&kept.0), 1);
        drop(kept);
        assert_eq!(drops.load(Ordering::SeqCst), DEEP as usize + 1);
    }

    // A sum some `lazy f(…)` returns: `L::deferred` holds the call.
    #[derive(Clone, Debug, PartialEq)]
    pub struct L__Leaf(u64);
    #[derive(Clone, Debug)]
    pub struct L__Node(L, L);
    #[derive(Clone, Debug)]
    pub struct L__Held(Arc<Counted>);

    impl super::HandOver for L__Leaf {}
    impl super::HandOver for L__Held {}

    impl super::HandOver for L__Node {
        fn hand_over(self, pending: super::Pending<'_>) {
            self.0.hand_over(pending);
            self.1.hand_over(pending);
        }
    }

    crate::thin_sum! {
        #[derive(Debug)]
        pub L / L__View / L__Pairs {
            0 Leaf(L__Leaf),
            1 Node(L__Node),
            2 Nil,
            3 Held(L__Held),
        } lazy 4
    }

    impl std::fmt::Debug for L {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Debug::fmt(&self.view(), f)
        }
    }

    impl WalkEq for L {
        fn walk_eq<'v>(&'v self, other: &'v Self, walk: Walk<'_, 'v>) -> bool {
            match L::view_pair(self, other) {
                None => false,
                Some(L__Pairs::Leaf(a, b)) => a == b,
                Some(L__Pairs::Nil) => true,
                Some(L__Pairs::Held(a, b)) => a.0 == b.0,
                Some(L__Pairs::Node(a, b)) => walk.node(self, other, true, |walk| {
                    a.0.walk_eq(&b.0, walk) && a.1.walk_eq(&b.1, walk)
                }),
            }
        }
    }

    fn l_leaf(n: u64) -> L {
        L::Leaf(L__Leaf(n))
    }

    fn l_node(a: L, b: L) -> L {
        L::Node(L__Node(a, b))
    }

    /// `n` levels of `Node(Leaf, below)`, each level below the first
    /// deferred, counting the calls made into `made`.
    fn spine(n: u64, made: &'static AtomicUsize) -> L {
        match n {
            0 => l_leaf(0),
            _ => {
                made.fetch_add(1, Ordering::SeqCst);
                let below = L::deferred(
                    (super::Kept(n - 1), (super::Kept(made), ())),
                    |(super::Kept(n), (super::Kept(made), ()))| spine(n, made),
                );
                l_node(l_leaf(n), below)
            }
        }
    }

    fn l_sum(t: &L) -> u64 {
        let mut total = 0;
        let mut at = t.clone();
        loop {
            let next = match at.view() {
                L__View::Leaf(leaf) => return total + leaf.0,
                L__View::Nil | L__View::Held(_) => return total,
                L__View::Node(node) => {
                    total += l_sum(&node.0);
                    node.1.clone()
                }
            };
            at = next;
        }
    }

    #[test]
    fn a_deferred_value_is_one_word_and_reads_as_the_call_returns() {
        static MADE: AtomicUsize = AtomicUsize::new(0);
        assert_eq!(size_of::<L>(), size_of::<usize>());
        assert_send_sync::<L>();
        let eager = l_node(l_leaf(3), l_node(l_leaf(2), l_node(l_leaf(1), l_leaf(0))));
        let lazy = L::deferred((super::Kept(3u64), ()), |(super::Kept(n), ())| {
            spine(n, &MADE)
        });
        assert_eq!(
            MADE.load(Ordering::SeqCst),
            0,
            "nothing is made before a look"
        );
        assert_eq!(lazy.tag(), 1);
        assert!(walk_eq(&lazy, &eager));
        assert!(walk_eq(&eager, &lazy));
        assert_eq!(format!("{:?}", lazy.view()), format!("{:?}", eager.view()));
        assert!(!walk_eq(&lazy, &l_leaf(3)));
    }

    #[test]
    fn a_call_is_made_once_however_often_the_value_is_looked_at() {
        static MADE: AtomicUsize = AtomicUsize::new(0);
        let lazy = spine(1, &MADE);
        let below = match lazy.view() {
            L__View::Node(node) => node.1.clone(),
            _ => unreachable!(),
        };
        let again = below.clone();
        for _ in 0..10 {
            assert!(matches!(below.view(), L__View::Leaf(leaf) if leaf.0 == 0));
            assert!(matches!(again.view(), L__View::Leaf(_)));
            assert!(walk_eq(&below, &again));
        }
        assert_eq!(MADE.load(Ordering::SeqCst), 1);
        // What it returned was written into its own node.
        use crate::NodeId;
        assert_eq!(below.addr(), super::Thin::addr(&below.0));
        assert_eq!(again.addr(), super::Thin::addr(&below.0));
    }

    #[test]
    fn a_call_shared_by_many_threads_is_made_once() {
        static MADE: AtomicUsize = AtomicUsize::new(0);
        static RUNS: AtomicUsize = AtomicUsize::new(0);
        let shared = L::deferred((super::Kept(()), ()), |_| {
            RUNS.fetch_add(1, Ordering::SeqCst);
            spine(20, &MADE)
        });
        let threads = if cfg!(miri) { 4 } else { 8 };
        std::thread::scope(|scope| {
            for _ in 0..threads {
                let mine = shared.clone();
                scope.spawn(move || assert_eq!(l_sum(&mine), (1..=20).sum::<u64>()));
            }
        });
        assert_eq!(RUNS.load(Ordering::SeqCst), 1);
        assert_eq!(MADE.load(Ordering::SeqCst), 20);
    }

    #[test]
    fn a_call_never_looked_at_is_never_made_and_drops_its_arguments() {
        static MADE: AtomicUsize = AtomicUsize::new(0);
        let drops = Arc::new(AtomicUsize::new(0));
        let held = T::Leaf(T__Leaf(Counted(drops.clone(), 1)));
        let lazy = L::deferred((super::Kept(held), ()), |_| spine(5, &MADE));
        drop(lazy);
        assert_eq!(MADE.load(Ordering::SeqCst), 0);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn a_call_that_panicked_has_used_up_its_arguments() {
        let drops = Arc::new(AtomicUsize::new(0));
        let held = T::Leaf(T__Leaf(Counted(drops.clone(), 1)));
        let lazy = L::deferred((super::Kept(held), ()), |(super::Kept(held), ())| {
            drop(held);
            panic!("the call fails")
        });
        let looked = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| lazy.tag()));
        assert!(looked.is_err());
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        drop(lazy);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    /// A call that knows the value 7 of the projection 1 of its result, the
    /// leaf `n`, counting the calls made into `made`.
    fn knowing_leaf(n: u64, made: &'static AtomicUsize) -> L {
        L::deferred_knowing(
            (super::Kept(n), (super::Kept(made), ())),
            |(super::Kept(n), (super::Kept(made), ()))| {
                made.fetch_add(1, Ordering::SeqCst);
                l_leaf(n)
            },
            1,
            n as i64 * 7,
        )
    }

    #[test]
    fn a_deferred_call_gives_the_value_it_knows_without_being_made() {
        static MADE: AtomicUsize = AtomicUsize::new(0);
        let lazy = knowing_leaf(3, &MADE);
        let shared = lazy.clone();
        for _ in 0..3 {
            assert_eq!(shared.deferred_known(1), Some(21));
        }
        assert_eq!(lazy.deferred_known(2), None, "only for its own projection");
        assert_eq!(MADE.load(Ordering::SeqCst), 0);
        assert!(!walk_eq(&lazy, &knowing_leaf(4, &MADE)), "known apart");
        assert_eq!(MADE.load(Ordering::SeqCst), 0);
        assert!(walk_eq(&lazy, &knowing_leaf(3, &MADE)));
        assert_eq!(MADE.load(Ordering::SeqCst), 2);
        assert_eq!(lazy.deferred_known(1), None, "made: the fields say it");
        assert!(matches!(lazy.view(), L__View::Leaf(leaf) if leaf.0 == 3));
        assert_eq!(MADE.load(Ordering::SeqCst), 2);
        // One that returned another call that knows: it stands for that one.
        let outer = L::deferred((super::Kept(()), ()), |_| knowing_leaf(5, &MADE));
        assert_eq!(outer.deferred_known(1), None);
        assert!(matches!(outer.view(), L__View::Leaf(leaf) if leaf.0 == 5));
        drop(knowing_leaf(9, &MADE));
        assert_eq!(MADE.load(Ordering::SeqCst), 3);
    }

    #[test]
    fn a_call_being_made_still_gives_what_it_knows_and_one_that_panicked_does_not() {
        thread_local! {
            static BEING_MADE: std::cell::RefCell<Option<L>> = const { std::cell::RefCell::new(None) };
        }
        let lazy = L::deferred_knowing(
            (super::Kept(()), ()),
            |_| {
                let known = BEING_MADE.with(|it| it.borrow().as_ref().unwrap().deferred_known(1));
                assert_eq!(known, Some(42));
                l_leaf(6)
            },
            1,
            42,
        );
        BEING_MADE.with(|it| *it.borrow_mut() = Some(lazy.clone()));
        assert!(matches!(lazy.view(), L__View::Leaf(leaf) if leaf.0 == 6));
        BEING_MADE.with(|it| *it.borrow_mut() = None);
        let failing =
            L::deferred_knowing((super::Kept(()), ()), |_| panic!("the call fails"), 1, 5);
        assert_eq!(failing.deferred_known(1), Some(5));
        let looked = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| failing.tag()));
        assert!(looked.is_err());
        assert_eq!(failing.deferred_known(1), None, "panicked");
    }

    #[test]
    fn threads_reading_what_a_call_knows_and_making_it_agree() {
        static MADE: AtomicUsize = AtomicUsize::new(0);
        let rounds = if cfg!(miri) { 4 } else { 200 };
        let threads = if cfg!(miri) { 3 } else { 8 };
        for round in 0..rounds {
            let shared = knowing_leaf(round, &MADE);
            std::thread::scope(|scope| {
                for i in 0..threads {
                    let mine = shared.clone();
                    scope.spawn(move || {
                        if i % 2 == 0 {
                            assert!(matches!(mine.view(), L__View::Leaf(leaf) if leaf.0 == round));
                        } else {
                            let known = mine.deferred_known(1);
                            assert!(known.is_none_or(|v| v == round as i64 * 7));
                        }
                    });
                }
            });
        }
        assert_eq!(MADE.load(Ordering::SeqCst), rounds as usize);
    }

    #[test]
    fn deep_chains_of_deferred_calls_are_made_and_freed_without_deep_recursion() {
        static MADE: AtomicUsize = AtomicUsize::new(0);
        // Each level's child deferred: looked at one level at a time.
        let top = spine(DEEP, &MADE);
        assert_eq!(l_sum(&top), DEEP * (DEEP + 1) / 2);
        drop(top);
        // A call whose result is itself a call, a million deep.
        fn chain(n: u64) -> L {
            match n {
                0 => l_leaf(7),
                _ => L::deferred((super::Kept(n - 1), ()), |(super::Kept(n), ())| chain(n)),
            }
        }
        let calls = chain(DEEP);
        assert!(matches!(calls.view(), L__View::Leaf(leaf) if leaf.0 == 7));
        drop(calls);
        // Calls never made, each holding the one before as its argument.
        let mut held = l_leaf(1);
        for _ in 0..DEEP {
            held = L::deferred((held, ()), |(below, ())| l_node(below, l_leaf(0)));
        }
        drop(held);
    }

    fn held(drops: &Arc<AtomicUsize>) -> L {
        L::Held(L__Held(Arc::new(Counted(drops.clone(), 0))))
    }

    #[test]
    fn a_call_returning_a_node_no_one_else_owns_moves_it_in() {
        use crate::NodeId;
        let drops = Arc::new(AtomicUsize::new(0));
        // The call returns its own argument, which only the call held.
        let lazy = L::deferred((held(&drops), ()), |(value, ())| value);
        assert_eq!(lazy.tag(), 3);
        assert_eq!(lazy.addr(), super::Thin::addr(&lazy.0));
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(lazy);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        // A constructor without fields.
        let nil = L::deferred((super::Kept(()), ()), |_| L::Nil);
        assert!(walk_eq(&nil, &L::Nil));
        assert!(matches!(nil.view(), L__View::Nil));
        assert_eq!(nil.addr(), super::Thin::addr(&nil.0));
    }

    #[test]
    fn a_call_returning_a_node_others_own_copies_its_fields() {
        use crate::NodeId;
        let drops = Arc::new(AtomicUsize::new(0));
        let kept = l_node(held(&drops), l_leaf(2));
        let lazy = L::deferred((kept.clone(), ()), |(value, ())| value);
        assert!(walk_eq(&lazy, &kept));
        assert_ne!(lazy.addr(), kept.addr());
        // One more owner of each field, and none more of the node.
        assert_eq!(super::Thin::strong_count(&kept.0), 1);
        match (lazy.view(), kept.view()) {
            (L__View::Node(a), L__View::Node(b)) => {
                assert!(L::ptr_eq(&a.0, &b.0));
                assert_eq!(super::Thin::strong_count(&b.0.0), 2);
            }
            _ => unreachable!(),
        }
        drop(kept);
        assert_eq!(drops.load(Ordering::SeqCst), 0);
        drop(lazy);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn a_call_returning_a_call_others_own_stands_for_it() {
        use crate::NodeId;
        static MADE: AtomicUsize = AtomicUsize::new(0);
        let inner = L::deferred((super::Kept(2u64), ()), |(super::Kept(n), ())| {
            spine(n, &MADE)
        });
        let outer = L::deferred((inner.clone(), ()), |(value, ())| value);
        assert_eq!(l_sum(&outer), 3);
        // Both are the node `inner`'s call was made in.
        assert_eq!(outer.addr(), super::Thin::addr(&inner.0));
        assert_eq!(inner.addr(), super::Thin::addr(&inner.0));
        assert!(walk_eq(&outer, &inner));
        drop(inner);
        assert_eq!(l_sum(&outer), 3);
    }

    #[test]
    fn a_call_with_arguments_larger_than_any_node_is_made_in_its_node() {
        let drops = Arc::new(AtomicUsize::new(0));
        let args = (super::Kept([1u64, 2, 3, 4, 5, 6, 7, 8]), (held(&drops), ()));
        let lazy = L::deferred(args, |(super::Kept(numbers), (value, ()))| {
            l_node(l_leaf(numbers.iter().sum()), value)
        });
        assert_eq!(l_sum(&lazy), 36);
        drop(lazy);
        assert_eq!(drops.load(Ordering::SeqCst), 1);
        // And one never made.
        let never = L::deferred(
            (super::Kept([0u64; 8]), (held(&drops), ())),
            |(_, (value, ()))| value,
        );
        drop(never);
        assert_eq!(drops.load(Ordering::SeqCst), 2);
    }

    #[test]
    fn many_threads_making_one_call_see_it_made_once() {
        static RUNS: AtomicUsize = AtomicUsize::new(0);
        let threads = if cfg!(miri) { 4 } else { 8 };
        let rounds = if cfg!(miri) { 2 } else { 200 };
        for _ in 0..rounds {
            RUNS.store(0, Ordering::SeqCst);
            let shared = L::deferred((super::Kept(()), ()), |_| {
                RUNS.fetch_add(1, Ordering::SeqCst);
                // Long enough for the others to arrive and wait.
                let mut below = l_leaf(1);
                for i in 0..if cfg!(miri) { 10 } else { 2000 } {
                    below = l_node(l_leaf(i), below);
                }
                below
            });
            let start = std::sync::Barrier::new(threads);
            std::thread::scope(|scope| {
                for _ in 0..threads {
                    let (mine, start) = (shared.clone(), &start);
                    scope.spawn(move || {
                        start.wait();
                        assert!(matches!(mine.view(), L__View::Node(_)));
                    });
                }
            });
            assert_eq!(RUNS.load(Ordering::SeqCst), 1);
        }
    }

    #[test]
    fn a_call_that_panicked_panics_every_look_on_every_thread() {
        static RUNS: AtomicUsize = AtomicUsize::new(0);
        let shared = L::deferred((super::Kept(()), ()), |_| {
            RUNS.fetch_add(1, Ordering::SeqCst);
            panic!("the call fails")
        });
        let threads = if cfg!(miri) { 4 } else { 8 };
        let start = std::sync::Barrier::new(threads);
        std::thread::scope(|scope| {
            for _ in 0..threads {
                let (mine, start) = (shared.clone(), &start);
                scope.spawn(move || {
                    start.wait();
                    let looked =
                        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| mine.tag()));
                    assert!(looked.is_err());
                });
            }
        });
        assert_eq!(RUNS.load(Ordering::SeqCst), 1);
    }

    #[test]
    fn last_owner_on_another_thread_frees() {
        let drops = Arc::new(AtomicUsize::new(0));
        let a = pair(leaf(&drops, 2), T::Nil, 0);
        let b = a.clone();
        let other = std::thread::spawn(move || {
            assert_eq!(sum(&b), 2);
            drop(b);
        });
        drop(a);
        other.join().unwrap();
        assert_eq!(drops.load(Ordering::SeqCst), 1);
    }
}
