//! A value of a recursive sum as one pointer.
//!
//! The Rust backend keeps a value of a recursive sum (a sum a value of which
//! can hold a value of the same type with no collection in between) as a
//! [`Thin`]: one pointer to a node that holds a header and then the fields of
//! THAT constructor only. The header is a 32-bit atomic count and the 32-bit
//! tag of the constructor, so a value is 8 bytes, an `Option` of one is 8
//! bytes too, and a node is 8 bytes plus its own fields rather than the room
//! of the largest constructor.
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
//! constructor: the call, not yet made (see [`Thin::deferred`]). Every way of looking at a
//! value (`view`, `view_pair`, `tag`, and through them `==`, hashing and
//! printing) makes the call first, once, and from then on looks at what it
//! returned, so no code can tell such a value from the one the call returns.
//!
//! The atomic operations and their orderings are those `Shared` uses, which
//! are `std::sync::Arc`'s strong count (library/alloc/src/sync.rs). The count
//! is 32 bits: a clone past `i32::MAX` aborts, as `Arc` does past
//! `isize::MAX`; reaching it takes two billion owners of one node, 16 GB of
//! pointers to it alone.

use std::cell::UnsafeCell;
use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::mem::MaybeUninit;
use std::ptr::NonNull;
use std::sync::Once;
use std::sync::atomic::{self, AtomicU32, Ordering};

/// Past it, a clone aborts rather than let the count wrap around.
const MAX_REFCOUNT: u32 = i32::MAX as u32;

/// The most constructors a sum laid out this way can have. A value of a
/// constructor without fields is a pointer to address `tag + 1`, so every
/// such address is at most `MAX_TAGS`, below any address an allocation gets.
pub const MAX_TAGS: usize = 256;

/// The header at the start of a node: the count, then the constructor's tag,
/// which never changes after the node is made.
#[repr(C)]
pub struct Header {
    count: AtomicU32,
    tag: u32,
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
        self.ptr.as_ptr().addr() <= MAX_TAGS
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
                tag,
            },
            fields,
        });
        let ptr = NonNull::from(Box::leak(node)).cast::<Header>();
        // An allocation never sits in the first page; were one to, a node
        // would read as a constructor without fields.
        assert!(ptr.as_ptr().addr() > MAX_TAGS);
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
            self.header().tag
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
    /// use of the fields happen before they are dropped.
    #[cold]
    #[inline(never)]
    fn drop_slow(&mut self, tag: u32) {
        atomic::fence(Ordering::Acquire);
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
    unsafe { S::free(node, header.tag, pending) }
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
/// returns (see the module docs): the call until it is made, then what it
/// returned. The first look at the value makes the call, on whichever thread
/// looks first; a look from another thread meanwhile waits for it, and every
/// later look reads the result. Making the call uses up its arguments. The
/// arguments and the function follow in the node ([`Deferred`]), of types
/// only `calls` knows.
#[repr(C)]
struct Lazy<S: 'static> {
    once: Once,
    /// Written once, by the call, under `once`.
    value: UnsafeCell<MaybeUninit<S>>,
    calls: &'static Calls<S>,
}

/// What a node of one [`Deferred`] type does with its call.
struct Calls<S> {
    /// Makes the call and writes `value`. Run once, under `once`.
    make: unsafe fn(NonNull<Header>),
    /// Frees the node whose count reached 0.
    free: unsafe fn(NonNull<Header>, Pending<'_>),
    phantom: PhantomData<fn() -> S>,
}

#[repr(C)]
struct Deferred<S: 'static, A, F> {
    lazy: Lazy<S>,
    /// Moved out by the call.
    args: UnsafeCell<ManuallyDrop<A>>,
    run: F,
}

impl<S: ThinSum + HandOver, A: HandOver, F: FnOnce(A) -> S + Copy> Deferred<S, A, F> {
    const CALLS: &'static Calls<S> = &Calls {
        make: Self::make,
        free: Self::free,
        phantom: PhantomData,
    };

    /// # Safety
    /// `node` is a node of this type, `once` runs this, and no one else
    /// reads the arguments.
    unsafe fn make(node: NonNull<Header>) {
        let node = node.cast::<Node<Self>>().as_ptr();
        let deferred = unsafe { &raw const (*node).fields };
        let args = unsafe { ManuallyDrop::take(&mut *(*deferred).args.get()) };
        let value = unsafe { ((*deferred).run)(args) };
        unsafe { (*(*deferred).lazy.value.get()).write(value) };
    }

    /// # Safety
    /// `node` is a node of this type whose count is 0.
    unsafe fn free(node: NonNull<Header>, pending: Pending<'_>) {
        let node = node.cast::<Node<Self>>().as_ptr();
        let deferred = unsafe { &raw mut (*node).fields };
        let lazy = unsafe { &raw mut (*deferred).lazy };
        // With no owner left no call is under way: the call was made (the
        // value is there), began and panicked (nothing is), or never began
        // (the arguments are). Only in the last two does this run.
        let mut made = true;
        unsafe { &(*lazy).once }.call_once_force(|state| {
            made = false;
            if !state.is_poisoned() {
                let args = unsafe { ManuallyDrop::take(&mut *(*deferred).args.get()) };
                args.hand_over(pending);
            }
        });
        if made {
            unsafe { (*(*lazy).value.get()).assume_init_read() }.hand_over(pending);
        }
        drop(unsafe { Box::from_raw(node.cast::<ManuallyDrop<Node<Self>>>()) });
    }
}

impl<S: ThinSum + HandOver> Thin<S> {
    /// A node of the hidden constructor `tag`, holding the call `run(args)`.
    ///
    /// # Safety
    /// `tag` is the tag of the hidden constructor of `S`, and `S::free`
    /// frees a node of it with [`Thin::free_deferred`].
    #[inline]
    pub unsafe fn deferred<A, F>(tag: u32, args: A, run: F) -> Self
    where
        A: HandOver + Send + Sync + 'static,
        F: FnOnce(A) -> S + Copy + Send + Sync + 'static,
    {
        // `made` reads a `Lazy` at the start of the fields, wherever the
        // whole `Deferred` puts its fields.
        const {
            assert!(
                std::mem::offset_of!(Node<Deferred<S, A, F>>, fields)
                    == std::mem::offset_of!(Node<Lazy<S>>, fields)
            )
        };
        let fields = Deferred {
            lazy: Lazy {
                once: Once::new(),
                value: UnsafeCell::new(MaybeUninit::uninit()),
                calls: Deferred::<S, A, F>::CALLS,
            },
            args: UnsafeCell::new(ManuallyDrop::new(args)),
            run,
        };
        unsafe { Self::new(tag, fields) }
    }

    /// What the call of a node of the hidden constructor returned, making
    /// it if no one has yet.
    ///
    /// # Safety
    /// The value is of the hidden constructor, made by [`Thin::deferred`].
    #[inline]
    pub unsafe fn made(&self) -> &S {
        let lazy = unsafe { &raw const (*self.ptr.cast::<Node<Lazy<S>>>().as_ptr()).fields };
        let calls = unsafe { (*lazy).calls };
        unsafe { &(*lazy).once }.call_once(|| unsafe { (calls.make)(self.ptr) });
        // Written by the call, which `once` has seen through.
        unsafe { (*(*lazy).value.get()).assume_init_ref() }
    }

    /// For [`ThinSum::free`]: frees a node of the hidden constructor.
    ///
    /// # Safety
    /// The node was made by [`Thin::deferred`] and its count is 0.
    #[inline]
    pub unsafe fn free_deferred(node: NonNull<Header>, pending: Pending<'_>) {
        let lazy = unsafe { &raw const (*node.cast::<Node<Lazy<S>>>().as_ptr()).fields };
        unsafe { ((*lazy).calls.free)(node, pending) }
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
    /// As `Arc::drop`: a `Release` decrement. The tag is read BEFORE it:
    /// once the count drops another owner may free the node.
    #[inline]
    fn drop(&mut self) {
        if self.is_empty_ctor() {
            return;
        }
        let tag = self.header().tag;
        if self.header().count.fetch_sub(1, Ordering::Release) != 1 {
            return;
        }
        self.drop_slow(tag);
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

            unsafe fn free(
                node: ::std::ptr::NonNull<$crate::thin::Header>,
                tag: u32,
                pending: $crate::thin::Pending<'_>,
            ) {
                match tag {
                    $( $tag => $crate::thin_sum!(@free $name node pending $( $fields )?), )+
                    $( $lazy => unsafe { $crate::thin::Thin::<$name>::free_deferred(node, pending) }, )?
                    _ => unreachable!("no constructor has tag {tag}"),
                }
            }
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

            /// What the hidden constructor's call returned, making the call
            /// if no one has yet (`aver_rt::thin::Thin::deferred`); the
            /// value itself when it is of a declared constructor, as every
            /// value of a sum no `lazy f(…)` returns is.
            #[inline]
            fn made(&self) -> &Self {
                let value = self;
                $(
                    let mut value = value;
                    while value.0.tag() == $lazy {
                        // SAFETY: a node of tag `$lazy` is made only by `deferred`.
                        value = unsafe { value.0.made() };
                    }
                )?
                value
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
            )?

            /// The constructor and its fields, to match on.
            #[inline]
            $vis fn view(&self) -> $view<'_> {
                let this = self.made();
                match this.0.tag() {
                    $( $tag => $view::$ctor $( (unsafe { this.0.fields::<$fields>() }) )?, )+
                    // Safe code makes a value only through the constructors
                    // above, whose tags are these, checked at compile time.
                    _ => unsafe { ::std::hint::unreachable_unchecked() },
                }
            }

            /// Both values' fields when they are of the same constructor.
            #[inline]
            $vis fn view_pair<'a>(a: &'a Self, b: &'a Self) -> ::std::option::Option<$pair<'a>> {
                let (a, b) = (a.made(), b.made());
                let tag = a.0.tag();
                if tag != b.0.tag() {
                    return None;
                }
                Some(match tag {
                    $( $tag => $pair::$ctor $( (unsafe { a.0.fields::<$fields>() }, unsafe { b.0.fields::<$fields>() }) )?, )+
                    // As in `view`.
                    _ => unsafe { ::std::hint::unreachable_unchecked() },
                })
            }

            /// The tag of the value's constructor (its declaration index).
            #[inline]
            $vis fn tag(&self) -> u32 {
                self.made().0.tag()
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
                $crate::thin::Thin::addr(&self.made().0)
            }

            #[inline]
            fn count(&self) -> usize {
                $crate::thin::Thin::strong_count(&self.made().0)
            }
        }
    };

    (@free $name:ident $node:ident $pending:ident $fields:ty) => {
        unsafe { $crate::thin::Thin::<$name>::free_node::<$fields>($node, $pending) }
    };
    (@free $name:ident $node:ident $pending:ident) => {
        unreachable!("a constructor without fields has no node")
    };
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
        fn walk_eq(&self, other: &Self, walk: Walk<'_>) -> bool {
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
    #[derive(Debug, PartialEq)]
    pub struct L__Leaf(u64);
    #[derive(Debug)]
    pub struct L__Node(L, L);

    impl super::HandOver for L__Leaf {}

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
        } lazy 2
    }

    impl std::fmt::Debug for L {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            std::fmt::Debug::fmt(&self.view(), f)
        }
    }

    impl WalkEq for L {
        fn walk_eq(&self, other: &Self, walk: Walk<'_>) -> bool {
            match L::view_pair(self, other) {
                None => false,
                Some(L__Pairs::Leaf(a, b)) => a == b,
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
            L__View::Leaf(_) => unreachable!(),
        };
        let again = below.clone();
        for _ in 0..10 {
            assert!(matches!(below.view(), L__View::Leaf(leaf) if leaf.0 == 0));
            assert!(matches!(again.view(), L__View::Leaf(_)));
            assert!(walk_eq(&below, &again));
        }
        assert_eq!(MADE.load(Ordering::SeqCst), 1);
        // The node it returned stands for it: one node, compared at once.
        use crate::NodeId;
        let made = match below.view() {
            L__View::Leaf(_) => below.addr(),
            L__View::Node(_) => unreachable!(),
        };
        assert_eq!(made, again.addr());
        assert_ne!(super::Thin::addr(&below.0), made);
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
