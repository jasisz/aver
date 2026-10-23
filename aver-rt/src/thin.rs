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
//! The atomic operations and their orderings are those `Shared` uses, which
//! are `std::sync::Arc`'s strong count (library/alloc/src/sync.rs). The count
//! is 32 bits: a clone past `i32::MAX` aborts, as `Arc` does past
//! `isize::MAX`; reaching it takes two billion owners of one node, 16 GB of
//! pointers to it alone.

use std::marker::PhantomData;
use std::mem::ManuallyDrop;
use std::ptr::NonNull;
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
        }
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
            let tags: &[u32] = &[$( $tag ),+];
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

            /// The constructor and its fields, to match on.
            #[inline]
            $vis fn view(&self) -> $view<'_> {
                match self.0.tag() {
                    $( $tag => $view::$ctor $( (unsafe { self.0.fields::<$fields>() }) )?, )+
                    // Safe code makes a value only through the constructors
                    // above, whose tags are these, checked at compile time.
                    _ => unsafe { ::std::hint::unreachable_unchecked() },
                }
            }

            /// Both values' fields when they are of the same constructor.
            #[inline]
            $vis fn view_pair<'a>(a: &'a Self, b: &'a Self) -> ::std::option::Option<$pair<'a>> {
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
                self.0.tag()
            }

            /// Whether both are the same node, or the same constructor
            /// without fields.
            #[inline]
            $vis fn ptr_eq(a: &Self, b: &Self) -> bool {
                $crate::thin::Thin::ptr_eq(&a.0, &b.0)
            }
        }

        impl $crate::NodeId for $name {
            #[inline]
            fn addr(&self) -> usize {
                $crate::thin::Thin::addr(&self.0)
            }

            #[inline]
            fn count(&self) -> usize {
                $crate::thin::Thin::strong_count(&self.0)
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
