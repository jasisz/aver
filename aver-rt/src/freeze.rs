//! Freezing what an independent product hands to its threads.
//!
//! # The rule
//!
//! The first time an `(a, b)!` at a given place in the program runs, before
//! it starts its threads, everything the elements' arguments lead to is
//! frozen: each node's count is moved past a mark (see
//! [`crate::counts::frozen`]) at which no one counts the node any more and
//! no one frees it. Threads that then share those values, such as the
//! environment a checker hands to its workers, no longer write to the
//! counts' cache lines on every copy and drop. A place freezes once: the
//! next time it runs its values are left as they are.
//!
//! The walk goes from the leaves up: a node is frozen only once everything
//! it holds is, and it stops at nodes already frozen and at nodes kept in
//! statics, whose counts are past the mark too. A call deferred and not yet
//! made is frozen as it is, without walking its arguments or making the
//! call. A value of a type that cannot be walked (a function value) is left
//! as it is, and so is every node that holds it.
//!
//! # Why it is safe
//!
//! A frozen node is never freed, so no thread can see it freed while it
//! still holds it. Nothing it holds is freed either: everything below a
//! frozen node is frozen, because the walk freezes a node only after its
//! children. Freezing adds the mark to the count rather than storing it,
//! so an increment or decrement another thread makes at the same moment is
//! kept, and the count stays past the mark. A frozen node always reads as
//! shared, so nothing changes it in place: a write copies it first, as for
//! any node with several owners.
//!
//! # Why the leak is bounded
//!
//! What is frozen is never freed, so freezing leaks. It leaks at most:
//!
//! - once per place in the program, of which there are as many as `(a, b)!`
//!   in its source, however many times each runs: a program that hands
//!   fresh data to its threads in a loop freezes the first round's only;
//! - a quarter of the machine's physical memory over the whole process.
//!   Past it a node is left as it is, and so is every node above it, which
//!   is then counted and freed as before: what leaks is exactly the frozen
//!   nodes, within the budget.
//!
//! Only a build with atomic counts (`[rust] threads = "many"`) freezes; with
//! `plain-counts` no code calls this module and it is not compiled.

use std::sync::OnceLock;
use std::sync::atomic::{AtomicBool, AtomicIsize, AtomicU32, AtomicUsize, Ordering};

use crate::counts::frozen::{F32, F64};

/// The bytes the process may still freeze: a quarter of the machine's
/// physical memory to begin with.
fn remaining() -> &'static AtomicIsize {
    static LEFT: OnceLock<AtomicIsize> = OnceLock::new();
    LEFT.get_or_init(|| {
        let budget = physical_memory() / 4;
        AtomicIsize::new(budget.min(isize::MAX as u64) as isize)
    })
}

/// Lowers what the process may still freeze to at most `bytes`. For tests
/// of the budget only; nothing in a program calls it.
#[doc(hidden)]
pub fn limit_budget_for_tests(bytes: usize) {
    let left = remaining();
    let bytes = bytes.min(isize::MAX as usize) as isize;
    let _ = left.fetch_min(bytes, Ordering::Relaxed);
}

/// The machine's physical memory in bytes, or 4 GiB when it cannot be read.
fn physical_memory() -> u64 {
    #[cfg(target_vendor = "apple")]
    {
        unsafe extern "C" {
            fn sysctlbyname(
                name: *const std::ffi::c_char,
                old: *mut std::ffi::c_void,
                oldlen: *mut usize,
                new: *const std::ffi::c_void,
                newlen: usize,
            ) -> i32;
        }
        let mut size: u64 = 0;
        let mut len = size_of::<u64>();
        // SAFETY: `hw.memsize` is a 64-bit integer, written into `size`.
        let r = unsafe {
            sysctlbyname(
                c"hw.memsize".as_ptr(),
                (&raw mut size).cast(),
                &mut len,
                std::ptr::null(),
                0,
            )
        };
        if r == 0 && size > 0 {
            return size;
        }
    }
    #[cfg(all(target_os = "linux", not(target_family = "wasm")))]
    {
        unsafe extern "C" {
            fn sysconf(name: i32) -> i64;
        }
        // `_SC_PHYS_PAGES` and `_SC_PAGESIZE` on Linux.
        // SAFETY: `sysconf` only reads.
        let (pages, page) = unsafe { (sysconf(85), sysconf(30)) };
        if pages > 0 && page > 0 {
            return pages as u64 * page as u64;
        }
    }
    4 << 30
}

/// Adds `mark` to `count` unless it is past it already, keeping any change
/// another thread makes to it meanwhile.
macro_rules! add_mark {
    ($count:expr, $mark:expr) => {{
        let count = $count;
        let mut now = count.load(Ordering::Relaxed);
        while now < $mark {
            match count.compare_exchange_weak(
                now,
                now + $mark,
                Ordering::Relaxed,
                Ordering::Relaxed,
            ) {
                Ok(_) => break,
                Err(seen) => now = seen,
            }
        }
    }};
}

/// One freezing walk.
pub struct Freezer {
    /// The budget ran out: nothing more is frozen.
    spent: bool,
}

impl Freezer {
    /// Takes `bytes` from the budget: false, and the walk freezes nothing
    /// more, past it.
    #[inline]
    fn take(&mut self, bytes: usize) -> bool {
        let bytes = bytes as isize;
        if remaining().fetch_sub(bytes, Ordering::Relaxed) < bytes {
            remaining().fetch_add(bytes, Ordering::Relaxed);
            self.spent = true;
            return false;
        }
        true
    }

    /// A node with a 64-bit count, of about `bytes` bytes, holding what
    /// `children` freezes: whether it is frozen now.
    #[inline]
    pub(crate) fn node64(
        &mut self,
        count: &AtomicUsize,
        bytes: usize,
        children: impl FnOnce(&mut Freezer) -> bool,
    ) -> bool {
        if count.load(Ordering::Relaxed) >= F64 {
            return true;
        }
        if self.spent || !children(self) || !self.take(bytes) {
            return false;
        }
        add_mark!(count, F64);
        true
    }

    /// [`Freezer::node64`] for a node of a recursive sum.
    #[inline]
    pub(crate) fn node32(
        &mut self,
        count: &AtomicU32,
        bytes: usize,
        children: impl FnOnce(&mut Freezer) -> bool,
    ) -> bool {
        if count.load(Ordering::Relaxed) >= F32 {
            return true;
        }
        if self.spent || !children(self) || !self.take(bytes) {
            return false;
        }
        add_mark!(count, F32);
        true
    }

    /// Freezes `value` and all it leads to: whether all of it is frozen.
    #[inline]
    pub fn freeze<T: Freeze + ?Sized>(&mut self, value: &T) -> bool {
        value.freeze(self)
    }
}

/// Runs `walk` for the place `site` of an independent product the first
/// time it runs there (see the module docs), unless the budget is spent.
pub fn at_first_fork(site: &AtomicBool, walk: impl FnOnce(&mut Freezer)) {
    if site.load(Ordering::Relaxed) || site.swap(true, Ordering::Relaxed) {
        return;
    }
    let mut f = Freezer {
        spent: remaining().load(Ordering::Relaxed) <= 0,
    };
    if !f.spent {
        walk(&mut f);
    }
}

/// A value whose nodes can be frozen; true when all of them are.
pub trait Freeze {
    fn freeze(&self, f: &mut Freezer) -> bool;
}

/// For generated code: freezes a capture whose type has [`Freeze`]
/// (`(&Probe(&x)).freeze_into(f)`), and skips one whose type has not
/// ([`ViaSkip`]), which method lookup picks only then.
pub struct Probe<'a, T: ?Sized>(pub &'a T);

pub trait ViaFreeze {
    fn freeze_into(&self, f: &mut Freezer) -> bool;
}

impl<T: Freeze + ?Sized> ViaFreeze for Probe<'_, T> {
    #[inline]
    fn freeze_into(&self, f: &mut Freezer) -> bool {
        self.0.freeze(f)
    }
}

/// A value of a type without [`Freeze`] is not frozen, and neither is
/// anything that holds it.
pub trait ViaSkip {
    fn freeze_into(&self, f: &mut Freezer) -> bool;
}

impl<T: ?Sized> ViaSkip for &Probe<'_, T> {
    #[inline]
    fn freeze_into(&self, _: &mut Freezer) -> bool {
        false
    }
}

macro_rules! leaf {
    ($($t:ty),* $(,)?) => {
        $( impl Freeze for $t {
            #[inline]
            fn freeze(&self, _: &mut Freezer) -> bool {
                true
            }
        } )*
    };
}

leaf!(
    (),
    bool,
    char,
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    u8,
    u16,
    u32,
    u64,
    u128,
    usize,
    f32,
    f64,
    String,
    crate::AverInt,
    crate::Count,
);

impl<T: Freeze + ?Sized> Freeze for &T {
    #[inline]
    fn freeze(&self, f: &mut Freezer) -> bool {
        (**self).freeze(f)
    }
}

impl<T: Freeze> Freeze for Option<T> {
    #[inline]
    fn freeze(&self, f: &mut Freezer) -> bool {
        self.as_ref().is_none_or(|v| v.freeze(f))
    }
}

impl<T: Freeze, E: Freeze> Freeze for Result<T, E> {
    #[inline]
    fn freeze(&self, f: &mut Freezer) -> bool {
        match self {
            Ok(v) => v.freeze(f),
            Err(e) => e.freeze(f),
        }
    }
}

impl<T: Freeze + ?Sized> Freeze for Box<T> {
    #[inline]
    fn freeze(&self, f: &mut Freezer) -> bool {
        (**self).freeze(f)
    }
}

impl<T: Freeze> Freeze for Vec<T> {
    fn freeze(&self, f: &mut Freezer) -> bool {
        self.iter().all(|v| v.freeze(f))
    }
}

impl<T: Freeze> Freeze for [T] {
    fn freeze(&self, f: &mut Freezer) -> bool {
        self.iter().all(|v| v.freeze(f))
    }
}

macro_rules! tuples {
    ($( ( $($n:tt $t:ident),+ ) )*) => {
        $( impl<$($t: Freeze),+> Freeze for ($($t,)+) {
            #[inline]
            fn freeze(&self, f: &mut Freezer) -> bool {
                $( self.$n.freeze(f) )&&+
            }
        } )*
    };
}

tuples! {
    (0 A)
    (0 A, 1 B)
    (0 A, 1 B, 2 C)
    (0 A, 1 B, 2 C, 3 D)
    (0 A, 1 B, 2 C, 3 D, 4 E)
    (0 A, 1 B, 2 C, 3 D, 4 E, 5 F)
    (0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G)
    (0 A, 1 B, 2 C, 3 D, 4 E, 5 F, 6 G, 7 H)
}

impl<T: Freeze> Freeze for crate::Shared<T> {
    #[inline]
    fn freeze(&self, f: &mut Freezer) -> bool {
        f.node64(crate::Shared::count_ref(self), size_of::<T>() + 8, |f| {
            (**self).freeze(f)
        })
    }
}

impl<T: crate::Counts + Freeze> Freeze for crate::Counted<T> {
    #[inline]
    fn freeze(&self, f: &mut Freezer) -> bool {
        f.node64(crate::Counted::count_ref(self), size_of::<T>(), |f| {
            (**self).freeze(f)
        })
    }
}

/// For [`crate::thin_sum!`]: freezes the node of `this` once `children`
/// froze what it holds; a constructor without fields has no node.
#[inline]
pub fn thin_node<S: crate::thin::ThinSum>(
    this: &crate::thin::Thin<S>,
    bytes: usize,
    f: &mut Freezer,
    children: impl FnOnce(&mut Freezer) -> bool,
) -> bool {
    match crate::thin::Thin::count_ref(this) {
        None => true,
        Some(count) => f.node32(count, bytes, children),
    }
}

/// The freezing of a thin sum, which `thin_sum!` expands to.
#[doc(hidden)]
#[macro_export]
macro_rules! __thin_freeze {
    ($name:ident; $( $tag:literal $( ( $fields:ty ) )? ),+ ; $( $lazy:literal )?) => {
        impl $crate::freeze::Freeze for $name {
            fn freeze(&self, f: &mut $crate::freeze::Freezer) -> bool {
                // A call not yet made is frozen as it is.
                $(
                    if self.0.lazy_tag() == $lazy {
                        return $crate::freeze::thin_node(&self.0, 16, f, |_| true);
                    }
                )?
                let this = &self.0;
                match this.tag() & 0xff {
                    $( $tag => $crate::__thin_freeze!(@node f this $tag $( $fields )?), )+
                    _ => true,
                }
            }
        }
    };
    (@node $f:ident $this:ident $tag:literal $fields:ty) => {
        $crate::freeze::thin_node($this, ::std::mem::size_of::<$fields>() + 8, $f, |f| {
            #[allow(unused_imports)]
            use $crate::freeze::{ViaFreeze as _, ViaSkip as _};
            // Fields of a type that cannot be frozen leave the node as it is.
            (&$crate::freeze::Probe(unsafe { $this.fields::<$fields>() })).freeze_into(f)
        })
    };
    (@node $f:ident $this:ident $tag:literal) => {
        true
    };
}
