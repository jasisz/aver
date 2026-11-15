//! Nodes born before the first fork, frozen by one scan of their heap.
//!
//! # The rule
//!
//! Until the first `(a, b)!` (or the first task begun on the native work
//! path) freezes (see [`crate::freeze::at_first_fork`]), every node of a
//! recursive sum that one thread makes, the *owner* (the first thread that
//! makes one), is allocated in a mimalloc heap of its own: the born heap. It
//! is counted as any other node, so one dropped before the fork is freed as
//! any other. When the owner reaches the first fork, it visits the born
//! heap's pages once, block by block, and freezes every node still alive
//! there: no pointer is followed, and the freezing walk that follows stops
//! at those nodes (it stops at frozen nodes). From then on new nodes go to
//! the ordinary heap, and nothing about counts changes: a clone or a drop
//! tests the sign bit of the count as before.
//!
//! A program that reads a large structure before it forks (a checker
//! building its environment) thereby skips a walk that chases a pointer per
//! node, and freezes nodes the walk did not reach, such as those behind a
//! function value.
//!
//! # Why it is safe
//!
//! The scan writes only counts, and only in blocks that hold a node: a block
//! is in the born heap only when [`alloc`] gave it to [`crate::thin::Thin`]
//! for a node, which is written before anything else runs on that thread.
//! The danger is a block another thread frees while the scan reads it,
//! whose first word mimalloc then reuses for its free list. That cannot
//! happen:
//!
//! - Only the owner allocates in the born heap. Any other thread that makes a
//!   node before the first fork ends the born phase ([`end`]) and allocates
//!   as usual; the born nodes are then never scanned and stay counted.
//! - A block of the born heap is a node of a recursive sum, and it goes back
//!   to mimalloc only through the code of [`crate::thin::Thin`] that frees a
//!   node: the drop of its last owner (`drop_slow`, which frees every node
//!   that drop leaves without owners), a `take` that empties it, and a lazy
//!   call that moves a node's fields out (`fill_with`). Each calls
//!   [`on_free`] first: while the born phase lasts, a thread other than the
//!   owner ends it before anything is given back. Ending it is a Dekker
//!   handshake with the scan on `ENDED` and `SCANNING` (sequentially
//!   consistent): either the scan sees `ENDED` and does not run, or the
//!   ending thread sees `SCANNING` and waits until the scan is over. The
//!   memory goes back through the global allocator, which must be
//!   [`crate::MiMalloc`] for the born heap to be used at all (checked once,
//!   by asking mimalloc whether a block the global allocator gave is its
//!   own; a program on the system allocator never uses it). `on_free` reads
//!   the owner from `WATCH` (acquire), which holds it only while the phase
//!   lasts and is cleared (release) only after the scan, so a free that
//!   skips the handshake happens after the scan. Once the phase is over the
//!   test is one load and a branch per drop that frees, not per node.
//! - The owner itself does not free while it scans, and if it exits before
//!   the first fork the born phase ends with it, so a later thread that gets
//!   the same thread pointer cannot scan.
//! - A node another thread still decrements meanwhile: the scan sets the
//!   mark with a compare-and-swap from the count it read, nonzero and below
//!   the mark. If the last decrement comes first, the swap fails and the
//!   block is left alone; if the swap comes first, the decrement finds the
//!   count frozen (or not its last) and frees nothing. A node that a
//!   compare-and-swap of `take` empties for reuse is likewise either taken
//!   (its count 0, which the scan leaves alone) or frozen, never both; its
//!   memory then goes back through the take's own free, or a new node is written into
//!   it whose header (`Thin::new_in`) is set by atomic stores, so a scan
//!   that reads the count meanwhile reads 0 or the new node's count, and
//!   freezing a live node is always safe.
//!
//! A frozen node is never freed, and anything a frozen node holds is not
//! freed while the node holds it, as for any frozen node (see
//! [`crate::freeze`]).
//!
//! # Why the leak is bounded
//!
//! A node alive at the fork that no thread uses afterwards is never freed:
//! the leak is at most what the owner's nodes alive at the first fork take,
//! once per process. A program that never forks never scans and leaks
//! nothing.

use std::cell::Cell;
use std::ffi::c_void;
use std::sync::atomic::{AtomicBool, AtomicPtr, AtomicU32, AtomicUsize, Ordering};

use crate::counts::frozen::F32;

#[repr(C)]
struct HeapArea {
    blocks: *mut c_void,
    reserved: usize,
    committed: usize,
    used: usize,
    block_size: usize,
    full_block_size: usize,
    reserved1: *mut c_void,
}

type Visitor =
    extern "C" fn(*const c_void, *const HeapArea, *mut c_void, usize, *mut c_void) -> bool;

unsafe extern "C" {
    fn mi_heap_new() -> *mut c_void;
    fn mi_heap_malloc(heap: *mut c_void, size: usize) -> *mut c_void;
    fn mi_heap_malloc_small(heap: *mut c_void, size: usize) -> *mut c_void;
    fn mi_heap_visit_blocks(
        heap: *mut c_void,
        visit_blocks: bool,
        visitor: Visitor,
        arg: *mut c_void,
    ) -> bool;
    fn mi_is_in_heap_region(p: *const c_void) -> bool;
}

/// `MI_SMALL_SIZE_MAX` in mimalloc: 128 words.
const SMALL: usize = 128 * size_of::<usize>();

/// The born phase may still be on: nodes go to the born heap, and a free
/// by another thread ends it. False once it is over, for good.
static ACTIVE: AtomicBool = AtomicBool::new(true);
/// The born phase was ended without a scan, or a scan has started: no
/// (further) scan runs.
static ENDED: AtomicBool = AtomicBool::new(false);
/// The owner is scanning the born heap.
static SCANNING: AtomicBool = AtomicBool::new(false);
/// The owner, by [`thread_word`]; 0 before the first node.
static OWNER: AtomicUsize = AtomicUsize::new(0);
/// The owner while the born phase lasts, for [`on_free`]; 0 before the
/// first node and once the phase is over.
static WATCH: AtomicUsize = AtomicUsize::new(0);
/// The born heap, made with the first node.
static HEAP: AtomicPtr<c_void> = AtomicPtr::new(std::ptr::null_mut());
/// How many nodes the scan froze, for tests.
static SCANNED: AtomicUsize = AtomicUsize::new(0);

/// A word unique to the running thread among the live ones, read without a
/// call where the platform keeps one in a register.
#[inline(always)]
fn thread_word() -> usize {
    #[cfg(all(target_vendor = "apple", target_arch = "aarch64"))]
    {
        let tp: usize;
        // SAFETY: reads the thread's own register.
        unsafe {
            std::arch::asm!("mrs {}, tpidrro_el0", out(reg) tp, options(nomem, nostack, preserves_flags))
        };
        tp
    }
    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    {
        let tp: usize;
        // SAFETY: the first word of the thread control block is its own
        // address.
        unsafe {
            std::arch::asm!("mov {}, fs:0", out(reg) tp, options(nostack, preserves_flags, readonly))
        };
        tp
    }
    #[cfg(all(target_os = "linux", target_arch = "aarch64"))]
    {
        let tp: usize;
        // SAFETY: reads the thread's own register.
        unsafe {
            std::arch::asm!("mrs {}, tpidr_el0", out(reg) tp, options(nomem, nostack, preserves_flags))
        };
        tp
    }
    #[cfg(not(any(
        all(target_vendor = "apple", target_arch = "aarch64"),
        all(
            target_os = "linux",
            any(target_arch = "x86_64", target_arch = "aarch64")
        )
    )))]
    {
        thread_local! {
            static WORD: u8 = const { 0 };
        }
        WORD.with(|w| w as *const u8 as usize)
    }
}

/// Ends the born phase on the owner's exit, so that no later thread scans.
struct OwnerExit;

impl Drop for OwnerExit {
    fn drop(&mut self) {
        end();
    }
}

thread_local! {
    static OWNER_EXIT: Cell<Option<OwnerExit>> = const { Cell::new(None) };
}

/// Ends the born phase: no scan runs after this returns, and none is
/// running. Waits for a scan that is.
#[cold]
#[inline(never)]
pub fn end() {
    if !ACTIVE.load(Ordering::Acquire) {
        return;
    }
    ENDED.store(true, Ordering::SeqCst);
    while SCANNING.load(Ordering::SeqCst) {
        std::thread::yield_now();
    }
    WATCH.store(0, Ordering::Release);
    ACTIVE.store(false, Ordering::Release);
}

/// Makes the calling thread the owner if there is none yet: whether it is
/// the owner. A second thread ends the born phase.
#[cold]
#[inline(never)]
fn claim(me: usize) -> bool {
    match OWNER.compare_exchange(0, me, Ordering::SeqCst, Ordering::SeqCst) {
        Ok(_) => {
            WATCH.store(me, Ordering::SeqCst);
            if !ACTIVE.load(Ordering::SeqCst) {
                // Ended meanwhile by another thread: nothing to watch.
                WATCH.store(0, Ordering::Release);
            }
            OWNER_EXIT.with(|exit| exit.set(Some(OwnerExit)));
            true
        }
        Err(owner) if owner == me => true,
        Err(_) => {
            end();
            false
        }
    }
}

/// The born heap, made on first use; null when the global allocator is not
/// mimalloc, which ends the born phase.
#[cold]
#[inline(never)]
fn heap() -> *mut c_void {
    let h = HEAP.load(Ordering::Acquire);
    if !h.is_null() {
        return h;
    }
    // A born node is freed through the global allocator: it must be
    // mimalloc. A test binary, for one, keeps the system allocator.
    let probe = Box::new(0u64);
    // SAFETY: only asks whether the address is in mimalloc's arenas.
    let mimalloc = unsafe { mi_is_in_heap_region((&raw const *probe).cast()) };
    drop(probe);
    if !mimalloc {
        end();
        return std::ptr::null_mut();
    }
    // SAFETY: a new heap, used by the owner alone.
    let new = unsafe { mi_heap_new() };
    match HEAP.compare_exchange(
        std::ptr::null_mut(),
        new,
        Ordering::AcqRel,
        Ordering::Acquire,
    ) {
        Ok(_) => new,
        Err(seen) => seen,
    }
}

/// Memory for a node of `size` bytes aligned to at most 16 in the born
/// heap, or null when the node is to be allocated as usual.
#[inline(always)]
pub fn alloc(size: usize) -> *mut u8 {
    if !ACTIVE.load(Ordering::Relaxed) {
        return std::ptr::null_mut();
    }
    let me = thread_word();
    if OWNER.load(Ordering::Relaxed) != me && !claim(me) {
        return std::ptr::null_mut();
    }
    let mut h = HEAP.load(Ordering::Relaxed);
    if h.is_null() {
        h = heap();
        if h.is_null() {
            return std::ptr::null_mut();
        }
    }
    // SAFETY: the owner allocating in its heap.
    unsafe {
        if size <= SMALL {
            mi_heap_malloc_small(h, size).cast()
        } else {
            mi_heap_malloc(h, size).cast()
        }
    }
}

/// Called by the global allocator before it gives back a block: a thread
/// other than the owner ends the born phase first (see the module docs).
#[inline(always)]
pub fn on_free() {
    let owner = WATCH.load(Ordering::Acquire);
    if owner != 0 && owner != thread_word() {
        end();
    }
}

extern "C" fn freeze_block(
    _heap: *const c_void,
    _area: *const HeapArea,
    block: *mut c_void,
    _size: usize,
    frozen: *mut c_void,
) -> bool {
    if block.is_null() {
        return true;
    }
    // SAFETY: every block of the born heap holds a node, whose header
    // starts with its 32-bit count, and no block is given back meanwhile.
    let count = unsafe { &*(block as *const AtomicU32) };
    let mut now = count.load(Ordering::Relaxed);
    while now != 0 && now < F32 {
        match count.compare_exchange_weak(now, now | F32, Ordering::Relaxed, Ordering::Relaxed) {
            Ok(_) => {
                // SAFETY: `frozen` is the scan's counter.
                unsafe { *(frozen as *mut usize) += 1 };
                break;
            }
            Err(seen) => now = seen,
        }
    }
    true
}

/// Ends the born phase at the first fork: on the owner, every node alive
/// in the born heap is frozen first. Returns how many.
pub fn freeze_born() -> usize {
    if !ACTIVE.load(Ordering::Acquire) {
        return 0;
    }
    let h = HEAP.load(Ordering::Acquire);
    if h.is_null() || OWNER.load(Ordering::SeqCst) != thread_word() {
        end();
        return 0;
    }
    SCANNING.store(true, Ordering::SeqCst);
    let mut frozen = 0usize;
    if !ENDED.swap(true, Ordering::SeqCst) {
        // SAFETY: the owner visits its heap; no block of it is given back
        // or allocated meanwhile (see the module docs).
        unsafe { mi_heap_visit_blocks(h, true, freeze_block, (&raw mut frozen).cast()) };
        SCANNED.fetch_add(frozen, Ordering::Relaxed);
    }
    SCANNING.store(false, Ordering::SeqCst);
    WATCH.store(0, Ordering::Release);
    ACTIVE.store(false, Ordering::Release);
    frozen
}

/// How many nodes the scan froze, for tests.
#[doc(hidden)]
pub fn scanned() -> usize {
    SCANNED.load(Ordering::Relaxed)
}

/// Whether the born phase is still on, for tests.
#[doc(hidden)]
pub fn active() -> bool {
    ACTIVE.load(Ordering::Acquire)
}
