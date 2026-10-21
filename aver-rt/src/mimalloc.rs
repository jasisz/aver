//! mimalloc as a global allocator, for `[rust] allocator = "mimalloc"`.
//!
//! Requests whose alignment mimalloc's size classes already give go to the
//! plain `mi_malloc` family, the way std sends them to `malloc` for the system
//! allocator. Every block of 16 bytes or more is 16-aligned and smaller ones
//! are aligned to their size. Passing every request to `mi_malloc_aligned`
//! instead over-allocates by `align - 1` whenever the matching page is full,
//! so a 48-byte value would land in a 64-byte block.

use std::alloc::{GlobalAlloc, Layout};
use std::ffi::c_void;

use libmimalloc_sys::{
    mi_free, mi_malloc, mi_malloc_aligned, mi_realloc, mi_realloc_aligned, mi_zalloc,
    mi_zalloc_aligned,
};

/// `MI_MAX_ALIGN_SIZE` in mimalloc's `types.h`.
const MI_MAX_ALIGN_SIZE: usize = 16;

fn natural(align: usize, size: usize) -> bool {
    align <= MI_MAX_ALIGN_SIZE && align <= size
}

pub struct MiMalloc;

unsafe impl GlobalAlloc for MiMalloc {
    #[inline]
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        unsafe {
            if natural(layout.align(), layout.size()) {
                mi_malloc(layout.size()) as *mut u8
            } else {
                mi_malloc_aligned(layout.size(), layout.align()) as *mut u8
            }
        }
    }

    #[inline]
    unsafe fn alloc_zeroed(&self, layout: Layout) -> *mut u8 {
        unsafe {
            if natural(layout.align(), layout.size()) {
                mi_zalloc(layout.size()) as *mut u8
            } else {
                mi_zalloc_aligned(layout.size(), layout.align()) as *mut u8
            }
        }
    }

    #[inline]
    unsafe fn dealloc(&self, ptr: *mut u8, _layout: Layout) {
        unsafe { mi_free(ptr as *mut c_void) }
    }

    #[inline]
    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        unsafe {
            if natural(layout.align(), new_size) {
                mi_realloc(ptr as *mut c_void, new_size) as *mut u8
            } else {
                mi_realloc_aligned(ptr as *mut c_void, new_size, layout.align()) as *mut u8
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    unsafe extern "C" {
        fn mi_usable_size(p: *const c_void) -> usize;
    }

    #[test]
    fn small_values_keep_their_size_class() {
        let layout = Layout::from_size_align(48, 8).unwrap();
        let ptrs: Vec<*mut u8> = (0..100_000)
            .map(|_| unsafe { MiMalloc.alloc(layout) })
            .collect();
        for &p in &ptrs {
            assert_eq!(unsafe { mi_usable_size(p as *const c_void) }, 48);
        }
        for p in ptrs {
            unsafe { MiMalloc.dealloc(p, layout) };
        }
    }

    #[test]
    fn every_alignment_is_honoured() {
        for align in [1usize, 2, 4, 8, 16, 32, 64, 4096] {
            for size in [1usize, 7, 8, 24, 48, 100, 5000] {
                let layout = Layout::from_size_align(size, align).unwrap();
                let mut ptrs = Vec::new();
                for _ in 0..64 {
                    let p = unsafe { MiMalloc.alloc(layout) };
                    assert!(!p.is_null());
                    assert_eq!(p as usize % align, 0, "alloc {size}/{align}");
                    let z = unsafe { MiMalloc.alloc_zeroed(layout) };
                    assert_eq!(z as usize % align, 0, "zalloc {size}/{align}");
                    assert!(
                        unsafe { std::slice::from_raw_parts(z, size) }
                            .iter()
                            .all(|&b| b == 0)
                    );
                    ptrs.push(p);
                    ptrs.push(z);
                }
                for p in ptrs {
                    unsafe { MiMalloc.dealloc(p, layout) };
                }
            }
        }
    }

    #[test]
    fn realloc_keeps_contents_and_alignment() {
        for align in [1usize, 8, 16, 32, 64, 4096] {
            let mut layout = Layout::from_size_align(8, align).unwrap();
            let mut p = unsafe { MiMalloc.alloc(layout) };
            unsafe { p.write_bytes(0xab, 8) };
            for new_size in [4usize, 40, 1000, 100_000, 24] {
                let kept = layout.size().min(new_size).min(4);
                p = unsafe { MiMalloc.realloc(p, layout, new_size) };
                assert_eq!(p as usize % align, 0, "realloc to {new_size}/{align}");
                assert!(
                    unsafe { std::slice::from_raw_parts(p, kept) }
                        .iter()
                        .all(|&b| b == 0xab)
                );
                layout = Layout::from_size_align(new_size, align).unwrap();
            }
            unsafe { MiMalloc.dealloc(p, layout) };
        }
    }
}
