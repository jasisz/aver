//! Aver WASM Runtime — IO, string operations, value formatting.
//!
//! Compiled to wasm32-wasip1 and merged with user programs via wasm-merge.
//! No allocator needed — operates on raw bytes in WASM linear memory.

#![no_std]

// WASI fd_write syscall
#[link(wasm_import_module = "wasi_snapshot_preview1")]
unsafe extern "C" {
    fn fd_write(fd: i32, iovs: i32, iovs_len: i32, nwritten: i32) -> i32;
}

// ---------------------------------------------------------------------------
// Value layout constants (must match src/codegen/wasm/value.rs)
// ---------------------------------------------------------------------------

const TAG_SHIFT: u32 = 60;
const PAYLOAD_MASK: u64 = (1u64 << 60) - 1;
const TAG_INT: u64 = 0;
const TAG_CONST: u64 = 1;
const TAG_HEAP: u64 = 2;

const CONST_FALSE: u64 = 0;
const CONST_TRUE: u64 = 1;
const CONST_UNIT: u64 = 2;
const CONST_NONE: u64 = 3;
const CONST_EMPTY_LIST: u64 = 4;

const OBJ_STRING: u64 = 0;
const OBJ_RECORD: u64 = 1;
const OBJ_VARIANT: u64 = 2;
const OBJ_WRAPPER: u64 = 3;
const OBJ_LIST_CONS: u64 = 4;
const OBJ_TUPLE: u64 = 5;

const HDR_KIND_SHIFT: u32 = 56;
const HDR_TAG_SHIFT: u32 = 48;

const WRAP_OK: u64 = 0;
const WRAP_ERR: u64 = 1;
const WRAP_SOME: u64 = 2;

// ---------------------------------------------------------------------------
// Scratch buffer for formatting (fixed area in linear memory)
// We use a 1KB buffer at a fixed address. The emitter must reserve this.
// ---------------------------------------------------------------------------

/// Base address of the IO scratch buffer in linear memory.
/// Must be below heap_base and not overlap with data section.
/// The emitter sets heap_base above this.
const IO_BUF_BASE: usize = 0xFF00; // 65280 — last 256 bytes of first 64KB
const IO_BUF_SIZE: usize = 256;

// WASI iovec lives just before the buffer
const IOVEC_BASE: usize = IO_BUF_BASE - 8;

// ---------------------------------------------------------------------------
// Public API — called by generated WASM code
// ---------------------------------------------------------------------------

/// Print a tagged Value to stdout, followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn aver_print_value(val: u64) {
    print_value(val);
    write_bytes(b"\n");
}

/// Print a tagged Value to stderr, followed by a newline.
#[unsafe(no_mangle)]
pub extern "C" fn aver_print_error(val: u64) {
    print_value_to_fd(val, 2);
    write_bytes_to_fd(b"\n", 2);
}

// ---------------------------------------------------------------------------
// Value formatting
// ---------------------------------------------------------------------------

fn print_value(val: u64) {
    print_value_to_fd(val, 1); // stdout
}

fn print_value_to_fd(val: u64, fd: i32) {
    let tag = (val >> TAG_SHIFT) & 0xF;
    let payload = val & PAYLOAD_MASK;

    match tag {
        t if t == TAG_INT => {
            // Sign-extend 60-bit to i64
            let n = if payload >= (1u64 << 59) {
                (payload as i64).wrapping_sub(1i64 << 60)
            } else {
                payload as i64
            };
            let mut buf = [0u8; 21]; // max i64 decimal = 20 digits + sign
            let len = int_to_buf(n, &mut buf);
            write_bytes_to_fd(&buf[..len], fd);
        }
        t if t == TAG_CONST => match payload {
            p if p == CONST_FALSE => write_bytes_to_fd(b"false", fd),
            p if p == CONST_TRUE => write_bytes_to_fd(b"true", fd),
            p if p == CONST_UNIT => {} // Unit prints nothing (like Aver interpreter)
            p if p == CONST_NONE => write_bytes_to_fd(b"None", fd),
            p if p == CONST_EMPTY_LIST => write_bytes_to_fd(b"[]", fd),
            _ => write_bytes_to_fd(b"<const>", fd),
        },
        t if t == TAG_HEAP => {
            let handle = payload as usize;
            let header = read_u64(handle);
            let kind = (header >> HDR_KIND_SHIFT) & 0xFF;
            let variant_tag = (header >> HDR_TAG_SHIFT) & 0xFF;
            let field_count = (header & 0xFFFFFFFF) as usize;

            match kind {
                k if k == OBJ_STRING => {
                    // Read raw bytes from handle+8
                    let ptr = handle + 8;
                    write_slice_to_fd(ptr, field_count, fd);
                }
                k if k == OBJ_WRAPPER => {
                    let tag_str = match variant_tag {
                        t if t == WRAP_OK => b"Result.Ok(" as &[u8],
                        t if t == WRAP_ERR => b"Result.Err(" as &[u8],
                        t if t == WRAP_SOME => b"Option.Some(" as &[u8],
                        _ => b"Wrapper(" as &[u8],
                    };
                    write_bytes_to_fd(tag_str, fd);
                    if field_count >= 1 {
                        let inner = read_u64(handle + 8);
                        print_value_to_fd(inner, fd);
                    }
                    write_bytes_to_fd(b")", fd);
                }
                k if k == OBJ_LIST_CONS => {
                    write_bytes_to_fd(b"[", fd);
                    print_list_to_fd(handle, fd);
                    write_bytes_to_fd(b"]", fd);
                }
                k if k == OBJ_TUPLE => {
                    write_bytes_to_fd(b"(", fd);
                    for i in 0..field_count {
                        if i > 0 {
                            write_bytes_to_fd(b", ", fd);
                        }
                        let field = read_u64(handle + 8 + i * 8);
                        print_value_to_fd(field, fd);
                    }
                    write_bytes_to_fd(b")", fd);
                }
                k if k == OBJ_RECORD => {
                    // Records: just print field values for now
                    write_bytes_to_fd(b"Record(", fd);
                    for i in 0..field_count {
                        if i > 0 {
                            write_bytes_to_fd(b", ", fd);
                        }
                        let field = read_u64(handle + 8 + i * 8);
                        print_value_to_fd(field, fd);
                    }
                    write_bytes_to_fd(b")", fd);
                }
                k if k == OBJ_VARIANT => {
                    write_bytes_to_fd(b"Variant(", fd);
                    for i in 0..field_count {
                        if i > 0 {
                            write_bytes_to_fd(b", ", fd);
                        }
                        let field = read_u64(handle + 8 + i * 8);
                        print_value_to_fd(field, fd);
                    }
                    write_bytes_to_fd(b")", fd);
                }
                _ => {
                    write_bytes_to_fd(b"<object>", fd);
                }
            }
        }
        _ => {
            write_bytes_to_fd(b"<unknown>", fd);
        }
    }
}

fn print_list_to_fd(handle: usize, fd: i32) {
    let mut current = handle;
    let mut first = true;
    let mut count = 0usize;
    loop {
        let header = read_u64(current);
        let kind = (header >> HDR_KIND_SHIFT) & 0xFF;
        if kind != OBJ_LIST_CONS {
            break;
        }
        if !first {
            write_bytes_to_fd(b", ", fd);
        }
        first = false;
        let head = read_u64(current + 8);
        print_value_to_fd(head, fd);
        let tail = read_u64(current + 16);
        let tail_tag = (tail >> TAG_SHIFT) & 0xF;
        if tail_tag == TAG_HEAP {
            current = (tail & PAYLOAD_MASK) as usize;
        } else {
            break;
        }
        count += 1;
        if count > 10000 {
            write_bytes_to_fd(b", ...", fd);
            break;
        }
    }
}

// ---------------------------------------------------------------------------
// Integer formatting
// ---------------------------------------------------------------------------

fn int_to_buf(mut n: i64, buf: &mut [u8; 21]) -> usize {
    if n == 0 {
        buf[0] = b'0';
        return 1;
    }

    let negative = n < 0;
    if negative {
        n = n.wrapping_neg();
    }

    let mut pos = 21usize;
    while n > 0 {
        pos -= 1;
        buf[pos] = b'0' + (n % 10) as u8;
        n /= 10;
    }
    if negative {
        pos -= 1;
        buf[pos] = b'-';
    }

    // Shift to start of buffer
    let len = 21 - pos;
    buf.copy_within(pos..21, 0);
    len
}

// ---------------------------------------------------------------------------
// Low-level IO
// ---------------------------------------------------------------------------

fn write_bytes(data: &[u8]) {
    write_bytes_to_fd(data, 1);
}

fn write_bytes_to_fd(data: &[u8], fd: i32) {
    if data.is_empty() {
        return;
    }
    unsafe {
        let mem = 0 as *mut u8;
        // Write iovec: (ptr, len) at IOVEC_BASE
        let iovec_ptr = mem.add(IOVEC_BASE) as *mut u32;
        *iovec_ptr = data.as_ptr() as u32;
        *iovec_ptr.add(1) = data.len() as u32;
        // Call fd_write
        fd_write(fd, IOVEC_BASE as i32, 1, (IOVEC_BASE - 4) as i32);
    }
}

fn write_slice_to_fd(ptr: usize, len: usize, fd: i32) {
    if len == 0 {
        return;
    }
    unsafe {
        let mem = 0 as *mut u8;
        let iovec_ptr = mem.add(IOVEC_BASE) as *mut u32;
        *iovec_ptr = ptr as u32;
        *iovec_ptr.add(1) = len as u32;
        fd_write(fd, IOVEC_BASE as i32, 1, (IOVEC_BASE - 4) as i32);
    }
}

fn read_u64(offset: usize) -> u64 {
    unsafe {
        let mem = 0 as *const u8;
        let ptr = mem.add(offset) as *const u64;
        *ptr
    }
}

// ---------------------------------------------------------------------------
// Panic handler
// ---------------------------------------------------------------------------

#[cfg(target_arch = "wasm32")]
#[panic_handler]
fn panic(_info: &core::panic::PanicInfo) -> ! {
    core::arch::wasm32::unreachable()
}
