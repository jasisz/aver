/// The compiler builds and drops terms by the million; mimalloc does that
/// much faster than the macOS system allocator, and no worse elsewhere. It is
/// the runtime's `MiMalloc`, which keeps small values in their own size class.
#[cfg(all(not(target_family = "wasm"), feature = "runtime"))]
#[global_allocator]
static GLOBAL: aver_rt::MiMalloc = aver_rt::MiMalloc;

fn main() {
    aver::cli_entry::main();
}
