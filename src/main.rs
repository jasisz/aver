/// The compiler allocates and frees many small terms; mimalloc does that
/// much faster than the macOS system allocator, and no worse elsewhere.
#[cfg(not(target_family = "wasm"))]
#[global_allocator]
static GLOBAL: mimalloc::MiMalloc = mimalloc::MiMalloc;

fn main() {
    aver::cli_entry::main();
}
