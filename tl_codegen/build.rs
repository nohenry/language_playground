fn main() {
    std::env::set_var("LLVM_SYS_150_PREFIX", "/opt/homebrew/opt/llvm@15");

    println!(r"cargo:rustc-link-search=/opt/homebrew/Cellar/zstd/1.5.5/lib/");
}
