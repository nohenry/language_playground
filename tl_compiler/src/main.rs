use tl_util::Rf;

fn main() {
    let mut m = Rf::new(0);

    // let red= m.clone();
    // *m.borrow_mut() = 7;

    tl_vm::run_file_new("test_files/test.xl")
}
