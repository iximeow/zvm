pub mod class_file;
pub mod vm;

use crate::class_file::read;
use crate::vm::VirtualMachine;

use std::fs::File;

fn main() {
    println!("zoom zoom");

    let env = std::env::args().collect::<Vec<_>>();
    let filename = &env[1];
    let methodname = &env[2];

    let class_file = read::class_header(
        &mut File::open(filename).unwrap_or_else(|_| panic!("No such file {}", filename)),
    )
    .unwrap();
    let mut vm = VirtualMachine::new();
    let class_ref = vm.load(class_file);
    let entrypoint = vm.get_method(&class_ref, methodname).unwrap();
    vm.execute(entrypoint);
}
